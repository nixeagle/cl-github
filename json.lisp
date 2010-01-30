(in-package :nisp.github)

;;; From Alexandria
(defun alist-hash-table (alist &rest hash-table-initargs)
  "Returns a hash table containing the keys and values of the association list
ALIST. Hash table is initialized using the HASH-TABLE-INITARGS."
  (let ((table (apply #'make-hash-table hash-table-initargs)))
    (dolist (cons alist)
      (setf (gethash (car cons) table) (cdr cons)))
    table))

(defparameter +github-class-map+
  (alist-hash-table '(("USER" . "USER") ("PLAN" . "PLAN") ("AUTHOR" . "SIMPLE-USER")
                      ("PARENTS" . "PARENT") ("COMMIT" . "COMMIT")
                      ("MODIFIED" . "FILE-DIFF") ("COMMITTER" . "SIMPLE-USER")
                      ("DELETE-TOKEN" . "DELETE-TOKEN") ("TREE" . "TREE")
                      ("BLOB" . "BLOB") ("BLOCKS" . "BLOCK")
                      ("HEADS" . "HEAD") ("COMMITS" . "COMMITS")
                      ("REPOSITORY" . "REPOSITORY")
                      ("PUBLIC-KEYS" . "PUBLIC-KEYS")
                      ("REPOSITORIES" . "REPOSITORIES")
                      ("NETWORK" . "NETWORK") ("USERS" . "USERS")
                      ("ISSUES" . "ISSUE") ("ISSUE" . "ISSUE")
                      ("COMMENT" . "COMMENT"))
                    :test #'equal)
  "mapping of class strings to real classes.")


(defun beginning-of-object ()
  "Do more at prototype init"
  (setq *previous-prototype* *current-prototype*)
  (setq *current-prototype* nil)
  (json::init-accumulator-and-prototype))

(defun camel-case-to-lisp (string)
  (declare (type string string))
  (string-upcase (iter (for char :in-string string)
                       (if (char= #\_ char)
                           (collect #\- :result-type string)
                           (collect char :result-type string)))))

(defgeneric key-add-or-set (key)
  (:documentation "Mark KEY a prototype if it is, and add it to the accumulator."))
(defmethod key-add-or-set (key)
  (let ((key (funcall #'camel-case-to-lisp key)))
    (let ((class-key (gethash key +github-class-map+ nil)))
      (if (and (not *current-prototype*)
               class-key)
          (progn (setq json::*accumulator-last*
                       (setf (cdr json::*accumulator-last*) (cons (cons key nil) nil)))
                 (setq *current-prototype* class-key)
                 #+ () (pushnew (cons "PROTOTYPE" key) (cddr json::*accumulator*))
                 (setq json::*prototype* class-key))
          (setq json::*accumulator-last*
                (setf (cdr json::*accumulator-last*) (cons (cons key nil) nil)))))
    json::*accumulator*))

(defgeneric value-add-or-set (value)
  (:documentation "If VALUE (in a JSON Object being decoded)
corresponds to a key which matches *PROTOTYPE-NAME*,
set VALUE to be the prototype of the Object.
Otherwise, do the same as ACCUMULATOR-ADD-VALUE."))
(defmethod value-add-or-set (value)
  (if (eql json::*prototype* t)
      (progn
        (check-type value (or json::prototype string)
                    (format nil "Invalid prototype: ~S." value))
        (setq json::*prototype* *current-prototype*)
        json::*accumulator*)
      (json::accumulator-add-value value)))

(defmethod value-add-or-set :after (value)
  (setq *current-prototype* nil))

(defgeneric as-symbol (object)
  (:method ((object string))
    "Change OBJECT to a symbol by interning it."
    (intern object))
  (:method ((object symbol))
    "Return OBJECT as is."
    object)
  (:documentation "Get the symbolic representation of object."))

(defgeneric accumulator-get-object ()
  (:documentation
   "Return a CLOS object, using keys and values accumulated so far in
the list accumulator as slot names and values, respectively.  If the
JSON Object had a prototype field infer the class of the object and
the package wherein to intern slot names from the prototype.
Otherwise, create a FLUID-OBJECT with slots interned in
*JSON-SYMBOLS-PACKAGE*."))

;;; Modified from cl-json 
(defmethod accumulator-get-object ()
  (flet ((intern-keys (bindings)
           (loop for (key . value) in bindings
              collect (cons (json:json-intern key) value))))
    (if (typep *previous-prototype* 'json::prototype)
        (with-slots (lisp-class lisp-superclasses lisp-package)
            *previous-prototype*
          (let* ((package-name (as-symbol lisp-package))
                 (json:*json-symbols-package*
                  (if package-name
                      (or (find-package package-name)
                          (error 'package-error :package package-name))
                      json::*json-symbols-package*))
                 (class (as-symbol lisp-class))
                 (superclasses (mapcar #'as-symbol lisp-superclasses)))
            (json::maybe-add-prototype
             (json:make-object (intern-keys (cdr json::*accumulator*))
                               class superclasses)
             *previous-prototype*)))
        (let ((bindings (intern-keys (cdr json::*accumulator*)))
              (class (if (stringp *previous-prototype*) (as-symbol *previous-prototype*))))
          (when (and *previous-prototype* (not class))
            (push (cons json::*prototype-name* *previous-prototype*) bindings))
          (if (and (not class) (listp bindings) (not (consp (cdr bindings))))
              (cdar bindings)
              (json:make-object bindings class))))))

(defmacro with-github-decoder (&body body)
  "Execute BODY with decoder bindings appropriate for github's api."
  `(json:bind-custom-vars
      (:beginning-of-object #'beginning-of-object 
                            :object-key #'key-add-or-set 
                            :object-value #'value-add-or-set
                            :end-of-object #'accumulator-get-object 
                            :object-scope '(json:*INTERNAL-DECODER*
                                            *current-prototype*
                                            *previous-prototype*))
     ,@body))

(defgeneric to-json (object)
  (:method :around (obj)
           (let ((json:*json-symbols-package* :nisp.github))
             (with-local-class-registry (:inherit nil)
               (call-next-method)))))
(defmethod to-json ((obj string))
  (with-github-decoder 
    (json:decode-json-from-string obj)))
(defmethod to-json ((obj stream))
  "Read directly from a stream and close the stream when done."
  (prog1 (with-github-decoder
           (json:decode-json obj))
    (close obj)))

(defgeneric json->alist (object))
(defmethod json->alist ((object stream))
  (with-decoder-simple-list-semantics
    (decode-json object)))
(defmethod json->alist :after ((object stream))
  (close object))

(defun ensure-list (object)
  "Ensure OBJECT is a list."
  (the list (if (listp object)
                object
                (list object))))
(defgeneric json->list (object))
(defmethod json->list ((object stream))
  (ensure-list (cdar (with-decoder-simple-list-semantics
                       (decode-json object)))))
(defmethod json->list :after ((object stream))
  (close object))

(defgeneric json->element (object)
  (:documentation "Return first element of OBJECT's json conversion result."))
(defmethod json->element ((object stream))
  (car (json->list object)))

(defgeneric json->class (object class)
  (:documentation "Store json in OBJECT to CLASS"))

(defmethod json->class ((object stream)
                        (class symbol))
  "Store json from STREAM in an instance of CLASS."
  (make-object (with-decoder-simple-list-semantics
                 (decode-json object))
               class))

(defmethod json->class :around (object class)
  "Set package to nisp.github and use local class registry."
  (let ((json:*json-symbols-package* :nisp.github))
    (with-local-class-registry (:inherit nil)
      (call-next-method))))

(defmethod json->class :after ((object stream) class)
  "Close STREAM after we are done with it."
  (close object))
