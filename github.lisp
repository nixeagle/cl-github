(in-package :nisp.github-system)

(defpackage #:nisp.github
  (:use :cl :json))

(in-package :nisp.github)

(defparameter +github-api-url+ "https://github.com/api/v2/"
  ;; Use only the json interface, we do not want to implement the xml or
  ;; yaml interfaces.
  "Github api location.
This is the same for every call.")

(pushnew (cons "application" "json") drakma:*text-content-types*)

(defvar *prot* nil)
(defvar *prot2* nil)

(defun github-request (uri login token &rest parameters)
  "Ask github about URI using LOGIN and TOKEN."
  (declare (type string login token))
  (drakma:http-request uri :method :post
                       :parameters
                       `(("login" . ,login)
                         ("token" . ,token)
                         ,@parameters)))


(defun set-prototype (key)
  "Make KEY the json `*PROTOTYPE*'."
  (setq json::*prototype* key))

(defun key-add-or-set (key)
  "Mark KEY a prototype if it is, and add it to the accumulator."
  (let ((key (funcall json::*json-identifier-name-to-lisp* key)))
    (print key)
    (if (and (not json::*prototype*)
             (or (string= key "USER")
                 (string= key "PLAN")))
        (progn (setq json::*accumulator-last*
                     (setf (cdr json::*accumulator-last*) (cons (cons key nil) nil)))
               (setq *prot* key)
              #+ () (pushnew (cons "PROTOTYPE" key) (cddr json::*accumulator*))
               (set-prototype t))
        (setq json::*accumulator-last*
              (setf (cdr json::*accumulator-last*) (cons (cons key nil) nil))))
    json::*accumulator*))


(defun value-add-or-set (value)
  "If VALUE (in a JSON Object being decoded) corresponds to a key which
matches *PROTOTYPE-NAME*, set VALUE to be the prototype of the Object.
Otherwise, do the same as ACCUMULATOR-ADD-VALUE."
  (if (eql json::*prototype* t)
      (progn
        (check-type value (or json::prototype string)
                    (format nil "Invalid prototype: ~S." value))
        (setq json::*prototype* *prot*)
        (print "it!")
        json::*accumulator*)
      (if nil #+ () *prot2*
          (json::accumulator-add-value *prot2*)
          (json::accumulator-add-value value))))

;;; Modified from cl-json 
(defun accumulator-get-object ()
  "Return a CLOS object, using keys and values accumulated so far in
the list accumulator as slot names and values, respectively.  If the
JSON Object had a prototype field infer the class of the object and
the package wherein to intern slot names from the prototype.
Otherwise, create a FLUID-OBJECT with slots interned in
*JSON-SYMBOLS-PACKAGE*."
  (flet ((as-symbol (value)
           (etypecase value
             (string (intern value))
             (symbol value)))
         (intern-keys (bindings)
           (loop for (key . value) in bindings
              collect (cons (json:json-intern key) value))))
    (if (typep *prot2* 'json::prototype)
        (with-slots (lisp-class lisp-superclasses lisp-package)
            *prot2*
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
             *prot2*)))
        (let ((bindings (intern-keys (cdr json::*accumulator*)))
              (class (if (stringp *prot2*) (as-symbol *prot2*))))
          (when (and *prot2* (not class))
            (push (cons json::*prototype-name* *prot2*) bindings))
          (print class)
          (json:make-object bindings class)))))

;;; JSON classes
(defclass user () ())

;;; End file
