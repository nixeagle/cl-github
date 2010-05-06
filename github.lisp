(in-package :cl-github)

(defparameter +github-api-url+ "http://github.com/api/v2/json"
  ;; Use only the json interface, we do not want to implement the xml or
  ;; yaml interfaces.
  "Github api location.
This is the same for every call.")

(defparameter +github-ssl-api-url+ "https://github.com/api/v2/json"
  ;; Use only the json interface, we do not want to implement the xml or
  ;; yaml interfaces.
  "Github api location.
This is the same for every call.")

(defvar *default-login* ""
  "Default user to log in as when possible.")
(defvar *default-token* ""
  "Default token to use when possible.")


(defmacro with-github-content-types (&body body)
  "Evaluate BODY treating application/json as text."
  `(let ((drakma:*text-content-types* '(("application" . "json")
                                        ("text" . nil))))
     ,@body))

(defun github-request->alist (&rest parameters)
  "Ask github about PARAMETERS and return them as an alist."
  (let ((result (apply #'github-simple-request parameters)))
    (prog1 (with-decoder-simple-list-semantics
             (let ((json:*json-symbols-package* :nisp.github))
               (decode-json result)))
      (close result))))

(defun github-request (&rest args
                       &key login token auth base-url
                       parameters method want-string &allow-other-keys)
  (let ((login (or login (and (member auth '(:default :force)) *default-login*)))
        (token (or token (and (member auth '(:default :force)) *default-token*)))
        (base-url (or base-url (if (and login token)
                                   +github-ssl-api-url+
                                   +github-api-url+))))
    (when (eq :force auth)
      (check-type login string)
      (check-type token string))
    (with-github-content-types
      (drakma:http-request (apply #'build-github-api-url
                                  base-url parameters)
                           :method (or method (if (and login token) :post :get))
                           :REDIRECT t
                           :want-stream (if want-string nil t)
                           :parameters
                           (apply #'build-parameters :login login :token token
                                  args)))))

(defun request (login token uri-parameters &rest args &key
                &allow-other-keys)
  (apply #'github-request :login login :token token :auth :default
         :parameters uri-parameters args))

(defun authed-request (login token uri-parameters &rest args &key
                       &allow-other-keys)
  (apply #'github-request :login login :token token :auth :force
         :parameters uri-parameters args))

(defun github-simple-request (&rest parameters)
  "Ask github about PARAMETERS."
  (github-request :parameters parameters))

(defun dash-to-underscore (string)
  "Change all instances of - to _ in STRING."
  (iter (for s :in-string string)
        (if (char= #\- s)
            (collect #\_ :result-type string)
            (collect s :result-type string))))

(defun build-parameters (&rest args &key parameters &allow-other-keys)
  "Convert ARGS to an alist of parameters."
  (declare (ignore parameters))
  (iter (generate arg in args)
        (let ((key (next arg))
              (value (next arg)))
          (when (and value (not (eq :parameters key))
                     (not (eq :auth key))
                     (not (eq :method key))
                     (not (eq :want-string key)))
            (collect (cons (dash-to-underscore
                            (string-downcase (symbol-name key))) value))))))

;;; Class related generics.

;;; JSON classes


(defclass status ()
  (status)
  (:documentation "Result status from github api"))

(defclass blob ()
  (name size sha data mode mime-type)
  (:documentation "Git blob that we get from github."))

(defclass treeish ()
  (name sha mode type)
  (:documentation "Treeish git object that we get from github."))


;;; utils
(defun build-github-api-url (&rest parameters)
  "Build a request url using PARAMETERS."
  (reduce (lambda (prior new)
            (if new
                (concatenate 'string prior "/" (url-encode new))
                prior))
          parameters))

(defmethod make-object :before (bindings
                                (class (eql nil))
                                &optional superclasses)
  "Debug helper to print the keys of BINDINGS."
  (declare (ignore superclasses))
  (write (mapcar #'car bindings)
         :case :downcase))

(defmacro not-done (&rest ignores)
  "Throw an error saying not done."
  `(progn (proclaim (list 'ignore ,@ignores))
          (error "Not done!")))

;;; API calls

;;; Object API
(defgeneric show-tree (username repository tree &key login token)
  (:documentation "List treeish objects for USERNAME's REPOSITORY at TREE."))
(defgeneric show-blob (username repository path tree &key login token)
  (:documentation "Show contents of the file at PATH in USERNAME's REPOSITORY."))
(defgeneric show-raw-blob (username repository sha &key login token)
  (:documentation "Show raw contents of SHA in USERNAME's REPOSITORY."))

(defmethod show-tree ((username string) (repository string)
                      (tree string) &key login token)
  (to-json (request login token `("tree" "show" ,username ,repository ,tree))))

(defmethod show-blob ((username string) (repository string)
                      (path string) (tree string) &key login token)
  (to-json (request login token `("blob" "show" ,username ,repository ,tree ,path))))
(defmethod show-raw-blob ((username string) (repository string)
                          (sha string) &key login token)
  (github-request :login login :token token :auth :default
                  :parameters `("blob" "show" ,username ,repository ,sha)
                  :want-string t))

(defun follow-user (username &key token login)
  "Follow USERNAME returning the followed username as a string."
  (declare (string username))
  (find username (follow username :token token :login login) :test #'equal))

(defpackage #:cl-github-extra
  (:use :cl :iterate :cl-github)
  (:export #:show-followers-not-followed))
(in-package :cl-github-extra)
;;; Extra
(defun show-followers-not-followed (username)
  "Show followers that USERNAME is not following."
  ;; Thanks to scott olson for the idea.
  (set-difference (show-followers username) (show-following username)
                  :test #'equal))

;;; End file
