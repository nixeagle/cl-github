(in-package :nisp.github-system)

(defpackage #:nisp.github
  (:use :cl :json :iterate))

(in-package :nisp.github)

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

(pushnew (cons "application" "json") drakma:*text-content-types*)

(defvar *current-prototype* nil
  "Stores the key of an object until its stored in `*PREVIOUS-PROTOTYPE*'.")
(defvar *previous-prototype* nil
  "Stores the prototype of the json class above the current one.

For example: {\"user\":{\"plan\":{\"name\":....}}}
When parsing the plan json object, this will be set to \"USER\".")

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
                       &key login token 
                       parameters &allow-other-keys)
  
  (with-github-content-types
    (drakma:http-request (apply #'build-github-api-url
                                (if (and login token)
                                    +github-ssl-api-url+
                                    +github-api-url+) parameters)
                         :method (if (and login token) :post :get)
                         :REDIRECT t
                         :want-stream t
                         :parameters
                         (apply #'build-parameters :login login :token token
                                args))))

(defun github-simple-request (&rest parameters)
  "Ask github about PARAMETERS."
  (github-request :parameters parameters))

(defun github-authed-request (&rest args
                              &key login token 
                              parameters &allow-other-keys)
  (declare (ignore parameters))
  (let ((login (or login *default-login*))
        (token (or token *default-token*)))
    (check-type login string)
    (check-type token string)
    (apply #'github-request :login login :token token args)))

(defun build-parameters (&rest args &key parameters &allow-other-keys)
  "Convert ARGS to an alist of parameters."
  (declare (ignore parameters))
  (iter (generate arg in args)
        (let ((key (next arg))
              (value (next arg)))
          (print key)
          (when (and value (not (eq :parameters key)))
            (collect (cons (string-downcase (symbol-name key)) value))))))

(defun set-prototype (key)
  "Make KEY the json `*PROTOTYPE*'."
  (setq json::*prototype* key))

(defun beginning-of-object ()
  "Do more at prototype init"
  (setq *previous-prototype* *current-prototype*) (setq *current-prototype* nil)
  (json::init-accumulator-and-prototype))

(defun camel-case-to-lisp (string)
  (declare (type string string))
  (string-upcase (iter (for char :in-string string)
                       (if (char= #\_ char)
                           (collect #\- :result-type string)
                           (collect char :result-type string)))))

(defun key-add-or-set (key)
  "Mark KEY a prototype if it is, and add it to the accumulator."
  (let ((key (funcall #'camel-case-to-lisp key)))
    (if (and (not *current-prototype*)
             (or (string= key "USER")
                 (string= key "PLAN")
                 (string= key "AUTHOR")
                 (string= key "PARENTS")
                 (string= key "COMMIT")
                 (string= key "MODIFIED")
                 (string= key "COMMITTER")
                 (string= key "COMMITS")
                 (string= key "REPOSITORY")
                 (string= key "PUBLIC-KEYS")
                 (string= key "REPOSITORIES")
                 (string= key "NETWORK")
                 (string= key "USERS")))
        (progn (setq json::*accumulator-last*
                     (setf (cdr json::*accumulator-last*) (cons (cons key nil) nil)))
               (setq *current-prototype* key)
              #+ () (pushnew (cons "PROTOTYPE" key) (cddr json::*accumulator*))
               (set-prototype t))
        (setq json::*accumulator-last*
              (setf (cdr json::*accumulator-last*) (cons (cons key nil) nil))))
    json::*accumulator*))


(defun value-add-or-set (value)
  "If VALUE (in a JSON Object being decoded) corresponds to a key which
matches *PROTOTYPE-NAME*, set VALUE to be the prototype of the Object.
Otherwise, do the same as ACCUMULATOR-ADD-VALUE."
  (prog1
      (if (eql json::*prototype* t)
          (progn
            (check-type value (or json::prototype string)
                        (format nil "Invalid prototype: ~S." value))
            (setq json::*prototype* *current-prototype*)
        
            (print "here")
            json::*accumulator*)
          (json::accumulator-add-value value))
    (setq *current-prototype* nil)))

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
          (print class)
          (json:make-object bindings class)))))

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

;;; JSON classes
(defclass user ()
  (plan gravatar-id name company location created-at
        collaborators disk-usage
        public-gist-count public-repo-count
        blog following-count id private-gist-count
        owned-private-repo-count total-private-repo-count
        followers-count login email))

(defclass plan ()
  (name collaborators space private-repos))

(defclass users ()
  (name location followers username language fullname
        repos id type pushed score created)
  (:documentation "Describes a github user search result."))

(defclass followers ()
  (users)
  (:documentation "List of users following someone."))
(defclass following ()
  (users)
  (:documentation "List of users someone follows."))

(defclass repository ()
  (description forks url homepage watchers fork open-issues private name owner))

(defclass watched-repository () 
  (description forks url homepage watchers fork open-issues
               private name owner)
  ;; currently used only for WATCHED-REPOSITORIES.
  (:documentation "Repository information."))

(defclass searched-repository ()
  (name size followers username language fork id type pushed
        forks description score created)
  (:documentation "Search repository result information."))

(defclass repositories (watched-repository searched-repository) ()
  (:documentation "Workaround for cl-json.

Basically objects with a key named REPOSITORIES have different values
depending on what action is being done with github. For now we use an
abstract class that inherits all the conflicting classes so that at all
times the result object at least makes sense and has no missing
slots."))

(defclass collaborators ()
  (collaborators)
  (:documentation "List of collaborators."))

(defclass network (repositories)
  ()
  (:documentation "A network is just another name for repositories."))

(defclass languages ()
  ((languages :reader languages))
  (:documentation "List of languages."))

(defclass tags ()
  ((tags :reader tags))
  (:documentation "List of tags on a repository."))

(defclass branches ()
  ((branches :reader branches))
  (:documentation "List of branches on a repository."))

(defclass parents ()
  (id)
  ;; Yes this is a little strange... but this is how github does it, it
  ;; can be cleaned up later.
  (:documentation "The id for the parent commit."))
(defclass contact-data ()
  (email login name)
  (:documentation "Person information."))

(defclass committer (contact-data) ())
(defclass author (contact-data) ())

(defclass commits ()
  (author authored-date committed-date committer
          id message parents tree url)
  (:documentation "A commit object."))

(defclass commit ()
  (added modified removed parents author url id committed-date
         authored-date message tree committer)
  (:documentation "Detailed information on a commit."))

(defclass modified ()
  (diff filename)
  (:documentation "Modification information for a commit."))

(defclass emails ()
  ((emails :reader emails))
  (:documentation "List of user emails."))

(defclass public-keys ()
  (title id key)
  (:documentation "Information on a public key."))

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
(defun show-user (user &key login token name blog email company location)
  (slot-value (to-json (github-request :parameters `("user" "show" ,user)
                                       :login login
                                       :token token
                                       :values\[blog\] blog
                                       :values\[name\] name
                                       :values\[email\] email
                                       :values\[company\] company
                                       :values\[location\] location)) 'user))

(defun show-followers (username)
  "List all followers of USERNAME."
  (declare (type string username))
  (json->class (github-simple-request "user" "show" username "followers")
               'followers))

(defun show-following (username)
  "List all users that USERNAME follows."
  (declare (type string username))
  (json->class (github-simple-request "user" "show" username "following")
               'following))

(defun follow (username &key login token)
  "Follow USERNAME using USER-LOGIN."
  (declare (type string username))
  (json->class (github-authed-request :login login :token token
                                      :parameters `("user" "follow" ,username))
               'following))

(defun unfollow (username &key login token)
  "Unfollow USERNAME using LOGIN."
  ;; Github seems to ignore this request.
  (declare (type string username))
  (json->class (github-authed-request :login login :token token
                                      :parameters `("user" "unfollow" ,username))
               'following))

(defun watched-repositories (username)
  "List repositories USERNAME watches."
  ;; Not 100% sure I like these named REPOSITORIES.
  (slot-value
   (to-json (github-simple-request "repos" "watched" username))
   'repositories))

(defun user-emails (&key login token)
  "List all emails LOGIN uses."
  (json->class (github-authed-request :login login :token token
                                      :parameters '("user" "emails"))
               'emails))

(defun add-user-email (email &key login token)
  "Add EMAIL to LOGIN's email list."
   (json->class (github-authed-request :login login :token token :email email
                                      :parameters '("user" "email" "add"))
               'emails))

(defun remove-user-email (email &key login token)
  "Remove EMAIL from LOGIN's email list."
  (json->class (github-authed-request :login login :token token :email email
                                      :parameters '("user" "email" "remove"))
               'emails))

(defun user-keys (&key login token)
  "List all public keys LOGIN uses."
  (slot-value (to-json (github-authed-request :login login :token token
                                              :parameters '("user" "keys")))
              'public-keys))

(defun add-user-key (name key &key login token)
  "Add KEY to LOGIN's key list."
  (slot-value (to-json (github-authed-request :login login :token token
                                              :name name :key key
                                              :parameters '("user" "key" "add")))
              'public-keys))

(defun remove-user-key (id &key login token)
  "REMOVE KEY by ID from LOGIN's key list.

ID can be either a string or a positive number."
  (slot-value (to-json (github-authed-request :login login :token token
                                              :id (princ-to-string id) 
                                              :parameters '("user" "key" "remove")))
              'public-keys))


(defun search-users (username)
  "Search github for USERNAME."
  (declare (type string username))
  (slot-value (to-json (github-simple-request "user" "search" username))
              'users))

;;; Repositories
(defun search-repositories (search-string)
  "Search github repositories for SEARCH-STRING."
  (declare (type string search-string))
  (slot-value (to-json (github-simple-request "repos" "search" search-string))
              'repositories))

(defun show-repository (username reponame &key login token)
  "Show information on USERNAME's REPONAME."
  (declare (type string username reponame))
  (slot-value (to-json (github-request :login login :token token
                        :parameters `("repos" "show" ,username ,reponame)))
              'repository))

(defun user-repositories (username)
  "List USERNAME's repositories."
  (declare (type string username))
  (slot-value (to-json (github-simple-request "repos" "show" username))
              'repositories))

(defun watch (username repository &key login token)
  "Watch REPOSITORY owned by USERNAME."
  (declare (type string username repository))
  (slot-value
   (to-json
    (github-authed-request :login login :token token
                           :parameters `("repos" "watch" ,username ,repository)))
   'repository))

(defun unwatch (username repository &key login token)
  "Stop watching REPOSITORY owned by USERNAME."
  (declare (type string username repository))
  (slot-value
   (to-json
    (github-authed-request :login login :token token
                           :parameters `("repos" "unwatch" ,username ,repository)))
   'repository))

(defun fork (username repository &key login token)
  "Fork REPOSITORY owned by USERNAME."
  (declare (type string username))
  (not-done username repository login token))

(defun create-repository (repository &key login token
                          description homepage
                          ;; Default to public.
                          (public 1))
  "Create new REPOSITORY on github."
  (declare (type string repository description homepage)
           (type (integer 0 1) public))
  (not-done repository description homepage public login token))

(defun delete-repository (repository &key login token)
  "Delete REPOSITORY on github."
  (declare (type string repository))
  (not-done repository login token))

(defun set-repository-private (repository &key login token)
  "Mark REPOSITORY as private on github."
  (declare (type string repository))
  (not-done repository login token))

(defun set-repository-public (repository &key login token)
  "Mark REPOSITORY as public on github."
  (declare (type string repository))
  (not-done repository login token))

(defun deply-keys (repository &key login token)
  "List REPOSITORY's deploy keys.

These are basically read only ssh keys."
  ;; Thanks charlie.
  (declare (type string repository))
  (not-done repository login token))

(defun add-deploy-key (repository &key title key login token)
  "Add KEY named TITLE as a deploy key for REPOSITORY."
  (declare (type string repository title key))
  (not-done repository title key login token))

(defun remove-deploy-key (repository &key id login token)
  "Remove key identified by ID as a deploy key for REPOSITORY."
  (declare (type string repository)
           (type fixnum id))
  (not-done repository id login token))

(defun collaborators (username repository)
  "List collaborators on REPOSITORY owned by USERNAME."
  (declare (type string username repository))
  (json->class (github-request "repos" "show" username repository "collaborators")
               'collaborators))

(defun add-collaborator (username repository &key login token)
  "Add USERNAME to the collaborators list of REPOSITORY."
  (declare (type string username repository))
  (not-done username repository login token))

(defun remove-collaborator (username repository &key login token)
  "Remove USERNAME from the collaborators list of REPOSITORY."
  (declare (type string username repository))
  (not-done username repository login token))

(defun show-network (username repository)
  "Show at network of USERNAME's REPOSITORY."
  (slot-value
   (to-json (github-request "repos" "show" username repository "network"))
   'network))

(defun show-languages (username repository)
  "List REPOSITORY's languages."
  (declare (type string username repository))
  (json->class (github-request "repos" "show" username repository "languages")
               'languages))

(defun show-tags (username repository)
  "List REPOSITORY's tags."
  (declare (type string username repository))
  (json->class (github-request "repos" "show" username repository "tags")
               'tags))

(defun show-branches (username repository)
  "List REPOSITORY's remote branches."
  (declare (type string username repository))
  (json->class (github-request "repos" "show" username repository "branches")
               'branches))

(defun show-commits (username repository branch &optional file)
  "List commits in USERNAME's REPOSITORY on BRANCH optionally for FILE."
  (declare (type string username repository branch))
  (slot-value
   (to-json (github-request "commits" "list" username repository branch file))
   'commits))

(defun show-commit (username repository sha)
  "Show data for commit identified by SHA on USERNAME's REPOSITORY."
  (declare (type string username repository sha))
  (slot-value (to-json (github-request "commits" "show" username repository sha))
              'commit))

;;; End file
