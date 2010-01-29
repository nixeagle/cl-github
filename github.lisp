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
                       &key login token auth
                       parameters &allow-other-keys)
  (let ((login (or login (and (member auth '(:default :force)) *default-login*)))
        (token (or token (and (member auth '(:default :force)) *default-token*))))
    (when (eq :force auth)
      (check-type login string)
      (check-type token string))
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

(defun build-parameters (&rest args &key parameters &allow-other-keys)
  "Convert ARGS to an alist of parameters."
  (declare (ignore parameters))
  (iter (generate arg in args)
        (let ((key (next arg))
              (value (next arg)))
          (print key)
          (when (and value (not (eq :parameters key))
                     (not (eq :auth key)))
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
(defgeneric set-repository-private (repository &key login token)
  (:documentation "Mark REPOSITORY as private on github."))
(defgeneric unfollow-user (user-login pass username)
  (:documentation "Unfollow USERNAME using USER-LOGIN."))
(defgeneric show-repository (username reponame &key login token)
  (:documentation "Show information on USERNAME's REPONAME."))
(defgeneric add-user-email (email &key login token)
  (:documentation "Add EMAIL to LOGIN's email list."))
(defgeneric remove-user-key (id &key login token)
  (:documentation "REMOVE KEY by ID from LOGIN's key list.

ID can be either a string or a positive number."))
(defgeneric show-followers (username)
  (:documentation "List all followers of USERNAME."))
(defgeneric deply-keys (repository &key login token)
  (:documentation "List REPOSITORY's deploy keys.

These are basically read only ssh keys."))
(defgeneric emails (object)
  (:documentation "NIL"))
(defgeneric show-following (username)
  (:documentation "List all users that USERNAME follows."))
(defgeneric show-commits (username repository branch &key file login token)
  (:documentation "List commits in USERNAME's REPOSITORY on BRANCH optionally for FILE."))
(defgeneric show-branches (username repository &key login token)
  (:documentation "List REPOSITORY's remote branches."))
(defgeneric add-deploy-key (repository &key title key login token)
  (:documentation "Add KEY named TITLE as a deploy key for REPOSITORY."))
(defgeneric watch (username repository &key login token)
  (:documentation "Watch REPOSITORY owned by USERNAME."))
(defgeneric delete-repository (repository &key login token)
  (:documentation "Delete REPOSITORY on github."))
(defgeneric watch-repository (username repository)
  (:documentation "Watch REPOSITORY owned by USERNAME."))
(defgeneric repository-network (username repository)
  (:documentation "Look at network of USERNAME's REPOSITORY."))
(defgeneric show-commit (username repository sha &key login token)
  (:documentation "Show data for commit identified by SHA on USERNAME's REPOSITORY."))
(defgeneric user-emails (&key login token)
  (:documentation "List all emails LOGIN uses."))
(defgeneric show-tags (username repository &key login token)
  (:documentation "List REPOSITORY's tags."))
(defgeneric search-users (username)
  (:documentation "Search github for USERNAME."))
(defgeneric json->class (object class)
  (:documentation "Store json in OBJECT to CLASS"))
(defgeneric remove-user-email (email &key login token)
  (:documentation "Remove EMAIL from LOGIN's email list."))
(defgeneric branches (object)
  (:documentation "NIL"))
(defgeneric fork-repository (username repository)
  (:documentation "Fork REPOSITORY owned by USERNAME."))
(defgeneric unfollow (username &key login token)
  (:documentation "Unfollow USERNAME using LOGIN."))
(defgeneric fork (username repository &key login token)
  (:documentation "Fork REPOSITORY owned by USERNAME."))
(defgeneric show-network (username repository &key login token)
  (:documentation "Show at network of USERNAME's REPOSITORY."))
(defgeneric create-repository (repository &key login token description
                                          homepage public)
  (:documentation "Create new REPOSITORY on github."))
(defgeneric user-keys (&key login token)
  (:documentation "List all public keys LOGIN uses."))
(defgeneric add-user-key (name key &key login token)
  (:documentation "Add KEY to LOGIN's key list."))
(defgeneric remove-collaborator (username repository &key login token)
  (:documentation "Remove USERNAME from the collaborators list of REPOSITORY."))
(defgeneric set-prototype (key)
  (:documentation "Make KEY the json `*PROTOTYPE*'."))
(defgeneric languages (object)
  (:documentation "NIL"))
(defgeneric collaborators (username repository &key login token)
  (:documentation "List collaborators on REPOSITORY owned by USERNAME."))
(defgeneric follow-user (username &key login token)
  (:documentation "Follow USERNAME using USER-LOGIN."))
(defgeneric search-repositories (search-string)
  (:documentation "Search github repositories for SEARCH-STRING."))
(defgeneric add-collaborator (username repository &key login token)
  (:documentation "Add USERNAME to the collaborators list of REPOSITORY."))
(defgeneric unwatch-repository (username repository)
  (:documentation "Stop watching REPOSITORY owned by USERNAME."))
(defgeneric watched-repositories (username)
  (:documentation "List repositories USERNAME watches."))
(defgeneric tags (object)
  (:documentation "NIL"))
(defgeneric remove-deploy-key (repository &key id login token)
  (:documentation "Remove key identified by ID as a deploy key for REPOSITORY."))
(defgeneric follow (username &key login token)
  (:documentation "Follow USERNAME using USER-LOGIN."))
(defgeneric show-languages (username repository &key login token)
  (:documentation "List REPOSITORY's languages."))
(defgeneric show-user (user &key login token name blog email company location)
  (:documentation "NIL"))
(defgeneric unwatch (username repository &key login token)
  (:documentation "Stop watching REPOSITORY owned by USERNAME."))
(defgeneric set-repository-public (repository &key login token)
  (:documentation "Mark REPOSITORY as public on github."))
(defgeneric user-repositories (username)
  (:documentation "List USERNAME's repositories."))



(defmethod show-user ((user string)
                      &key login token name blog email company location)
  (slot-value (to-json (github-request :parameters `("user" "show" ,user)
                                       :auth (when (string= user *default-login*)
                                               :force)
                                       :login login
                                       :token token
                                       :values\[blog\] blog
                                       :values\[name\] name
                                       :values\[email\] email
                                       :values\[company\] company
                                       :values\[location\] location)) 'user))

(defmethod show-followers ((username string))
  (json->class (github-simple-request "user" "show" username "followers")
               'followers))

(defmethod show-following ((username string))
  (json->class (github-simple-request "user" "show" username "following")
               'following))

(defmethod follow ((username string) &key login token)
  (json->class (authed-request login token `("user" "follow" ,username))
               'following))
               
(defmethod unfollow ((username string) &key login token)
  ;; Github seems to ignore this request.
  (json->class (authed-request login token `("user" "unfollow" ,username))
               'following))

(defmethod watched-repositories ((username string))
  (slot-value
   (to-json (github-simple-request "repos" "watched" username))
   'repositories))

(defmethod user-emails (&key login token)
  (json->class (authed-request login token '("user" "emails"))
               'emails))

(defmethod add-user-email ((email string) &key login token)
  (json->class (authed-request login token '("user" "email" "add")
                               :email email)
               'emails))

(defmethod remove-user-email ((email string) &key login token)
  (json->class (authed-request login token '("user" "email" "remove")
                               :email email)
               'emails))

(defmethod user-keys (&key login token)
  (slot-value (to-json (authed-request login token '("user" "keys")))
              'public-keys))

(defmethod add-user-key ((name string) (key string) &key login token)
  (slot-value (to-json (authed-request login token '("user" "key" "add")
                                       :name name :key key))
              'public-keys))

(defmethod remove-user-key ((id string) &key login token)
  (slot-value (to-json (authed-request login token '("user" "key" "remove")
                                       :id (princ-to-string id)))
              'public-keys))


(defmethod search-users ((username string))
  (slot-value (to-json (github-simple-request "user" "search" username))
              'users))

;;; Repositories
(defmethod search-repositories ((search-string string))
  (slot-value (to-json (github-simple-request "repos" "search" search-string))
              'repositories))

(defmethod show-repository ((username string) repository &key login token)
  (slot-value
   (to-json (request login token `("repos" "show" ,username ,repository)))
              'repository))

(defmethod user-repositories ((username string))
  (slot-value (to-json (github-simple-request "repos" "show" username))
              'repositories))

(defmethod watch ((username string) (repository string) &key login token)
  (slot-value
   (to-json
    (request login token `("repos" "watch" ,username ,repository)))
   'repository))

(defmethod unwatch ((username string) (repository string) &key login token)
  (slot-value
   (to-json
    (authed-request login token `("repos" "unwatch" ,username ,repository)))
   'repository))

(defmethod fork ((username string) (repository string) &key login token)
  (slot-value
   (to-json
    (authed-request login token `("repos" "fork" ,username ,repository)))
   'repository))

(defmethod create-repository ((repository string) &key login token
                          description homepage
                          ;; Default to public.
                          (public 1))
  (not-done repository description homepage public login token))

(defmethod delete-repository ((repository string) &key login token)
  (not-done repository login token))

(defmethod set-repository-private ((repository string) &key login token)
  (not-done repository login token))

(defmethod set-repository-public ((repository string) &key login token)
  (not-done repository login token))

(defmethod deply-keys ((repository string) &key login token)
  (not-done repository login token))

(defmethod add-deploy-key ((repository string) &key title key login token)
  (not-done repository title key login token))

(defmethod remove-deploy-key ((repository string) &key id login token)
  (not-done repository id login token))

(defmethod collaborators ((username string) (repository string) &key login token)
  (json->class (request login token `("repos" "show" ,username
                                              ,repository "collaborators"))
               'collaborators))

(defmethod add-collaborator ((username string) (repository string) &key login token)
  (json->class
   (authed-request login token `("repos" "collaborators" ,repository
                                         "add" ,username))
   'collaborators))

(defmethod remove-collaborator ((username string) (repository string) &key login token)
  (json->class
   (authed-request login token `("repos" "collaborators" ,repository
                                                "remove" ,username))
   'collaborators))

(defmethod show-network ((username string) (repository string) &key login token)
  (slot-value
   (to-json (authed-request login token `("repos" "show" ,username
                                                  ,repository "network")))
   'network))

(defmethod show-languages ((username string) (repository string) &key login token)
  (json->class (request login token `("repos" "show" ,username
                                              ,repository "languages"))
               'languages))

(defmethod show-tags ((username string) (repository string) &key login token)
  (json->class (request login token `("repos" "show" ,username ,repository "tags"))
               'tags))

(defmethod show-branches ((username string) (repository string) &key login token)
  (json->class
   (request login token `("repos" "show" ,username ,repository "branches"))
               'branches))

(defmethod show-commits ((username string) (repository string) (branch string)
                         &key file login token)
  (slot-value
   (to-json (request login token `("commits" "list" ,username
                                                    ,repository ,branch ,file)))
   'commits))

(defmethod show-commit ((username string) (repository string) (sha string)
                        &key login token)
  (slot-value (to-json (request login token `("commits" "show" ,username
                                                               ,repository ,sha)))
              'commit))

;;; End file
