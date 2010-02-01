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

(defclass parent ()
  (id)
  ;; Yes this is a little strange... but this is how github does it, it
  ;; can be cleaned up later.
  (:documentation "The id for the parent commit."))

(defclass network-data-commit ()
  (message time parents date author id space gravatar login)
  (:documentation "We get commit data like this from the Network API."))

(defclass commits (network-data-commit)
  (author authored-date committed-date committer
          id message parents tree url)
  (:documentation "A commit object."))

(defclass commit ()
  (added modified removed parents author url id committed-date
         authored-date message tree committer)
  (:documentation "Detailed information on a commit."))

(defclass file-diff ()
  (diff filename)
  (:documentation "Modification information for a commit."))

(defclass public-key ()
  (title id key)
  (:documentation "Information on a public key."))

(defclass delete-token ()
  ((delete-token :reader delete-token))
  (:documentation "Token github gives us to confirm deletion."))

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
(defgeneric set-repository-private (repository &key login token)
  (:documentation "Mark REPOSITORY as private on github."))

(defgeneric show-repository (username reponame &key login token)
  (:documentation "Show information on USERNAME's REPONAME."))




(defgeneric deply-keys (repository &key login token)
  (:documentation "List REPOSITORY's deploy keys.

These are basically read only ssh keys."))
(defgeneric show-commits (username repository branch &key file login token)
  (:documentation "List commits in USERNAME's REPOSITORY on BRANCH optionally for FILE."))
(defgeneric show-branches (username repository &key login token)
  (:documentation "List REPOSITORY's remote branches."))
(defgeneric add-deploy-key (repository title key &key login token)
  (:documentation "Add KEY named TITLE as a deploy key for REPOSITORY."))

(defgeneric delete-repository (repository &key login token)
  (:documentation "Delete REPOSITORY on github."))
(defgeneric repository-network (username repository)
  (:documentation "Look at network of USERNAME's REPOSITORY."))
(defgeneric show-commit (username repository sha &key login token)
  (:documentation "Show data for commit identified by SHA on USERNAME's REPOSITORY."))

(defgeneric show-tags (username repository &key login token)
  (:documentation "List REPOSITORY's tags."))
(defgeneric json->class (object class)
  (:documentation "Store json in OBJECT to CLASS"))
(defgeneric branches (object)
  (:documentation "NIL"))
(defgeneric fork-repository (username repository)
  (:documentation "Fork REPOSITORY owned by USERNAME."))
(defgeneric fork (username repository &key login token)
  (:documentation "Fork REPOSITORY owned by USERNAME."))
(defgeneric show-network (username repository &key login token)
  (:documentation "Show at network of USERNAME's REPOSITORY."))
(defgeneric create-repository (repository &key login token description
                                          homepage public)
  (:documentation "Create new REPOSITORY on github."))
(defgeneric remove-collaborator (username repository &key login token)
  (:documentation "Remove USERNAME from the collaborators list of REPOSITORY."))
(defgeneric set-prototype (key)
  (:documentation "Make KEY the json `*PROTOTYPE*'."))
(defgeneric languages (object)
  (:documentation "NIL"))
(defgeneric show-collaborators (username repository &key login token)
  (:documentation "List collaborators on REPOSITORY owned by USERNAME."))
(defgeneric search-repositories (search-string)
  (:documentation "Search github repositories for SEARCH-STRING."))
(defgeneric add-collaborator (username repository &key login token)
  (:documentation "Add USERNAME to the collaborators list of REPOSITORY."))
(defgeneric remove-deploy-key (repository id &key login token)
  (:documentation "Remove key identified by ID as a deploy key for REPOSITORY."))
(defgeneric show-languages (username repository &key login token)
  (:documentation "List REPOSITORY's languages."))
(defgeneric set-repository-public (repository &key login token)
  (:documentation "Mark REPOSITORY as public on github."))
(defgeneric user-repositories (username)
  (:documentation "List USERNAME's repositories."))


(defmethod deploy-keys ((repository string) &key login token)
  (to-json (authed-request login token `("repos" "keys" ,repository))))

(defmethod add-deploy-key ((repository string) (title string)
                           (key string) &key login token)
  (to-json (authed-request login token `("repos" "key" ,repository "add")
                           :title title :key key)))
(defmethod remove-deploy-key ((repository string) (id string) &key login token)
  (to-json (authed-request login token `("repos" "key" ,repository "remove")
                           :id id)))
(defmethod remove-deploy-key ((repository string) (id integer) &key login token)
  (remove-deploy-key repository (princ-to-string id) :login login :token token))



;;; Repositories
(defmethod search-repositories ((search-string string))
  (to-json (github-simple-request "repos" "search" search-string)))

(defmethod show-repository ((username string) (repository string) &key login token)
  (to-json (request login token `("repos" "show" ,username ,repository))))

(defmethod user-repositories ((username string))
  (to-json (github-simple-request "repos" "show" username)))


(defmethod fork ((username string) (repository string) &key login token)
  (to-json (authed-request login token `("repos" "fork" ,username ,repository))))

(defmethod create-repository ((repository string) &key login token
                              description homepage public)
  (to-json (authed-request login token '("repos" "create")
                           :name repository
                           :description description
                           :homepage homepage
                           :public public)))

(defmethod delete-repository ((repository string) &key login token)
  (flet ((del-repo (&optional delete-token)
           (json->element
            (authed-request login token
                            `("repos" "delete" ,repository)
                            :delete-token delete-token))))
    (del-repo (del-repo))))

(defmethod set-repository-private ((repository string) &key login token)
  (to-json (authed-request login token `("repos" "set" "private" ,repository))))

(defmethod set-repository-public ((repository string) &key login token)
  (to-json (authed-request login token `("repos" "set" "public" ,repository))))



(defmethod show-collaborators ((username string) (repository string)
                               &key login token)
  (json->list (request login token `("repos" "show" ,username
                                                    ,repository "collaborators"))))

(defmethod add-collaborator ((username string) (repository string) &key login token)
  (json->list
   (authed-request login token `("repos" "collaborators" ,repository
                                         "add" ,username))))

(defmethod remove-collaborator ((username string) (repository string) &key login token)
  (json->list
   (authed-request login token `("repos" "collaborators" ,repository
                                         "remove" ,username))))

(defmethod show-network ((username string) (repository string) &key login token)
  (to-json (authed-request login token `("repos" "show" ,username
                                                 ,repository "network"))))

(defmethod show-languages ((username string) (repository string) &key login token)
  (json->list (request login token `("repos" "show"
                                             ,username ,repository "languages"))))

(defmethod show-tags ((username string) (repository string) &key login token)
  (json->list (request login token `("repos" "show" ,username
                                                    ,repository "tags"))))

(defmethod show-branches ((username string) (repository string) &key login token)
  (json->list
   (request login token `("repos" "show" ,username ,repository "branches"))))

(defmethod show-commits ((username string) (repository string) (branch string)
                         &key file login token)
  (to-json (request login token `("commits" "list" ,username
                                            ,repository ,branch ,file))))

(defmethod show-commit ((username string) (repository string) (sha string)
                        &key login token)
  (to-json (request login token `("commits" "show" ,username ,repository ,sha))))

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




(defpackage #:nisp.github-extra
  (:use :cl :iterate :nisp.github)
  (:export #:show-followers-not-followed))
(in-package :nisp.github-extra)
;;; Extra
(defun show-followers-not-followed (username)
  "Show followers that USERNAME is not following."
  ;; Thanks to scott olson for the idea.
  (set-difference (show-followers username) (show-following username)
                  :test #'equal))

;;; End file
