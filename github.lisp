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
          (print key)
          (when (and value (not (eq :parameters key))
                     (not (eq :auth key))
                     (not (eq :method key))
                     (not (eq :want-string key)))
            (collect (cons (dash-to-underscore
                            (string-downcase (symbol-name key))) value))))))

;;; Class related generics.

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

(defclass network-meta-user ()
  (name repo heads)
  (:documentation "User object returned from github's Network API."))

(defclass users (network-meta-user)
  (name location followers username language fullname
        repos id type pushed score created)
  (:documentation "Describes a github user search result."))

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

(defclass modified ()
  (diff filename)
  (:documentation "Modification information for a commit."))

(defclass emails ()
  ((emails :reader emails))
  (:documentation "List of user emails."))

(defclass public-keys ()
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

(defclass tree ()
  (name sha mode type) 
  (:documentation "Treeish git object that we get from github."))

(defclass blocks ()
  (name count start)
  (:documentation "Blocks of something that github gives when querying
the network api."))

(defclass heads ()
  (name id)
  (:documentation "Heads of branches returned from github's Network API."))

(defclass github-network-meta ()
  (blocks
   (nethash :reader nethash)
   focus dates users)
  (:documentation "Toplevel result from github's Network API."))

(defclass issue ()
  (number votes created-at body title updated-at closed-at user labels state)
  (:documentation "Github issue information."))

(defclass issues (issue)
  ()
  (:documentation "Github issue information."))

(defclass comment ()
  (comment status)
  (:documentation "Comment on a github issue."))

;;; utils
(defun build-github-api-url (&rest parameters)
  "Build a request url using PARAMETERS."
  (reduce (lambda (prior new)
            (if new
                (concatenate 'string prior "/" (url-encode new))
                prior))
          parameters))

(defclass issue-labels ()
  (labels)
  (:documentation "Github issue tracker labels."))

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
(defgeneric add-deploy-key (repository title key &key login token)
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
(defgeneric remove-deploy-key (repository id &key login token)
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
  (json->list (github-simple-request "user" "show" username "followers")))

(defmethod show-following ((username string))
  (json->list (github-simple-request "user" "show" username "following")))

(defmethod follow ((username string) &key login token)
  (json->list (authed-request login token `("user" "follow" ,username))))
               
(defmethod unfollow ((username string) &key login token)
  ;; Github seems to ignore this request.
  (json->list (authed-request login token `("user" "unfollow" ,username))))

(defmethod watched-repositories ((username string))
  (slot-value
   (to-json (github-simple-request "repos" "watched" username))
   'repositories))

(defmethod user-emails (&key login token)
  (json->list (authed-request login token '("user" "emails"))))

(defmethod add-user-email ((email string) &key login token)
  (json->list (authed-request login token '("user" "email" "add")
                              :email email)))

(defmethod remove-user-email ((email string) &key login token)
  (json->list (authed-request login token '("user" "email" "remove")
                              :email email)))

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
  (slot-value (to-json (authed-request login token
                                       `("repos" "unwatch" ,username ,repository)))
              'repository))

(defmethod fork ((username string) (repository string) &key login token)
  (slot-value (to-json (authed-request login token
                                       `("repos" "fork" ,username ,repository)))
              'repository))

(defmethod create-repository ((repository string) &key login token
                              description homepage public)
  (slot-value (to-json (authed-request login token '("repos" "create")
                                       :name repository
                                       :description description
                                       :homepage homepage
                                       :public public))
              'repository))

(defmethod delete-repository ((repository string) &key delete-token login token)
  (error "broken.")
  (json->element
   (authed-request login token
                   `("repos" "delete" ,repository)
                   :delete-token (delete-repository repository))))

(defmethod set-repository-private ((repository string) &key login token)
  (slot-value (to-json (authed-request login token
                                       `("repos" "set" "private" ,repository)))
              'repository))

(defmethod set-repository-public ((repository string) &key login token)
  (slot-value (to-json (authed-request login token
                                       `("repos" "set" "public" ,repository)))
              'repository))

(defmethod deploy-keys ((repository string) &key login token)
  (slot-value (to-json (authed-request login token `("repos" "keys" ,repository)))
              'public-keys))

(defmethod add-deploy-key ((repository string) (title string)
                           (key string) &key login token)
  (slot-value (to-json (authed-request login token `("repos" "key" ,repository
                                                             "add")
                                       :title title
                                       :key key))
              'public-keys))

(defmethod remove-deploy-key ((repository string) (id string) &key login token)
  (slot-value (to-json (authed-request login token `("repos" "key" ,repository
                                                             "remove")
                                       :id id))
              'public-keys))
(defmethod remove-deploy-key ((repository string) (id integer) &key login token)
  (remove-deploy-key repository (princ-to-string id) :login login :token token))

(defmethod show-collaborators ((username string) (repository string)
                               &key login token)
  (json->list (request login token `("repos" "show" ,username
                                                    ,repository "collaborators"))))

(defmethod add-collaborator ((username string) (repository string) &key login token)
  (cdar (json->alist
         (authed-request login token `("repos" "collaborators" ,repository
                                               "add" ,username)))))

(defmethod remove-collaborator ((username string) (repository string) &key login token)
  (cdar (json->alist
         (authed-request login token `("repos" "collaborators" ,repository
                                               "remove" ,username)))))

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
  (json->list (request login token `("repos" "show" ,username
                                                    ,repository "tags"))))

(defmethod show-branches ((username string) (repository string) &key login token)
  (cdar (json->alist
         (request login token `("repos" "show" ,username ,repository "branches")))))

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

;;; Object API
(defgeneric show-tree (username repository tree &key login token)
  (:documentation "List treeish objects for USERNAME's REPOSITORY at TREE."))
(defgeneric show-blob (username repository path tree &key login token)
  (:documentation "Show contents of the file at PATH in USERNAME's REPOSITORY."))
(defgeneric show-raw-blob (username repository sha &key login token)
  (:documentation "Show raw contents of SHA in USERNAME's REPOSITORY."))

(defmethod show-tree ((username string) (repository string)
                      (tree string) &key login token)
  (slot-value (to-json (request login token `("tree" "show" ,username
                                                     ,repository ,tree)))
              'tree))

(defmethod show-blob ((username string) (repository string)
                      (path string) (tree string) &key login token)
  (slot-value (to-json (request login token `("blob" "show" ,username
                                                     ,repository ,tree
                                                     ,path)))
              'blob))

(defmethod show-raw-blob ((username string) (repository string)
                          (sha string) &key login token)
  (github-request :login login :token token :auth :default
                  :parameters `("blob" "show" ,username ,repository ,sha)
                  :want-string t))

;;; Network API
(defgeneric show-network-meta (username repository &key login token)
  (:documentation "Network meta information for USERNAME's REPOSITORY."))
(defgeneric show-network-data (username repository
                                        &key network-meta login token
                                        start end)
  (:documentation "Data on last 100 commits."))

(defmethod show-network-meta ((username string) (repository string)
                              &key login token)
  (let ((*current-prototype* "GITHUB-NETWORK-META"))
    (to-json (github-request :login login :token token :auth :default
                             :parameters `(,username ,repository "network_meta")
                             :base-url "http://github.com"))))

(defmethod show-network-data ((username string) (repository string)
                              &key network-meta login token start end)
  (let ((network-meta (or network-meta
                          (nethash (show-network-meta username
                                                      repository
                                                      :token token
                                                      :login login)))))
    (to-json (github-request :login login :token token :auth :default
                             :parameters `(,username ,repository
                                                     "network_data_chunk")
                             :base-url "http://github.com"
                             :nethash network-meta
                             :start start
                             :end end))))

;;; Issues API
(defgeneric search-issues (username repository state term &key login token)
  (:documentation "Search for TERM with STATE on USERNAME's REPOSITORY."))
(defgeneric show-issues (username repository state &key login token)
  (:documentation "Show all issues with STATE on USERNAME's REPOSITORY."))
(defgeneric show-issue (username repository issue &key login token)
  (:documentation "Show ISSUE on USERNAME's REPOSITORY."))
(defgeneric open-issue (username repository title body &key login token)
  (:documentation "Open issue about TITLE with BODY on USERNAME's REPOSITORY."))
(defgeneric close-issue (username repository issue &key login token)
  (:documentation "Close ISSUE on USERNAME's REPOSITORY."))
(defgeneric reopen-issue (username repository issue &key login token)
  (:documentation "Reopen ISSUE on USERNAME's REPOSITORY."))
(defgeneric edit-issue (username repository title body issue &key login token)
  (:documentation "Edit ISSUE setting TITLE and BODY on USERNAME's REPOSITORY.

Editing an issue causes your TITLE and BODY to completely replace the
original TITLE and BODY."))
(defgeneric show-labels (username repository &key login token)
  (:documentation "Show issue labels for USERNAME's REPOSITORY."))
(defgeneric add-label (username repository label issue &key login token)
  (:documentation "Add LABEL to ISSUE on USERNAME's REPOSITORY."))
(defgeneric remove-label (username repository label issue &key login token)
  (:documentation "Remove LABEL from ISSUE on USERNAME's REPOSITORY."))
(defgeneric add-comment (username repository comment issue &key login token)
  (:documentation "Add COMMENT to ISSUE on USERNAME's REPOSITORY."))

(deftype valid-issue-state ()
  "Github issues have two valid states."
  '(member :open :closed))

(defmethod search-issues ((username string) (repository string)
                          (state string) (term string)
                          &key login token)
  (slot-value (to-json (request login token
                                `("issues" "search" ,username
                                           ,repository ,state ,term)))
              'issues))

(defmethod show-issues ((username string) (repository string)
                        (state string) &key login token)
  (slot-value (to-json (request login token `("issues" "list" ,username
                                                       ,repository ,state)))
              'issues))

(defmethod show-issue ((username string) (repository string)
                       (issue string) &key login token)
  (slot-value (to-json (request login token `("issues" "show" ,username
                                                       ,repository ,issue)))
              'issue))

(defmethod open-issue ((username string) (repository string)
                       (title string) (body string)
                       &key login token)
  (slot-value (to-json (authed-request login token
                                       `("issues" "open"
                                                  ,username
                                                  ,repository)
                                       :title title
                                       :body body))
              'issue))

(defmethod close-issue ((username string) (repository string)
                        (issue string)
                        &key login token)
  (slot-value (to-json (authed-request login token
                                       `("issues" "close"
                                                  ,username
                                                  ,repository
                                                  ,issue)))
              'issue))

(defmethod reopen-issue ((username string) (repository string)
                         (issue string)
                         &key login token)
  (slot-value (to-json (authed-request login token
                                       `("issues" "reopen"
                                                  ,username
                                                  ,repository
                                                  ,issue)))
              'issue))

(defmethod edit-issue ((username string) (repository string)
                       (title string) (body string) (issue string)
                       &key login token)
  (slot-value (to-json (authed-request login token
                                       `("issues" "edit"
                                                  ,username
                                                  ,repository
                                                  ,issue)
                                       :title title
                                       :body body))
              'issue))

(defmethod show-labels ((username string) (repository string)
                        &key login token)
  (json->list (request login token
                              `("issues" "labels" ,username ,repository))))

(defmethod add-label ((username string) (repository string)
                      (label string) (issue string)
                      &key login token)
  (json->list (authed-request login token
                              `("issues" "label" "add"
                                         ,username ,repository
                                         ,label ,issue))))

(defmethod remove-label ((username string) (repository string)
                         (label string) (issue string)
                         &key login token)
  (json->list (authed-request login token
                              `("issues" "label" "remove"
                                         ,username ,repository
                                         ,label ,issue))))

(defmethod add-comment ((username string) (repository string)
                        (comment string) (issue string)
                        &key login token)
  (slot-value (to-json (authed-request login token
                                       `("issues" "comment" ,username
                                                  ,repository ,issue)
                                       :comment comment))
              'comment))

;;; End file
