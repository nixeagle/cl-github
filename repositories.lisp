(in-package :nisp.github)

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

(defclass languages ()
  ((languages :reader languages))
  (:documentation "List of languages."))

(defclass collaborators ()
  (collaborators)
  (:documentation "List of collaborators."))

(defclass network (repositories)
  ()
  (:documentation "A network is just another name for repositories."))


;;; Repository meta information stuff
(defgeneric search-repositories (search-string)
  (:documentation "Search github repositories for SEARCH-STRING."))
(defgeneric show-repository (username reponame &key login token)
  (:documentation "Show information on USERNAME's REPONAME."))
(defgeneric show-user-repositories (username)
  (:documentation "List USERNAME's repositories."))


(defmethod search-repositories ((search-string string))
  (to-json (github-simple-request "repos" "search" search-string)))
(defmethod show-repository ((username string) (repository string) &key login token)
  (to-json (request login token `("repos" "show" ,username ,repository))))
(defmethod show-user-repositories ((username string))
  (to-json (github-simple-request "repos" "show" username)))

;;; Watch/unwatch
(defgeneric watch (username repository &key login token)
  (:documentation "Watch REPOSITORY owned by USERNAME."))
(defgeneric unwatch (username repository &key login token)
  (:documentation "Stop watching REPOSITORY owned by USERNAME."))
(defgeneric watched-repositories (username)
  (:documentation "List repositories USERNAME watches."))

(defmethod watch ((username string) (repository string) &key login token)
  (to-json (request login token `("repos" "watch" ,username ,repository))))
(defmethod unwatch ((username string) (repository string) &key login token)
  (to-json (authed-request login token `("repos" "unwatch" ,username ,repository))))
(defmethod watched-repositories ((username string))
  (to-json (github-simple-request "repos" "watched" username)))

;;; Create/delete/fork
(defgeneric fork (username repository &key login token)
  (:documentation "Fork REPOSITORY owned by USERNAME."))
(defgeneric create-repository (repository &key login token description
                                          homepage public)
  (:documentation "Create new REPOSITORY on github."))
(defgeneric delete-repository (repository &key login token)
  (:documentation "Delete REPOSITORY on github."))


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


;;; Public/private
(defgeneric set-repository-private (repository &key login token)
  (:documentation "Mark REPOSITORY as private on github."))
(defgeneric set-repository-public (repository &key login token)
  (:documentation "Mark REPOSITORY as public on github."))



(defmethod set-repository-private ((repository string) &key login token)
  (to-json (authed-request login token `("repos" "set" "private" ,repository))))

(defmethod set-repository-public ((repository string) &key login token)
  (to-json (authed-request login token `("repos" "set" "public" ,repository))))


;;; Repository keys
(defgeneric deply-keys (repository &key login token)
  (:documentation "List REPOSITORY's deploy keys.

These are basically read only ssh keys."))
(defgeneric add-deploy-key (repository title key &key login token)
  (:documentation "Add KEY named TITLE as a deploy key for REPOSITORY."))
(defgeneric remove-deploy-key (repository id &key login token)
  (:documentation "Remove key identified by ID as a deploy key for REPOSITORY."))


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


;;; Collaborators
(defgeneric show-collaborators (username repository &key login token)
  (:documentation "List collaborators on REPOSITORY owned by USERNAME."))
(defgeneric remove-collaborator (username repository &key login token)
  (:documentation "Remove USERNAME from the collaborators list of REPOSITORY."))
(defgeneric add-collaborator (username repository &key login token)
  (:documentation "Add USERNAME to the collaborators list of REPOSITORY."))

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


;;; Repository refs stuff
(defgeneric show-tags (username repository &key login token)
  (:documentation "List REPOSITORY's tags."))
(defgeneric show-languages (username repository &key login token)
  (:documentation "List REPOSITORY's languages."))
(defgeneric show-branches (username repository &key login token)
  (:documentation "List REPOSITORY's remote branches."))


(defmethod show-languages ((username string) (repository string) &key login token)
  (json->list (request login token `("repos" "show"
                                             ,username ,repository "languages"))))

(defmethod show-tags ((username string) (repository string) &key login token)
  (json->list (request login token `("repos" "show" ,username
                                                    ,repository "tags"))))

(defmethod show-branches ((username string) (repository string) &key login token)
  (json->list
   (request login token `("repos" "show" ,username ,repository "branches"))))
