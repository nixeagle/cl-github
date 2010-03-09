(in-package :cl-github)


(defclass contact-data ()
  (email login name)
  (:documentation "Person information."))

(defclass simple-user (contact-data) ()
  (:documentation "About the simplest user information github will send."))

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

;;; Direct user stuff

(defgeneric search-users (username)
  (:documentation "Search github for USERNAME."))
(defgeneric show-user (user &key login token name blog email company location)
  (:documentation "NIL"))

(defmethod search-users ((username string))
  (to-json (github-simple-request "user" "search" username)))
(defmethod show-user ((user string)
                      &key login token name blog email company location)
  ;; Not going to export this right now, I want to make this more lisp
  ;; like by using setf.
  (to-json (github-request :parameters `("user" "show" ,user)
                           :auth (when (string= user *default-login*)
                                   :force)
                           :login login
                           :token token
                           :values\[blog\] blog
                           :values\[name\] name
                           :values\[email\] email
                           :values\[company\] company
                           :values\[location\] location)))

;;; Following
(defgeneric show-followers (username)
  (:documentation "List all followers of USERNAME."))
(defgeneric show-following (username)
  (:documentation "List all users that USERNAME follows."))
(defgeneric follow (username &key login token)
  (:documentation "Follow USERNAME using USER-LOGIN."))
(defgeneric unfollow (username &key login token)
  (:documentation "Unfollow USERNAME using LOGIN."))

(defmethod show-followers ((username string))
  (json->list (github-simple-request "user" "show" username "followers")))
(defmethod show-following ((username string))
  (json->list (github-simple-request "user" "show" username "following")))
(defmethod follow ((username string) &key login token)
  (json->list (authed-request login token `("user" "follow" ,username))))
(defmethod unfollow ((username string) &key login token)
  ;; Github seems to ignore this request.
  (json->list (authed-request login token `("user" "unfollow" ,username))))

;;; User public key management.
(defgeneric user-keys (&key login token)
  (:documentation "List all public keys LOGIN uses."))
(defgeneric add-user-key (name key &key login token)
  (:documentation "Add KEY to LOGIN's key list."))
(defgeneric remove-user-key (id &key login token)
  (:documentation "REMOVE KEY by ID from LOGIN's key list.

ID can be either a string or a positive number."))

(defmethod user-keys (&key login token)
  (to-json (authed-request login token '("user" "keys"))))

(defmethod add-user-key ((name string) (key string) &key login token)
  (to-json (authed-request login token '("user" "key" "add")
                           :name name :key key)))

(defmethod remove-user-key ((id string) &key login token)
  (to-json (authed-request login token '("user" "key" "remove")
                           :id (princ-to-string id))))


;;; User email management.
(defgeneric user-emails (&key login token)
  (:documentation "List all emails LOGIN uses."))
(defgeneric add-user-email (email &key login token)
  (:documentation "Add EMAIL to LOGIN's email list."))
(defgeneric remove-user-email (email &key login token)
  (:documentation "Remove EMAIL from LOGIN's email list."))


(defmethod user-emails (&key login token)
  (json->list (authed-request login token '("user" "emails"))))
(defmethod add-user-email ((email string) &key login token)
  (json->list (authed-request login token '("user" "email" "add")
                              :email email)))
(defmethod remove-user-email ((email string) &key login token)
  (json->list (authed-request login token '("user" "email" "remove")
                              :email email)))

(defgeneric show-pushable (&key login token))

(defmethod show-pushable (&key login token)
  (json->list (authed-request login token '("repos" "pushable"))))

;;; END