(in-package :cl-github)

;;; Network API
(defgeneric show-network-meta (username repository &key login token)
  (:documentation "Network meta information for USERNAME's REPOSITORY."))
(defgeneric show-network-data (username repository
                                        &key network-meta login token
                                        start end)
  (:documentation "Data on last 100 commits."))

(defclass commit-range ()
  (name count start)
  (:documentation "Blocks of something that github gives when querying
the network api."))

(defclass head ()
  (name id)
  (:documentation "Heads of branches returned from github's Network API."))

(defclass github-network-meta ()
  (blocks
   (nethash :reader nethash)
   focus dates users)
  (:documentation "Toplevel result from github's Network API."))

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