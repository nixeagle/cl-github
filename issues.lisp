(in-package :nisp.github)

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

(defclass issue-labels ()
  (labels)
  (:documentation "Github issue tracker labels."))

(defclass issue ()
  (number votes created-at body title updated-at closed-at user labels state)
  (:documentation "Github issue information."))

(defclass comment ()
  (comment status)
  (:documentation "Comment on a github issue."))


(deftype valid-issue-state ()
  "Github issues have two valid states."
  '(member :open :closed))

(defmethod search-issues ((username string) (repository string)
                          (state string) (term string)
                          &key login token)
  (to-json (request login token `("issues" "search" ,username
                                           ,repository ,state ,term))))

(defmethod show-issues ((username string) (repository string)
                        (state string) &key login token)
  (to-json (request login token `("issues" "list" ,username ,repository ,state))))

(defmethod show-issue ((username string) (repository string)
                       (issue string) &key login token)
  (to-json (request login token `("issues" "show" ,username ,repository ,issue))))

(defmethod open-issue ((username string) (repository string)
                       (title string) (body string)
                       &key login token)
  (to-json (authed-request login token `("issues" "open" ,username ,repository)
                           :title title :body body)))

(defmethod close-issue ((username string) (repository string)
                        (issue string)
                        &key login token)
  (to-json (authed-request login token `("issues" "close" ,username
                                                  ,repository ,issue))))

(defmethod reopen-issue ((username string) (repository string)
                         (issue string)
                         &key login token)
  (to-json (authed-request login token `("issues" "reopen" ,username
                                                  ,repository ,issue))))

(defmethod edit-issue ((username string) (repository string)
                       (title string) (body string) (issue string)
                       &key login token)
  (to-json (authed-request login token `("issues" "edit" ,username
                                                  ,repository ,issue)
                           :title title :body body)))

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
  (to-json (authed-request login token `("issues" "comment" ,username
                                                  ,repository ,issue)
                           :comment comment)))
