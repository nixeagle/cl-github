(in-package :nisp.github-system)

(defpackage #:nisp.github
  (:use :cl :json :iterate)
  (:export

   ;; Following people related.
   #:show-followers
   #:show-following
   #:follow
   #:unfollow                           ;currently not working github side.
   
   ;; Project collaborators
   #:show-collaborators
   #:add-collaborator
   #:remove-collaborator

   ;; User Emails
   #:user-emails
   #:add-user-email
   #:remove-uer-email

   ;; User keys
   #:user-keys
   #:add-user-key
   #:remove-user-key

   ;; Repository keys
   #:deploy-keys
   #:add-deploy-key
   #:remove-deploy-key


   ;; Show commit info
   #:show-commit

   ;; Github issues tracker.
   #:search-issues
   #:show-issues
   #:show-issue
   #:open-issue
   #:close-issue
   #:reopen-issue
   #:add-label
   #:remove-label
   #:show-labels
   #:add-comment
   ))

(in-package :nisp.github)