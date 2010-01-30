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

   ))

(in-package :nisp.github)