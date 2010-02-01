(in-package :cl-user)
(defpackage #:cl-github-system
  (:use :cl :asdf)
  (:nicknames :nisp.github-system))
(in-package :cl-github-system)


(defsystem :cl-github
  :version "0.1.0"
  :author "James S <i@nixeagle.org>"
  :maintainer "James S <i@nixeagle.org>"
  :license "BSD"
  :depends-on (:iterate
                :drakma
                :cl-json)
  :serial t
  :components
  ((:file "package")
   (:file "url-utils")
   (:file "users")
   (:file "repositories")
   (:file "issues")
   (:file "json")
   (:file "github")
   (:file "network")))                    ;Needs COMMITS and USERS.

;;; end
