(in-package :cl-user)
(defpackage #:cl-github-system
  (:use :cl :asdf)
  (:nicknames :nisp.github-system))
(in-package :cl-github-system)


(defsystem :cl-github
  :version "0.2.0"
  :license "BSD"
  :depends-on (:iterate :drakma :cl-json :eos)
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
