(in-package :cl-user)
(defpackage #:clithub-system
  (:use :cl :asdf)
  (:nicknames :nisp.github-system))
(in-package :clithub-system)


(defsystem :clithub
  :version "0.1.1"
  :license "BSD"
  :depends-on (:iterate :drakma :cl-json)
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
