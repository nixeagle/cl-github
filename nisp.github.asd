(in-package :cl-user)
(defpackage #:nisp.github-system
  (:use :cl :asdf))
(in-package :nisp.github-system)


(defsystem :nisp.github
  :version "0.1.0"
  :author "James S <i@nixeagle.org>"
  :maintainer "James S <i@nixeagle.org>"
  :license "GPLv3 or later"
  :depends-on (:iterate
                :drakma
                :flexi-streams
                :cl-json
                #+ sbcl :sb-introspect
                :nisp.util)
  :serial t
  :components
  ((:file "package")
   (:file "url-utils")
   (:file "users")
   (:file "json")
   (:file "github")))