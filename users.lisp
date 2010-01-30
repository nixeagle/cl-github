(in-package :nisp.github)

(defclass contact-data ()
  (email login name)
  (:documentation "Person information."))

(defclass simple-user (contact-data) ()
  (:documentation "About the simplest user information github will send."))