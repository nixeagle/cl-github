(in-package :nisp.github-system)

(defpackage #:nisp.github
  (:use :cl :json))

(in-package :nisp.github)

(defparameter +github-api-url+ "https://github.com/api/v2/"
  ;; Use only the json interface, we do not want to implement the xml or
  ;; yaml interfaces.
  "Github api location.
This is the same for every call.")


