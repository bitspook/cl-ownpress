(defpackage cl-ownpress.setup
  (:use :cl))
(in-package :cl-ownpress.setup)

(log:config :debug)

(setf cffi::*foreign-library-directories*
      (cffi::explode-path-environment-variable "CLOWN_LIBRARY_PATH"))

(setf lparallel:*kernel* (lparallel:make-kernel 4))
