(defpackage cl-ownpress.setup
  (:use :cl))
(in-package :cl-ownpress.setup)

(ql:quickload '(:cl-migratum
                :cl-migratum.provider.local-path
                :cl-migratum.driver.dbi
                :cl-dbi
                :log4cl
                :lass
                :cffi
                :spinneret
                :str
                :ppath
                :osicat
                :sxql
                :lparallel
                :usocket
                :yason))

(log:config :debug)

(setf cffi::*foreign-library-directories*
      (cffi::explode-path-environment-variable "CLOWN_LIBRARY_PATH"))

(setf lparallel:*kernel* (lparallel:make-kernel 4))
