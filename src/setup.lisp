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
                :ppath))

(log:config :debug)

(setf cffi::*foreign-library-directories*
      (cffi::explode-path-environment-variable "CLOWN_LIBRARY_PATH"))
