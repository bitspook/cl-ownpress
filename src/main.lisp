(defpackage cl-ownpress
  (:use :cl :cl-ownpress.db))
(in-package :cl-ownpress)

(ql:quickload '(:cl-migratum
                :cl-migratum.provider.local-path
                :cl-migratum.driver.dbi
                :cl-dbi
                :log4cl
                :lass
                :cffi))

(log:config :debug)

(setf cffi::*foreign-library-directories*
      (cffi::explode-path-environment-variable "CLOWN_LIBRARY_PATH"))

(defclass provider ()
  ((name
    :type string
    :initarg :name
    :initform (error "Provider must have a name")
    :reader provider-name)))

(defgeneric invoke-provider (provider &key)
  (:documentation "Execute a provider."))

(defparameter *org-roam-provider*
    (make-instance
     'provider
     :name "org-roam-provider"))

(defmethod invoke-provider ((provider (eql *org-roam-provider*)) &key)
  (let ((script-path "./providers/org-roam/org-roam.el")
        (db-name (dbi:connection-database-name *conn*)))
    (uiop:run-program (format nil "emacs --script ~s ~s" script-path db-name))))
