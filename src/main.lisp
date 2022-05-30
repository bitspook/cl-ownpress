(defpackage cl-ownpress
  (:nicknames :clown)
  (:use :cl :clown.db))
(in-package :cl-ownpress)

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
  (let ((script-path (asdf:system-relative-pathname "cl-ownpress" "./src/providers/org-roam/org-roam.el"))
        (db-name (dbi:connection-database-name *conn*)))
    (uiop:run-program (format nil "emacs --script ~a ~a" script-path db-name)
                      :output *standard-output*
                      :error-output *standard-output*)))
