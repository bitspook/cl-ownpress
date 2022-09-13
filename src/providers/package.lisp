(defpackage clown-providers
  (:use :cl)
  (:export
   provider invoke-provider
   *org-roam-provider* *org-file-provider*))
(in-package :clown-providers)

(defclass provider ()
  ((name
    :type string
    :initarg :name
    :initform (error "Provider must have a name")
    :reader provider-name)))

(defgeneric invoke-provider (provider &key)
  (:documentation "Execute a provider."))

(defmethod invoke-provider :before (provider &key)
  (clown:run-pending-migrations))
