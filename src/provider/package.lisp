(defpackage in.bitspook.cl-ownpress/provider
  (:use #:cl #:serapeum/bundle #:in.bitspook.cl-ownpress)
  (:export
   org-file-provider denote-provider org-project-provider))
(in-package #:in.bitspook.cl-ownpress/provider)

(defclass identifiable () ()
  (:documentation "An object which can be identified."))

(defgeneric identify (obj)
  (:documentation "Return a string which can be used to identify OBJ."))

(defmethod identify ((obj identifiable))
  "Default implementation for IDENTIFIABLE objects. Returns value of ID slot."
  (slot-value obj 'id))
