(defpackage in.bitspook.cl-ownpress/provider
  (:use #:cl #:serapeum/bundle #:in.bitspook.cl-ownpress))
(in-package #:in.bitspook.cl-ownpress/provider)

(export-always 'provide-all)
(defgeneric provide-all (obj &rest args)
  (:documentation "Every provider should have a PROVIDE-ALL."))
