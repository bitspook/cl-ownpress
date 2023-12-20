;; A provider provides content. It reaches the source, collect, sanitize and stores the content in a
;; way which loses as little information from the source as possible. =provider=-s are specialized
;; for the source, not the destination. For example, A =denote-note= should represents a note, not a
;; blog post which can be created using it.

(defpackage in.bitspook.cl-ownpress/provider
  (:use #:cl #:serapeum/bundle #:in.bitspook.cl-ownpress))
(in-package #:in.bitspook.cl-ownpress/provider)

(export-always 'provide-all)
(defgeneric provide-all (obj &rest args)
  (:documentation "Every provider should have a PROVIDE-ALL."))
