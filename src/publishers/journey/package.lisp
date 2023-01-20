(defpackage :clown.publishers.journey
  (:use :cl :alexandria :clown-publishers)
  (:export journey
           journey-id journey-name journey-tagline journey-slug
           journey-html-description journey-html-content
           journey-notes-with-tags))
(in-package #:clown.publishers.journey)

(defclass journey ()
  ((id :initarg :id :reader journey-id)
   (name :initarg :name :accessor journey-name)
   (notes-with-tags :initarg :notes-with-tags :accessor journey-notes-with-tags)
   (tagline :initarg :tagline :accessor journey-tagline)
   (slug :initarg :slug :accessor journey-slug)
   (html-description :initarg :html-description :accessor journey-html-description)
   (html-content :initarg :html-content :accessor journey-html-content)))
