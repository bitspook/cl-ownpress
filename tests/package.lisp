(uiop:define-package in.bitspook.cl-ownpress/tests
  (:use :cl :in.bitspook.cl-ownpress)
  (:import-from :spinneret :with-html-string)
  (:mix :parachute :serapeum/bundle))
(in-package #:in.bitspook.cl-ownpress/tests)

(defparameter *test-dir* #p"/tmp/cl-ownpress-tests/")
