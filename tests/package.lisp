(uiop:define-package in.bitspook.cl-ownpress/tests
  (:use :cl :in.bitspook.cl-ownpress/publisher)
  (:import-from :spinneret :with-html-string)
  (:mix :parachute :serapeum/bundle))
(in-package #:in.bitspook.cl-ownpress/tests)

(sb-ext:add-package-local-nickname :cpub :in.bitspook.cl-ownpress/publisher)

(defparameter *test-dir* #p"/tmp/cl-ownpress-tests/")

