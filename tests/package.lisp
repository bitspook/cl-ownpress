(uiop:define-package cl-ownpress/tests
  (:use :cl)
  (:import-from :spinneret :with-html-string)
  (:mix :parachute :serapeum/bundle))
(in-package :cl-ownpress/tests)

(sb-ext:add-package-local-nickname :cpub :clown-publishers)

(defparameter *test-dir* #p"/tmp/cl-ownpress-tests/")

