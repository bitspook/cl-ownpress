(defpackage cl-ownpress/tests/main
  (:use :cl
        :cl-ownpress
        :rove))
(in-package :cl-ownpress/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-ownpress)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
