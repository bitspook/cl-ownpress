(in-package #:cl-ownpress/tests)

(use-package :clown-publishers)

(define-test clown-publishers)
(define-test asset-publisher :parent clown-publishers
  (define-test .publish
    (define-test "publish CONTENT as a file at PATH")

    (define-test "throw error if file at PATH already exists")

    (define-test "if CONTENT is a `pathname'"
      (define-test "copy file at CONTENT to PATH")
      (define-test "recursively copy dir at CONTENT to PATH")

      (define-test "if PATH already exists"
        (define-test "throw error CONTENT is a file")
        (define-test "copy fiels over without if CONTENT is a directory")))))
