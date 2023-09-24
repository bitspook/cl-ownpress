(in-package #:cl-ownpress/tests)

(defun cleanup ()
  (uiop:delete-directory-tree
   *test-dir* :validate t :if-does-not-exist :ignore))

(define-test "blog-post-publisher" 
  (cleanup))
