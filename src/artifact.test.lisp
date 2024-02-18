(in-package #:in.bitspook.cl-ownpress/tests)

(define-test "artifact")

(define-test "all-deps" :parent "artifact"
  (defclass parent (artifact) nil)

  (defclass child (artifact) nil)

  (let* ((p (make 'parent))
         (a (make 'child :deps (list p)))
         (b (make 'child :deps (list a))))
    (true (set-equal (list p a) (all-deps b)))))
