(in-package #:cl-ownpress/tests)

(use-package :clown-publishers)

(define-test "widget")

(define-test "defwidget"
  :parent "widget"

  (define-test "creates a new instance of WIDGET"
    (let ((button (defwidget button (title) nil
                    (:button title))))
      (of-type cpub:widget button)))

  (define-test "creates a default implementation for DOM-OF"
    (let ((button (defwidget button (title) nil
                    (:button title))))
      (true (string=
             "<button>Lol</button>"
             (with-html-string (dom-of button :title "Lol"))))))

  (define-test "creates a default implementation for lass-of"
    (let* ((button (defwidget button (title)
                       `((button :background blue)
                         (span :background red))
                     (:button title)))
           (css (lass:write-sheet
                 (apply #'lass:compile-sheet
                        (lass-of button))
                 :pretty nil)))
      (true (string= "button{background:blue;}span{background:red;}" css)))))
