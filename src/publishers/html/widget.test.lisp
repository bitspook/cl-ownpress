(in-package #:cl-ownpress/tests)

(use-package :clown-publishers)

(define-test "widget")

(define-test "defwidget" :parent "widget"
  (define-test "creates a new class for WIDGET"
    (defwidget button (title) nil
      (:button title))

    (true (eq 'standard-class (type-of (find-class-safe 'button)))))

  (define-test "creates a default implementation for DOM-OF"
    (defwidget button (title) nil
      (:button title))

    (let ((btn (make-instance 'button :title "Lol")))
      (true (string=
             "<button>Lol</button>"
             (spinneret:with-html-string (dom-of btn))))))

  (define-test "creates a default implementation for LASS-OF"
    (defwidget button (title)
        `((button :display ,(if title 'flex 'none))
          (span :background red))
      (:button title))

    (let* ((btn1 (make-instance 'button :title "Lol"))
           (btn2 (make-instance 'button :title nil))
           (css1 (lass:write-sheet (apply #'lass:compile-sheet (lass-of btn1)) :pretty nil))
           (css2 (lass:write-sheet (apply #'lass:compile-sheet (lass-of btn2)) :pretty nil)))
      (true (string= "button{display:flex;}span{background:red;}" css1))
      (true (string= "button{display:none;}span{background:red;}" css2)))))

(define-test "dom-of" :parent "widget"
  (define-test "returns dom of the given widget"
    (defwidget button (title) nil
      (:button title))

    (let ((btn (make-instance 'button :title "Lol")))
      (true
       (string=
        "<button>Lol</button>"
        (spinneret:with-html-string (dom-of btn))))))

  (define-test "returns dom of the nested widgets as well"
    (defwidget button (title) nil
      (:button title))

    (defparameter btn (make-instance 'button :title "Lol"))

    (defwidget form () nil
      (:form (dom-of btn)))

    (let* ((*print-pretty* nil)
           (frm (make-instance 'form)))
      (true (string=
             "<form><button>Lol</button></form>"
             (spinneret:with-html-string (dom-of frm))))))

  (define-test "allow extending the returned dom"
    (defwidget button (title) nil
      (:button title))

    (defparameter btn (make-instance 'button :title "Lol"))

    (let* ((*print-pretty* nil))
      (defmethod dom-of :around ((widget (eql btn)))
        (spinneret:with-html
          (:h1 "Heading")
          (call-next-method)
          (:footer "Rofl")))
      (true (string=
             "<h1>Heading</h1><button>Lol</button><footer>Rofl</footer>"
             (spinneret:with-html-string (dom-of btn)))))))

(define-test "lass-of" :parent "widget"
  (define-test "returns lass of the given widget"
    (defwidget button (title)
        `((button :background ,(if title 'blue 'red))
          (span :background red))
      (:button title))

    (let* ((lass:*pretty* nil)
           (btn1 (make-instance 'button :title "Lol"))
           (btn2 (make-instance 'button :title nil))
           (css1 (lass:write-sheet (apply #'lass:compile-sheet (lass-of btn1))))
           (css2 (lass:write-sheet (apply #'lass:compile-sheet (lass-of btn2)))))
      (true (string= "button{background:blue;}span{background:red;}" css1))
      (true (string= "button{background:red;}span{background:red;}" css2))))

  (define-test "allow extending the returned lass"
    (defwidget button (title)
        `((button :background blue)
          (span :background red))
      (:button title))

    (let* ((lass:*pretty* nil)
           (btn (make-instance 'button :title "Lol"))
           (css (lass:write-sheet (apply #'lass:compile-sheet (lass-of btn))))
           (extended-css nil))

      (defmethod lass-of :around ((widget (eql btn)))
        (concatenate
         'list '((span :background cyan))
         (call-next-method)))

      (setf extended-css
            (lass:write-sheet
             (apply #'lass:compile-sheet (lass-of btn))))

      (true (string= "button{background:blue;}span{background:red;}" css))
      (true (string= "span{background:cyan;}button{background:blue;}span{background:red;}" extended-css)))))

(define-test "render" :parent "widget"
  (define-test "add WIDGET to *render-stack* before rendering it"
    (defwidget button (title) nil (:button title))

    (defwidget navbar () nil
      (:nav (render 'button :title "Click me")))

    (defwidget post () nil
      (render 'navbar)
      (:p "I am a blog post"))

    (let* ((cpub:*render-stack* nil)
           (*print-pretty* nil)
           (html (with-html-string (render 'post))))
      (true (string= "<nav><button>Click me</button></nav><p>I am a blog post" html))
      (true (eq 3 (length cpub:*render-stack*)))
      (true (eq 'button (class-name-of (car cpub:*render-stack*)))))))
