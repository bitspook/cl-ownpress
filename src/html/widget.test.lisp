(in-package #:in.bitspook.cl-ownpress/tests)

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

  (define-test "creates a default implementation for LASS-OF and CSS-OF"
    (defwidget button (title)
        `((button :display ,(if title 'flex 'none))
          (span :background red))
      (:button title))

    (let* ((btn1 (make-instance 'button :title "Lol"))
           (btn2 (make-instance 'button :title nil))
           (*print-pretty* nil)
           (css1 (css-of btn1))
           (css2 (css-of btn2)))
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

    (defwidget form () nil
      (:form (dom-of (make 'button :title "Lol"))))

    (let* ((*print-pretty* nil)
           (frm (make-instance 'form)))
      (true (string=
             "<form><button>Lol</button></form>"
             (spinneret:with-html-string (dom-of frm))))))

  (define-test "allow extending the returned dom"
    (defwidget button (title) nil
      (:button title))

    (let* ((*print-pretty* nil)
           (btn (make-instance 'button :title "Lol")))
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
  (define-test "add widget's DOM at call-site and add it as dependency of current widget"
    (defwidget button (title) '((button :background "blue")) (:button title))

    (defwidget navbar ()
        '((nav :background "cyan"))
      (let ((btn (make 'button :title "Click me")))
        (:nav (:li (render btn))
              (:li (render btn)))))

    ;; Add navbar without instantiating it
    (defwidget post ()
        '((p :background "parrot"))
      (render 'navbar)
      (:p "I am a blog post"))

    (let* ((*print-pretty* nil)
           (post (make 'post))
           (html (with-html-string (dom-of post))))
      (true (eq 2 (length (all-deps post))))
      (true (string= "<nav><li><button>Click me</button><li><button>Click me</button></nav><p>I am a blog post" html))))

  ;; Not exactly *desired* behavior here, but adding test to document significant behavior
  (define-test "only add WIDGET's child-widgets to its deps when it is RENDERed"
    (defwidget button (title) nil (:button title))

    (defwidget nav () nil
      (:nav (render 'button :title "Title")))

    (let* ((nav (make 'nav)))
      (true (eq 0 (length (all-deps nav))))
      (render nav)

      (true (eq 1 (length (all-deps nav)))))))

(define-test "rendered-css" :parent "widget"
  (define-test "return CSS for *all* deps of widget (including self)"
    (defwidget button (title) '((button :background "blue")) (:button title))

    (defwidget navbar ()
        '((nav :background "cyan"))
      (:nav (render 'button :title "Click me")))

    (defwidget post ()
        '((.post :background "parrot"))
      (render 'navbar)
      (:p "I am a blog post"))

    (let* ((*print-pretty* nil)
           (post (make 'post)))
      (with-html-string (render post))
      (true (string= ".post{background:parrot;}nav{background:cyan;}button{background:blue;}"
                     (rendered-css post)))))

  (define-test "does not add duplicate CSS if a widget is rendered more than once"
    (defwidget button (title) '((button :background "blue")) (:button title))

    (defwidget navbar ()
        '((nav :background "cyan"))
      (:nav (render 'button :title "Click me")
            (render 'button :title "Click me again")
            (render 'button :title "Click me once more")))

    (defwidget post ()
        '((p :background "parrot"))
      (render 'navbar)
      (:p "I am a blog post"))

    (let* ((*print-pretty* nil)
           (post (make 'post)))
      ;; resolve post's deps
      ;; with-html-string so it won't write to stdout
      (with-html-string (render post))

      (true (string= "p{background:parrot;}nav{background:cyan;}button{background:blue;}" (rendered-css post))))))

(define-test "tagged-lass" :parent "widget"
  (define-test "returns top-level lass-forms as-is"
    (let ((lass:*pretty* nil))
      (true
       (equal
        (let ((lass:*pretty* nil))
          (apply #'lass:compile-and-write
                 (tagged-lass
                  '((body :background blue)
                    (p :margin 10px)))))
        "body{background:blue;}p{margin:10px;}"))))

  (define-test "returns lass-forms immediately following a tag with specifier applied"
    (let ((lass:*pretty* nil))
      (true
       (equal
        (let ((lass:*pretty* nil)
              (*lass-tags* '((:md "(min-width: 0px)" :media-query))))
          (apply #'lass:compile-and-write
                 (tagged-lass
                  '((body :background blue)
                    (p :margin 10px))
                  :md '((body :background red)
                        (p :margin 0)))))
        "body{background:blue;}p{margin:10px;}@media (min-width: 0px){body{background:red;}p{margin:0;}}"))))

  (define-test "returns lass-forms immediately following a multiple-tags with specifier applied"
    (true
     (equal
      (let ((lass:*pretty* nil)
            (*lass-tags* '((:md "(min-width: 0px)" :media-query)
                           (:sm "(min-width: 10px)" :media-query))))
        (apply #'lass:compile-and-write
               (tagged-lass
                '((body :background blue)
                  (p :margin 10px))

                :md :sm '((body :background red)
                          (p :margin 0)))))
      "body{background:blue;}p{margin:10px;}@media (min-width: 10px) or (min-width: 0px){body{background:red;}p{margin:0;}}"))))
