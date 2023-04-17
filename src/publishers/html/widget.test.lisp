(in-package #:cl-ownpress/tests)

(use-package :clown-publishers)

(define-test "widget")

(define-test "defwidget" :parent "widget"
  (define-test "creates a new instance of WIDGET"
    (let ((button (defwidget button (title) nil
                    (:button title))))
      (of-type cpub:widget button)))

  (define-test "creates a default implementation for DOM-OF"
    (let ((button (defwidget button (title) nil
                    (:button title))))
      (true (string=
             "<button>Lol</button>"
             (spinneret:with-html-string (dom-of button :title "Lol"))))))

  (define-test "assigns all used widgets as WIDGET-CHILDREN"
    (let* ((body (defwidget layout () nil (:p "Body")))
           (footer (defwidget footer () nil (:footer "Footer")))
           (layout (defwidget layout () nil
                     (:h1 "Heading")
                     (dom-of body)
                     (dom-of footer)))
           (children (widget-children layout)))
      (true (and (eq (car children) 'footer)
                 (eq (cadr children) 'body)))))

  (define-test "creates a default implementation for LASS-OF"
    (let* ((button (defwidget button (title)
                       `((button :background blue)
                         (span :background red))
                     (:button title)))
           (css (lass:write-sheet
                  (apply #'lass:compile-sheet (lass-of button))
                  :pretty nil)))
      (true (string= "button{background:blue;}span{background:red;}" css)))))

(define-test "dom-of" :parent "widget"
  (define-test "returns dom of the given widget"
    (let ((button (defwidget button (title) nil
                    (:button title))))
      (true (string=
             "<button>Lol</button>"
             (spinneret:with-html-string (dom-of button :title "Lol"))))))

  (define-test "returns dom of the nested widgets as well"
    (let* ((*print-pretty* nil)
           (button (defwidget button (title) nil
                     (:button title)))
           (form (defwidget form (title) nil
                   (:form title (dom-of button :title "Lol")))))
      (true (string=
             "<form><button>Lol</button></form>"
             (spinneret:with-html-string (dom-of form))))))

  (define-test "allow extending the returned dom"
    (let* ((*print-pretty* nil)
           (button (defwidget button (title) nil
                     (:button title))))
      (defmethod dom-of :around ((widget (eql button)) &key)
        (spinneret:with-html
          (:h1 "Heading")
          (call-next-method)
          (:footer "Rofl")))
      (true (string=
             "<h1>Heading</h1><button>Lol</button><footer>Rofl</footer>"
             (spinneret:with-html-string (dom-of button :title "Lol")))))))

(define-test "lass-of" :parent "widget"
  (define-test "returns lass of the given widget"
    (let* ((button (defwidget button (title)
                       `((button :background blue)
                         (span :background red))
                     (:button title)))
           (css (lass:write-sheet
                  (apply #'lass:compile-sheet (lass-of button))
                  :pretty nil)))
      (true (string= "button{background:blue;}span{background:red;}" css))))

  (define-test "allow extending the returned lass"
    (let* ((*print-pretty* nil)
           (button (defwidget button (title)
                       `((button :background blue)
                         (span :background red))
                     (:button title)))
           (css (lass:write-sheet
                  (apply #'lass:compile-sheet (lass-of button))
                  :pretty nil))
           (extended-css nil))

      (defmethod lass-of :around ((widget (eql button)))
        (concatenate
         'list '((span :background cyan))
         (call-next-method)))

      (setf extended-css
            (lass:write-sheet
             (apply #'lass:compile-sheet (lass-of button))
             :pretty nil))

      (true (string= "button{background:blue;}span{background:red;}" css))
      (true (string= "span{background:cyan;}button{background:blue;}span{background:red;}" extended-css)))))
