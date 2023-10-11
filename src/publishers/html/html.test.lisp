(in-package #:cl-ownpress/tests)

(use-package :clown-publishers)

(define-test "html-publisher")

(defparameter *ass*
  (make-instance
   'cpub:asset-publisher
   :dest *test-dir*))

(defparameter *pub*
  (make-instance
   'cpub:html-publisher
   :asset-pub *ass*
   :dest *test-dir*))

(defparameter *test-src-dir* #p"/tmp/clown-test-src/"
              "Directory where source files to be used in tests should be stored.")

(defun setup ()
  (uiop:delete-directory-tree *test-dir* :validate t :if-does-not-exist :ignore)
  (uiop:delete-directory-tree *test-src-dir* :validate t :if-does-not-exist :ignore)
  (ensure-directories-exist *test-src-dir*))

(defun identity-html-builder (&key css-file html)
  (spinneret:with-html (:raw html)))

(define-test "html-publisher.publish" :parent "html-publisher"
  (define-test "creates a css file in `dest' named styles-<content-hash>.css"
    (defwidget btn (title)
        '((.button :color red))
      (:button title))
    (defwidget nav ()
        '((.nav :color blue))
      (:nav (render 'btn :title "Rofl")
            (render 'btn :title "Lmao")
            (render 'btn :title "Roflmao")))

    (let* ((src-filename "styles.css")
           (content ".nav{color:blue;}.button{color:red;}")
           (expected-filename (cpub::append-content-hash
                               src-filename
                               (md5:md5sum-string content)))
           (*print-pretty* nil))
      (setup)
      (publish *pub*
               :page-builder #'identity-html-builder
               :root-widget (make-instance 'nav)
               :path "pages/nav.html")

      (true (uiop:file-exists-p (path-join *test-dir* expected-filename)))
      (true (str:from-file (path-join *test-dir* expected-filename))
            ".nav{color:blue;}.button{color:red;}")))

  (define-test "creates html file at given PATH relative to DEST"
    ;; Simple widget
    (progn
      (defwidget btn (title) nil (:button title))

      (setup)

      (publish
       *pub*
       :page-builder #'identity-html-builder
       :path #p"simple-widgets/simple-button.html"
       :root-widget (make-instance 'btn :title "Lol"))

      (true (uiop:file-exists-p (path-join *test-dir* "simple-widgets/simple-button.html")))
      (true (str:from-file (path-join *test-dir* "simple-widgets/simple-button.html")) "<button>Lol</button>"))

    ;; A bit more complex widget
    (progn
      (defwidget btn (title) nil (:button title))
      (defwidget nav () nil
        (:nav (render 'btn :title "Rofl")
              (render 'btn :title "Lmao")
              (render 'btn :title "Roflmao")))
      (setup)

      (publish *pub*
               :page-builder #'identity-html-builder
               :root-widget (make-instance 'nav)
               :path "pages/nav.html")

      (true (uiop:file-exists-p (path-join *test-dir* "pages/nav.html")))
      (true (str:from-file (path-join *test-dir* "pages/nav.html")) "<nav><button>Rofl</button><button>Lmao</button><button>Roflmao</button></nav>")))

  (define-test "accepts a page-builder to build final html page"
    (define-test "provides `css-file' and `html' as argument to widget when rendering HTML"
      (defwidget btn (title)
          '((.button :color red))
        (:button title))
      (defwidget nav ()
          '((.nav :color blue))
        (:nav (render 'btn :title "Rofl")))
      (defwidget root ()
          '((body :background magenta))
        (:div (render 'nav)))

      (let* ((src-filename "styles.css")
             (content "body{background:magenta;}.nav{color:blue;}.button{color:red;}")
             (expected-css-file (cpub::append-content-hash
                                 src-filename
                                 (md5:md5sum-string content)))
             (*print-pretty* nil)
             (build-html-page
               (lambda (&key css-file html)
                 (spinneret:with-html
                   (:html
                    (:head
                     (:title "Test page")
                     (:link :href css-file))
                    (:body (:raw html)))))))
        (setup)

        (publish *pub*
                 :page-builder build-html-page
                 :root-widget (make-instance 'root)
                 :path "pages/page.html")

        (true (uiop:file-exists-p (path-join *test-dir* "pages/page.html")))
        (true (str:from-file (path-join *test-dir* "pages/page.html"))
              (format nil "<html lang=en><head><meta charset=UTF-8>~
                           <title>Test page</title>~
                           <link href=~a>~
                           </head><body>~
                           <div><nav><button>Rofl</button></nav>~
                           </div></body></html>" expected-css-file))
        (true (uiop:file-exists-p (path-join *test-dir* expected-css-file)))
        (true (str:from-file (path-join *test-dir* expected-css-file))
              "body{background:magenta;}.nav{color:blue;}.button{color:red;}")))))

(define-test "tagged-lass" :parent "html-publisher"
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
