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

(define-test "publish" :parent "html-publisher"
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
           (content ".button{color:red;}.nav{color:blue;}")
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
            ".button{color:red;}.nav{color:blue;}")))

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
             (content ".button{color:red;}.nav{color:blue;}body{background:magenta;}")
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

        (true (uiop:file-exists-p (path-join *test-dir* expected-css-file)))
        (true (uiop:file-exists-p (path-join *test-dir* "pages/page.html")))
        (true (str:from-file (path-join *test-dir* expected-css-file))
              ".button{color:red;}.nav{color:blue;}body{background:magenta;}")
        (true (str:from-file (path-join *test-dir* "pages/page.html"))
              (format nil "<html lang=en><head><meta charset=UTF-8>~
                           <title>Test page</title>~
                           <link href=styles-9F61EA.css>~
                           </head><body>~
                           <div><nav><button>Rofl</button></nav>~
                           </div></body></html>"))))))
