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

(define-test "publish" :parent "html-publisher"
  (define-test "creates html file at given PATH relative to DEST"
    ;; Simple widget
    (progn
      (defwidget btn (title) nil (:button title))

      (setup)

      (publish
       *pub*
       :path #p"simple-widgets/simple-button.html"
       :widget (make-instance 'btn :title "Lol"))

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
               :widget (make-instance 'nav)
               :path "pages/nav.html")

      (true (uiop:file-exists-p (path-join *test-dir* "pages/nav.html")))
      (true (str:from-file (path-join *test-dir* "pages/nav.html")) "<nav><button>Rofl</button><button>Lmao</button><button>Roflmao</button></nav>"))))
