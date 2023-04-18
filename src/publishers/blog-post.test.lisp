(in-package #:cl-ownpress/tests)

(define-test "blog-post-publisher")

(defun cleanup ()
  (uiop:delete-directory-tree
   *test-dir* :validate t :if-does-not-exist :ignore))

(define-test "bpp.publish" :parent "blog-post-publisher"
  (define-test "uses LAYOUT to create html file for given BLOG-POST in BASE-DIR"
    (cleanup)
    (defwidget layout (post)
        `((p :color ,(if post 'red 'blue)))
      (:h1 (post-title post))
      (:p (post-description post))
      (:div
       (:raw (post-body post))))

    (let* ((ass (make-instance
                 'cpub:asset-publisher
                 :dest *test-dir*))
           (pub (make-instance
                 'cpub:blog-post-publisher
                 :dest *test-dir*
                 :asset-publisher ass
                 :base-dir #p"blog/"))
           (post (make-instance
                  'cpub:blog-post
                  :title "Test"
                  :body "<h1>Testing test.</h1>"
                  :description "Testing.")))
      (publish pub :post post :layout 'layout)
      (true
       (uiop:file-exists-p
        (path-join *test-dir* "blog/" "test/index.html")))))

  (define-test "publishes blog posts as <slug>.html files when :CLEAN-URLS-P is nil
"
    (cleanup)
    (defwidget layout (post)
        `((p :color ,(if post 'red 'blue)))
      (:h1 (post-title post))
      (:p (post-description post))
      (:div
       (:raw (post-body post))))

    (let* ((ass (make-instance
                 'cpub:asset-publisher
                 :dest *test-dir*))
           (pub (make-instance
                 'cpub:blog-post-publisher
                 :dest *test-dir*
                 :asset-publisher ass
                 :base-dir #p"blog/"))
           (post (make-instance
                  'cpub:blog-post
                  :title "Test"
                  :body "<h1>Testing test.</h1>"
                  :description "Testing.")))
      (publish pub :post post :layout 'layout :clean-urls-p nil)
      (true
       (uiop:file-exists-p
        (path-join *test-dir* "blog/" "test.html")))))

  (define-test "publishes css used in LAYOUT using asset publisher to css/styles.css"
    (cleanup)
    (let* ((ass (make-instance 'cpub:asset-publisher :dest *test-dir*))
           (pub (make-instance 'cpub:blog-post-publisher :dest *test-dir* :asset-publisher ass :base-dir #p"blog/"))
           (post (make-instance 'cpub:blog-post :title "Test" :body "<h1>Testing test.</h1>" :description "Testing."))
           (footer (defwidget footer ()
                       '((footer :color red))
                     (:footer "Footer")))
           (layout (defwidget layout (post)
                       `((p :color 'red))
                     (:h1 (post-title post))
                     (:p (post-description post))
                     (:div
                      (:raw (post-body post)))
                     (dom-of footer))))
      (publish pub :post post :layout layout :clean-urls-p nil)
      (true
       (uiop:file-exists-p
        (path-join *test-dir* "css/" "styles.css")))
      (true
       (string=
        (str:from-file (path-join *test-dir* "css/" "styles.css"))
        "footer{color:red;}p{color:red;}"))))

  (define-test "provides ASSET-PUBLISHER to LAYOUT so it can link to published css")

  (cleanup))
