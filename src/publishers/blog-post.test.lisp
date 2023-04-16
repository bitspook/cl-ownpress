(in-package #:cl-ownpress/tests)

(define-test "blog-post-publisher")

(defun cleanup ()
  (uiop:delete-directory-tree
   *test-dir* :validate t :if-does-not-exist :ignore))

(define-test "bpp.publish" :parent "blog-post-publisher"
  (define-test "uses LAYOUT to create html file for given BLOG-POST in BASE-DIR"
    (cleanup)
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
                  :description "Testing."))
           (layout (defwidget layout (post)
    `((p :color ,(if post 'red 'blue)))
  (:h1 (post-title post))
  (:p (post-description post))
  (:div
                      (:raw (post-body post))))))
  (publish pub :post post :layout layout)
  (true
       (uiop:file-exists-p
        (path-join *test-dir* "blog/" "test/index.html")))))

  (define-test "publishes blog posts as <slug>.html files when :CLEAN-URLS-P is nil"
    (cleanup)
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
                  :description "Testing."))
           (layout (defwidget layout (post)
    `((p :color ,(if post 'red 'blue)))
  (:h1 (post-title post))
  (:p (post-description post))
  (:div
                      (:raw (post-body post))))))
  (publish pub :post post :layout layout :clean-urls-p nil)
  (true
       (uiop:file-exists-p
        (path-join *test-dir* "blog/" "test.html")))))

  (define-test "publishes css used in LAYOUT using asset publisher")
  (define-test "provides ASSET-PUBLISHER to LAYOUT so it can link to published css")

  (cleanup))
