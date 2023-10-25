(ql:quickload '(:cl-ownpress))

(in-package :cl-ownblog)

(defun build-test-blog ()
  (let* ((www #p"/tmp/www/")
         (author (make-instance 'persona
                                :name "Charanjit Singh"
                                :handles '(("Mastodon" "bitspook" "https://infosec.exchange/@bitspook"))))
         (post (make-instance 'blog-post
                              :title "Test Title"
                              :slug "best-title"
                              :description "Just trying to publish a dummy post"
                              :created-at (local-time:today)
                              :updated-at (local-time:today)
                              :author author
                              :tags '("test" "post" "is" "still" "just" "a" "post")
                              :body (str:from-file "~/Downloads/test.html")))
         (root-w (make-instance 'blog-post-w :post post))
         (asset-pub (make-instance 'asset-publisher
                                   :dest www))
         (post-pub (make-instance 'blog-post-publisher
                                  :asset-pub asset-pub
                                  :dest www)))

    (uiop:delete-directory-tree www :validate t :if-does-not-exist :ignore)
    (publish asset-pub :content #p"~/Documents/work/bitspook.github.io/static/")
    (publish post-pub :post post :layout root-w)))

;; quick hack to auto-build
;; elisp
;; (defun build-blog (successp notes buffer loadp)
;;   (sly-eval '(cl-ownblog::build-test-blog) "cl-ownblog"))
;; (add-hook 'sly-compilation-finished-hook #'build-blog)
