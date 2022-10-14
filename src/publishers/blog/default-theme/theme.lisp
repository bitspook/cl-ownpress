(in-package #:default-theme)

(defparameter theme
  (make-instance
   'blog-theme
   :home (lambda (title posts) (home-html title posts))
   :listing (lambda (title posts &key rss-url) (listing-html title posts :rss-url rss-url))
   :post (lambda (post) (post-html post))
   :assets-dir (system-local "src/publishers/blog/default-theme/assets")))
