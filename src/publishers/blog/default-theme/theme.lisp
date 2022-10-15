(in-package #:default-theme)

(defparameter theme
  (make-instance
   'blog-theme
   :home (lambda (title posts) (render home :title title :recent-posts posts :rss-url "/archive/feed.xml"))
   :listing (lambda (title posts &key rss-url) (render posts-listing-page :title title :posts posts :rss-url rss-url))
   :post (lambda (post) (render post-page :post post))
   :assets-dir (system-local "src/publishers/blog/default-theme/assets")))