(in-package #:default-theme)

(defparameter default-theme
  (make-instance
   'theme
   :home (lambda (title posts) (render home :title title :recent-posts posts :rss-url "/archive/feed.xml"))
   :listing (lambda (title posts &key rss-url) (render posts-listing-page :title title :posts posts :rss-url rss-url))
   :post (lambda (post) (render post-page :post post))
   :project (lambda (project) (render project-page :project project))
   :projects-listing (lambda (title projects)
                       (render projects-listing-page :title title :projects projects))
   :about-me (lambda (body) (render about-page :body body))
   :assets-dir (system-local "src/publishers/blog/default-theme/assets")))
