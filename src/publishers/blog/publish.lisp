(in-package #:clown-blog)

(defun render (t-fn &rest args)
  (apply (funcall t-fn (conf :theme)) args))

(defun publish-rss-feed (title posts &key (filepath "feed.xml"))
  (publish-file filepath (rss-str title posts)))

(defun publish-listing (title posts &key filepath rss-url)
  (let* ((rss-url (or rss-url (clown:join-paths "/" filepath "feed.xml")))
         (html (with-html-string
                 (render #'theme-listing-template title posts :rss-url rss-url))))
    (publish-html-file filepath html)))

(defun publish-post (post)
  (publish-html-file
   (post-public-path post)
   (with-html-string (render #'theme-post-template post))))

(defun publish-home (title)
  (publish-html-file
   "index.html"
   (with-html-string (render #'theme-home-template title (fetch-recent-posts 5)))))

(defun publish-projects ()
  (let ((projects (clown-blog:fetch-all-projects)))
    (clown-publishers:publish-html-file
     "projects/index.html"
     (with-html-string (render #'theme-projects-listing-template "Projects" projects)))

    (loop :for project :in projects
          :do
             (clown-publishers:publish-html-file
              (clown-blog:project-public-path project)
              (with-html-string (render #'theme-project-template project))))))

(defun publish-about-page ()
  (clown-publishers:publish-html-file
   "about"
   (with-html-string
     (render
      #'theme-about-me-template
      (conf :about-me)))))

(defun publish-blog (title)
  (publish-static (theme-assets-dir (conf :theme)))
  (mapcar #'publish-static (conf :static-dirs))

  (let ((tagged-posts (make-hash-table :test 'equal))
        (categorized-posts (make-hash-table :test 'equal))
        (listed-posts (remove-if-not #'post-category (fetch-recent-posts -1))))
    (loop :for post :in listed-posts
          :for cat := (post-category post)
          :do (loop
                :for tag :in (post-tags post)
                :do (if (gethash tag tagged-posts)
                        (push post (gethash tag tagged-posts))
                        (setf (gethash tag tagged-posts) (list post))))
              (if (gethash cat categorized-posts)
                  (push post (gethash cat categorized-posts))
                  (setf (gethash cat categorized-posts) (list post))))

    (maphash
     (lambda (tag posts)
       (let ((title (format nil "Posts tagged `~a'" tag))
             (posts (reverse posts)))
         (publish-listing title posts :filepath (format nil "tags/~a/" tag))
         (publish-rss-feed title posts :filepath (format nil "tags/~a/feed.xml" tag))))
     tagged-posts)

    (maphash
     (lambda (cat posts)
       (let ((posts (reverse posts))
             (title (format nil "Posts categorized as `~a'" cat)))
         (publish-listing title posts :filepath cat)
         (publish-rss-feed title posts :filepath (format nil "~a/feed.xml" cat))))
     categorized-posts)

    (publish-listing "All content" listed-posts :filepath "archive/")
    (publish-rss-feed "All content" listed-posts :filepath "archive/feed.xml")

    (mapcar #'publish-post listed-posts)
    (mapcar #'publish-post (fetch-unlisted-posts)))

  (publish-about-page)
  (publish-projects)
  (publish-home title))
