(in-package #:clown-blog)

(import 'clown-blog.views:home-html)
(import 'clown-blog.views:listing-html)
(import 'clown-blog.views:post-html)
(import 'clown-blog.views:rss-str)

(defun write-html-to-file (dest html &key (clean-urls? t))
  "Write HTML to DEST. If CLEAN-URL?, write as dest/index.html"
  (let* ((dest (if (and clean-urls? (not (string= (ppath:basename dest) "index.html")))
                   (str:concat dest "/index.html")
                   dest)))
    (publish-file dest html)))

(defun publish-rss-feed (title posts &key (filepath "feed.xml"))
  (publish-file filepath (rss-str title posts)))

(defun publish-listing (title posts &key filepath rss-url)
  (let* ((rss-url (or rss-url (clown:join-paths "/" filepath "feed.xml")))
         (html (listing-html title posts :rss-url rss-url)))
    (publish-html-file filepath html)))

(defun publish-post (post)
  (publish-html-file (post-public-path post) (post-html post)))

(defun publish-home (&key title)
  (let ((clown-blog.views::recent-posts (fetch-recent-posts 5)))
    (declare (ignorable clown-blog.views::recent-posts))
    (publish-html-file "index.html" (home-html :title title))))

(defun publish-blog (title)
  (publish-static (clown:system-local "src/publishers/blog/assets"))
  (mapcar #'publish-static (conf :static-dirs))

  (let ((tagged-posts (make-hash-table :test 'equal))
        (categorized-posts (make-hash-table :test 'equal))
        (all-posts (remove-if-not #'post-category (fetch-recent-posts -1))))
    (loop :for post :in all-posts
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

    (publish-listing "All content" all-posts :filepath "archive/")
    (publish-rss-feed "All content" all-posts :filepath "archive/feed.xml")

    (mapcar #'publish-post all-posts))


  (publish-home :title title))
