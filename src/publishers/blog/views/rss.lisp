(in-package :clown-blog.views)

(defun rss-str (title posts)
  (with-output-to-string (str)
    (xml:with-rss2 (str)
      (xml:rss-channel-header title (conf :site-url)
                              :generator "cl-ownpress"
                              :image (conf :avatar))
      (mapcar (lambda (post)
                (let ((post-url (clown:join-paths (conf :site-url) (post-public-path post))))
                  (xml:rss-item
                   (post-title post)
                   :guid (post-id post)
                   :category (post-category post)
                   :link post-url
                   :author (conf :author)
                   :description (post-html-content post)
                   :pubdate (post-published-at post)))) posts))))
