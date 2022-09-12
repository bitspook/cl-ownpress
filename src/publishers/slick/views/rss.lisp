(in-package :clown-slick.views)

(defun rss-str (posts &key title feed-url feed-updated-at)
  (with-output-to-string (str)
    (xml:with-rss2 (str)
      (xml:rss-channel-header title (conf :site-url)
                              :generator "cl-ownpress"
                              :image (conf :avatar))
      (mapcar (lambda (post)
                (let ((post-url (clown:join-paths (conf :site-url) (clown:post-output-path post))))
                  (xml:rss-item
                   (clown:post-title post)
                   :guid (clown:post-id post)
                   :category (clown:post-category post)
                   :link post-url
                   :author (conf :author)
                   :description (clown:post-html-content post)
                   :pubdate (clown:post-published-at post)))) posts))))
