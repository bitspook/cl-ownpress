(in-package :default-theme)

(defwidget posts-listing-page (posts title rss-url)
  :styles
  (concatenate
   'list
   (top-level-css)
   `(,@(adjustable-width-css ".content")
       (.content
        (h1
         :display flex
         :align-items center
         (.rss-sub
          :margin 0
          :margin-left 2rem)))))
  :render
  (with-html
    (:html
     (:head
      (:title title)
      (when rss-url
        (:link :rel "alternate" :type "application/atom+xml" :title "RSS Feed" :href rss-url))
      (:style (:raw (compile-and-write-lass-blocks (styles-of posts-listing-page)))))
     (:body
      (:div.container
       (render navbar-widget)
       (:div.content
        (:h1 title (:span.rss-sub
                    (:a :href rss-url
                        :title (format nil "Follow ~a via RSS" title)
                        :target "blank"
                        (:span.rss))))
        (render posts-listing-widget :posts posts))
       (render footer-widget))))))
