(in-package :default-theme)

(defwidget post-page (post)
  :styles
  (concatenate
   'list
   (top-level-css)
   `((.content :font-family "Cantarell, sans-serif"
               :font-style normal
               :font-weight 500
               :font-size 1.5em
               :line-height 1.4
               :margin 0 auto
               :padding 0 124px)
     ,@(adjustable-width-css ".content")
     (.content-header (h1 :margin-bottom 1rem :font-size 2em))
     (.content-meta :margin-top 0
                    :margin-bottom 2rem
                    :font-family "Roboto, sans-serif"
                    :color ,(css-color :secondary)
                    :display flex

                    (.meta-item :border-right ,(format nil "1px solid ~a" (css-color :separator))
                                :line-height 1
                                :padding 0 1rem
                                :font-size 0.8rem
                                :display flex)
                    ((:and .meta-item :last-child) :padding-right 0
                                                   :border none)
                    (.date :padding 0
                           :padding-right 1rem)

                    (.tags  :list-style-type none
                            :display flex
                            :flex-wrap wrap

                            (li :padding-right 1rem
                                (a :color ,(css-color :secondary)))))

     (.main-article :font-family "Cantarell, sans-serif"
                    :min-height 500px

                    ((:or strong b) :font-weight bold)
                    (p :font-family inherit
                       :font-size inherit
                       :margin 1rem 0)
                    (h3 :margin 1.2rem 0 0.4rem 0)
                    (a :text-decoration underline)
                    (ul :margin-left 1.4rem)
                    (li :margin 0.7rem
                        :margin-right 0))))
  :render
  (with-html
    (:html
     (:head
      (:title (clown-blog:post-title post))
      (:link :rel "alternate" :type "application/atom+xml" :title "RSS Feed" :href "/archive/feed.xml")
      (:style (:raw (compile-and-write-lass-blocks (styles-of post-page)))))
     (:body
      (:div
       (render navbar-widget)
       (:section
        :class "content"
        (:header
         :class "content-header"
         (:h1 (clown-blog:post-title post))
         (:span
          :class "content-meta"
          (:span
           :class "meta-item date"
           (local-time:format-timestring
            nil (clown-blog:post-published-at post)
            :format '(:long-month " " :day ", " :year)))
          (when-let ((tags (clown-blog:post-tags post)))
            (:ul
             :class "meta-item tags"
             (dolist (tag tags)
               (:li.tag
                (:a :href
                    (str:concat "/tags/" tag)
                    (str:concat "#" (str:downcase tag)))))))))
        (:article :class "main-article"
                  (:raw (clown-blog:post-html-content post))))
       (render footer-widget))))))
