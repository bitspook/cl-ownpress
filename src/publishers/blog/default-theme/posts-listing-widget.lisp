(in-package #:default-theme)

(defwidget posts-listing-widget (posts)
  :styles `((.listing
             :list-style-type none
             :font-family "Cantarell, sans-serif"

             (li :display flex
                 :line-height 1.3
                 :padding 0.32rem 0
                 :align-items center
                 :margin-bottom 0.6rem))

            (.li-icon :display block
                      :background-repeat no-repeat
                      :background-position 0 0
                      :background-size contain
                      :content ""
                      :width 88px
                      :height 48px
                      :flex-shrink 0)

            (.li-icon--blog :background-image "url(/images/icons/post.svg)")
            (.li-icon--talks :background-image "url(/images/icons/talk.svg)")
            (.li-icon--poems :background-image "url(/images/icons/poems.svg)")

            (.li-title :font-size 1.5rem)

            (.li-meta :margin-top 0.5rem
                      :font-size 1rem
                      :display flex)

            ((:and .li-meta (.li-meta a)) :font-family "Roboto, sans-serif"
                                          :color ,(css-color :secondary)
                                          :display flex)

            (.meta-item :line-height 1
                        :padding 0
                        :padding-right 1em
                        :border-right ,(format nil "1px solid ~a" (css-color :separator)))
            ((:and .meta-item :last-child) :padding-right 0
                                           :border none)

            (.tags :display flex
                   :flex-wrap wrap

                   (a :margin-left 1rem))

            (.rss :width 24px
                  :height 24px
                  :margin-right 1rem
                  :background "url(\"/images/icons/rss.svg\")"
                  :background-repeat no-repeat
                  :background-size contain))
  :render (with-html
            (:ul.listing
             (when (or (not posts) (eq posts nil))
               (:div "No posts yet 😕"))
             (dolist (post posts)
               (:li
                (:span :class (format nil "li-icon li-icon--~a" (clown-blog:post-category post)))
                (:div.li-content
                 (:a.li-title :href (clown-blog:post-public-path post) (clown-blog:post-title post))
                 (:span.li-meta
                  (:span :class "meta-item date"
                         (local-time:format-timestring
                          nil (clown-blog:post-published-at post)
                          :format '(:long-month " " :day ", " :year)))
                  (when-let ((tags (clown-blog:post-tags post)))
                    (:span :class "meta-item tags"
                           (dolist (tag tags)
                             (:a :href (str:concat "/tags/" tag) (str:concat "#" (str:downcase tag)))))))))))))
