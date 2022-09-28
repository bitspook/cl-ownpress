(in-package :default-theme)

(defmacro listing-html (title posts &key rss-url)
  (let ((css '((top-level-css)
               (navbar-css)
               (listing-css)
               (footer-css))))
    `(html-str (:title ,title :css ,css :rss-url ,rss-url)
       ,(listing-dom title posts rss-url))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun listing-css ()
    `(,@(adjustable-width-css ".content")
      (.content (h1 :display flex
                    :align-items center
                    (.rss-sub :margin 0
                              :margin-left 2rem)))

      (.listing :list-style-type none
                :font-family "Cantarell, sans-serif"

                (li :display flex
                    :line-height 1.3
                    :padding 0.32em 0
                    :align-items center
                    :margin-bottom 0.6em))

      (.li-icon :display block
                :background-repeat no-repeat
                :background-position 0 0
                :content ""
                :width 88px
                :height 48px
                :flex-shrink 0)

      (.li-icon--blog :background-image "url(/images/icons/post.svg)")
      (.li-icon--talks :background-image "url(/images/icons/talk.svg)")
      (.li-icon--poems :background-image "url(/images/icons/poems.svg)"
                       :background-size contain)

      (.li-title :font-size 1.5em)

      (.li-meta :margin-top 0.5em
                :font-size 1em
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

             (a :margin-left 1em))

      (.rss :width 24px
            :height 24px
            :margin-right 1rem
            :background "url(\"/images/icons/rss.svg\")"
            :background-repeat no-repeat
            :background-size contain)))

  (defun listing-dom (title posts rss-url)
    "DOM for rendering a listing of posts.

Required context:
 - `title`: Title of the listing. Shown as a heading.
 - `posts`: List of `post's
 - `rss-url`: URL to RSS feed for this listing
     - "
    `(let ((title ,title)
           (posts ,posts)
           (rss-url ,rss-url))
       (:div.container
        ,(navbar-dom)
        (:div.content
         (:h1 title (:span.rss-sub
                     (:a :href rss-url
                         :title (format nil "Follow ~a via RSS" title)
                         :target "blank"
                         (:span.rss))))
         (:ul.listing
          (dolist (post posts)
            (:li
             (:span :class (str:concat "li-icon li-icon--" (clown-blog:post-category post)))
             (:div.li-conent
              (:a.li-title
               :href (clown-blog:post-public-path post)
               (clown-blog:post-title post))
              (:span.li-meta
               (:span :class "meta-item date"
                      (local-time:format-timestring
                       nil (clown-blog:post-published-at post)
                       :format '(:long-month " " :day ", " :year)))
               (when-let ((tags (clown-blog:post-tags post)))
                 (:span :class "meta-item tags"
                        (dolist (tag tags)
                          (:a :href (str:concat "/tags/" tag) (str:concat "#" (str:downcase tag)))))))))
            posts)))
        ,(footer-dom)))))
