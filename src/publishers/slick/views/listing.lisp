(in-package :clown-slick.views)

(defmacro listing-html (&key title posts rss-url)
  (let ((css '((top-level-css)
               (navbar-css)
               (listing-css)
               (footer-css))))
    `(html-str (:title ,title :css ,css :rss-url rss-url)
       ,(listing-dom))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun listing-css ()
    `(,@(clown-slick:adjustable-width-css ".content")
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

  (defun listing-dom ()
    "DOM for rendering a listing of posts.

     Required context:
     - `title`: Title of the listing. Shown as a heading.
     - `posts`: List of `post's"
    `(:div.container
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
           (:span :class (str:concat "li-icon li-icon--" (clown:post-category post)))
           (:div.li-conent
            (:a.li-title
             :href (clown:post-output-path post)
             (clown:post-title post))
            (:span.li-meta
             (:span :class "meta-item date"
                    (local-time:format-timestring
                     nil (clown:post-published-at post)
                     :format '(:long-month " " :day ", " :year)))
             (when-let ((tags (post-tags post)))
               (:span :class "meta-item tags"
                      (dolist (tag tags)
                        (:a :href (str:concat "/tags/" tag) (str:concat "#" (str:downcase tag)))))))))
          posts)))
      ,(footer-dom))))

(defun publish-listing (&key posts title dest)
  (let* ((feed-dest (clown:join-paths (ppath:dirname dest) "feed.xml"))
         (rss-url (clown:join-paths (conf :site-url) (str:replace-first (conf :dest) "" feed-dest)))
         (html (listing-html :title title :posts posts :rss-url rss-url))
         (rss (rss-str (subseq posts 0 (min (length posts) (conf :rss-max-posts)))
                       :title title
                       :feed-url rss-url
                       :feed-updated-at (clown:post-published-at (first posts)))))
    (clown-slick:write-html-to-file dest html)
    (clown-slick:write-to-file feed-dest rss)))

(defun publish-post-listings ()
  "Publish all the listing pages. It includes:
   1. Listing page for each tag
   2. Listing page for each category
   3. Listing page with all the posts"
  (let ((tagged-posts (make-hash-table :test 'equal))
        (categorized-posts (make-hash-table :test 'equal))
        (all-posts (remove-if-not #'clown:post-category (publish-recent-posts -1))))
    (loop :for post :in all-posts
          :for cat := (clown:post-category post)
          :do (loop
                :for tag :in (post-tags post)
                :do (if (gethash tag tagged-posts)
                        (push post (gethash tag tagged-posts))
                        (setf (gethash tag tagged-posts) (list post))))
              (if (gethash cat categorized-posts)
                  (push post (gethash cat categorized-posts))
                  (setf (gethash cat categorized-posts) (list post))))

    (loop :for tag :being :each :hash-key :of tagged-posts
          :do (publish-listing
               :posts (reverse (gethash tag tagged-posts))
               :title (format nil "Posts tagged `~a'" tag)
               :dest (ppath:join (conf :dest) (format nil "tags/~a/" tag))))

    (maphash (lambda (cat posts)
               (publish-listing :posts (reverse posts)
                                :title (format nil "Posts categorized as `~a'" cat)
                                :dest (ppath:join (conf :dest) (format nil "~a/" cat))))
             categorized-posts)

    (publish-listing
     :posts all-posts
     :title "All Posts"
     :dest (ppath:join (conf :dest) "archive/"))

    t))
