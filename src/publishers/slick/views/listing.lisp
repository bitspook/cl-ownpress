(in-package :clown-slick.views)

(defun listing-css ()
  `(,@(clown-slick:adjustable-width-css ".content")

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
                :padding-right 1em)

    (.tags :display flex
           :flex-wrap wrap
           :text-transform capitalize

           (a :margin-left 1em))))

(defun listing-dom ()
  "DOM for rendering a listing of posts.

Required context:
- `title`: Title of the listing. Shown as a heading.
- `posts`: List of `post's"
  `(:div.container
    ,(navbar-dom)
    (:div.content
     (:h1 title)
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
           (:span :class "meta-item tags"
                  (dolist (tag (clown:post-tags post))
                    (:a :href (str:concat "/tags/" tag) (str:capitalize tag)))))))
        posts)))))

(defmacro listing-html (&key title posts)
  (let ((css '(top-level-css (navbar-css) (listing-css))))
    `(html-str (:title ,title :css ,css)
       ,(listing-dom))))

(defun publish-listing (&key posts title dest)
  (let ((html (listing-html :title title :posts posts)))
    (clown-slick:write-html-to-file dest html)))

(defun publish-post-listings ()
  "Publish all the listing pages. It includes:
   1. Listing page for each tag
   2. Listing page for each category
   3. Listing page with all the posts"
  (let ((tagged-posts (make-hash-table :test 'equal))
        (categorized-posts (make-hash-table :test 'equal))
        (all-posts (publish-recent-posts -1)))
    (loop :for post :in all-posts
          :for cat := (clown:post-category post)
          :do (loop
                :for tag :in (clown:post-tags post)
                :do (if (gethash tag tagged-posts)
                        (push post (gethash tag tagged-posts))
                        (setf (gethash tag tagged-posts) (list post))))
              (if (gethash cat categorized-posts)
                  (push post (gethash cat categorized-posts))
                  (setf (gethash cat categorized-posts) (list post))))

    (loop :for tag :being :each :hash-key :of tagged-posts
          :do (publish-listing
               :posts (gethash tag tagged-posts)
               :title (format nil "Posts tagged `~a'" tag)
               :dest (ppath:join (conf :dest) "tags" tag "index.html")))

    (maphash (lambda (cat posts)
               (publish-listing :posts posts
                                :title (format nil "Posts categorized as `~a'" cat)
                                :dest (ppath:join (conf :dest) cat "index.html")))
             categorized-posts)

    (publish-listing
     :posts all-posts
     :title "All Posts"
     :dest (ppath:join (conf :dest) "archive/index.html"))

    t))
