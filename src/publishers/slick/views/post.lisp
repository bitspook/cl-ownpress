(in-package :clown-slick.views)

(defmacro post-html ()
  "Produce HTML required for publishing a `post'. A variable named 'post' must be
present at execution"
  (let ((styles '((top-level-css)
                  (post-css))))
    `(html-str (:title (post-title post) :css ,styles)
       ,(post-dom))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun post-css ()
    `(,@(navbar-css)
      (.content :font-family "Cantarell, sans-serif"
                :font-style normal
                :font-weight 500
                :font-size 1.5em
                :line-height 1.4
                :margin 0 auto
                :padding 0 124px)
      ,@(clown-slick:adjustable-width-css ".content")
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
                         :margin-right 0))
      ,@(footer-css)))

  (defun post-dom ()
    `(:div ,(navbar-dom)
           (:section :class "content"
                     (:header :class "content-header"
                              (:h1 (post-title post))
                              (:span :class "content-meta"
                                     (:span :class "meta-item date"
                                            (local-time:format-timestring
                                             nil (post-published-at post)
                                             :format '(:long-month " " :day ", " :year)))
                                     (when-let ((tags (post-tags post)))
                                       (:ul :class "meta-item tags"
                                            (dolist (tag tags)
                                              (:li.tag (:a :href (str:concat "/tags/" tag) (str:concat "#" (str:downcase tag)))))))))
                     (:article :class "main-article" (:raw (post-html-content post))))
           ,(footer-dom)))

  (defun publish-post (post)
    "Publish a POST. It writes the HTML to a file, update database. Returns
   `published-post'."
    (let* ((public-path (ppath:normpath (str:concat "/" (post-category post) "/" (post-slug post) "/")))
           (dest (str:concat (conf :dest) public-path)))
      (clown-slick:write-html-to-file dest (post-html))

      (make-instance 'clown-slick:published-post
                     :title (post-title post)
                     :id (post-id post)
                     :slug (post-slug post)
                     :tags (post-tags post)
                     :public-path public-path
                     :category (post-category post)
                     :published-at (post-published-at post)
                     :html-content (post-html-content post))))

  (defun publish-recent-posts (&optional (limit 5))
    (mapcar #'publish-post (fetch-posts (sxql:limit limit)
                                        (sxql:where (:not-in :category (conf :unlisted-categories))))))

  (defun publish-unlisted-posts (&optional (limit -1))
    (mapcar #'publish-post (fetch-posts (sxql:limit limit)))))
