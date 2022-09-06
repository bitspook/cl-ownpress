(in-package :clown-slick.views)

(defun listing-css () nil)

(defun listing-dom ()
  "DOM for rendering a listing of posts.

Required context:
- `title`: Title of the listing. Shown as a heading.
- `posts`: List of `post's"
  `(:div
    (:h1 title)
    (:ul.generic-listing
     (dolist (post posts)
       (:li.generic-li
        (:span :class (str:concat "li-icon li-icon--" (clown:post-category post)))
        (:div.generic-li-conent
         (:a.generic-li-title
          :href (clown:post-output-path post)
          (clown:post-title post))
         (:span.generic-li-meta
          (:span :class "meta-item date"
                 (local-time:format-timestring
                  nil (clown:post-published-at post)
                  :format '(:long-month " " :day ", " :year)))
          (:span :class "meta-item tags"
                 (dolist (tag (clown:post-tags post))
                   (:a :href (str:concat "/tags/" tag) (str:capitalize tag)))))))
       posts))))

(defmacro listing-html (&key title posts)
  (let ((css '(top-level-css)))
    `(html-str (:title ,title :css ,css)
       ,(listing-dom))))

(defun publish-listing (&key posts title dest)
  (let ((html (listing-html :title title :posts posts)))
    (clown-slick:write-html-to-file dest html)))
