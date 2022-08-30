(in-package :clown-slick.views)

(defun post-css () `())

(defun post-dom (post)
  (with-slots ((title clown:title)) post
    `(:h1 ,title)))

(defun post-html (post)
  (let ((title (str:concat (conf 'author) "'s online home"))
        (styles (list (font-defs)
                      (top-level-defs)
                      (post-css))))
    (html-str :title title :cssom styles :dom (post-dom post))))
