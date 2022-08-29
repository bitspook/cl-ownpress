(in-package :clown-slick.views)

(defun post-css () `())

(defun post-dom (input)
  `(:h1 ,(format nil "~s" (getf input :|title|))))

(defun post (input)
  (let ((title (str:concat (conf 'author) "'s online home"))
        (styles (list (font-defs)
                      (top-level-defs)
                      (post-css))))
    (html-str :title title :cssom nil :dom (post-dom input))))
