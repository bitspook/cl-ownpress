(in-package :clown-slick.views)

(defun post-css () `())

(defun post-dom (input)
  `(:h1 ,(format nil "~s" (getf input :|title|))))

(defmacro post (input)
  `(let ((body (post-dom input))
         (spinneret:*suppress-inserted-spaces* t)
         (spinneret:*html-style* (if *debug-transpiles* :human :tree))
         (*print-pretty* *debug-transpiles*)
         (title (str:concat (conf 'author) "'s online home"))
         (styles (to-css-str
                  (font-defs)
                  (top-level-defs)
                  (post-css))))
     (with-html-string
       (:doctype)
       (:html
        (:head
         (:title title)
         (:style (:raw styles)))
        (:body body)))))
