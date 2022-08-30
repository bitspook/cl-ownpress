(defpackage :clown-slick.views
  (:nicknames :slick-views)
  (:use :cl)
  (:import-from :spinneret :with-html-string)
  (:import-from :clown-slick
                conf
                *debug-transpiles*
                font-defs
                css-var
                css-color
                top-level-defs
                button-defs
                to-css-str)
  (:export home-html
           navbar-html
           post-html))
(in-package :slick-views)

(defmacro html-str (&key title cssom dom)
  `(let ((spinneret:*suppress-inserted-spaces* t)
         (spinneret:*html-style* (if *debug-transpiles* :human :tree))
         (*print-pretty* *debug-transpiles*)
         (styles (apply #'to-css-str ,cssom)))
     (with-html-string
       (:doctype)
       (:html
        (:head
         (:title ,title)
         (:style (:raw styles)))
        (:body (spinneret:interpret-html-tree ,dom))))))
