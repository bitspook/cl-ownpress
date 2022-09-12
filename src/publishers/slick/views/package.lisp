(defpackage :clown-slick.views
  (:nicknames :slick-views)
  (:use :cl :alexandria)
  (:import-from :spinneret :with-html-string)
  (:import-from :clown-slick
                conf
                *debug-transpiles*
                font-css
                css-var
                css-color
                top-level-css
                button-css
                to-css-str
                post-tags)
  (:export home-html
           post-html
           publish-post-listings))
(in-package :slick-views)

(defmacro html-str ((&key title css (rss-url nil)) &body dom)
  `(let ((spinneret:*suppress-inserted-spaces* t)
         (spinneret:*html-style* (if *debug-transpiles* :human :tree))
         (*print-pretty* *debug-transpiles*)
         (styles (to-css-str ,@css)))
     (with-html-string
       (:doctype)
       (:html
        (:head
         (:title ,title)
         (when ,rss-url
           (:link :rel "alternate" :type "application/atom+xml" :title "RSS Feed" :href ,rss-url))
         (:style (:raw styles)))
        (:body ,@dom)
        ,(mixpanel-dom)))))