(defpackage :clown-slick.views
  (:nicknames :slick-views)
  (:local-nicknames (:xml :xml-emitter))
  (:use :cl :alexandria :clown-slick)
  (:import-from :spinneret :with-html-string)
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
