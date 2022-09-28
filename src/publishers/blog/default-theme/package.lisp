(defpackage :clown-blog.themes.default
  (:nicknames :default-theme)
  (:local-nicknames (:xml :xml-emitter))
  (:use :cl :serapeum/bundle)
  (:import-from :spinneret :with-html-string)
  (:import-from #:clown conf system-local)
  (:import-from #:clown-blog blog-theme)
  (:export *debug-transpiles* theme))
(in-package :default-theme)

(defparameter *debug-transpiles* t)

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

