(defpackage :clown-blog.theme.default
  (:nicknames :default-theme)
  (:local-nicknames (:xml :xml-emitter))
  (:use :cl :serapeum/bundle :clown-theme :clown-blog)
  (:import-from :spinneret with-html)
  (:import-from #:clown conf system-local)
  (:import-from #:clown-blog blog-theme)
  (:export theme))
(in-package :default-theme)

(defalias parse-html #'plump:parse)
(defalias html-children #'plump:children)
(defalias serialize-to-html #'plump-dom:serialize)
(defalias get-dom-attribute #'plump-dom:get-attribute)
(defalias nesting-node-p #'plump-dom:nesting-node-p)

(defun html-sections (html-content class)
  "Return sections from HTML-CONTENT which have CLASS."
  (loop
    :for node :across (html-children (parse-html html-content))
    :when (and
           (nesting-node-p node)
           (str:containsp class (get-dom-attribute node "class")))
      :collect (with-output-to-string (str) (serialize-to-html node str))))
