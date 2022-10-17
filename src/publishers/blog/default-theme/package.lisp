(defpackage :clown-blog.theme.default
  (:nicknames :default-theme)
  (:local-nicknames (:xml :xml-emitter))
  (:use :cl :serapeum/bundle :clown-theme :clown-blog)
  (:import-from :spinneret with-html)
  (:import-from #:clown conf system-local)
  (:import-from #:clown-blog blog-theme)
  (:export theme))
(in-package :default-theme)

