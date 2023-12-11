(in-package #:in.bitspook.cl-ownblog)

(defclass blog-post-listing-publisher (html-publisher)
  ((asset-publisher :initform (error "Not implemented"))
   (posts-provider :initform (error "Not implemented")))
  (:documentation "Publish a blog post."))

(defmethod publish ((pub blog-post-listing-publisher) &key post)
  "Publish blog POST."
  (format t "Publishing ~a" post))

(defmethod public-path ((pub blog-post-listing-publisher) &key)
  nil)
