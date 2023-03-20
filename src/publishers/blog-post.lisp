(in-package #:clown-publishers)

(defclass blog-post-publisher (publisher)
  ((asset-publisher
    :documentation "A PUBLISHER to use for publishing assets (e.g Css, Js, images)."))
  (:documentation "Publish a blog post."))

(defmethod publish ((pub blog-post-publisher) &key post)
  "Publish blog POST."
  (format t "Publishing ~a" post))
