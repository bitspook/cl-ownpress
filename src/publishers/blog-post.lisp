(in-package #:clown-publishers)

(defclass blog-post-publisher (html-publisher)
  ((asset-publisher
    :documentation "A PUBLISHER to use for publishing assets (e.g Css, Js, images).")
   (base-dir
    :documentation "Directory in which generated HTML files will be stored."))
  (:documentation "Publish a blog post."))

(defclass blog-post ()
  ((title :initarg :title
          :initform (error "Post `title` is required")
          :accessor post-title)
   (body :initarg :body
         :initform (error "Post `body` is required"))
   (description :initarg :description
                :initform (error "Post `description` is required")
                :accessor post-description)))

(defmethod publish ((pub blog-post-publisher) &key post)
  "Publish blog POST."
  nil)
