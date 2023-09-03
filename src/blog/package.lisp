(defpackage #:cl-ownblog
  (:use #:cl #:serapeum/bundle))

(defclass blog-post ()
  ((title :initarg :title
          :initform (error "Post `title` is required")
          :accessor post-title)
   (description :initarg :description
                :initform (error "Post `description` is required")
                :accessor post-description)
   (tags :initarg :tags
         :initform '()
         :accessor post-tags)
   (created-at :initarg :created-at
               :initform (error "Post `created-at' is required")
               :accessor post-created-at)
   (updated-at :initarg :updated-at
               :initform (error "Post `updated-at' is required")
               :accessor post-updated-at)
   (body :initarg :body
         :initform (error "Post `body` is required")
         :accessor post-body)))
