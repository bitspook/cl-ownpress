(in-package #:clown-publishers)

(export-always 'blog-post)
(export-always 'post-title)
(export-always 'post-description)
(export-always 'post-body)
(defclass blog-post ()
  ((title :initarg :title
          :initform (error "Post `title` is required")
          :accessor post-title)
   (body :initarg :body
         :initform (error "Post `body` is required")
         :accessor post-body)
   (description :initarg :description
                :initform (error "Post `description` is required")
                :accessor post-description)))

(export-always 'blog-post-publisher)
(defclass blog-post-publisher (html-publisher)
  ((asset-publisher
    :initarg :asset-publisher
    :documentation "A PUBLISHER to use for publishing assets (e.g Css, Js, images).")
   (base-dir
    :initarg :base-dir
    :documentation "Directory in which generated HTML files will be stored."))
  (:documentation "Publish a blog post."))

(defmethod publish ((pub blog-post-publisher)
                    &key post layout (clean-urls-p t))
  "Publish blog POST using LAYOUT widget
LAYOUT must be a WIDGET which accepts the POST as an argument."
  (with-slots (base-dir) pub
    (let* ((html (spinneret:with-html-string
                   (dom-of layout :post post)))
           (slug (slugify (post-title post)))
           (target-dir (path-join
                        (publisher-dest pub) base-dir
                        (if clean-urls-p (concat slug "/") ""))))

      (ensure-directories-exist target-dir)

      (str:to-file
       (path-join target-dir
                  (if clean-urls-p "index.html"
                      (concat slug ".html")))
       html))))
