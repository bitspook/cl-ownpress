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

(defun publish-html (html slug dir &optional (clean-urls-p t))
  "Write HTML to a file for SLUG in DIR. If CLEAN-URLS-P is non-nil, the file is
named SLUG/index.html, otherwise SLUG.html"
  (let* ((target-dir (path-join dir (if clean-urls-p (concat slug "/") ""))))
    (ensure-directories-exist target-dir)

    (str:to-file
     (path-join target-dir (if clean-urls-p "index.html" (concat slug ".html")))
     html)))

;; css/styles.css is hard-coded for now. I have a few things in mind to how to
;; publish styles, which I'll make a decision on once this API is put in
;; production. A few things I have in mind right now:
;; 1. Make the css-files unique by their content. i.e append a hash obtained
;; from css content to the file name.
;; 2. Make the css-file name somehow configurable
(defmethod publish ((pub blog-post-publisher)
                    &key post layout (clean-urls-p t))
  "Publish blog POST using LAYOUT widget
LAYOUT must be a WIDGET which accepts the POST as an argument."
  (with-slots (base-dir) pub
    (let* ((html (spinneret:with-html-string
                   (dom-of layout :post post)))
           (slug (slugify (post-title post)))
           (all-child-widgets (child-widgets layout)))

      (publish-html html slug (path-join (publisher-dest pub) base-dir) clean-urls-p))))
