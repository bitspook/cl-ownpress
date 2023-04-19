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

(defun publish-render-stack-css (path)
  "Publish all the css used by *RENDER-STACK* to a file at PATH.
Duplicate CSS is omitted at widget level. If a widget has 3 instances w1, w2,
and w3, from which w3's css is customized; w1 and w2 will contribute one css
fragment, and w3 will contribute another; regardless of how small the change in
CSS w3 has."
  (ensure-directories-exist (directory-namestring path))

  ;; We create CSS strings per-widget, so that we can then do a
  ;; simple STRING= to remove duplicates. This allow us to keep
  ;; user's customization done at widget instance level, but still
  ;; remove duplicates.
  (let ((css (mapcar #'css-of *render-stack*)))
    (str:to-file
     path
     (str:join
      (if *print-pretty* #\NewLine "")
      ;; Remove duplicate CSS.
      (nub css :test #'string=)))))

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
    (let* ((*render-stack* nil)
           (dest (publisher-dest pub))
           (html (spinneret:with-html-string
                   (render layout :post post)))
           (slug (slugify (post-title post)))
           (css-filepath (path-join dest "css/" "styles.css")))

      (publish-html
       html slug
       (path-join dest base-dir) clean-urls-p)

      (publish-render-stack-css css-filepath))))
