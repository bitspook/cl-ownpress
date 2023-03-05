(defpackage :clown-blog.theme
  (:nicknames :clown-theme)
  (:use :cl :serapeum/bundle)
  (:export defwidget styles-of render compile-and-write-lass-blocks
           with-html-string *debug-transpiles*)
  ;; theme class
  (:export
   theme theme-home-template theme-listing-template theme-post-template
   theme-about-me-template theme-project-template theme-projects-listing-template theme-assets-dir))
(in-package :clown-theme)

(defvar *debug-transpiles* t
  "Transpiling of HTML/JS/CSS should be done in debug mode i.e without
minification or obfuscation.")

(defun compile-and-write-lass-blocks (styles)
  (let ((lass:*pretty* *debug-transpiles*))
    (with-output-to-string (stream)
      (dolist (style-block styles)
        (lass:write-sheet
         (lass:compile-sheet style-block)
         :stream stream)))))

(defmacro with-html-string (dom)
  `(let ((spinneret:*suppress-inserted-spaces* t)
         (spinneret:*html-style* (if *debug-transpiles* :human :tree))
         (*print-pretty* *debug-transpiles*)
         (ps:*ps-print-pretty* *debug-transpiles*))
     (spinneret:with-html-string
       ,dom)))

(defclass theme ()
  ((home :initform (error "Home view is required")
         :initarg :home
         :accessor theme-home-template)
   (listing :initform (error "Listing view is required")
            :initarg :listing
            :accessor theme-listing-template)
   (post :initform (error "Post view is required")
         :initarg :post
         :accessor theme-post-template)
   (project :initform (error "Project template is required")
            :initarg :project
            :accessor theme-project-template)
   (projects-listing :initform (error "Projects-listing template is required")
                     :initarg :projects-listing
                     :accessor theme-projects-listing-template)
   (about-me :initform (error "About page template is required")
             :initarg :about-me
             :accessor theme-about-me-template)
   (assets-dir :initform (error "Assets directory is required")
               :initarg :assets-dir
               :accessor theme-assets-dir)))
