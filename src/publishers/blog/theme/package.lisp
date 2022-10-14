(defpackage :clown-blog.theme
  (:nicknames :clown-theme)
  (:use :cl :serapeum/bundle)
  (:export defwidget styles-of render
           compile-and-write-lass-blocks with-html-string
           *debug-transpiles*))
(in-package :clown-theme)

(defvar *debug-transpiles* t)

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
