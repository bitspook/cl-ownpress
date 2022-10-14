(defpackage :clown-blog.theme
  (:nicknames :clown-theme)
  (:use :cl :serapeum/bundle)
  (:export defwidget styles-of render
           compile-and-write-lass-blocks))
(in-package :clown-theme)

(defun compile-and-write-lass-blocks (styles)
  (with-output-to-string (stream)
    (dolist (style-block styles)
      (lass:write-sheet
       (lass:compile-sheet style-block)
       :stream stream))))
