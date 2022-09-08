(defpackage :clown-slick
  (:use :cl :alexandria)
  (:import-from :spinneret :with-html-string)
  (:export
   conf
   *conf*
   *debug-transpiles*
   *css-vars*
   css-var
   css-color
   font-css
   top-level-css
   adjustable-width-css
   build
   write-html-to-file))
(in-package :clown-slick)

(defparameter *conf*
  `((:author . nil)
    (:avatar . nil)
    (:twitter . nil)
    (:linkedin . nil)
    (:github . nil)
    (:handle . nil)
    (:resume . nil)
    (:dest . "./_site/")
    (:static-dir . nil)
    (:mixpanel-token . nil)))

(defparameter *debug-transpiles* t)

(defun conf (key)
  (cdr (assoc key *conf*)))

(defun write-html-to-file (dest html &key (clean-urls? t))
  "Write HTML to DEST. If CLEAN-URL?, write as dest/index.html"
  (let* ((dest (if (and clean-urls? (not (string= (ppath:basename dest) "index.html")))
                   (str:concat dest "/index.html")
                   dest)))
    ;; `ensure-directories-exist' ignore the last part of the path if it is not
    ;; suffixed with "/"; so we can directly use dest as an argument to it
    (ensure-directories-exist dest)
    (str:to-file dest html)))
