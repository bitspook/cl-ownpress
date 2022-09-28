(defpackage #:clown-publishers
  (:use #:cl #:serapeum/bundle #:clown)
  (:export
   publish-static
   publish-file
   publish-html-file))
(in-package #:clown-publishers)

(defun copy-dirs (src dest)
  (uiop:run-program (format nil "cp -r ~a ~a" (ppath:join src "*") dest)
                    :output *standard-output*
                    :error-output *standard-output*))

(defun publish-static (dir)
  (copy-dirs dir (conf :dest)))

(defun publish-file (dest content)
  (let ((dest (ppath:join (conf :dest) dest)))
    (ensure-directories-exist dest)
    (str:to-file dest content)))

(defun publish-html-file (dest html &key (clean-urls? t))
  (let* ((dest (if (and clean-urls? (not (string= (ppath:basename dest) "index.html")))
                   (clown:join-paths dest "/index.html")
                   dest)))
    (publish-file dest html)))
