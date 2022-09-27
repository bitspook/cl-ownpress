(defpackage #:clown-publishers
  (:use #:cl #:serapeum/bundle)
  (:export ))
(in-package #:clown-publishers)

(defclass static-publisher (publisher)
  ((dir :initform (error "static dir is required")
        :initarg dir)))

(defun publish-static (dir)
  (copy-dirs pub (conf :dest)))

(defclass atom-feed-publisher (publisher)
  ((posts :initform (error "Posts are required")
          :initarg :posts)
   (filepath :initform "feed.xml"
             :initarg :filepath)))

(defun publish-file (dest content)
  (let ((dest (ppath:join (conf :dest) dest)))
    (ensure-directories-exist dest)
    (str:to-file dest content)))

(defun publish-html-file (dest html &key (clean-urls? t))
  (let* ((dest (if (and clean-urls? (not (string= (ppath:basename dest) "index.html")))
                   (clown:join-paths dest "/index.html")
                   dest)))
    (publish-file dest html)))

(defun publish-rss-feed (&key title filepath items)
  (let ((feed-url (clown:join-paths (conf :site-url) filepath))))
  (publish-file
   filepath (rss-str :title title :feed-url filepath)))

(defun publish-listing (&key posts title dest)
  (let* ((html (listing-html :title title :posts posts :rss-url rss-url)))
    (clown-slick:write-html-to-file dest html)))

(defun publish-home (&key title)
  (publish-html-file "index.html" (home-html :title title)))
