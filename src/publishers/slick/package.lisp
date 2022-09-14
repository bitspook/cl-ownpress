(defpackage :clown-slick
  (:use :cl :alexandria)
  (:import-from :spinneret :with-html-string)
  (:export
   *conf* conf conf-merge
   *debug-transpiles*
   *css-vars* css-var css-color font-css top-level-css adjustable-width-css to-css-str button-css
   post-id post-slug post-title post-tags post-category post-published-at post-html-content fetch-posts
   published-post post-public-path
   write-to-file write-html-to-file
   build))
(in-package :clown-slick)

(clown:make-conf
 `(:author nil
   :avatar nil
   :twitter nil
   :linkedin nil
   :github nil
   :handle nil
   :resume nil
   :dest "./_site/"
   :static-dir nil
   :site-url nil
   :mixpanel-token nil
   :rss-max-posts 10
   :control-tags ("blog-post" "published")
   :exclude-tags ("draft")))

(defparameter *debug-transpiles* t)

(defun write-to-file (fname str)
  "Write STR to FNAME ensuring all the required directories exist."
  ;; `ensure-directories-exist' ignore the last part of the path if it is not
  ;; suffixed with "/"; so we can directly use dest as an argument to it
  (ensure-directories-exist fname)
  (str:to-file fname str))

(defun write-html-to-file (dest html &key (clean-urls? t))
  "Write HTML to DEST. If CLEAN-URL?, write as dest/index.html"
  (let* ((dest (if (and clean-urls? (not (string= (ppath:basename dest) "index.html")))
                   (str:concat dest "/index.html")
                   dest)))
    (write-to-file dest html)))

(defun copy-dirs (src dest)
  (uiop:run-program (format nil "cp -r ~a ~a" (ppath:join src "*") dest)
                    :output *standard-output*
                    :error-output *standard-output*))

(defun ensure-dirname (name)
  "Ensure that NAME is a drectory name i.e ends with a `/`."
  (if (str:ends-with? "/" name) name (str:concat name "/")))
