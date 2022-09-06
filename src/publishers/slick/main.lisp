(in-package :clown-slick)

(defun copy-dirs (src dest)
  (uiop:run-program (format nil "cp -r ~a ~a" (ppath:join src "*") dest)
                    :output *standard-output*
                    :error-output *standard-output*))

(defun build (&optional (dest (conf :dest)) &key (clean-dest? t) (clean-urls? t))
  "Use slick publisher to build a publishable bundle to DEST"
  (let ((dest (if (str:ends-with? "/" dest) dest (str:concat dest "/")))
        (assets-dir (format nil "~a" (asdf:system-relative-pathname "cl-ownpress" "src/publishers/slick/assets/")))
        (dest-assets-dir dest))
    (when (uiop:directory-exists-p dest)
      (if (not clean-dest?)
          (error (format nil "Destination already exists. Aborting [dest=~a]" dest))
          (osicat:delete-directory-and-files dest)))
    (ensure-directories-exist dest)
    (ensure-directories-exist dest-assets-dir)
    (copy-dirs assets-dir dest-assets-dir)

    (slick-views:publish-post-listings)

    (write-html-to-file
     (ppath:join dest "index.html") (slick-views:home-html) :clean-urls? clean-urls?)))
