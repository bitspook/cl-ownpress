(in-package :clown-slick)

(defun build (&key (clean-dest? t) (clean-urls? t))
  "Use slick publisher to build a publishable bundle to DEST"
  (let ((dest (ensure-dirname (conf :dest)))
        (pub-assets-dir (uiop:unix-namestring
                         (asdf:system-relative-pathname "cl-ownpress" "src/publishers/slick/assets/")))
        (user-assets-dir (when-let ((dir (conf :static-dir))) (ensure-dirname dir))))
    (when (uiop:directory-exists-p dest)
      (if (not clean-dest?)
          (error (format nil "Destination already exists. Aborting [dest=~a]" dest))
          (osicat:delete-directory-and-files dest)))

    (ensure-directories-exist dest)
    (copy-dirs pub-assets-dir dest)
    (when user-assets-dir
      (copy-dirs user-assets-dir dest))

    (slick-views:publish-post-listings)

    (write-html-to-file
     (ppath:join dest "index.html") (slick-views:home-html) :clean-urls? clean-urls?)))
