(in-package :clown-slick)

(defun write-html-to-file (dest html &key clean-urls?)
  "Write HTML to DEST. If CLEAN-URL?, write as dest/index.html"
  (let ((dest (if (and clean-urls? (not (string= (ppath:basename dest) "index.html")))
                  (str:concat dest "/index.html")
                  dest))
        (dest-dirs (ppath:dirname dest)))
    (unless (or (string= dest-dirs ".") (str:emptyp dest-dirs))
      (ensure-directories-exist dest-dirs))
    (str:to-file dest html)))

(defun copy-dirs (src dest)
  (uiop:run-program (format nil "cp -r ~a ~a" (ppath:join src "*") dest)
                    :output *standard-output*
                    :error-output *standard-output*))

(defun main (dest &key clean-dest? clean-urls?)
  "Use slick publisher to build a publishable bundle to DEST"
  (let ((dest (if (str:ends-with? "/" dest) dest (str:concat dest "/")))
        (assets-dir "./assets/")
        (dest-assets-dir dest))
    (when (uiop:directory-exists-p dest)
      (if (not clean-dest?)
          (error (format nil "Destination already exists. Aborting [dest=~a]" dest))
          (osicat:delete-directory-and-files dest)))
    (ensure-directories-exist dest)
    (ensure-directories-exist dest-assets-dir)
    (copy-dirs assets-dir dest-assets-dir)

    (write-html-to-file
     (ppath:join dest "index.html") (slick-views:home) :clean-urls? clean-urls?)))

(defun publish-all-posts ()
  (let* ((query (sxql:yield
                 (sxql:select (:*)
                   (sxql:from :inputs))))
         (conn (clown:make-connection))
         (query (dbi:execute (dbi:prepare conn query))))
    (loop :for row := (dbi:fetch query)
          :while row
          :collect (slick-views:post row))))
