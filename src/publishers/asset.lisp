(in-package #:clown-publishers)

(export-always 'asset-publisher)
(defclass asset-publisher (publisher)
  nil
  (:documentation "Publish static assets."))

;; TODO Replace this with CL implementation
;; TODO Write tests for copy-dirs
(defun copy-dirs (src dest)
  "Recursively copy SRC to DEST. It uses *nix `cp' under the hood, so don't use it
on Windows maybe."
  (uiop:run-program (format nil "cp -r ~a ~a" (path-join src "*") dest)
                    :output *standard-output*
                    :error-output *standard-output*))

(defmethod publish ((pub asset-publisher) &key path content)
  "Publish CONTENT as a file at PATH. If CONTENT is a `pathname', it
is (recursively if it is a directory) copied to PATH. PATH is a suggestion for
where the new file should be created, path of the created artifact might differ.
For example: asset-publisher might decide to change the name of created-artifact
in case of a conflict. Return the path of created artifact."
  (let ((dest (path-join (publisher-dest pub) path)))
    (when (or (uiop:file-exists-p dest)
              (uiop:directory-exists-p dest))
      (error 'artifact-already-exists :path dest))

    (ensure-directories-exist (directory-namestring dest))

    (when (pathnamep content)
      (when (uiop:directory-exists-p content)
        (return-from publish (copy-dirs content dest)))

      (return-from publish (uiop:copy-file content dest)))

    (str:to-file dest content)))

;; TODO
;; - Write tests for asset-publisher
;; - Write tests for blog-post-publisher
