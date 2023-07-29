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

;; TODO Write tests for the poor thing
(defun append-content-hash (filename md5-hash &optional (hash-length 6))
  "Append HASH-LENGTH long MD5-HASH to FILENAME"
  (let* ((parts (str:rsplit "." filename))
         (ext (last-elt parts))
         (name (str:join "." (nbutlast parts)))
         (hash (format nil "~{~X~}" (map 'list #'identity md5-hash)))
         (appendable-hash (str:substring 0 hash-length hash)))
    (str:concat name "-" appendable-hash "." ext)))

(defmethod publish ((pub asset-publisher) &key path content hash-artifact-name-p)
  "Publish CONTENT as a file at PATH. PATH is a suggestion for where the new file should be created,
path of the created artifact might differ, for example, asset-publisher might decide to change the
name of created-artifact in case of a conflict.

- If CONTENT is a `pathname', it is copied to PATH (recursively if it is a directory)
- If HASH-ARTIFACT-NAME-P is non-`nil', published artifact's name (i.e filename without extension)
  is appended with hash of the content of the artifact

Returns the path of created artifact"
  (let ((dest (path-join (publisher-dest pub) path)))
    (when (or (uiop:file-exists-p dest)
              (uiop:directory-exists-p dest))
      (error 'artifact-already-exists :path dest))

    (ensure-directories-exist (directory-namestring dest))

    (when (pathnamep content)
      (when (uiop:directory-exists-p content)
        (when hash-artifact-name-p
          (error "HASH-ARTIFACT-NAME-P can not be used to publish a directory"))

        (return-from publish (copy-dirs content dest)))

      (return-from publish
        (uiop:copy-file
         content
         (if hash-artifact-name-p
             (append-content-hash (namestring dest) (md5sum-file content))
             dest))))

    (str:to-file (if hash-artifact-name-p
                     (append-content-hash (namestring dest) (md5sum-string content))
                     dest)
                 content)))
