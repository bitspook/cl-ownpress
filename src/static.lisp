(in-package #:in.bitspook.cl-ownpress)

;; TODO Replace this with CL implementation
;; TODO Write tests for copy-dirs
(defun copy-dirs (src dest)
  "Recursively copy SRC to DEST. It uses *nix `cp' under the hood, so don't use it
on Windows maybe."
  (uiop:run-program (format nil "cp -r ~a ~a" (path-join src "*") dest)
                    :output *standard-output*
                    :error-output *standard-output*))

(export-always 'append-content-hash)
;; TODO Write tests for the poor thing
(defun append-content-hash (filename md5-hash &optional (hash-length 6))
  "Append HASH-LENGTH long MD5-HASH to FILENAME"
  (let* ((parts (str:rsplit "." filename))
         (ext (last-elt parts))
         (name (str:join "." (nbutlast parts)))
         (hash (format nil "~{~X~}" (map 'list #'identity md5-hash)))
         (appendable-hash (str:substring 0 hash-length hash)))
    (str:concat name "-" appendable-hash "." ext)))

(export-always 'file-already-exists)
(define-condition file-already-exists (error)
  ((path :initarg :path :reader file-path))
  (:documentation "Condition indicating that an file at the same location already exists."))

(export-always 'skip-existing)
(defun skip-existing (c)
  (invoke-restart 'skip-existing))

(export-always 'publish-static)
(defmethod publish-static (&key dest-dir path content hash-file-name-p)
  "Publish CONTENT as a file or at PATH. PATH is a suggestion for where the new file should be created,
path of the created file might differ, for example, it might change the name of created file in case
of a conflict.

- If CONTENT is a `pathname', it is copied to PATH (recursively if it is a directory)
- If HASH-FILE-NAME-P is non-`nil', published artifact's name (i.e filename without extension)
  is appended with hash of the content of the artifact

Returns the path of created file/dir."
  (labels ((publish ()
             (let ((dest-dir (namestring dest-dir))
                   (dest (base-path-join dest-dir (or path ""))))
               (when (or (uiop:file-exists-p dest)
                         (uiop:directory-exists-p dest))
                 (error 'file-already-exists :path dest))

               (ensure-directories-exist (directory-namestring dest))

               (when (pathnamep content)
                 (when (uiop:directory-exists-p content)
                   (when hash-file-name-p
                     (error "HASH-FILE-NAME-P can not be used to publish a directory"))

                   (return-from publish-static
                     (progn
                       (copy-dirs content dest)
                       (str:replace-first dest-dir "" (namestring dest)))))

                 (return-from publish-static
                   (let ((dest (if hash-file-name-p
                                   (append-content-hash (namestring dest) (md5sum-file content))
                                   dest)))
                     (uiop:copy-file content dest)
                     (str:replace-first dest-dir "" (namestring dest)))))

               (let ((dest (if hash-file-name-p
                               (append-content-hash (namestring dest) (md5sum-string content))
                               dest)))
                 (str:to-file dest content)
                 (str:replace-first dest-dir "" (namestring dest))))))
    ;; TODO Write tests for restarts
    (restart-case (publish)
      (skip-existing () nil))))
