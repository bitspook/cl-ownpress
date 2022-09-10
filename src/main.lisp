(in-package :cl-ownpress)

(defclass provider ()
  ((name
    :type string
    :initarg :name
    :initform (error "Provider must have a name")
    :reader provider-name)))

(defgeneric invoke-provider (provider &key)
  (:documentation "Execute a provider."))

(defmethod invoke-provider :before (provider &key)
  (run-pending-migrations))

(defparameter *org-roam-provider*
  (make-instance
   'provider
   :name "org-roam-provider"))

(defmethod invoke-provider ((provider (eql *org-roam-provider*)) &key)
  (let ((script-path (asdf:system-relative-pathname "cl-ownpress" "./src/providers/org-roam/org-roam.el")))
    (with-rpc-server (:processor #'save-post)
      (uiop:run-program (format nil "emacs --script ~a" script-path)
                        :output *standard-output*
                        :error-output *standard-output*))))

(defparameter *fs-provider* (make-instance 'provider :name "filesystem"))

(defun recursive-directory-files (start-dir)
  (let ((files (uiop:directory-files start-dir))
        (subdirs (uiop:subdirectories start-dir)))
    (concatenate 'list files (mapcan #'recursive-directory-files subdirs))))

(defmethod invoke-provider ((provider (eql *fs-provider*)) &key content-dir)
  (let ((content-dir (namestring (uiop:truename* content-dir)))
        (content-files (recursive-directory-files content-dir))
        (script-path (asdf:system-relative-pathname "cl-ownpress" "./src/providers/org-file/org-file.el")))
    (with-rpc-server
        (:processor (lambda (post)
                      (setf (gethash "category" post)
                            (str:replace-all
                             "/" ""
                             (first
                              (ppath:split
                               (str:replace-first
                                content-dir ""
                                (gethash "filepath" post))))))
                      (setf (gethash "provider" post) (provider-name *fs-provider*))
                      (save-post post)))
      (uiop:run-program (format nil "emacs --script ~a ~{~a ~}" script-path content-files)
                        :output *standard-output*
                        :error-output *standard-output*))))

(defun save-post (post)
  "Save a blog POST into the database. POST is a hash-table, to be obtained as a
   message to rpc-server"
  (log:d "Processing post: ~a~%" (gethash "title" post))
  (let ((conn (make-connection)))
    (multiple-value-bind (stmt values)
        (sxql:yield
         (sxql:insert-into :inputs
           (sxql:set= :id (gethash "id" post)
                      :slug (gethash "slug" post)
                      :title (gethash "title" post)
                      :tags (gethash "tags" post)
                      :category (gethash "category" post)
                      :metadata (gethash "metadata" post)
                      :provider (gethash "provider" post)
                      :published_at (gethash "published-at" post)
                      :content_raw (gethash "content_raw" post)
                      :content_html (gethash "content_html" post))
           (sxql:on-conflict-do-nothing)))
      (dbi:fetch-all (dbi:execute (dbi:prepare conn stmt) values)))))
