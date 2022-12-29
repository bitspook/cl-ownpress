(in-package :clown-providers)

(defclass org-file-provider (provider)
  ((content-dir :initarg :content-dir
                :initform (error "content-dir is required")
                :documentation "Directory which will be traversed recursively to pick all .org files")
   (keep-file-if
    :initarg :test
    :initform (lambda (fpath) (declare (ignorable fpath)) t)
    :documentation "Function to determine whether to include a file or not.
It is called with the path of every file in `content-dir'")
   (elisp-script
    :initarg :elisp-script
    :documentation "Elisp script to execute in Emacs."))
  (:default-initargs
   :elisp-script (asdf:system-relative-pathname "cl-ownpress" "./src/providers/elisp/org-file.el")))

(defmethod categorize ((prov org-file-provider) &key msg meta filepath)
  "Return parent directory of the `filepath' as its category."
  (declare (ignore msg))
  (or (gethash "category" meta)
      (let ((cat (str:replace-all "/" "" (first (ppath:split filepath)))))
        (if (string= cat "") nil cat))))

(defmethod slugify ((prov org-file-provider) &key msg meta filepath)
  (declare (ignore msg))
  (or
   (gethash "slug" meta)
   (str:replace-all "_" "-" (first (str:split "." (ppath:basename filepath))))))

(defmethod identify ((prov org-file-provider) &key msg meta filepath)
  "Return id from META, or `slugified' MSG as its id."
  (or
   (gethash "id" meta)
   (gethash "identifier" meta)
   (slugify prov :msg msg :meta meta :filepath filepath)))

(defmethod invoke-provider ((provider org-file-provider) &key)
  (with-slots (content-dir keep-file-if identify categorize slugify elisp-script) provider
    (when (not (uiop:truename* content-dir))
      (error "content-dir does not exist: ~s" content-dir))
    (let* ((content-dir (namestring (uiop:truename* content-dir)))
           (content-files (remove-if-not
                           (op (funcall keep-file-if (file-namestring _)))

                           (clown:recursive-directory-files content-dir))))

      (with-rpc-server (rpc-server)
        (rpc-action "new-org-file"
                    (lambda (msg)
                      (let ((meta (yason:parse (gethash "metadata" msg)))
                            (filepath (str:replace-first content-dir "" (gethash "filepath" msg))))
                        (setf (gethash "category" meta)
                              (categorize provider :msg msg :meta meta :filepath filepath))
                        (setf (gethash "slug" meta)
                              (slugify provider :msg msg :meta meta :filepath filepath))

                        (let ((id (identify provider :msg msg :meta meta :filepath filepath)))
                          (insert-into :provided_content
                            (:id id
                             :provider (provider-name provider)
                             :metadata (with-output-to-string (str) (yason:encode meta str))
                             :body (gethash "body_raw" msg)
                             :type "org-mode")
                            :conflict-cols (:id))
                          (insert-into :processed_content
                            (:prov_cont_id id
                             :type "html"
                             :processor "emacs"
                             :body (gethash "body_html" msg))
                            :conflict-cols (:prov_cont_id :type :processor))))
                      t))
        (rpc-final-action "close-connection"
                          (lambda (msg)
                            (declare (ignore msg))
                            (jsonrpc:clear-methods (rpc-server))))

        (log:d "RUNNING SCRIPT: ~a" elisp-script)
        (uiop:run-program (format nil "emacsclient -e '(load \"~a\")' ~
                                       '(cl-ownpress--main ~
                                          (list ~{\"~a\" ~}))'"
                                  elisp-script
                                  content-files)
                          :output *standard-output*
                          :error-output *standard-output*)))))
