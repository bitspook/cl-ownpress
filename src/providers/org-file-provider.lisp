(in-package :clown-providers)

(defclass org-file-provider (provider)
  ((content-dir :initarg :content-dir
                :initform (error "content-dir is required")
                :documentation "Directory which will be traversed recursively to pick all .org files")))

(defmethod invoke-provider ((provider org-file-provider) &key)
  (with-slots (content-dir) provider
    (let* ((content-dir (namestring (uiop:truename* content-dir)))
           (content-files (clown:recursive-directory-files content-dir))
           (script-path (asdf:system-relative-pathname "cl-ownpress" "./src/providers/elisp/org-file.el")))
      (labels ((process-rpc-message (msg)
                 (let ((meta (yason:parse (gethash "metadata" msg))))
                   (setf (gethash "category" meta)
                         (let ((cat (str:replace-all
                                     "/" "" (first (ppath:split
                                                    (str:replace-first
                                                     content-dir "" (gethash "filepath" msg)))))))
                           (if (string= cat "") nil cat)))
                   (insert-into :provided_content
                     (:id (gethash "id" msg)
                      :provider (provider-name provider)
                      :metadata (with-output-to-string (str) (yason:encode meta str))
                      :body (gethash "body_raw" msg)
                      :type "org-mode")
                     :conflict-cols (:id)))
                 (insert-into :processed_content
                   (:prov_cont_id (gethash "id" msg)
                    :type "html"
                    :processor "emacs"
                    :body (gethash "body_html" msg))
                   :conflict-cols (:prov_cont_id :type :processor))))
        (with-rpc-server
            (:processor #'process-rpc-message)
          (uiop:run-program (format nil "emacsclient -e '(load \"~a\")' ~
                                       '(clown--main ~
                                          :files (list ~{\"~a\" ~}))'"
                                    script-path content-files)
                            :output *standard-output*
                            :error-output *standard-output*))))))
