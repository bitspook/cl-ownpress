(in-package :clown-providers)

(defparameter *org-file-provider* (make-instance 'provider :name "org-file"))

(defmethod invoke-provider ((provider (eql *org-file-provider*)) &key content-dir)
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
                    :provider (provider-name *org-file-provider*)
                    :metadata (with-output-to-string (str) (yason:encode meta str))
                    :body (gethash "body_raw" msg)
                    :type "org-mode")))
               (insert-into :processed_content
                 (:prov_cont_id (gethash "id" msg)
                  :type "html"
                  :processor "emacs"
                  :body (gethash "body_html" msg)))))
      (with-rpc-server
          (:processor #'process-rpc-message)
        (uiop:run-program (format nil "emacsclient -e '(load \"~a\")' ~
                                       '(clown--main ~
                                          :files (list ~{\"~a\" ~}))'"
                                  script-path content-files)
                          :output *standard-output*
                          :error-output *standard-output*)))))
