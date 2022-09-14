(in-package :clown-providers)

(defparameter *org-roam-provider*
  (make-instance
   'provider
   :name "org-roam"))

(defmethod invoke-provider ((provider (eql *org-roam-provider*)) &key (tags '("blog-post")))
  (labels ((process-rpc-message (msg)
             (insert-into :provided_content
               (:id (gethash "id" msg)
                :provider (provider-name *org-roam-provider*)
                :metadata (gethash "metadata" msg)
                :body (gethash "body_raw" msg)
                :type "org-mode"))
             (insert-into :processed_content
               (:prov_cont_id (gethash "id" msg)
                :type "html"
                :processor "emacs"
                :body (gethash "body_html" msg)))))
    (let ((script-path (asdf:system-relative-pathname "cl-ownpress" "./src/providers/elisp/org-roam.el")))
      (with-rpc-server (:processor #'process-rpc-message)
        (uiop:run-program (format nil
                                  "emacsclient -e '(load \"~a\")' '(clown--main :tags (list ~{\"~a\" ~}))'"
                                  script-path tags)
                          :output *standard-output*
                          :error-output *standard-output*)))))
