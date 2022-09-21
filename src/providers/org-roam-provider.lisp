(in-package :clown-providers)

(defparameter *org-roam-provider*
  (make-instance
   'provider
   :name "org-roam"))

(defun contains-list (list1 list2 &key (test #'eq))
  "Return `t' if LIST1 contains all elements of LIST2"
  (eq (length (intersection list1 list2 :test test)) (length list2)))

(defmethod invoke-provider ((provider (eql *org-roam-provider*))
                            &key
                              (tags '("blog-post"))
                              (listed-category "blog")
                              (unlisted-category "notes"))
  (labels ((process-rpc-message (msg)
             (insert-into :provided_content
               (:id (gethash "id" msg)
                :provider (provider-name *org-roam-provider*)
                :metadata (gethash "metadata" msg)
                :body (gethash "body_raw" msg)
                :type "org-mode")
               :conflict-cols (:id))
             (insert-into :processed_content
               (:prov_cont_id (gethash "id" msg)
                :type "html"
                :processor "emacs"
                :body (gethash "body_html" msg))
               :conflict-cols (:prov_cont_id :type :processor))))
    (let ((script-path (asdf:system-relative-pathname "cl-ownpress" "./src/providers/elisp/org-roam.el")))
      (with-rpc-server (:processor #'process-rpc-message)
        (uiop:run-program (format nil
                                  "emacsclient -e '(load \"~a\")' ~
                                   '(clown--main :tags (list ~{\"~a\" ~}) ~
                                     :listed-category \"~a\" :unlisted-category \"~a\")'"
                                  script-path tags listed-category unlisted-category)
                          :output *standard-output*
                          :error-output *standard-output*)))))
