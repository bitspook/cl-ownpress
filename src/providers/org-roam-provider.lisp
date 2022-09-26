(in-package :clown-providers)

(defun contains-list (list1 list2 &key (test #'eq))
  "Return `t' if LIST1 contains all elements of LIST2"
  (eq (length (intersection list1 list2 :test test)) (length list2)))

(defclass org-roam-provider (provider)
  ((tags :initarg :tags
         :initform '("blog-post")
         :documentation "Notes with *all* the tags present in this list are provided.")
   (listed-category :initarg :listed-category
                    :initform "blog"
                    :documentation "Category assigned to directly provided notes.")
   (unlisted-category :initarg :unlisted-category
                      :initform "notes"
                      :documentation "Category assigned to in-directly provided notes. Indirectly
                                      provided notes are those which are linked in a provided tag, but
                                      themselves don't meet the criteria of a published note.")))

(defmethod invoke-provider ((provider org-roam-provider) &key)
  (with-slots (tags listed-category unlisted-category) provider
    (labels ((process-rpc-message (msg)
               (insert-into :provided_content
                 (:id (gethash "id" msg)
                  :provider (provider-name provider)
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
                            :error-output *standard-output*))))))
