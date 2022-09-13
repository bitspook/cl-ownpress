(in-package :clown-providers)

(defparameter *org-roam-provider*
  (make-instance
   'provider
   :name "org-roam-provider"))

(defmethod invoke-provider ((provider (eql *org-roam-provider*)) &key)
  (let ((script-path (asdf:system-relative-pathname "cl-ownpress" "./src/providers/org-roam/org-roam.el")))
    (with-rpc-server (:processor #'save-post)
      (uiop:run-program (format nil "emacsclient -e '(load \"~a\")' '(clown--main)'" script-path)
                        :output *standard-output*
                        :error-output *standard-output*))))
