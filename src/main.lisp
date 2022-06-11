(defpackage cl-ownpress
  (:nicknames :clown)
  (:use :cl :clown.db))
(in-package :cl-ownpress)

(defclass provider ()
  ((name
    :type string
    :initarg :name
    :initform (error "Provider must have a name")
    :reader provider-name)))

(defgeneric invoke-provider (provider &key)
  (:documentation "Execute a provider."))

(defparameter *org-roam-provider*
    (make-instance
     'provider
     :name "org-roam-provider"))

(defun store-input (input)
  (log:d "Got Input: ~a~%" (gethash "title" input))
  (multiple-value-bind (stmt values)
      (sxql:yield
       (sxql:insert-into :inputs
         (sxql:set= :id (gethash "id" input)
                    :slug (gethash "slug" input)
                    :title (gethash "title" input)
                    :tags (gethash "tags" input)
                    :metadata (gethash "metadata" input)
                    :provider (provider-name *org-roam-provider*)
                    :published_at (gethash "published-at" input)
                    :content (gethash "content" input))))
    (log:d "EXECUTING ~a~%" stmt)
    (dbi:fetch-all (dbi:execute (dbi:prepare *conn* stmt) values))))

(defmethod invoke-provider ((provider (eql *org-roam-provider*)) &key)
  (let* ((script-path (asdf:system-relative-pathname "cl-ownpress" "./src/providers/org-roam/org-roam.el"))
         (rpc-channel (lparallel:make-channel)))

    (lparallel:submit-task rpc-channel #'start-rpc-server #'store-input)

    (uiop:run-program (format nil "emacs --script ~a" script-path)
                      :output *standard-output*
                      :error-output *standard-output*)

    (lparallel:receive-result rpc-channel)))

(defun start-rpc-server (msg-processor)
  (let* ((socket (usocket:socket-listen #(127 0 0 1) 8192 :reuse-address t))
         (conn (usocket:socket-accept socket :element-type 'character))
         (msg-sep (concatenate 'list "<<<<RPC-EOM>>>>")))
    (unwind-protect
         (loop
           :with stream := (usocket:socket-stream conn)
           :for
           msg := (str:trim
                   (str:replace-first msg-sep ""
                    (with-output-to-string (msg-str)
                      (loop :named inner
                            :with trail := '()
                            :for char := (read-char stream nil 'eof)
                            :do (princ char msg-str)
                                (when (>= (length trail) (length msg-sep))
                                  (setq trail (subseq trail 1)))
                                (setq trail (nconc trail (list char)))
                                (when (equal trail msg-sep) (return-from inner))))))
           :until (string= msg "DONE")
           :do (funcall msg-processor (yason:parse msg)))
      (progn
        (usocket:socket-close conn)
        (usocket:socket-close socket)))))
