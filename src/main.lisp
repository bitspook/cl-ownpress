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

;; Save a blog post into the database.
(defun save-post (post)
  (log:d "Got Post: ~a~%" (gethash "title" post))
  (let ((conn (make-connection)))
    (multiple-value-bind (stmt values)
        (sxql:yield
         (sxql:insert-into :inputs
           (sxql:set= :id (gethash "id" post)
                      :slug (gethash "slug" post)
                      :title (gethash "title" post)
                      :tags (gethash "tags" post)
                      :metadata (gethash "metadata" post)
                      :provider (provider-name *org-roam-provider*)
                      :published_at (gethash "published-at" post)
                      :content_raw (gethash "content_raw" post)
                      :content_html (gethash "content_html" post))))
      (log:d "EXECUTING ~a~%" stmt)
      (dbi:fetch-all (dbi:execute (dbi:prepare conn stmt) values)))))

(defmethod invoke-provider ((provider (eql *org-roam-provider*)) &key)
           (let ((script-path (asdf:system-relative-pathname "cl-ownpress" "./src/providers/org-roam/org-roam.el")))
             (uiop:run-program (format nil "emacs --script ~a" script-path)
                               :output *standard-output*
                               :error-output *standard-output*)))

;; RPC server should be used for communicating with external processes a
;; provider might start. `MSG-PROCESSSOR' is a function which receives a
;; JSON-parsed message. Messages received by RPC server are JSON encoded strings
;; separated by "<<<<RPC-EOM>>>>". Server stops when a "DONE" message is
;; received.
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

(defun main ()
  (let ((rpc-channel (lparallel:make-channel)))
    (run-pending-migrations)
    (lparallel:submit-task rpc-channel #'start-rpc-server #'save-post)

    (invoke-provider *org-roam-provider*)

    (lparallel:receive-result rpc-channel)))
