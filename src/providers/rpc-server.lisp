(in-package :clown-providers)

(defun start-rpc-server (msg-processor)
  "RPC server should be used for communicating with external processes a
   provider might start. `MSG-PROCESSSOR' is a function which receives a
   JSON-parsed message. Messages received by RPC server are JSON encoded strings
   separated by `<<<<RPC-EOM>>>>'. Server stops when a `DONE' message is
   received."
  (let* ((socket (usocket:socket-listen #(127 0 0 1) 8192 :reuse-address t))
         (conn (usocket:socket-accept socket :element-type 'character))
         (msg-sep (concatenate 'list "<<<<RPC-EOM>>>>")))
    (unwind-protect
         (loop
           :with stream := (usocket:socket-stream conn)
           :for
           msg := (str:trim
                   (str:replace-first
                    msg-sep ""
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
        (usocket:socket-close socket)))
    socket))

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

(defvar _rpc-server_ nil)

(defun rpc-server ()
  (when (not _rpc-server_)
    (setf _rpc-server_ (jsonrpc:make-server))
    (jsonrpc:server-listen _rpc-server_ :port 1337 :mode :tcp))

  _rpc-server_)


(defmacro with-rpc-server (server &body body)
  (labels ((rpc-action-p (cell) (eq (car cell) 'rpc-action))
           (rpc-final-action-p (_1) (eq (car _1) 'rpc-final-action))
           (rpc-action-name (action) (second action))
           (rpc-action-cb (action) (third action)))
    (let ((server server)
          (rpc-actions (remove-if-not #'rpc-action-p body))
          (rpc-final-action (remove-if-not #'rpc-final-action-p body))
          (body (remove-if (op (or (rpc-action-p _1) (rpc-final-action-p _1))) body)))
      (setf rpc-final-action (first rpc-final-action))

      ;; TODO Ensure that duplicates listeners aren't added
      `(progn
         ,@(loop :for action :in rpc-actions
                 :collect `(jsonrpc:expose ,server ,(rpc-action-name action) ,(rpc-action-cb action)))

         ;; When client wants to end it, handle the final request and clear all
         ;; the request-handlers
         (jsonrpc:expose
          ,server ,(rpc-action-name rpc-final-action)
          (compose (lambda (_)
                     (declare (ignore _))
                     ;; FIXME instead of clearing all callbacks, only remove the
                     ;; ones added by this macro
                     (jsonrpc:clear-methods ,server))
                   ,(rpc-action-cb rpc-final-action)))

         ,@body))))

;; (with-rpc-server (rpc-server)
;;   (rpc-action 'my-request-1 (lambda (msg) t))
;;   (rpc-action 'my-request-2 (lambda (msg) t))
;;   (rpc-final-action 'my-done-request (lambda (msg) 'done))

;;   (uiop:run-program "emacsclient -e '(message \"Lol bro\")'"))
