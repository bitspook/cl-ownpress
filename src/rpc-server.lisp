(in-package :clown)

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

(defmacro with-rpc-server ((&key processor) &body body)
  "Run BODY with a running rpc-server, and wait until the server stops. Stopping
the server is responsibility of BODY. If BODY don't stop the server, a call to
this macro will hang indefinitely until heap exhaustion."
  ;; FIXME It shouldn't be BODY's responsibility to close the server
  (let ((channel (gensym)))
    `(let ((,channel (lparallel:make-channel)))
       (lparallel:submit-task ,channel #'start-rpc-server ,processor)
       ,@body
       (lparallel:receive-result ,channel))))
