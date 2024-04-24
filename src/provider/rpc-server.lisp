(in-package #:in.bitspook.cl-ownpress)

(defparameter *rpc-request-handlers* (dict))

(defun clack-handler (req)
  (let* ((content-length (or (getf req :content-length) 0))
         (raw-body (getf req :raw-body))
         (str-body (with-output-to-string (out)
                     (setf (flexi-streams:flexi-stream-external-format raw-body) '(:utf-8 :EOL-STYLE :LF))
                     (let* ((buffer (make-array content-length :element-type 'character))
                            (position (read-sequence buffer raw-body)))
                       (write-sequence buffer out :end position))))
         (parsed-body nil))
    (handler-case (setf parsed-body (yason:parse str-body))
      (error ()
        (return-from clack-handler
          (list 400 nil
                `("Expected a json of shape: [{ \"type\": string, \"payload\": any }]\n"
                  ,(format nil "Got: ~a" str-body)) ))))
    (let* ((request-name (@ parsed-body "type"))
           (request-body (@ parsed-body "payload"))
           (request-handler (@ *rpc-request-handlers* request-name)))
      (handler-case (list 200 nil (if request-handler (funcall request-handler request-body)
                                      (error (format nil "No handler for: ~a" request-name))))
        (error (c) (list 500 nil (list (format nil "~a" c))))))))

(export-always 'start-rpc-server)
(defun start-rpc-server (port)
  (clack:clackup (lambda (req) (funcall #'clack-handler req)) :port port))

(export-always 'stop-rpc-server)
(defun stop-rpc-server (server)
  (clack:stop server))

(defun add-rpc-request-handler (name handler)
  (setf (gethash name *rpc-request-handlers*) handler))

(defun remove-rpc-request-handler (name)
  (remhash name *rpc-request-handlers*))

(defmacro with-rpc-server ((msg-var) &body body)
  "Macro to work with json-rpc server. RPC server is started on *RPC-PORT*.
SERVER is an instance of jsonrpc:server. MSG-VAR is variable which holds value of received message.
BODY contains 3 kind of forms:
1. Multiple `:case' forms which register an rpc message handler. These look like
   `:case 'my-message-type (format t \"Received: ~a\" msg'
2. Single `:finally' form which is executed when client asks to disconnect
3. Forms evaluated after all `:case' and `:finally' are registered.

Example:
```
(with-rpc-server (msg)
  (:case :my-request-1 (format t \"I got 1 ~a\" msg))
  (:case :my-request-2 (format t \"I got 2 ~a\" msg))
  (:finally :my-done-request (format t \"I got finally ~a\" msg))

  (uiop:run-program \"emacsclient -e '(message \"Lol bro\")'\"))
```"
  (labels ((actionp (cell) (eq (car cell) :case))
           (final-actionp (_1) (eq (car _1) :finally))
           (action-name (action) (let ((name (second action)))
                                   (if (symbolp name) (string-downcase (symbol-name name)) name)))
           (action-cb (action) `(lambda (,msg-var)
                                  (declare (ignorable ,msg-var))
                                  ,@(cddr action))))
    (let ((rpc-actions (remove-if-not #'actionp body))
          (rpc-final-action (first (remove-if-not #'final-actionp body)))
          (body (remove-if (op (or (actionp _1) (final-actionp _1))) body)))

      (when (not rpc-final-action) (error ":finally clause for with-rpc-server must be provided. It is used to determine when to tear-down rpc event handlers."))

      `(progn
         ,@(loop :for action :in rpc-actions
                 :collect `(add-rpc-request-handler ,(action-name action) ,(action-cb action)))

         ;; When client wants to end it, handle the final request and clear all
         ;; the request-handlers
         (add-rpc-request-handler
          ,(action-name rpc-final-action)
          (lambda (msg)
            (funcall ,(action-cb rpc-final-action) msg)
            ,@(loop :for action :in rpc-actions
                    :collect `(remove-rpc-request-handler ,(action-name action)))
            (remove-rpc-request-handler ,(action-name rpc-final-action))))

         ,@body))))
