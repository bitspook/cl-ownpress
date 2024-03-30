(in-package #:in.bitspook.cl-ownpress)

(defvar _rpc-server_ nil)
(defparameter *rpc-port* 1337
  "Port on which rpc-server will be started")

(defun rpc-server ()
  (when (not _rpc-server_)
    (setf _rpc-server_ (jsonrpc:make-server))
    (jsonrpc:server-listen _rpc-server_ :port *rpc-port* :mode :tcp))

  _rpc-server_)

(defmacro with-rpc-server ((server msg-var) &body body)
  "Macro to work with json-rpc server. RPC server is started on *RPC-PORT*.
SERVER is an instance of jsonrpc:server. MSG-VAR is variable which holds value of received message.
BODY contains 3 kind of forms:
1. Multiple `:case' forms which register an rpc message handler. These look like
   `:case 'my-message-type (format t \"Received: ~a\" msg'
2. Single `:finally' form which is executed when client asks to disconnect
3. Forms evaluated after all `:case' and `:finally' are registered.

Example:
```
(with-rpc-server ((rpc-server) msg)
  (:case :my-request-1 (format t \"I got 1 ~a\" msg))
  (:case :my-request-2 (format t \"I got 2 ~a\" msg))
  (:finally :my-done-request (format t \"I got finally ~a\" msg))

  (uiop:run-program \"emacsclient -e '(message \"Lol bro\")'\"))
```"
  (labels ((actionp (cell) (eq (car cell) :case))
           (final-actionp (_1) (eq (car _1) :finally))
           (action-name (action) (let ((name (second action)))
                                   (if (symbolp name) (string-downcase (symbol-name name)) name)))
           (action-cb (action) `(lambda (,msg-var) ,@(cddr action))))
    (let ((server server)
          (rpc-actions (remove-if-not #'actionp body))
          (rpc-final-action (first (remove-if-not #'final-actionp body)))
          (body (remove-if (op (or (actionp _1) (final-actionp _1))) body)))

      (when (not rpc-final-action) (error "It is mandatory to provide a final json rpc action with :finally"))

      ;; TODO Ensure that duplicates listeners aren't added
      `(progn
         ,@(loop :for action :in rpc-actions
                 :collect `(jsonrpc:expose ,server ,(action-name action) ,(action-cb action)))

         ;; When client wants to end it, handle the final request and clear all
         ;; the request-handlers
         (jsonrpc:expose
          ,server ,(action-name rpc-final-action)
          (compose (lambda (_)
                     (declare (ignore _))
                     ;; FIXME instead of clearing all callbacks, only remove the
                     ;; ones added by this macro
                     (jsonrpc:clear-methods ,server))
                   ,(action-cb rpc-final-action)))

         ,@body))))
