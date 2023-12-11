;; clown-common -- Common elisp utilities for cl-ownpress Emacs interaction
;;
;;; Commentary:
;;; These are utility functions and variables to be used in Elisp written
;;; with/for cl-ownpress.
;; 
;;; Code:

(require 'seq)
(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'jsonrpc)
(require 'htmlize)
(require 's)

(defvar _clown-rpc-server nil)

(defun clown-rpc-server ()
  "Get `jsonrpc' server instance to communicate with cl-ownpress."
  (when (not (and _clown-rpc-server (jsonrpc-running-p _clown-rpc-server)))
    (setf _clown-rpc-server
          (make-instance
           'jsonrpc-process-connection
           :name "clown-rpc"
           :on-shutdown (lambda (_conn) "Clown RPC Disconnected!")
           :process (open-network-stream "clown-rpc"
                                         "*clown-rpc*"
                                         "127.0.0.1"
                                         1337)
           :request-dispatcher (lambda (_conn method args)
                                 (message "RPC REQUEST: %s" method)
                                 "pong"))))

  _clown-rpc-server)

(defun clown-get-org-file-props (filename)
  "Get file-level org props for FILENAME."
  (with-temp-buffer
    (insert-file filename)
    (org-mode)
    (let ((props (org-element-map (org-element-parse-buffer 'greater-element)
                     '(keyword)
                   (lambda (kwd)
                     (let ((data (cadr kwd)))
                       (cons (downcase (plist-get data :key))
                             (plist-get data :value)))))))
      props)))



(provide 'clown-common)
;;; clown-common.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("clown" . "cl-ownpress-"))
;; End:
