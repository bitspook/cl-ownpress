(in-package #:in.bitspook.cl-ownpress)

(export-always 'emacs-provider)
(export-always 'script)
(defclass emacs-provider ()
  ((script :initarg :script
           :initform (error ":script is required")
           :documentation "SCRIPT must be path to an emacs-lisp script. This script must contain a function named `main'. All
the arguments given to this provider are passed to `main' function of this script."))
  (:documentation "Provides jsonrpc events produced by running an Emacs-lisp script in Emacs. It uses emacsclient to
run the script, so Emacs must be running in server mode (and hence all Emacs configuration is
effective)."))

(defmethod provide-all ((provider emacs-provider) &rest script-args)
  "Starts a jsonrpc server, runs PROVIDER's :script, collects and returns all messages of type :event
emitted from Emacs script, until message of type :done is emitted."
  (let ((events nil)
        (server (rpc-server)))

    (with-slots (script) provider
      (with-rpc-server (server msg)
        (:case event (push msg events))
        (:finally done (declare (ignore msg)) t)
        (uiop:run-program (format nil "emacsclient -e '(load \"~a\")' ~
                                       '(main ~{\"~a\" ~})'"
                                  script script-args)
                          :output *standard-output*
                          :error-output *standard-output*)))
    events))
