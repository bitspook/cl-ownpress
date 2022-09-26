(defpackage clown-providers
  (:use :cl)
  (:export
   provider invoke-provider
   org-roam-provider *org-file-provider*))
(in-package :clown-providers)

(defmacro insert-into (table values &key conflict-cols)
  "Utility to cut some boilerplate to insert a set of values into a table.
Insert into TABLE VALUES represented as a plist."
  `(let ((conn (clown:make-connection)))
     (multiple-value-bind (stmt vals)
         (sxql:yield
          (sxql:insert-into ,table
            (sxql:set= ,@values)
            (sxql:on-conflict-do-update
             '(,@conflict-cols) (sxql:set=
                  ,@values
                  :updated_at (local-time:format-timestring
                               nil (local-time:now))))))
       (dbi:fetch-all (dbi:execute (dbi:prepare conn stmt) vals)))))

(defclass provider ()
  ((name
    :type string
    :initarg :name
    :initform (error "Provider must have a name")
    :reader provider-name)))

(defgeneric invoke-provider (provider &key)
  (:documentation "Execute a provider."))

(defmethod invoke-provider :before (provider &key)
  (clown:run-pending-migrations))
