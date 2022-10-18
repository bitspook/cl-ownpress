(defpackage clown-providers
  (:use :cl :serapeum/bundle)
  (:export
   provider invoke-provider categorize identify slugify
   org-roam-provider org-file-provider denote-provider org-project-provider))
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
    :reader provider-name)
   (purge-strategy
    :initarg :purge-strategy
    :initform 'purge-all-before-invoke)))

(defgeneric invoke-provider (provider &key)
  (:documentation "Execute a provider."))

(defgeneric slugify (provider &key msg)
  (:documentation "Create slug for a MSG provided by PROVIDER."))
(defgeneric identify (provider &key msg)
  (:documentation "Create id for a MSG provided by PROVIDER."))
(defgeneric categorize (provider &key msg)
  (:documentation "Create category for a MSG provided by PROVIDER."))

(defmethod invoke-provider :before (provider &key)
  (clown:run-pending-migrations)
  (with-slots (purge-strategy name) provider
    (when (eq purge-strategy 'purge-all-before-invoke)
      (multiple-value-bind (stmt vals)
          (sxql:yield
           (sxql:delete-from :provided_content
             (sxql:where (:= :provider name))))
        (log:d "Purge query: ~a ~a" stmt vals)
        (let ((conn (clown:make-connection)))
          (dbi:execute (dbi:prepare conn stmt) vals))))))

(defmethod invoke-provider :after (provider &key)
  (with-slots (purge-strategy) provider
    (when (and (functionp purge-strategy) purge-strategy)
      (funcall purge-strategy provider))))
