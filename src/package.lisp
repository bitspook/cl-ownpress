(defpackage cl-ownpress
  (:nicknames :clown)
  (:use :cl)
  (:export
   make-connection
   run-pending-migrations
   post id title slug tags html-content
   fetch-recent-posts
   published-post output-path
   db-to-post))
(in-package :cl-ownpress)

(require :cl-migratum)
(require :cl-migratum.provider.local-path)
(require :cl-migratum.driver.dbi)
(require :cl-dbi)

(log:config :debug)

(setf cffi::*foreign-library-directories*
      (cffi::explode-path-environment-variable "CLOWN_LIBRARY_PATH"))

(setf lparallel:*kernel* (lparallel:make-kernel 4))

(defparameter *conn* nil
  "The database connection. Should not be used directly, use `make-connection'
instead.")

(defparameter *migration-driver* nil
  "`migratum.driver' for running migrations")

;; `prep-migrations' should be called to prepare migrations before any code
;; related to migrations can be executed.
(defun prep-migrations ()
  (when *migration-driver*
    (return-from prep-migrations *migration-driver*))

  (let* ((migrations-dirs (list (asdf:system-relative-pathname :cl-ownpress "sql/migrations/")))
         (provider (migratum.provider.local-path:make-provider migrations-dirs)))
    (migratum:provider-init provider)

    (setq *migration-driver* (migratum.driver.dbi:make-driver provider (make-connection)))
    (migratum:driver-init *migration-driver*)
    *migration-driver*))

(defun make-connection (&optional (name "clownpress.db"))
  (when *conn* (return-from make-connection *conn*))

  (setq *conn* (dbi:connect :sqlite3 :database-name (asdf:system-relative-pathname :cl-ownpress name)))
  *conn*)

(defun run-pending-migrations ()
  (migratum:apply-pending (prep-migrations)))

(defun create-new-migration (desc &optional up-str down-str)
  "Create new up+down migrations with DESC."
  (let ((mid (migratum:make-migration-id)))
    (migratum:provider-create-migration
     :up :sql *provider* mid desc
     (or up-str ""))
    (migratum:provider-create-migration
     :down :sql *provider* mid desc
     (or down-str ""))))
