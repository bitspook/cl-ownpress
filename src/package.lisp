(defpackage cl-ownpress
  (:nicknames :clown)
  (:use :cl)
  (:export
   *conf* conf conf-merge
   make-connection
   run-pending-migrations
   post post-id slug tags post-html-content post-tags post-category post-published-at post-title
   published-post output-path post-output-path
   fetch-recent-posts db-to-post
   join-paths make-conf
   invoke-provider *org-roam-provider* *fs-provider*))
(in-package :cl-ownpress)

(require :cl-migratum)
(require :cl-migratum.provider.local-path)
(require :cl-migratum.driver.dbi)
(require :cl-dbi)

(log:config :debug)

(setf cffi::*foreign-library-directories*
      (cffi::explode-path-environment-variable "CLOWN_LIBRARY_PATH"))

(setf lparallel:*kernel* (lparallel:make-kernel 4))

(make-conf '(:db-name "clownpress.db"))

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

(defun make-connection ()
  (when *conn* (return-from make-connection *conn*))

  (setq *conn* (dbi:connect :sqlite3 :database-name (conf :db-name)))
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
