(defpackage cl-ownpress.db
  (:use :cl)
  (:export *conn*
           create-new-migration))
(in-package :cl-ownpress.db)

(require :cl-migratum)
(require :cl-migratum.provider.local-path)
(require :cl-migratum.driver.dbi)
(require :cl-dbi)

(defparameter *conn*
  (dbi:connect :sqlite3 :database-name (asdf:system-relative-pathname :cl-ownpress "clownpress.db")))

(defparameter *provider*
  (migratum.provider.local-path:make-provider (list (asdf:system-relative-pathname :cl-ownpress "sql/migrations/"))))

(migratum:provider-init *provider*)

(defparameter *driver*
  (migratum.driver.dbi:make-driver *provider* *conn*))

(migratum:driver-init *driver*)

(defun create-new-migration (desc &optional up-str down-str)
  "Create new up+down migrations with DESC."
  (let ((mid (migratum:make-migration-id)))
    (migratum:provider-create-migration
     :up :sql *provider* mid desc
     (or up-str ""))
    (migratum:provider-create-migration
     :down :sql *provider* mid desc
     (or down-str ""))))
