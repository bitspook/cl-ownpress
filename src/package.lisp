(defpackage cl-ownpress
  (:nicknames :clown)
  (:use :cl :serapeum/bundle)
  (:export
   *conf* conf conf-merge with-conf
   make-connection run-pending-migrations create-new-migration
   join-paths system-local recursive-directory-files))
(in-package :cl-ownpress)

(require :cl-migratum)
(require :cl-migratum.provider.local-path)
(require :cl-migratum.driver.dbi)
(require :cl-dbi)

(log:config :debug)

(setf cffi::*foreign-library-directories*
      (cffi::explode-path-environment-variable "CLOWN_LIBRARY_PATH"))

(setf lparallel:*kernel* (lparallel:make-kernel 4))

(defparameter *conf* '(:db-name "clownpress.db"))

(defun conf (key)
  "Get configuration value corresponding to KEY."
  (getf *conf* key))

(defun (setf conf) (val key)
  (setf (getf *conf* key) val))

(defun conf-merge (val)
  "Merge NEW-CONF into default `*conf*' and return the result.

## Example

```lisp
(let ((*conf* (conf-merge `((:site-url \"https://mysite.com/\")))))
  (build))
```"
  (concatenate 'list val *conf*))
