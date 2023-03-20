(defpackage cl-ownpress
  (:nicknames :clown)
  (:use :cl :serapeum/bundle :40ants-doc)
  (:export
   *conf* conf conf-merge
   make-connection run-pending-migrations create-new-migration
   join-paths system-local recursive-directory-files))
(in-package :cl-ownpress)

(require :cl-migratum)
(require :cl-migratum.provider.local-path)
(require :cl-migratum.driver.dbi)
(require :cl-dbi)

(defsection @index (:title "Cl-Ownpress")
  "Cl-ownpress (hereby also referred as clown) is a personal publishing platform
for the lisp hacker. It should help you gather content and publish it in a
hacker friendly manner."

  "## Architecture

Publishing with cl-ownpress happens in 2 phases, represented by its two primary modules.

1. [Providers](/providers) <br />
   Providers provide data to be published. Their job is to gather the data from
wherever, and add it to an sqlite database.

2. [Publishers](/publishers) <br />
   Publishers publish the content collected in the clown db.
"

  "# API"

  (:clown package)
  "- Nicknames : `:clown`"

  "## Configuration"
  (*conf* variable)
  (conf function)
  (conf-merge function)

  "### Database"
  (make-connection function)
  (run-pending-migrations function)

  "### Helper utilities"
  (create-new-migration function)
  (join-paths function)
  (system-local function)
  (recursive-directory-files function))

(log:config :debug)

(setf cffi::*foreign-library-directories*
      (cffi::explode-path-environment-variable "CLOWN_LIBRARY_PATH"))

(setf lparallel:*kernel* (lparallel:make-kernel 4))

(defparameter *conf* '(:db-name "clownpress.db")
  "Global configuration")

(defun conf (key)
  "Get configuration value corresponding to KEY."
  (getf *conf* key))

(defun (setf conf) (val key)
  (setf (getf *conf* key) val))

(defun conf-merge (val)
  "Merge NEW-CONF argument into default `*conf*' and return the result.

**Example**

```lisp
(let ((*conf* (conf-merge `((:site-url \"https://mysite.com/\")))))
  (build))
```"
  (concatenate 'list val *conf*))
