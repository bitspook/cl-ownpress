(defpackage cl-ownpress
  (:nicknames :clown)
  (:use :cl)
  (:export
   *conf* conf conf-merge
   make-connection run-pending-migrations create-new-migration
   join-paths make-conf recursive-directory-files))
(in-package :cl-ownpress)

(require :cl-migratum)
(require :cl-migratum.provider.local-path)
(require :cl-migratum.driver.dbi)
(require :cl-dbi)

(log:config :debug)

(setf cffi::*foreign-library-directories*
      (cffi::explode-path-environment-variable "CLOWN_LIBRARY_PATH"))

(setf lparallel:*kernel* (lparallel:make-kernel 4))

(defmacro make-conf (initial-value)
  "Create a trivial configuration management functionality.

In the context of current package, it creates:

1. A variable named `*conf*' which should be a plist initialized with INITIAL-VALUE
2. A function `conf' for getting/setting a configuration value saved in a KEY
3. A non-destructive function `conf-merge' for merging a subset of configuration
with present configuration"
  (let ((global-conf (intern "*CONF*" (sb-int:sane-package)))
        (conf (intern "CONF" (sb-int:sane-package)))
        (conf-merge (intern "CONF-MERGE" (sb-int:sane-package)))
        (key (gensym))
        (val (gensym)))
    `(progn
       (defparameter ,global-conf ,initial-value)

       (defun ,conf (,key)
         "Get configuration value corresponding to KEY."
         (getf ,global-conf ,key))

       (defun (setf ,conf) (,val ,key)
         (setf (getf ,global-conf ,key) ,val))

       (defun ,conf-merge (,val)
         "Merge NEW-CONF into default `*conf*' and return the result.

## Example

```lisp
(let ((*conf* (conf-merge `((:site-url \"https://mysite.com/\")))))
  (build))
```"
         (concatenate 'list ,val ,global-conf)))))

(make-conf '(:db-name "clownpress.db"))
