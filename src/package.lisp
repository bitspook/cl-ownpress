(defpackage in.bitspook.cl-ownpress
  (:use :cl :serapeum/bundle)
  (:export
   *conf* conf conf-merge
   make-connection run-pending-migrations create-new-migration
   join-paths system-local recursive-directory-files))
(in-package #:in.bitspook.cl-ownpress)

(log:config :debug)

(setf lparallel:*kernel* (lparallel:make-kernel 4))
