(defpackage #:in.bitspook.cl-ownpress/publisher
  (:use #:cl #:serapeum/bundle #:in.bitspook.cl-ownpress)
  (:import-from #:spinneret :with-html :with-html-string)
  (:import-from #:slug :slugify)
  (:import-from #:md5 :md5sum-string :md5sum-file))
(in-package #:in.bitspook.cl-ownpress/publisher)

(defclass publisher ()
  ((dest :documentation "Directory in which publishable artifacts are created."
         :initform (error "dest is required")
         :initarg :dest
         :accessor publisher-dest))
  (:documentation "A publisher creates publishable artifacts. For example Html+Css+Js+Rss files for a
website."))

(export-always 'publish)
(defgeneric publish (publisher &key)
  (:documentation "Instruct the PUBLISHER to create artifacts needed for publishing."))

(defgeneric public-path (publisher &key)
  (:documentation "Provide the public path of artifacts produced by PUBLISHER."))

;; TODO: Implement following restarts
;;
;; 1. ignore duplicate content: Check if content of artifact to create and already existing artifact
;; is same. If it is, return the path of existing artifact
;;
(export-always 'artifact-already-exists)
(define-condition artifact-already-exists (error)
  ((path :initarg :path :reader artifact-path))
  (:documentation "Condition indicating that an artifact at the same location already exists."))
