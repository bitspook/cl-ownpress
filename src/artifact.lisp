(in-package #:in.bitspook.cl-ownpress)

(export-always 'artifact)
(export-always 'artifact-deps)
(export-always 'artifact-id)
(defclass artifact ()
  ((id :initarg :id :accessor artifact-id)
   (deps :initarg :deps :initform nil :accessor artifact-deps
         :documentation "List of ARTIFACTs current ARTIFACT depends on."))
  (:documentation "An artifact is something that will eventually be published and can depend on other artifacts.
They provide a convenient way to represent entire 'is also published' objects, which can be linked
to. This prevents the problem of 'make sure X thing is published at this exact address which I am
linking to in this file'; but passing the artifact to link to and just saying 'link to this
artifact, wherever it is published.'"))

(defmethod print-object ((art artifact) out)
  (print-unreadable-object (art out :type t)
    (format out "~a" (artifact-location art))))

(export-always 'add-dep)
(defgeneric add-dep (art dep)
  (:documentation "Add a new dependency if it isn't already added.")
  (:method ((art artifact) (dep artifact))
    (setf (artifact-deps art)
          (adjoin dep (artifact-deps art)))))

(export-always 'all-deps)
(defun all-deps (art)
  (declare (artifact art))
  (let ((deps (artifact-deps art)))
    (nub (flatten (concatenate 'list deps (mapcar #'all-deps deps))))))

(export-always 'artifact-location)
(defgeneric artifact-location (art)
  (:documentation "Uniquely locate ART within given context. Context here is a vague notion determined by intention of
obtaining the location. e.g when materializing the artifact as HTML file."))

(export-always 'artifact-content)
(defgeneric artifact-content (art)
  (:documentation "Artifact-content is what is actually published at end of publishing lifecycle.")
  (:method ((art artifact)) nil))

(export-always 'embed-artifact-as)
(defgeneric embed-artifact-as (art as &key)
  (:documentation "Embed an ART artifact in some context as AS e.g embed css-file-artifact in an html file as
'stylesheet by prepending *base-url* to css location."))

(export-always 'publish-artifact)
(defgeneric publish-artifact (art dest-dir)
  (:documentation "Publish ART artifact to DEST-DIR. It must also publish its dependencies."))

;; TODO Figure out how to implement this so that this gets called for methods of any child class as
;; well. This code need to be repeated for every instance of this method. This don't work because
;; child class's functions get called before this.
;; (defmethod publish-artifact :around ((art artifact) dest)
;;   (let* ((loc (namestring (artifact-location art)))
;;          (publish-p (not (find loc *already-published-artifacts* :test #'equal))))
;;     (push *already-published-artifacts* loc)
;;     (when publish-p (call-next-method art dest))))

(export-always '*already-published-artifacts*)
;; Ugly way of handing circular dependencies
(defparameter *already-published-artifacts* nil
  "List of artifacts which have already been published, and should not be published again. This is used
to resolve circular dependencies when two artifacts depend on each other. It is publisher's
responsibility to set it to NIL when publishing a new artifact.")

(defclass artifact-registry ()
  ((store :initform (dict)
          :accessor registry-store)
   (indices :initform (make-hash-table)
            :accessor registry-indices
            :documentation "INDICES are lists of ARTIFACT-IDs")))

(defmethod registry-add-index ((registry artifact-registry) (name symbol) &optional (default (dict)))
  (setf (@ (registry-indices registry) name) default))

(defgeneric registry-query (registry key &key)
  (:method ((registry artifact-registry) (key string) &key)
    (@ (registry-store registry) key)))

(defmethod registry-add-artifact ((registry artifact-registry) (artifact artifact))
  (setf (@ (registry-store registry) (artifact-id artifact)) artifact)

  (loop :for key :in (hash-table-keys (registry-indices registry))
        :do (registry-on-index-artifact registry artifact key)))

(defgeneric registry-on-index-artifact (registry artifact index-name &key)
  (:documentation "Add ARTIFACT in to INDEX-NAMEd index of REGISTRY.")
  (:method ((registry artifact-registry) (artifact artifact) (index-name symbol) &key keys)
    (loop :for key :in keys
          :for ids := (@ (registry-indices registry) index-name key)
          :do (setf (@ (registry-indices registry) index-name key)
                    (adjoin (artifact-id artifact) ids :test #'equal)))))
