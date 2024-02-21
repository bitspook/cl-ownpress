(in-package #:in.bitspook.cl-ownpress)

(export-always 'artifact)
(export-always 'artifact-deps)
(defclass artifact ()
  ((deps :initarg :deps :initform nil :accessor artifact-deps
         :documentation "List of ARTIFACTs current ARTIFACT depends on."))
  (:documentation "An artifact is something that will eventually be published and can depend on other artifacts.
They provide a convenient way to represent entire 'is also published' objects, which can be linked
to. This prevents the problem of 'make sure X thing is published at this exact address which I am
linking to in this file'; but passing the artifact to link to and just saying 'link to this
artifact, wherever it is published.'"))

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
  (:documentation "Artifact-content is what is actually published at end of publishing lifecycle."))

;; Publishing
(export-always 'publish-artifact)
(defun publish-artifact (artifact &key dest-dir)
  "Publish ART and all its dependencies to DEST-DIR."
  (dolist (art (append (list artifact) (all-deps artifact)))
    (publish-static
     :dest-dir dest-dir
     :content (artifact-content art)
     :path (artifact-location art))))
