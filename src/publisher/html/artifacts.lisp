(in-package #:in.bitspook.cl-ownpress/publisher)

(export-always 'html-page-artifact)
(defclass html-page-artifact (artifact)
  ((location :initarg :location :accessor artifact-location)
   (builder :initarg :builder)
   (root-widget :initarg :root-widget)))

(defmethod artifact-content ((art html-page-artifact))
  (let* ((*render-stack* nil)
         (css (car (artifact-deps art)))
         (body-html (with-html-string (dom-of (slot-value art 'root-widget))))
         (page-html
           (with-html-string (funcall (slot-value art 'builder)
                                      :css css :body-html body-html))))
    page-html))

(defclass css-file-artifact (artifact)
  ((dest-dir :initarg :dest-dir
             :initform (error "css-file-artifact's :dest-dir is required"))))

(defmethod artifact-content ((art css-file-artifact))
  (rendered-css))

(defmethod artifact-location ((art css-file-artifact))
  (let* ((hash (str:downcase
                (str:substring
                 0 6
                 (format nil "~{~X~}" (map 'list #'identity (md5:md5sum-string (artifact-content art))))))))
    (base-path-join
     (slot-value art 'dest-dir)
     (str:concat "style" "-" hash ".css"))))
