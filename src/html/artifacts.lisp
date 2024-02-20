(in-package #:in.bitspook.cl-ownpress)

(export-always 'html-page-artifact)
(defclass html-page-artifact (artifact)
  ((location :initarg :location :accessor artifact-location)
   (builder :initarg :builder)
   (root-widget :initarg :root-widget)))

(defmethod artifact-content ((art html-page-artifact))
  (let* ((root-w (slot-value art 'root-widget))
         (body-html (with-html-string (render root-w)))
         (css-art
           (find-if
            (op (eq (class-name-of _) 'css-file-artifact))
            (artifact-deps art)))
         (page-html
           (with-html-string
             (funcall
              (slot-value art 'builder)
              :css css-art :body-html body-html))))
    page-html))

;; CSS
(defclass css-file-artifact (artifact)
  ((location :initarg :location
             :initform (error "css-file-artifact's :location is required"))
   (root-widget :initarg :root-widget)))

(defmethod artifact-content ((art css-file-artifact))
  (with-slots (root-widget) art
    (rendered-css root-widget)))

(defmethod artifact-location ((art css-file-artifact))
  (let* ((hash (md5:md5sum-string (artifact-content art))))
    (append-content-hash (namestring (slot-value art 'location)) hash)))
