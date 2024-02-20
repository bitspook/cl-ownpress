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

(export-always 'sheet)
(defmethod embed-as ((art css-file-artifact) (as (eql 'sheet)) &key)
  (with-html (:link :rel "stylesheet" :href (artifact-location art))))

;; Publishing
(export-always 'publish)
(defmethod publish ((art html-page-artifact) &key dest-dir)
  "Publish ART in DEST-DIR."
  (let* ((html (artifact-content art))
         (css-art (find-if
                   (op (eq (class-name-of _) 'css-file-artifact))
                   (artifact-deps art))))

    (publish-static
     :dest-dir dest-dir
     :content html
     :path (artifact-location art))

    (when css-art
      (publish-static
       :dest-dir dest-dir
       :content (artifact-content css-art)
       :path (artifact-location css-art)))))
