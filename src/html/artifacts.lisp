(in-package #:in.bitspook.cl-ownpress)

(export-always '*base-url*)
(defparameter *base-url* "/")

(export-always 'link)
(defmethod embed-artifact-as ((art artifact) (as (eql 'link)) &key)
  (when *self* (add-dep *self* art))
  (let* ((loc (namestring (artifact-location art)))
         (clean-loc (if (equal (path-basename loc) "index.html")
                        (str:replace-all "/index.html" "" loc)
                        loc)))
    (str:concat
     *base-url*
     (unless (str:ends-with-p "/" *base-url*) "/")
     (if (str:starts-with-p "/" clean-loc) (str:substring 1 nil clean-loc) clean-loc))))

(export-always 'html-page-artifact)
(defclass html-page-artifact (artifact)
  ((location :initarg :location :accessor artifact-location)
   (root-component :initarg :root-component)))

(export-always 'make-html-page-artifact)
(defun make-html-page-artifact (&key location root-component css-location (id nil))
  (let ((css-art (make 'css-file-artifact
                       :location css-location
                       :root-component root-component)))
    (when (slot-exists-p root-component 'css-file-artifact)
      (setf (slot-value root-component 'css-file-artifact) css-art))
    (make 'html-page-artifact
          :id id
          :location location
          :root-component root-component
          :deps (list css-art))))

(defmethod artifact-content ((art html-page-artifact))
  (with-html-string (render (slot-value art 'root-component))))

(defmethod publish-artifact ((art html-page-artifact) dest-dir)
  (when (find (artifact-location art) *already-published-artifacts*)
    (return-from publish-artifact))

  (appendf *already-published-artifacts* (list (artifact-location art)))

  (let ((content (artifact-content art)))
    (dolist (dep (artifact-deps art))
      (publish-artifact dep dest-dir))

    (let* ((root-w-deps (all-deps (slot-value art 'root-component)))
           ;; There is no publishing components; they get published as html content
           (non-component-deps (remove-if #'componentp root-w-deps)))
      (mapcar (op (publish-artifact _ dest-dir)) non-component-deps))

    (publish-static
     :dest-dir dest-dir
     :content content
     :path (artifact-location art))))

;; CSS
(defclass css-file-artifact (artifact)
  ((location :initarg :location
             :initform (error "css-file-artifact's :location is required")
             :accessor artifact-location)
   (root-component :initarg :root-component)))

(defmethod artifact-content ((art css-file-artifact))
  (with-slots (root-component) art
    (with-html-string (render root-component))
    (rendered-css root-component)))

(defmethod publish-artifact ((art css-file-artifact) dest-dir)
  (when (find (artifact-location art) *already-published-artifacts*)
    (return-from publish-artifact))
  (appendf *already-published-artifacts* (list (artifact-location art)))

  (let ((content (artifact-content art)))
    ;; since css files are content-hashed, we can simply ignore conflicts
    (handler-bind ((file-already-exists #'skip-existing))
      (publish-static
       :dest-dir dest-dir
       :content content
       :path (artifact-location art)))))

;; Fonts
(export-always 'font-artifact)
(defclass font-artifact (artifact)
  ((files :initarg :locations
          :initform (error "font-artifact's :files are required"))
   (out-dir :initarg :out-dir
            :initform "/fonts/"
            :documentation "Directory (relative to in which font files will be published to")
   (weight :initarg :weight
           :initform 500)
   (style :initarg :style
          :initform 'normal)))

(export-always 'make-font-artifact)
(defun make-font-artifact (&key files (weight 500) (style 'normal) (out-dir "/fonts/"))
  (loop :for file :in files
        :unless (uiop:file-exists-p file)
          :do (error (format nil "Font file does not exist: ~a" file)))

  (make 'font-artifact
        :locations files
        :out-dir out-dir
        :weight weight
        :style style))

(defmethod artifact-location ((art font-artifact))
  (apply #'values
         (with-slots (files out-dir) art
           (loop :for file :in files
                 :for filename := (path-basename file)
                 :collect (base-path-join out-dir filename)))))

(export-always 'font-face)
(defmethod embed-artifact-as ((art font-artifact) (as (eql 'font-face)) &key family)
  (when *self* (add-dep *self* art))
  (with-slots (weight style) art
    `(@font-face :font-family ,family
                 :src ,(str:join
                        ", "
                        (loop :for file :in (multiple-value-list (artifact-location art))
                              :for format := (pathname-type file)
                              :collect (format
                                        nil "url('~a') format('~a')"
                                        (str:concat *base-url* (namestring (artifact-location art))) format)))
                 :font-weight ,weight
                 :font-stype ,style)))

(defmethod publish-artifact ((art font-artifact) dest-dir)
  (when (find (artifact-location art) *already-published-artifacts*)
    (return-from publish-artifact))
  (appendf *already-published-artifacts* (list (artifact-location art)))

  (with-slots (files) art
    (loop :for file :in files
          :for i :from 0 :to (1- (length files))
          :with locs := (multiple-value-list (artifact-location art))
          :for location := (nth i locs)
          :do (progn
                ;; Conflicting fonts are probably same. Maybe. Let's hope.
                (handler-bind ((file-already-exists #'skip-existing))
                  (publish-static
                   :dest-dir dest-dir
                   :content (pathname file)
                   :path location))))))
