(in-package #:in.bitspook.cl-ownpress)

(export-always 'html-page-artifact)
(defclass html-page-artifact (artifact)
  ((location :initarg :location :accessor artifact-location)
   (builder :initarg :builder)
   (root-widget :initarg :root-widget)))

(export-always 'make-html-page-artifact)
(defun make-html-page-artifact (&key location builder root-widget
                                  (css-location "/css/styles.css"))
  (let ((css-art (make 'css-file-artifact
                       :location css-location
                       :root-widget root-widget)))
    (make 'html-page-artifact
          :location location
          :root-widget root-widget
          :builder builder
          :deps (list css-art))))

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

(export-always '*already-published-artifacts*)
;; Ugly way of handing circular dependencies
(defparameter *already-published-artifacts* nil
  "List of artifacts which have already been published, and should not be published again. This is used
to resolve circular dependencies when two artifacts depend on each other.")

(defmethod publish-artifact ((art html-page-artifact) dest-dir)
  (setf *already-published-artifacts* (concatenate 'list *already-published-artifacts* (list art)))
  (let ((content (artifact-content art)))
    (dolist (dep (artifact-deps art))
      (publish-artifact dep dest-dir))

    (let* ((root-w-deps (all-deps (slot-value art 'root-widget)))
           ;; There is no publishing widgets; they get published as html content
           (non-widget-deps (remove-if #'widgetp root-w-deps))
           (non-published-deps (remove-if (op (find _ *already-published-artifacts*)) non-widget-deps)))
      (mapcar (op (publish-artifact _ dest-dir)) non-published-deps))

    (publish-static
     :dest-dir dest-dir
     :content content
     :path (artifact-location art))))

;; CSS
(defclass css-file-artifact (artifact)
  ((location :initarg :location
             :initform (error "css-file-artifact's :location is required"))
   (root-widget :initarg :root-widget)))

(defmethod artifact-content ((art css-file-artifact))
  (with-slots (root-widget) art
    (rendered-css root-widget)))

(defmethod artifact-location ((art css-file-artifact))
  (let ((hash (md5:md5sum-string (artifact-content art))))
    (append-content-hash (namestring (slot-value art 'location)) hash)))

(defmethod publish-artifact ((art css-file-artifact) dest-dir)
  (let ((content (artifact-content art)))
    ;; since css files are content-hashed, we can simply ignore conflicts
    (handler-bind ((file-already-exists #'skip-existing))
      (publish-static
       :dest-dir dest-dir
       :content content
       :path (artifact-location art)))))

(export-always '*base-url*)
(defparameter *base-url* "/")

(export-always 'link)
(defmethod embed-artifact-as ((art artifact) (as (eql 'link)) &key)
  (when *self* (add-dep *self* art))
  (let* ((loc (artifact-location art))
         (path-frags (str:split "/" (namestring loc)))
         (clean-loc (when (equal (car (last path-frags)) "index.html")
                      (str:join "/"
                                (firstn
                                 (- (length path-frags) 1)
                                 path-frags)))))
    (base-path-join *base-url* (or clean-loc loc))))

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
                                        (base-path-join *base-url* (artifact-location art)) format)))
                 :font-weight ,weight
                 :font-stype ,style)))

(defmethod publish-artifact ((art font-artifact) dest-dir)
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
