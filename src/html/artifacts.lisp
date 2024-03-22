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
  (let ((hash (md5:md5sum-string (artifact-content art))))
    (append-content-hash (namestring (slot-value art 'location)) hash)))

(export-always '*base-url*)
(defparameter *base-url* "/")

(export-always 'link)
(defmethod embed-artifact-as ((art artifact) (as (eql 'link)) &key)
  (base-path-join *base-url* (artifact-location art)))

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
  (with-slots (weight style) art
    `(@font-face :font-family ,family
                 :src ,(str:join
                        ", "
                        (loop :for file :in (multiple-value-list (artifact-location art))
                              :for format := (pathname-type file)
                              :collect (format nil "url('~a') format('~a')" (artifact-location art) format)))
                 :font-weight ,weight
                 :font-stype ,style)))
