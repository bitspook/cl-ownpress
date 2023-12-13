(in-package #:in.bitspook.cl-ownpress/provider)

(defclass denote-provider (emacs-provider)
  ((script :initform (system-local "src/provider/elisp/denote.el"))))

(export-always 'denote-file)
(export-always 'denote-file-id)
(export-always 'denote-file-metadata)
(export-always 'denote-file-filepath)
(export-always 'denote-file-body-raw)
(export-always 'denote-file-body-html)
(defclass denote-file ()
  ((id :initarg :id :accessor denote-file-id)
   (filepath :initarg :filepath :accessor denote-file-filepath)
   (metadata :initarg :metadata :accessor denote-file-metadata)
   (body-raw :initarg :body-raw :accessor denote-file-body-raw)
   (body-html :initarg :body-html :accessor denote-file-body-html))
  (:documentation "A Denote file as provided by denote.el emacs-lisp script."))

(defmethod provide-all ((provider denote-provider) &rest script-args)
  (let ((notes (call-next-method provider script-args)))
    (mapcar
     (lambda (note)
       (make-instance
        'denote-file
        :id (@ note "id")
        :filepath (@ note "filepath")
        :metadata (when (@ note "metadata")
                    (yason:parse (@ note "metadata")))
        :body-raw (@ note "body_raw")
        :body-html (@ note "body_html")))
     notes)))

(defmethod categorize ((prov denote-provider) &key msg meta filepath)
  (or (call-next-method prov :msg msg :meta meta :filename filepath)
      (if (find "blog-post" (@ meta "tags") :test 'equal) "blog" "notes")))

(defmethod slugify ((prov denote-provider) &key msg meta filepath)
  (declare (ignore msg))
  (or (@ meta "slug") (first (str:split "__" (second (str:split "--" filepath))))))

(defmethod identify ((prov denote-provider) &key msg meta filepath)
  (declare (ignore msg))
  (or (@ meta "id") (first (str:split "--" filepath))))
