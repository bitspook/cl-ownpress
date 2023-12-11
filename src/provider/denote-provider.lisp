(in-package #:in.bitspook.cl-ownpress/provider)

(defclass denote-provider (emacs-provider)
  ((script :initform (system-local "src/provider/elisp/org-file.el"))))

(defmethod categorize ((prov denote-provider) &key msg meta filepath)
  (or (call-next-method prov :msg msg :meta meta :filename filepath)
      (if (find "blog-post" (@ meta "tags") :test 'equal) "blog" "notes")))

(defmethod slugify ((prov denote-provider) &key msg meta filepath)
  (declare (ignore msg))
  (or (@ meta "slug") (first (str:split "__" (second (str:split "--" filepath))))))

(defmethod identify ((prov denote-provider) &key msg meta filepath)
  (declare (ignore msg))
  (or (@ meta "id") (first (str:split "--" filepath))))

(let ((notes (make-instance 'denote-provider)))
  (provide-all notes))
