(in-package :clown-providers)

(defclass denote-provider (org-file-provider)
  ((keep-file-if
    :initarg :keep-file-if
    :initform (op (< 0 (ppcre:count-matches "_blog-post_" _))))))

(defmethod categorize ((prov denote-provider) &key msg meta filepath)
  (or (call-next-method prov :msg msg :meta meta :filename filepath)
      (if (find "blog-post" (@ meta "tags") :test 'equal) "blog" "notes")))

(defmethod slugify ((prov denote-provider) &key msg meta filepath)
  (declare (ignore msg))
  (or (@ meta "slug") (first (str:split "__" (second (str:split "--" filepath))))))

(defmethod identify ((prov denote-provider) &key msg meta filepath)
  (declare (ignore msg))
  (or (@ meta "id") (first (str:split "--" filepath))))
