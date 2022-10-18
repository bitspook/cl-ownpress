(in-package :clown-providers)

(defclass org-project-provider (org-file-provider)
  ((elisp-script
    :initarg :elisp-script))
  (:default-initargs
   :elisp-script (asdf:system-relative-pathname "cl-ownpress" "./src/providers/elisp/org-project-file.el")))

(defmethod categorize ((prov org-project-provider) &key msg)
  (declare (ignore msg))
  "project")
