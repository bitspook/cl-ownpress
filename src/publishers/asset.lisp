(in-package #:clown-publishers)

(export-always 'asset-publisher)
(defclass asset-publisher (publisher)
  nil
  (:documentation "Publish static assets."))

(defmethod publish ((pub asset-publisher) &key path content)
  "Publish CONTENT as a file at PATH. If CONTENT is a `pathname', it
is (recursively if it is a directory) copied to PATH. PATH is a suggestion for
where the new file should be created, path of the created artifact might differ.
Return the path of created artifact."
  (list path content))

;; TODO
;; - Write tests for asset-publisher
;; - Write tests for blog-post-publisher
