(in-package :clown)

(defclass post ()
  ((id :initarg :id)
   (slug :initarg :slug)
   (title :initarg :title)))

(defmethod print-object ((obj post) out)
  (print-unreadable-object (obj out)
    (with-slots (title) obj
      (format out "Post: title={~a}" title))))

(defclass published-post (post)
  ((output-path :initarg :output-path)))

(defun db-to-post (row)
  "Try to make a `post' from a database row."
  (when row
    (make-instance
     'post :title (getf row :|title|)
          :id (getf row :|id|)
          :slug (getf row :|slug|))))
