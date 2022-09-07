(in-package :clown)

(defclass post ()
  ((id :initarg :id)
   (slug :initarg :slug)
   (title :initarg :title :accessor post-title)
   (tags :initarg :tags :accessor post-tags)
   (category :initarg :category :accessor post-category)
   (published-at :initarg :published-at :accessor post-published-at)
   (html-content :initarg :html-content)))

(defmethod print-object ((obj post) out)
  (print-unreadable-object (obj out)
    (format out "Post: title={~a}" (post-title obj))))

(defun fetch-recent-posts (&optional (limit -1))
  "Create a fetcher to fetch recent posts, one post at a time. Return the
query itself as second value.

# Examples

## Use the fetcher
```lisp
(loop :with fetcher := (fetch-recent-posts 5)
      :for post := (funcall fetcher)
      :while post
      :collect post)
```

## Fetch all recent posts
```lisp
(multiple-value-bind (fetcher query) (fetch-recent-posts 5)
 (mapcar #'db-to-post (dbi:fetch-all query)))
```"
  (let* ((query (sxql:yield
                 (sxql:select (:*)
                   (sxql:from :inputs)
                   (sxql:order-by (:desc :published_at))
                   (sxql:limit limit))))
         (conn (clown:make-connection))
         (query (dbi:execute (dbi:prepare conn query))))
    (values (lambda () (let ((row (dbi:fetch query)))
                         (when row (db-to-post row))))
            query)))

(defclass published-post (post)
  ((output-path :initarg :output-path :accessor post-output-path)))

(defun db-to-post (row)
  "Try to make a `post' from a database row."
  (labels ((parse-tags (tag-str)
             (let ((tags (yason:parse tag-str)))
               (if (hash-table-p tags) nil
                   tags)))
           (parse-category (cat-str)
             (when (not (str:emptyp cat-str)) cat-str)))

    (let ((pub-date (uiop:if-let ((date (getf row :|published_at|)))
                      (local-time:parse-timestring date :date-time-separator #\Space)
                      nil)))
      (make-instance
       'post
       :title (getf row :|title|)
       :id (getf row :|id|)
       :slug (getf row :|slug|)
       :tags (parse-tags (getf row :|tags|))
       :category (parse-category (getf row :|category|))
       :published-at pub-date
       :html-content (getf row :|content_html|)))))
