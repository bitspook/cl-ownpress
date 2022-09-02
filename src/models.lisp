(in-package :clown)

(defclass post ()
  ((id :initarg :id)
   (slug :initarg :slug)
   (title :initarg :title)
   (tags :initarg :tags)
   (html-content :initarg :html-content)))

(defmethod print-object ((obj post) out)
  (print-unreadable-object (obj out)
    (with-slots (title) obj
      (format out "Post: title={~a}" title))))

(defun fetch-recent-posts (&optional (limit 5))
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
                   (sxql:order-by :published_at)
                   (sxql:limit limit))))
         (conn (clown:make-connection))
         (query (dbi:execute (dbi:prepare conn query))))
    (values (lambda () (db-to-post (dbi:fetch query)))
            query)))

(defclass published-post (post)
  ((output-path :initarg :output-path)))

(defun db-to-post (row)
  "Try to make a `post' from a database row."
  (when row
    (make-instance
     'post :title (getf row :|title|)
           :id (getf row :|id|)
           :slug (getf row :|slug|)
           :tags (yason:parse (getf row :|tags|))
           :html-content (getf row :|content_html|))))
