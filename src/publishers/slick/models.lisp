(in-package :clown-slick)

(defclass post ()
  ((id :initarg :id :reader post-id)
   (slug :initarg :slug :accessor post-slug)
   (title :initarg :title :accessor post-title)
   (tags :initarg :tags)
   (category :initarg :category :accessor post-category)
   (published-at :initarg :published-at :accessor post-published-at)
   (html-content :initarg :html-content :accessor post-html-content)))

(defun post-tags (post)
  "Return list of valid `post' tags.
`clown-slick' reserves some tags to be \"control\" tags (configured as
`:control-tags' in `*conf'), which aren't published."
  (remove-if (lambda (tag) (find tag (conf :control-tags) :test #'equal)) (slot-value post 'tags)))

(defun post-content (c-type post)
  "Return POST content of type C-TYPE."
  (slot-value post content))

(defmethod print-object ((obj post) out)
  (print-unreadable-object (obj out)
    (format out "Post: title={~a}" (post-title obj))))

(defclass published-post (post)
  ((public-path :initarg :public-path :accessor post-public-path)))

(defmacro fetch-posts (&rest query-frags)
  `(multiple-value-bind (stmt vals)
       (sxql:yield
        (sxql:select (:prov.id :prov.metadata (:as :proc.body :content_html)
                               (:as (:raw "json_extract(metadata, \"$.tags\")") :tags)
                               (:as (:raw "json_extract(metadata, \"$.category\")") :category)
                               (:as (:raw "json_extract(metadata, \"$.date\")") :published_at)
                               (:as (:raw "json_extract(metadata, \"$.title\")") :title)
                               (:as (:raw "json_extract(metadata, \"$.slug\")") :slug))
          (sxql:from (:as :provided_content :prov))
          (sxql:left-join (:as :processed_content :proc) :on (:= :prov.id :proc.prov_cont_id))
          (sxql:where (:= "html" :proc.type))
          (sxql:order-by (:desc :published_at))
          (sxql:where (if (conf :exclude-tags)
                          `(:and
                            (:or (:is-null :tags)
                                 ,@(loop :for tag :in (conf :exclude-tags) :collect
                                         `(:not (:like :tags ,(format nil "%\"~a\"%" tag))))))
                          1))
          ,@query-frags))
     (log:d "Executing SQL: ~a ~%[With vals: ~a]" stmt vals)
     (let* ((conn (clown:make-connection))
            (query (dbi:execute (dbi:prepare conn stmt) vals)))
       (mapcar #'db-to-post (dbi:fetch-all query)))))

(defun db-to-post (row)
  "Try to make a `post' from a database row."
  (let ((pub-date (or (uiop:if-let ((date (getf row :|published_at|)))
                        (local-time:parse-timestring date :date-time-separator #\Space)
                        nil)
                      (local-time:now))))
    (make-instance
     'post
     :id (getf row :|id|)
     :title (getf row :|title|)
     :slug (getf row :|slug|)
     :tags (when-let ((tags (getf row :|tags|))) (yason:parse tags))
     :category (getf row :|category|)
     :published-at pub-date
     :html-content (getf row :|content_html|))))
