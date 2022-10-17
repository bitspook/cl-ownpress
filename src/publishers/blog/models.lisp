(in-package :clown-blog)

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
`clown-blog' reserves some tags to be \"control\" tags (configured as
`:control-tags' in `*conf'), which aren't published."
  (remove-if (lambda (tag) (find tag (conf :control-tags) :test #'equal)) (slot-value post 'tags)))

(defun post-content (c-type post)
  "Return POST content of type C-TYPE."
  (slot-value post content))

(defmethod print-object ((obj post) out)
  (print-unreadable-object (obj out)
    (format out "Post: title={~a}" (post-title obj))))

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
          (sxql:where (:not (:= "project" :category)))
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

(defun fetch-recent-posts (&optional (limit 5))
  (fetch-posts (sxql:limit limit)
               (sxql:where (:not-in :category (conf :unlisted-categories)))))

(defun fetch-unlisted-posts (&optional (limit -1))
  (fetch-posts (sxql:limit limit)))

(defclass project ()
  ((id :initarg :id :reader project-id)
   (slug :initarg :slug :accessor project-slug)
   (name :initarg :name :accessor project-name)
   (tagline :initarg :tagline :accessor project-tagline)
   (html-description :initarg :html-description :accessor project-html-description)
   (languages :initarg :languages :accessor project-languages)
   (source-code :initarg :source-code :accessor project-source-code)
   (documentation :initarg :documentation :accessor project-docs)
   (issue-tracker :initarg :issue-tracker :accessor project-issue-tracker)
   (html-content :initarg :html-content :accessor project-html-content)
   (updated-at :initarg :updated-at :accessor project-updated-at)))

(defmethod print-object ((obj project) out)
  (print-unreadable-object (obj out)
    (format out "Project: name={~a}" (project-name obj))))

(defun db-to-project (row)
  "Try to make a `project' from a database row."
  (make-instance
   'project
   :id (getf row :|id|)
   :slug (getf row :|slug|)
   :name (getf row :|name|)
   :tagline (getf row :|tagline|)
   :html-description (getf row :|html_description|)
   :languages (when-let ((languages (getf row :|languages|)))
                (yason:parse languages))
   :source-code (getf row :|source_code|)
   :documentation (getf row :|documentation|)
   :issue-tracker (getf row :|issue_tracker|)
   :html-content (getf row :|html_content|)
   :updated-at (getf row :|updated_at|)))

(defmacro fetch-projects (&rest query-frags)
  `(multiple-value-bind (stmt vals)
       (sxql:yield
        (sxql:select (:prov.id
                      (:as :proc.body :html_content)
                      (:as (:raw "json_extract(metadata, \"$.title\")") :name)
                      (:as (:raw "json_extract(metadata, \"$.tagline\")") :tagline)
                      (:as (:raw "json_extract(metadata, \"$.category\")") :category)
                      (:as (:raw "json_extract(metadata, \"$.tags\")") :tags)
                      (:as (:raw "json_extract(metadata, \"$.languages\")") :languages)
                      (:as (:raw "json_extract(metadata, \"$.source_code\")") :source_code)
                      (:as (:raw "json_extract(metadata, \"$.issue_tracker\")") :issue_tracker)
                      (:as (:raw "json_extract(metadata, \"$.documentation\")") :documentation)
                      (:as (:raw "json_extract(metadata, \"$.slug\")") :slug)
                      (:as (:raw "json_extract(metadata, \"$.description_html\")") :html_description)
                      (:as (:raw "json_extract(metadata, \"$.updated_at\")") :updated_at))
          (sxql:from (:as :provided_content :prov))
          (sxql:left-join (:as :processed_content :proc) :on (:= :prov.id :proc.prov_cont_id))
          (sxql:where (:= "html" :proc.type))
          (sxql:where (:= "project" :category))
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
       (mapcar #'db-to-project (dbi:fetch-all query)))))

(defun fetch-all-projects (&optional (limit -1))
  (fetch-projects (sxql:limit limit)))

(defun project-public-path (project)
  (format nil "/projects/~a" (project-slug project)))
