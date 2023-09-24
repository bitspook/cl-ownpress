(in-package #:cl-ownblog)

(defwidget blog-post-w (post) nil
  (with-slots (title created-at tags body) post
    (:div
     (render 'navbar-w)
     (:section
      :class "content"
      (:header
       :class "content-header"
       (:h1 title)
       (:span
        :class "content-meta"
        (:span
         :class "meta-item date"
         (local-time:format-timestring
          nil created-at
          :format '(:long-month " " :day ", " :year)))
        (when-let ((tags tags))
          (:ul
           :class "meta-item tags"
           (dolist (tag tags)
             (:li.tag
              (:a :href
                  (str:concat "/tags/" tag)
                  (str:concat "#" (str:downcase tag)))))))))
      (:article :class "main-article"
                (:raw body)))
     (render 'footer-w :post post))))
