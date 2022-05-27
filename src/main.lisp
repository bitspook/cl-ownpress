(defpackage cl-ownpress
  (:nicknames :clown)
  (:use :cl :clown.db)
  (:shadowing-import-from :spinneret :with-html))
(in-package :cl-ownpress)

(defclass provider ()
  ((name
    :type string
    :initarg :name
    :initform (error "Provider must have a name")
    :reader provider-name)))

(defgeneric invoke-provider (provider &key)
  (:documentation "Execute a provider."))

(defparameter *org-roam-provider*
    (make-instance
     'provider
     :name "org-roam-provider"))

(defmethod invoke-provider ((provider (eql *org-roam-provider*)) &key)
  (let ((script-path (asdf:system-relative-pathname "cl-ownpress" "./src/providers/org-roam/org-roam.el"))
        (db-name (dbi:connection-database-name *conn*)))
    (uiop:run-program (format nil "emacs --script ~a ~a" script-path db-name)
                      :output *standard-output*
                      :error-output *standard-output*)))

(defun home-tpl (avatar author handle twitter github linkedin resume)
  `(:div :class "home"
         (:div :class "sidebar"
               (:div :class "author"
                     (:img :class "avatar" :src ,avatar :alt ,author)
                     (:h2 :class "name" ,author)
                     (:p :class "handle"
                         (:a :href ,twitter "@" ,handle)))
               (:div :class "quote" "Math is the new sexy")
               (:div :class "social"
                     (:a :href ,github :title ,author " on Github" :target "_blank" (:span :class "github"))
                     (:a :href ,twitter :title ,author " on Twitter" :target "_blank" (:span :class "twitter"))
                     (:a :href ,linkedin :title ,author " on LinkedIn" :target "_blank" (:span :class "linkedin"))
                     (:a :href "/feed.xml" :title "Follow via RSS" :target "_blank" (:span :class "rss")))
               (:img :class "pub-key-qr" :alt (who:conc ,author "'s Public GPG Key")))
         (:div :class "main"
               (:section :class "about-me-snippet"
                         (:header
                          (:h2 "About Me"))
                         (:p "I write software to make a living. I have been on voluntary unemployment since March 26, 2022.")
                         (:p "I also enjoy writing, giving talks and discussing computers, security and politics.")
                         (:p "This website has things I am willing to share publicly. You can go through my"
                             (:a :href "/blog/" "blog") "," (:a :href "/poems" "poems") "," (:a :href ,github :target "_blank" "projects") "," (:a :href "/talks" "talks") ", even my" (:a :href ,resume :target "_blank" "resume") " as well.")
                         (:footer "More " (:a :href "/about") "about me."))
               (:section :class "recent-content"
                         (:header (:h2 "Recent Content"))
                         (:ul :class "recent-content-list"
                              (:li (:a :href "/blog/post"
                                       :class "recent-content-item content-type--blog"
                                       "Blog post")))
                         (:footer (:a :class "btn btn-primary read-more-btn"
                                      :href "/archive"
                                      "See all"))))))

(defun layout-tpl (title &rest body)
  `(:html5
    (:head
     (:title ,title))
    (:body ,@body)))
