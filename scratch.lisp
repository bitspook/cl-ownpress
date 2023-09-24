(ql:quickload '(:cl-ownpress))

(in-package :cl-ownblog)

(let* ((www #p"/tmp/www/")
       (author (make-instance 'persona
                              :name "Charanjit Singh"
                              :handles '(("Mastodon" "bitspook" "https://infosec.exchange/@bitspook"))))
       (post (make-instance 'blog-post
                            :title "Test Title"
                            :description "Just trying to publish a dummy post"
                            :created-at (local-time:today)
                            :updated-at "2023-09-11"
                            :author author
                            :tags '("test" "post")
                            :body "<h1>'tis but a flesh wound</h1>"))
       (root-w (make-instance 'blog-post-w :post post))
       (asset-pub (make-instance 'asset-publisher
                                 :dest www))
       (post-pub (make-instance 'blog-post-publisher
                                :asset-pub asset-pub
                                :dest www)))
  (defmethod published-page ((pub (eql post-pub)) &key html css-file)
    "Create HTML page for a blog-post"
    (spinneret:with-html
      (:html (:head :title "Lol")
             (:link :type "stylesheet/css" :href css-file)
             (:body (:raw html)))))

  (uiop:delete-directory-tree www :validate t :if-does-not-exist :ignore)
  (publish post-pub :post post :layout root-w))
