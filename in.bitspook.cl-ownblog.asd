(defsystem "in.bitspook.cl-ownblog"
  :author "Charanjit Singh"
  :license "AGPL-3.0-only"
  :depends-on (:in.bitspook.cl-ownpress :local-time)
  :components ((:module "src/blog"
                :components ((:file "package")
                             (:file "modern-normalize")
                             (:file "pollen")
                             (:file "global-lass")
                             (:file "blog-post")
                             (:module "widgets"
                              :components ((:file "navbar")
                                           (:file "footer")
                                           (:file "blog-post"))))))
  :description "A blog implemented using cl-ownpress")
