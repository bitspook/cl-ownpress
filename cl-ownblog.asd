(defsystem "cl-ownblog"
  :author "Charanjit Singh"
  :license "AGPL-3.0-only"
  :depends-on (:cl-ownpress)
  :components ((:module "src/blog"
                :components ((:file "package"))))
  :description "A blog implemented using cl-ownpress")
