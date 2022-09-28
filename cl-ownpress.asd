(defsystem "cl-ownpress"
  :version "0.1.0"
  :author "Charanjit Singh"
  :license "AGPL"
  :depends-on ("alexandria"
               "serapeum"
               "cl-dbi"
               "cl-migratum"
               "cl-migratum.provider.local-path"
               "cl-migratum.driver.dbi"
               "lass"
               "log4cl"
               "cffi"
               "spinneret"
               "str"
               "ppath"
               "osicat"
               "sxql"
               "lparallel"
               "usocket"
               "yason"
               "xml-emitter")
  :components ((:module "src"
                :serial nil
                :components
                ((:file "package")
                 (:file "utils")
                 (:file "db")
                 (:module "providers"
                  :serial t
                  :components ((:file "package")
                               (:file "rpc-server")
                               (:file "org-file-provider")
                               (:file "org-roam-provider")))
                 (:module "publishers"
                  :serial t
                  :components ((:file "package")))
                 (:module
                  "publishers/blog"
                  :serial t
                  :components ((:file "package")
                               (:file "models")
                               (:file "rss")
                               (:file "publish")))
                 (:module "publishers/blog/default-theme"
                  :serial t
                  :components ((:file "package")
                               (:file "css")
                               (:file "navbar")
                               (:file "mixpanel")
                               (:file "footer")
                               (:file "post")
                               (:file "listing")
                               (:file "home")
                               (:file "theme")))
                 (:file "publishers/convinient-blog-re-exports"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-ownpress/tests"))))

(defsystem "cl-ownpress/tests"
  :author "Charanjit Singh"
  :license "AGPL"
  :depends-on ("cl-ownpress"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "package"))))
  :description "Test system for cl-ownpress"
  :perform (test-op (op c) (symbol-call :rove :run c)))
