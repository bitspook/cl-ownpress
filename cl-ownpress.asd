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
               "parenscript"
               "str"
               "ppath"
               "osicat"
               "sxql"
               "lparallel"
               "usocket"
               "yason"
               "xml-emitter"
               "plump")
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
                               (:file "denote-provider")
                               (:file "org-project-provider")
                               (:file "org-roam-provider")))
                 (:module "publishers"
                  :serial t
                  :components ((:file "package")))
                 (:module
                  "publishers/blog/theme"
                  :serial t
                  :components ((:file "package")
                               (:file "widget")))
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
                               (:file "navbar-widget")
                               (:file "mixpanel-widget")
                               (:file "footer-widget")
                               (:file "oracle-nav-widget")
                               (:file "posts-listing-widget")
                               (:file "gh-language-colors")
                               (:file "projects-listing-page")
                               (:file "project-page")
                               (:file "post-page")
                               (:file "posts-listing-page")
                               (:file "home-page")
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
