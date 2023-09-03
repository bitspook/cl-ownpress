(defsystem "cl-ownpress"
  :version "0.1.0"
  :author "Charanjit Singh"
  :license "AGPL-3.0-only"
  :depends-on ("serapeum"
               "cl-dbi"
               "cl-migratum"
               "log4cl"
               "cl-migratum.provider.local-path"
               "cl-migratum.driver.dbi"
               "jsonrpc"
               "lass"
               "cffi"
               "spinneret"
               "str"
               "ppath"
               "sxql"
               "lparallel"
               "usocket"
               "yason"
               "xml-emitter"
               "plump"
               "cl-slug"
               "md5")
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
                               (:file "org-project-provider")))

                 (:module "publishers"
                  :serial t
                  :components ((:file "package")
                               (:file "asset")
                               (:file "html/html")
                               (:file "html/widget")
                               (:file "blog-post")
                               (:file "blog-post-listing")))

                 (:module "publishers/journey"
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
                               (:file "about-page")
                               (:file "widgets/notebook-btn")
                               (:file "journeys-listing-page")
                               (:file "journey-page")
                               (:file "post-page")
                               (:file "posts-listing-page")
                               (:file "home-page")
                               (:file "default-theme")))
                 (:file "publishers/convinient-blog-re-exports"))))
  :description "Personal publishing platform"
  :in-order-to ((test-op (test-op "cl-ownpress/tests"))))

(defsystem "cl-ownpress/tests"
  :author "Charanjit Singh"
  :license "AGPL-3.0-only"
  :depends-on (:cl-ownpress :parachute)
  :components ((:module "tests"
                :components
                ((:file "package")))
               (:module "src"
                :components
                ((:file "publishers/asset.test")
                 (:file "publishers/html/html.test")
                 (:file "publishers/html/widget.test")
                 (:file "publishers/blog-post.test"))))
  :description "Test system for cl-ownpress"
  :perform (test-op (op c) (symbol-call :parachute :test :cl-ownpress/tests)))
