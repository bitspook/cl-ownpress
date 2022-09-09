(defsystem "cl-ownpress"
  :version "0.1.0"
  :author "Charanjit Singh"
  :license "AGPL"
  :depends-on ("alexandria"
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
               "yason")
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "utils")
                 (:file "models")
                 (:file "rpc-server")
                 (:file "main")))
               (:module "src/publishers/slick"
                :serial t
                :components
                ((:file "package")
                 (:file "css")
                 (:module "views"
                  :serial t
                  :components
                  ((:file "package")
                   (:file "navbar")
                   (:file "mixpanel")
                   (:file "footer")
                   (:file "post")
                   (:file "listing")
                   (:file "home")))
                 (:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-ownpress/tests"))))

(defsystem "cl-ownpress/tests"
  :author "Charanjit Singh"
  :license "AGPL"
  :depends-on ("cl-ownpress"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "db")
                 (:file "main" :depends-on ("db")))))
  :description "Test system for cl-ownpress"
  :perform (test-op (op c) (symbol-call :rove :run c)))
