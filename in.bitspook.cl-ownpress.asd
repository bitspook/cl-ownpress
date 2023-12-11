(defsystem "in.bitspook.cl-ownpress"
  :version "0.1.0"
  :author "Charanjit Singh"
  :license "AGPL-3.0-only"
  :depends-on ("serapeum"
               "log4cl"
               "jsonrpc"
               "lass"
               "cffi"
               "spinneret"
               "str"
               "ppath"
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

                 (:module "provider"
                  :serial t
                  :components ((:file "package")
                               (:file "rpc-server")
                               (:file "emacs")))

                 (:module "publisher"
                  :serial t
                  :components ((:file "package")
                               (:file "asset")
                               (:file "html/widget")
                               (:file "html/html"))))))
  :description "Personal publishing platform"
  :in-order-to ((test-op (test-op "cl-ownpress/tests"))))

(defsystem "in.bitspook.cl-ownpress/tests"
  :author "Charanjit Singh"
  :license "AGPL-3.0-only"
  :depends-on (:in.bitspook.cl-ownpress :parachute)
  :components ((:module "tests"
                :components
                ((:file "package")))
               (:module "src"
                :components
                ((:file "publishers/asset.test")
                 (:file "publishers/html/html.test")
                 (:file "publishers/html/widget.test"))))
  :description "Test system for cl-ownpress"
  :perform (test-op (op c) (symbol-call :parachute :test :cl-ownpress/tests)))
