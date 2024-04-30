(defsystem "in.bitspook.cl-ownpress"
  :version "0.1.0"
  :author "Charanjit Singh"
  :license "AGPL-3.0-only"
  :depends-on ("serapeum" "str"
               "lass" "spinneret" "cl-slug"
               "flexi-streams" "clack" "yason" "md5")
  :components ((:module "src"
                :serial nil
                :components
                ((:file "package")
                 (:file "utils")

                 (:file "static")

                 (:file "artifact")
                 (:file "html/widget")

                 (:file "html/artifacts")

                 (:file "provider")
                 (:file "provider/rpc-server")
                 (:file "provider/emacs"))))
  :description "Personal publishing platform"
  :in-order-to ((test-op (test-op "in.bitspook.cl-ownpress/tests"))))

(defsystem "in.bitspook.cl-ownpress/tests"
  :author "Charanjit Singh"
  :license "AGPL-3.0-only"
  :depends-on (:in.bitspook.cl-ownpress :parachute)
  :components ((:module "tests"
                :components
                ((:file "package")))
               (:module "src"
                :components
                ((:file "artifact.test")
                 (:file "static.test")
                 (:file "html/widget.test"))))
  :description "Test system for cl-ownpress"
  :perform (test-op (op c) (symbol-call :parachute :test :in.bitspook.cl-ownpress/tests)))
