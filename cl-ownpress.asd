(defsystem "cl-ownpress"
  :version "0.1.0"
  :author "Charanjit Singh"
  :license "AGPL"
  :depends-on ("cl-dbi"
               "cl-migratum"
               "lass")
  :components ((:module "src"
                :components
                ((:file "main"))))
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
