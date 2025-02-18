(defsystem "in.bitspook.web-components"
  :version "0.1.0"
  :author "Charanjit Singh"
  :license "AGPL-3.0-only"
  :depends-on ("serapeum" "str" "lass" "spinneret"
                          "flexi-streams" "cl-slug")
  :components ((:module "src/web-components"
                :serial nil
                :components
                ((:file "package")))))

(defsystem "in.bitspook.web-components/tests"
  :version "0.1.0"
  :author "Charanjit Singh"
  :license "AGPL-3.0-only"
  :depends-on (:in.bitspook.web-components :parachute)
  :components ((:module "src/web-components"
                :serial nil
                :components
                ((:file "package.test"))))
  :description "Test system for cl-ownpress"
  :perform (test-op (op c) (symbol-call :parachute :test :in.bitspook.web-components/tests)))
