(defsystem "clown-slick"
  :version "0.1.0"
  :author "Charanjit Singh"
  :license "AGPL"
  :depends-on ("cl-ownpress"
               "lass"
               "log4cl"
               "spinneret")
  :pathname "src/publishers/slick"
  :serial t
  :components ((:file "config")
               (:file "css")
               (:file "views/home")
               (:file "slick"))
  :description "")
