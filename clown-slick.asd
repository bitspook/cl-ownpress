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
  :components ((:file "conf")
               (:file "css")
               (:file "views/package")
               (:file "views/home")
               (:file "views/navbar")
               (:file "slick"))
  :description "")
