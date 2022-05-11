(in-package :cl-user)

(ql:quickload '(:cl-project))

(cl-project:make-project
 #p"../cl-ownpress"
 :author "Charanjit Singh"
 :license "AGPL"
 :depends-on '(:cl-dbi :cl-migratum :lass))
