(defpackage :clown-slick.config
  (:use :cl)
  (:export config
           *debug-transpiles*))
(in-package :clown-slick.config)

(defparameter *conf*
  '((author . "Charanjit Singh")
    (avatar . "/images/avatar.png")
    (twitter . "https://twitter.com/bitspook")
    (linkedin . "https://linked.com/in/bitspook")
    (github . "https://github.com/bitspook")
    (handle . "bitspook")
    (resume . "https://docs.google.com/document/d/1HFOxl97RGtuhAX95AhGWwa808SO9qSCYLjP1Pm39la0/")))

(defparameter *debug-transpiles* t)

(defun conf (key)
  (cdr (assoc key *conf*)))
