(defpackage :clown-slick
  (:use :cl)
  (:import-from :spinneret :with-html-string)
  (:export
   conf
   *debug-transpiles*
   *css-vars*
   css-var
   css-color
   font-defs
   top-level-defs
   adjustable-width
   main))
(in-package :clown-slick)

(defparameter *conf*
  '((:author . "Charanjit Singh")
    (:avatar . "/images/avatar.png")
    (:twitter . "https://twitter.com/bitspook")
    (:linkedin . "https://linked.com/in/bitspook")
    (:github . "https://github.com/bitspook")
    (:handle . "bitspook")
    (:resume . "https://docs.google.com/document/d/1HFOxl97RGtuhAX95AhGWwa808SO9qSCYLjP1Pm39la0/")))

(defparameter *debug-transpiles* t)

(defun conf (key)
  (cdr (assoc key *conf*)))
