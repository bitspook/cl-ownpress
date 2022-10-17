(in-package #:default-theme)

(defparameter +gh-lang-colors+
  (yason:parse
   (str:from-file
    (clown:system-local "src/publishers/blog/default-theme/gh-language-colors.json")))
  "Language colors used by Github in HEX notation.")
