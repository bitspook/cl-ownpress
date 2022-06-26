(defpackage :clown-slick.views
            (:nicknames :slick-views)
            (:use :cl)
            (:import-from :spinneret :with-html-string)
            (:import-from :clown-slick.conf conf *debug-transpiles*)
            (:import-from :clown-slick.css
                          font-defs
                          css-var
                          css-color
                          top-level-defs
                          button-defs
                          to-css-str)
            (:export home
                     navbar))
