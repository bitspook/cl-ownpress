(defpackage :clown-slick.css
  (:nicknames :slick.css)
  (:use :cl)
  (:import-from :clown-slick.conf *debug-transpiles*)
  (:export *css-vars*
           css-var
           css-color
           font-defs
           top-level-defs
           adjustable-width))
(in-package :clown-slick.css)

(defparameter *css-vars*
  '((width-l . "990px")
    (width-md . "840px")
    (width-sm . "480px")
    (colors . (:background "#fcfcfc"
               :background-dark1 "#f9f9f9"
               :background-dark2 "#efefef"
               :primary "#006992"
               :secondary "#465c69"
               :cta "#b75d69"
               :cta-dark1 "#b75d69"
               :cta-dark2 "#b75d69"
               :primary-text "#010400"
               :separator "#6f6f6fd1"))))

(defun css-var (name)
  (cdr (assoc name *css-vars*)))

(defun css-color (name)
  (let ((colors (css-var 'colors)))
    (getf colors name)))

(defun font-defs ()
  (mapcar
   (lambda (args)
     (destructuring-bind (family style weight asset) args
       `(@font-face
         :font-family ,family
         :font-style ,style
         :font-weight ,weight
         :src (url ,(format nil "/fonts/~a" asset)))))
   '(("Roboto" "normal" 300 "roboto-thin.woff2")
     ("Roboto" "normal" 500 "roboto-regular.woff2")
     ("Roboto" "normal" "bold" "roboto-bold.woff2")
     ("Cantarell" "normal" "bold" "Cantarell-Bold.ttf")
     ("Cantarell" "normal" "regular" "Cantarell-Regular.ttf"))))

(defun top-level-defs ()
  `(("*" :margin 0
         :padding 0
         :box-sizing "border-box")
    (body :font-family "Roboto"
          :font-size "14px"
          :background ,(css-color :background)
          :color ,(css-color :primary-text))
    (a :text-decoration "underline"
       :color ,(css-color :primary-text))
    ((:or h1 h2 h3 h4 h5 h6)
     :font-family "Roboto, sans-serif"
     :margin "4rem 0 2rem 0")
    ((:or img figure)
     (figcaption
      :padding "0.4em"
      :padding-top 0
      :border-bottom "2px solid" ,(css-color :secondary)
      :font-size "0.7em"))
    (blockquote :background-color ,(css-color :secondary)
                :padding 0.01em 1em
                :margin 1em 0
                :border-left 4px solid ,(css-color :secondary))
    (pre :overflow auto
         :padding 1em)
    (.title :margin 2rem 0
            (h1 :font-family "Roboto" sans-serif
                :font-weight bold
                :font-size 1.7em
                :line-height 1.2
                :text-transform capitalize))
    (.postamble
     :font-family monospace
     :color "#666"
     :margin 4rem auto

     (.validation :display none))
    (:media ,(format nil "(max-width: ~a)" (css-var 'width-md))
     (.postamble
      :max-width 100%
      :padding 0 8%))
    (:media ,(format nil "(max-width: ~a)" (css-var 'width-sm))
     (.postamble
      :max-width 100%
      :padding 0 4%))
    ("#mc_embed_signup" :max-width 600px
                        :background transparent
                        :margin-bottom 4rem)
    (.newsletter-email :border 1px solid ,(css-color :secondary)
                       :border-radius 25px
                       :width 100%
                       :margin-bottom 1rem
                       :padding 0.4rem 0.8rem)))

(defun adjustable-width (&rest selectors)
  `((,@selectors :max-width 70%
                :padding 0 124px
                :margin 0 auto)
    (:media ,(format nil "(max-width: ~a)" (css-var 'width-md))
            (,@selectors :max-width 100%
                        :padding 0 8%))
    (:media ,(format nil "(max-width: ~a)" (css-var 'width-sm))
            (,@selectors :max-width 100%
                        :padding 0 4%))))

(defun button-defs ()
  `((.btn :display inline-flex
          :border 2px solid
          :border-radius 25px
          :padding 0 2.1em
          :cursor pointer
          :font-family "Roboto"
          :font-weight normal
          :line-height 2.2em
          :align-items center
          :text-align center
          :text-decoration none)

    (.btn-cta :border-color ,(css-color :cta)
              :background ,(css-color :cta)
              :color ,(css-color :background))
    (".btn-cta:hover" :background ,(css-color :cta-dark1))
    (".btn-cta:active" :background ,(css-color :cta-dark2))

    (.btn-primary :border-color ,(css-color :primary)
                  :color ,(css-color :primary)
                  :background ,(css-color :background))
    (".btn-primary:hover" :background ,(css-color :background-dark1))
    (".btn-primary:active" :background ,(css-color :background-dark2))

    (.btn-secondary :border-color ,(css-color :secondary)
                    :color ,(css-color :secondary)
                    :background ,(css-color :background))
    (".btn-secondary:hover" :background ,(css-color :background-dark1))
    (".btn-secondary:active" :background ,(css-color :background-dark2))))

(defun to-css-str (&rest defs)
  (let ((lass:*pretty* *debug-transpiles*))
    (str:join
     (if *debug-transpiles* #\Newline #\Space)
     (mapcar #'lass:compile-and-write (apply #'concatenate 'list defs)))))
