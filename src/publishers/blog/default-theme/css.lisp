(in-package :default-theme)

(defparameter *css-vars*
  '((:width-xl . "1080px")
    (:width-l . "990px")
    (:width-md . "840px")
    (:width-sm . "520px")
    (:title-font-family . "Roboto, sans-serif")
    (:content-font-family . "Cantarell, sans-serif")
    (:colors . (:background "var(--bg)"
                :background-dark1 "var(--bg-d1)"
                :background-dark2 "var(--bg-d2)"
                :primary "var(--primary)"
                :secondary "var(--secondary)"
                :cta "var(--cta)"
                :cta-dark1 "var(--cta-em1)"
                :cta-dark2 "var(--cta-em2)"
                :success "var(--success)"
                :warning "var(--warning)"
                :error "var(--error)"
                :primary-text "var(--primary-text)"
                :dim-text "var(--dim-text)"
                :separator "var(--separator)"
                :separator-light "var(--separator-em1)"))))

(defun css-var (name)
  (cdr (assoc name *css-vars*)))

(defun css-color (name)
  (let ((colors (css-var :colors)))
    (getf colors name)))

(defparameter font-css
  (mapcar
   (lambda (args)
     (destructuring-bind (family style weight asset) args
       `(@font-face
         :font-family ,family
         :font-style ,style
         :font-weight ,weight
         :font-display "swap"
         :src (url ,(format nil "/fonts/~a" asset)))))
   '(("Roboto" "normal" 300 "roboto-thin.woff2")
     ("Roboto" "normal" 500 "roboto-regular.woff2")
     ("Roboto" "normal" "bold" "roboto-bold.woff2")
     ("Cantarell" "normal" "bold" "Cantarell-Bold.ttf")
     ("Cantarell" "normal" "regular" "Cantarell-Regular.ttf"))))

(defun top-level-css ()
  `((:media
     "(prefers-color-scheme: light)"
     (":root"
      :--bg "#fcfcfc"
      :--bg-d1 "#efefef"
      :--bg-d2 "#ececec"
      :--primary "#006992"
      :--secondary "#465c69"
      :--cta "#b75d69"
      :--cta-em1 "#b75d69"
      :--cta-em2 "#b75d69"
      :--primary-text "#010400"
      :--dim-text "#6f6f6f"
      :--separator "#888"
      :--separator-em1 "#6f6f6f29"
      :--success "green"
      :--warning "orange"
      :--error "red"))

    (:media
     "(prefers-color-scheme: dark)"
     (":root"
      :--bg "#0c0c0d"
      :--bg-d1 "#19191cc4"
      :--bg-d2 "#2f2f37"
      :--primary "#006992"
      :--secondary "#465c69"
      :--cta "#b75d69"
      :--cta-em1 "#b75d69"
      :--cta-em2 "#b75d69"
      :--primary-text "#a4a4a4"
      :--dim-text "#6f6f6f"
      :--separator "#888"
      :--separator-em1 "#6f6f6f29"
      :--success "green"
      :--warning "orange"
      :--error "red"))

    ,@font-css
    ("*" :margin 0
         :padding 0
         :box-sizing "border-box")
    (body :font-family ,(css-var :content-font-family)
          :font-size "14px"
          :background ,(css-color :background)
          :color ,(css-color :primary-text))
    ((:or .title header)
     :font-family ,(css-var :title-font-family))
    (a :text-decoration "underline"
       :color ,(css-color :primary-text))
    ((:or h1 h2 h3 h4 h5 h6)
     :font-family ,(css-var :title-font-family)
     :margin "4rem 0 2rem 0")
    ((:or img figure)
     (figcaption
      :padding "0.4em"
      :padding-top 0
      :border-bottom "2px solid" ,(css-color :secondary)
      :font-size "0.7em"))

    ((:or ol ul) :padding-left 0.8rem)

    (blockquote :padding 0.01em 1em
                :margin 1em 0
                :border-left 4px solid ,(css-color :secondary)
                :font-size 1.2em)
    (pre :overflow auto
         :padding 1em
         :font-family monospace
         :font-size 1.2rem)
    (code :font-family monospace
          :color ,(css-color :cta)
          :font-size 1rem)
    (.title :margin 2rem 0
            (h1 :font-family "Roboto" sans-serif
                :font-weight bold
                :font-size 1.7em
                :line-height 1.2
                :text-transform capitalize))
    (video :width 100%)
    (col :min-width 10rem)
    ((:and tr (:nth-child even))
     :background-color ,(css-color :background-dark2))
    (td :padding .4em .8em)
    (.src :font-size 1rem
          :padding 1em
          :border 1px solid ,(css-color :separator-light)
          :border-radius 8px
          :box-sizing scroll
          :margin 2em 0)))

(defun adjustable-width-css (&rest selectors)
  `((,@selectors :max-width ,(css-var :width-xl)
                 :padding 0 124px
                 :margin 0 auto)
    (:media ,(format nil "(max-width: ~a)" (css-var :width-md))
            (,@selectors :max-width 100%
                         :padding 0 8%))
    (:media ,(format nil "(max-width: ~a)" (css-var :width-sm))
            (,@selectors :max-width 100%
                         :padding 0 4%))))

(defun button-css ()
  `((.btn :display inline-flex
          :border 2px solid
          :border-radius 25px
          :padding 0 2.1rem
          :cursor pointer
          :font-family "Roboto"
          :font-weight normal
          :line-height 2.2rem
          :align-items center
          :text-align center
          :background-color ,(css-color :background)
          :text-decoration none)
    (|.btn:hover| :background-color ,(css-color :background-dark1))
    (".btn:active" :background ,(css-color :background-dark2))
    (".btn[disabled]:hover" :background ,(css-color :background)
                            :cursor default)

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
