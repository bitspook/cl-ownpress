(defpackage "cl-ownpress.publishers.slick"
  (:nicknames :clown-slick)
  (:use :cl)
  (:shadowing-import-from :spinneret :with-html-string))
(in-package :clown-slick)

(defparameter *debug-transpiles* t)

(defmacro with-layout ((&key title styles) &body body)
  `(with-html-string
     (:doctype)
     (:html
      (:head
       (:title ,title)
       ,(when styles `(:style ,styles)))
      (:body ,@body))))

(defun home-html ()
  (let ((spinneret:*html-style* :tree)
        (*print-pretty* (not *debug-transpiles*))
        (author "Charanjit Singh")
        (avatar "/images/avatar.png")
        (twitter "https://twitter.com/bitspook")
        (linkedin "https://linked.com/in/bitspook")
        (github "https://github.com/bitspook")
        (handle "bitspook")
        (resume "https://docs.google.com/document/d/1HFOxl97RGtuhAX95AhGWwa808SO9qSCYLjP1Pm39la0/"))
    (with-layout (:title (str:concat author "'s online home")
                  :styles (global-css))
      (:div :class "home"
            (:div :class "sidebar"
                  (:div :class "author"
                        (:img :class "avatar" :src avatar :alt author)
                        (:h2 :class "name" author)
                        (:p :class "handle"
                            (:a :href twitter "@" handle)))
                  (:div :class "quote" "Math is the new sexy")
                  (:div :class "social"
                        (:a :href github
                            :title (str:concat author " on Github")
                            :target "_blank"
                            (:span :class "github"))
                        (:a :href twitter
                            :title (str:concat author " on Twitter")
                            :target "_blank"
                            (:span :class "twitter"))
                        (:a :href linkedin
                            :title (str:concat author " on LinkedIn")
                            :target "_blank" (:span :class "linkedin"))
                        (:a :href "/feed.xml"
                            :title "Follow via RSS"
                            :target "_blank"
                            (:span :class "rss")))
                  (:img :class "pub-key-qr" :alt  author "'s Public GPG Key"))
            (:div :class "main"
                  (:section :class "about-me-snippet"
                            (:header
                             (:h2 "About Me"))
                            (:p "I write software to make a living. I have been on voluntary unemployment since March 26, 2022.")
                            (:p "I also enjoy writing, giving talks and discussing computers, security and politics.")
                            (:p "This website has things I am willing to share publicly. You can go through my"
                                (:a :href "/blog/" "blog") ", " (:a :href "/poems" "poems") ", " (:a :href github :target "_blank" "projects") ", " (:a :href "/talks" "talks") ", and my " (:a :href resume :target "_blank" "resume") " as well.")
                            (:footer "More " (:a :href "/about") "about me."))
                  (:section :class "recent-content"
                            (:header (:h2 "Recent Content"))
                            (:ul :class "recent-content-list"
                                 (:li (:a :href "/blog/post"
                                          :class "recent-content-item content-type--blog"
                                          "Blog post")))
                            (:footer (:a.btn.btn-primary.read-more-btn
                                      :href "/archive"
                                      "See all"))))))))

(defparameter *css-vars*
  '((width-l . "990px")
    (width-md . "840px")
    (width-sm . "480px")
    (colors . (:background "#fcfcfc"
               :primary "#006992"
               :secondary "#465c69"
               :cta-color "#b75d69"
               :primary-text "#010400"
               :separator "#6f6f6fd1"))))

(defun css-var (name)
  (cdr (assoc name *css-vars*)))

(defun css-color (name)
  (let ((colors (css-var 'colors)))
    (getf colors name)))

(defun fonts-defs ()
  (mapcar
   (lambda (args)
     (destructuring-bind (family style weight asset) args
       `(@font-face
         :font-family ,family
         :font-style ,style
         :font-weight ,weight
         :src (url ,(format nil "/assets/fonts/~a.woff2" asset))
         :font-display "optional")))
   '(("Roboto" "normal" 300 "Roboto/Roboto-Thin")
     ("Roboto" "normal" 500 "Roboto/Roboto-Regular")
     ("Roboto" "normal" "bold" "Roboto/Roboto-Bold")
     ("Cantarell" "normal" "bold" "Cantarell/Cantarell-Bold")
     ("Cantarell" "normal" "regular" "Cantarell/Cantarell-Regular"))))

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

(defun global-css ()
  (let ((lass:*pretty* (not *debug-transpiles*)))
    (str:join
     #\Newline
     (mapcar #'lass:compile-and-write
             (concatenate
              'list
              (fonts-defs)
              (top-level-defs))))))

(defun build-html-file (dest html &key clean-url?)
  "Write HTML to DEST. If CLEAN-URL?, write as dest/index.html"
  (let ((dest (if (and clean-url? (not (string= (ppath:basename dest) "index.html")))
                  (str:concat dest "/index.html")
                  dest))
        (dest-dirs (ppath:dirname dest)))
    (unless (or (string= dest-dirs ".") (str:emptyp dest-dirs))
      (uiop:ensure-all-directories-exist dest-dirs))
    (str:to-file dest html)))
