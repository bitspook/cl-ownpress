(defpackage "cl-ownpress.publishers.slick"
  (:nicknames :clown-slick)
  (:use :cl)
  (:shadowing-import-from :spinneret :with-html-string))
(in-package :clown-slick)

(defparameter *debug-transpiles* t)

(defparameter *conf*
  '((author . "Charanjit Singh")
    (avatar . "/images/avatar.png")
    (twitter . "https://twitter.com/bitspook")
    (linkedin . "https://linked.com/in/bitspook")
    (github . "https://github.com/bitspook")
    (handle . "bitspook")
    (resume . "https://docs.google.com/document/d/1HFOxl97RGtuhAX95AhGWwa808SO9qSCYLjP1Pm39la0/")))

(defun conf (key)
  (cdr (assoc key *conf*)))

(defun navbar-tree ()
  `(:nav :class "top-nav"
         (:div :class "brand"
               (:a :href "/"
                   (:img :class "brand-avatar"
                         :src (conf 'avatar)
                         :alt (conf 'author))))
         (:div
          (:ul (:li (:a :href "/blog" "Blog"))
               (:li (:a :href "/poems" "Poems"))
               (:li (:a :href (conf 'github) "Projects"))))))

(defun home-tree ()
  `(:div :class "home"
         (:div :class "sidebar"
               (:div :class "author"
                     (:img :class "avatar" :src (conf 'avatar) :alt (conf 'author))
                     (:h2 :class "name" (conf 'author))
                     (:p :class "handle"
                         (:a :href (conf 'twitter) "@" (conf 'handle))))
               (:div :class "quote" "Math is the new sexy")
               (:div :class "social"
                     (:a :href (conf 'github)
                         :title (str:concat (conf 'author) " on Github")
                         :target "_blank"
                         (:span :class "github"))
                     (:a :href (conf 'twitter)
                         :title (str:concat (conf 'author) " on Twitter")
                         :target "_blank"
                         (:span :class "twitter"))
                     (:a :href (conf 'linkedin)
                         :title (str:concat (conf 'author) " on LinkedIn")
                         :target "_blank" (:span :class "linkedin"))
                     (:a :href "/feed.xml"
                         :title "Follow via RSS"
                         :target "_blank"
                         (:span :class "rss")))
               (:img :class "pub-key-qr"
                     :alt ,(str:concat (conf 'author) "'s Public GPG Key")
                     :src "/images/public-key-qr.svg"))
         (:div :class "main"
               (:section :class "about-me-snippet"
                         (:header
                          (:h2 "About Me"))
                         (:p "I write software to make a living. I have been on voluntary unemployment since March 26, 2022.")
                         (:p "I also enjoy writing, giving talks and discussing computers, security and politics.")
                         (:p "This website has things I am willing to share publicly. You can go through my "
                             (:a :href "/blog/" "blog") ", " (:a :href "/poems" "poems") ", " (:a :href (conf 'github) :target "_blank" "projects") ", " (:a :href "/talks" "talks") ", and my " (:a :href (conf 'resume) :target "_blank" "resume") " as well.")
                         (:footer "More " (:a :href "/about" "about me.")))
               (:section :class "recent-content"
                         (:header (:h2 "Recent Content"))
                         (:ul :class "recent-content-list"
                              (:li (:a :href "/blog/post"
                                       :class "recent-content-item content-type--blog"
                                       "Blog post")))
                         (:footer (:a.btn.btn-primary.read-more-btn
                                   :href "/archive"
                                   "See all"))))))

(defmacro home-html ()
  (let ((body (home-tree)))
    `(let ((spinneret:*suppress-inserted-spaces* t)
           (spinneret:*html-style* (if *debug-transpiles* :human :tree))
           (*print-pretty* *debug-transpiles*)
           (title (str:concat (conf 'author) "'s online home"))
           (styles (to-css-str
                    (font-defs)
                    (top-level-defs)
                    (button-defs)
                    (home-defs))))
       (with-html-string
         (:doctype)
         (:html
          (:head
           (:title title)
           (:style (:raw styles)))
          (:body ,body))))))

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

(defun navbar-defs ()
  `((.top-nav
     :height 90px
     :display flex
     :position relative
     :justify-content space-between

     (ul :list-style-type none
         :display flex
         :align-items center
         :padding 2em 0)

     (li :flex-grow 1
         :padding-right 2em

         (a :text-decoration underline
            :font-size 1.4em
            :font-family Roboto sans-serif
            :color ,(css-color :primary))

         ("a:hover" :color ,(css-color :secondary)))

     ("li:last-child" :padding 0)

     (.brand :width 60px
             :height 60px
             :align-self center
             :flex-grow 1)

     (.brand-avatar :height 100%))
    (:media ,(format nil "(max-width: ~a)" (css-var 'width-sm))
            (.top-nav :padding-right 1em
                      (a :font-size 1.2em)))
    ,@(adjustable-width ".top-nav")))

(defun home-defs ()
  `((:media ,(format nil "(max-width: ~a)" (css-var 'width-md))
            (.home :flex-direction column)
            (.sidebar :width 100%)
            (.main :width 100%))

    (:media ,(format nil "(max-width: ~a)" (css-var 'width-l))
            (.sidebar :width 40%)
            (.main :width 60%))

    (.home :display flex
           (.sidebar :width 450px
                     :left 0
                     :top 0%
                     :bottom 0%
                     :padding 35px
                     :position relative
                     :text-align center
                     :border-right 0.2px solid ,(css-color :separator))

           (.name :font-size 2em
                  :margin-top 2.6em
                  :font-family "Roboto"
                  :font-weight normal)
           (.handle :font-size 1.3em
                    :line-height 0
                    :font-family "Roboto"
                    :font-weight 300
                    (a :text-decoration none))
           (.quote :padding 20px
                   :font-family "Roboto"
                   :font-weight 300
                   :font-size 1.4em
                   :position relative
                   :margin 15px 0
                   :margin-top 45px)
           (".quote::before" :content ""
                             :background (url "/images/icons/quote.svg") no-repeat
                             :position absolute
                             :width 14px
                             :height 14px
                             :left 21px
                             :top 0px)
           (.social :width 148px
                    :position relative
                    :overflow hidden
                    :margin 0 auto
                    :padding-top 40px

                    (a :clear top
                       :display block
                       :float left
                       :width "calc(50% - 20px)"
                       :margin 10px)

                    (span :display block
                          :position relative
                          :width 48px
                          :height 48px)

                    (.github :background (url "/images/icons/github.svg") no-repeat
                             :background-size contain)
                    (.twitter :background (url "/images/icons/twitter.svg") no-repeat
                              :background-size contain)
                    (.rss :background (url "/images/icons/rss.svg") no-repeat
                          :background-size contain)
                    (.linkedin :background (url "/images/icons/linkedin.svg") no-repeat
                               :background-size contain))

           (.pub-key-qr :margin 55px auto
                        :margin-bottom 0))

    (.main :width "calc(100% - 450px)"
           :max-width 872px
           :position relative
           :padding 40px

           (a :text-decoration none)

           (h2 :font-size 2.2em
               :padding 10px 0
               :font-weight normal
               :font-family "Roboto")

           (.about-me-snippet
            :font-family "Cantarell"
            :font-size 1.35em
            :line-height 1.5
            :align-items center
            :margin 0.8em 0
            :font-weight 500

            (a :text-decoration underline
               :font-weight normal)

            ("a:active" :color ,(css-color :secondary)))

           (.recent-content
            :font-family "Contarell, sans-serif"

            (.recent-content-list
             :list-style-type none
             :margin 2em 0

             (li :margin 0
                 :padding 0))

            (.recent-content-item
             :display flex
             :line-height 1.4
             :padding 0.3em 0
             :font-size 1.6em
             :align-items center
             :margin-bottom 0.4em)

            (".recent-content-item::before"
             :display block
             :background-repeat no-repeat
             :background-position 0 0
             :content ""
             :width 88px
             :height 48px
             :flex-shrink 0)

            (".content-type--blog::before"
             :background-image (url "/images/icons/post.svg"))
            (".content-type--talks::before"
             :background-image (url "/images/icons/talk.svg"))
            (".content-type--poems::before"
             :background-image (url "/images/icons/poems.svg")
             :background-size contain)

            (.read-more-btn :font-size 1.3em
                            :margin 0.7em 0)))))

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

(defun write-html-to-file (dest html &key clean-urls?)
  "Write HTML to DEST. If CLEAN-URL?, write as dest/index.html"
  (let ((dest (if (and clean-urls? (not (string= (ppath:basename dest) "index.html")))
                  (str:concat dest "/index.html")
                  dest))
        (dest-dirs (ppath:dirname dest)))
    (unless (or (string= dest-dirs ".") (str:emptyp dest-dirs))
      (ensure-directories-exist dest-dirs))
    (str:to-file dest html)))

(defun copy-dirs (src dest)
  (uiop:run-program (format nil "cp -r ~a ~a" (ppath:join src "*") dest)
                    :output *standard-output*
                    :error-output *standard-output*))

(defun build (dest &key clean-dest? clean-urls?)
  "Use slick publisher to build a publishable bundle to DEST"
  (let ((dest (if (str:ends-with? "/" dest) dest (str:concat dest "/")))
        (assets-dir "./assets/")
        (dest-assets-dir dest)
        (html (home-html)))
    (when (uiop:directory-exists-p "./dist/")
      (if (not clean-dest?)
          (error (format nil "Destination already exists. Aborting [dest=~a]" dest))
          (osicat:delete-directory-and-files dest)))
    (ensure-directories-exist dest)
    (ensure-directories-exist dest-assets-dir)
    (copy-dirs assets-dir dest-assets-dir)

    (write-html-to-file
     (ppath:join dest "index.html") html :clean-urls? clean-urls?)))
