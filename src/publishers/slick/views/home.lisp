(in-package :clown-slick.views)

;; Need these available at compile time for the home-html macro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter home-css
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

  (defparameter home-dom
    '(:div :class "home"
      (:div :class "sidebar"
       (:div :class "author"
        (:img :class "avatar" :src (conf :avatar) :alt (conf :author))
        (:h2 :class "name" (conf :author))
        (:p :class "handle"
         (:a :href (conf :twitter) "@" (conf :handle))))
       (:div :class "quote" "Math is the new sexy")
       (:div :class "social"
        (:a :href (conf :github)
            :title (str:concat (conf :author) " on Github")
            :target "_blank"
         (:span :class "github"))
        (:a :href (conf :twitter)
            :title (format nil "~a on Twitter" (conf :author))
            :target "_blank"
         (:span :class "twitter"))
        (:a :href (conf :linkedin)
            :title (format nil "~a on LinkedIn" (conf :author))
            :target "_blank" (:span :class "linkedin"))
        (:a :href "/feed.xml"
            :title "Follow via RSS"
            :target "_blank"
         (:span :class "rss")))
       (:img :class "pub-key-qr"
             :alt (format nil "~a's Public GPG Key" (conf :author))
             :src "/images/public-key-qr.svg"))
      (:div :class "main"
       (:section :class "about-me-snippet"
        (:header
         (:h2 "About Me"))
        (:p "I write software to make a living. I have been on voluntary unemployment since March 26, 2022.")
        (:p "I also enjoy writing, giving talks and discussing computers, security and politics.")
        (:p "This website has things I am willing to share publicly. You can go through my "
         (:a :href "/blog/" "blog") ", " (:a :href "/poems" "poems") ", " (:a :href (conf :github) :target "_blank" "projects") ", " (:a :href "/talks" "talks") ", and my " (:a :href (conf :resume) :target "_blank" "resume") " as well.")
        (:footer "More " (:a :href "/about" "about me.")))
       (:section :class "recent-content"
        (:header (:h2 "Recent Content"))
        (:ul :class "recent-content-list"
         (dolist (rp (publish-recent-posts 5))
           (with-slots ((href clown:output-path)) rp
             (:li (:a :href href
                      :class "recent-content-item content-type--blog"
                      (clown:post-title rp))))))
        (:footer (:a :class "btn btn-primary read-more-btn"
                     :href "/archive"
                  "See all")))))))

(defmacro home-html ()
  (let ((styles '(top-level-css
                  button-css
                  home-css)))
    `(html-str (:title ("~a's online home" (conf :author)) :css ,styles)
       ,home-dom)))
