(in-package :default-theme)

(defwidget newsletter-widget ()
  :styles `(,@(adjustable-width-css ".postamble")
              ,@(button-css)
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
                                 :padding 0.4rem 0.8rem))
  :render
  (with-html
    (:div#mc_embed_signup
     (:form :action "https://bitspook.us14.list-manage.com/subscribe/post?u=de25614414d7e23ac4c3ea700&id=b8b47d5b6e"
            :method "post"
            :id  "mc-embedded-subscribe-form"
            :name "mc-embedded-subscribe-form"
            :class "validate"
            :target "_blank"

            (:div#mc_embed_signup_scroll
             (:h2 "Get updates via email")
             (:div.mc-field-group
              (:input :type "email" :value ""
                      :placeholder "Email Address"
                      :name "EMAIL"
                      :class "required email newsletter-email"
                      :id "mce-EMAIL"))
             (:div :style "position: absolute; left: -5000px;"
                   :aria-hidden "true"
                   (:input :type "text"
                           :name "b_de25614414d7e23ac4c3ea700_b8b47d5b6e"
                           :tabindex "-1"
                           :value ""))
             (:div#mce-responses
              :class "clear foot"
              (:div :class "response"
                    :id "mce-error-response"
                    :style "display: none")
              (:div :class "response"
                    :id "mce-success-response"
                    :style "display: none"))
             (:div :class "optionalParent"
                   (:div :class "clear foot"
                         (:input :type "submit"
                                 :value "Subscribe"
                                 :name "subscribe"
                                 :id "mc-embedded-subscribe"
                                 :class "btn btn-primary"))))))))

(defwidget footer-widget ()
  :styles
  `((.rss-sub
     :margin 2rem 0
     (a :display flex
        :align-items center
        (.rss :width 24px
              :height 24px
              :margin-right 1rem
              :background "url(\"/images/icons/rss.svg\")"
              :background-repeat no-repeat
              :background-size contain))))
  :render
  (with-html
    (:footer.site-footer.postamble
     (render newsletter-widget)
     (:p.rss-sub
      (:a :href "/archive/feed.xml"
          :title "Follow via RSS"
          :target "blank"
          (:span.rss) "Or Follow via RSS"))
     (:p ("Author: ~a " (conf :author))
         (:a :href (conf :mastodon) ("@~a" (conf :handle)))))
    (render mixpanel-widget)))
