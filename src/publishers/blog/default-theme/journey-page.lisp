(in-package #:default-theme)

(defwidget journey-page (journey related-posts)
  :styles
  (concatenate
   'list
   (top-level-css)
   `((nav.top-nav :padding 0)
     (footer.postamble :padding 0)
     (.container
      :max-width 1080px
      :margin 0 auto
      :color ,(css-color :primary-text)

      ("header.main"
       :margin 2rem 0

       (.title :font-size 2.5rem
               :margin 0
               :margin-bottom 0.4rem)

       (.subtitle :font-size 1.2rem
                  :color ,(css-color :dim-text)))

      ("article.main"
       :min-height 40rem
       :font-size 1.4rem

       (p :margin 1rem 0))

      ((:or .btn-github .btn-issues .btn-docs)
       :padding 0 1rem
       :font-size 1.2rem
       :color ,(css-color :secondary))

      (.icon :background-size contain
             :display inline-block
             :width 1.4rem
             :height 1.4rem
             :margin-right 0.4rem
             :background-repeat no-repeat)

      (.btn-github (.icon :background-image (url "/images/icons/github.svg")))

      (.notebook
       :padding 1rem
       :max-width 25rem
       :width 100%
       :display flex
       :margin 2rem 0
       :border-color ,(css-color :separator-light)

       (.icon :width 8rem
              :height 4rem)
       (.icon-nb :background-image (url "/images/icons/nb.svg"))

       (.title
        :margin 0
        :margin-left 0.8rem
        :text-align left

        (h2 :margin 0
            :margin-bottom 0.6rem
            :font-size 1.8rem))

       (.meta :color ,(css-color :dim-text)
              :font-size 1rem
              :line-height 1.2
              (.count :margin-right 0.3rem)))

      (.activity

       (.title
        :font-size 2rem
        :font-weight bold)

       (select :border none
         :background white
         :font-size 1.7rem
         :padding 0.4rem 0
         :border-bottom 2px solid ,(css-color :primary-text)
         :min-width 10rem))

      (.activity-log
       :font-size 1.2rem

       (.activity-item
        :padding 1rem 0
        :position relative
        :display flex

        (.icon-simple-cal
         :width 2rem
         :height 2rem
         :z-index 1
         :background ,(css-color :background-dark1) no-repeat
         :background-image (url "/images/icons/simple-cal.svg")
         :background-size 50%
         :background-position 50% 50%
         :margin-right 1rem
         :margin-left -1rem
         :border 2px solid ,(css-color :separator-light)
         :border-radius 50%))

       (.activity-body
        (.date
         :font-size 1rem
         :color ,(css-color :dim-text)
         :margin-bottom 0.8rem)
        (.events :list-style-type none
                 (li :margin-bottom 0.4rem))))

      (".activity-item::before"
       :width 2px
       :height 100%
       :content ""
       :position absolute
       :left 0 :top 0 :bottom 0
       :background-color ,(css-color :separator-light)))))
  :render
  (with-html
    (:doctype)
    (:html
     (:head (:title "infosec - @bitspook's journey")
            (:style (:raw (compile-and-write-lass-blocks (styles-of journey-page)))))
     (:body
      (render navbar-widget)
      (with-accessors
            ((name journey-name)
             (tagline journey-tagline)
             (html-description journey-html-description)
             (html-content journey-html-content))
          journey
        (:section.container
         (:header.main
          (:h1.title name)
          (when tagline (:p.subtitle tagline)))
         (:article.main
          (:raw html-description)

          (:button.btn.notebook
           (:i.icon.icon-nb)
           (:div.title
            (:h2 "Notebook")
            (:div.meta
             (:span.count "20")
             (:span "notes with tags #infosec #cryptography #ctf #wargame and 24 more"))))

          (:section.activity
           (:header.title (:span "Activity log for") (:select.log-time
                                                      (:option :value "last-week" "last week")
                                                      (:option :value "this-month" "this month")))
           (:article
            (:div.activity-graph
             :style "display: none;"
             (:img :src "/images/journey-example-punch-card.png"))

            (:div.activity-log
             (:div.activity-item
              (:span.icon.icon-simple-cal)
              (:div.activity-body
               (:div.date "Wednesday 15 Nov, 2022")
               (:ul.events
                (:li
                 (:a :href "#" "1 commit")
                 " made to project "
                 (:a :href "#" "slurp"))
                (:li
                 "Created "
                 (:a :href "#" "1 new note")))))
             (:div.activity-item
              (:span.icon.icon-simple-cal)
              (:div.activity-body
               (:div.date "Wednesday 16 Nov, 2022")
               (:ul.events
                (:li
                 (:a :href "#" "2 commits")
                 " made to project "
                 (:a :href "#" "cl-ownpress"))
                (:li
                 "Modified "
                 (:a :href "#" "4 notes"))
                (:li
                 "Created "
                 (:a :href "#" "2 new notes"))
                (:li
                 "Pwned "
                 (:a :href "#" "Supermo")
                 " box on "
                 (:a :href "#" "HackTheBox"))))))))

          (:raw html-content)

          (:div#explore
           (:h2.title "Related content")
           (render posts-listing-widget :posts related-posts)))
         (render footer-widget)))))))
