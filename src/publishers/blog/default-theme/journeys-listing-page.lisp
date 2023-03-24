(in-package #:default-theme)

(defwidget journeys-listing-page (journeys title description)
  :styles
  (concatenate
   'list
   (top-level-css)
   `((nav.top-nav :padding 0)
     (footer.postamble :padding 0)
     (.main
      :position relative
      :margin 0 auto
      :max-width 1080px
      :min-height 40rem
      :font-family ,(css-var :content-font-family)
      :font-size 1.4rem

      (p :margin 1rem 0)

      (:header :font-family ,(css-var :title-font-family))
      (.title :font-size 2.5rem)

      (.main-list :list-style-type none
                  :margin 4rem 0)

      (.journey
       :margin 1rem 0
       :padding 1rem
       :border 1px solid ,(css-color :separator-light)
       :border-radius 1rem
       :display flex

       (.primary :flex-grow 1)
       (.health :width 15rem
                :height 15rem
                :margin 0 2rem)

       (.activity :list-style-type none
                  :font-size 1rem
                  :color ,(css-color :dim-text)
                  :padding 0.4rem 1rem)

       (.grade :border 1rem solid ,(css-color :success)
               :color ,(css-color :success)
               :border-radius 50%
               :height 10rem
               :width 10rem
               :display flex
               :align-items center
               :justify-content center
               :font-weight bold
               :font-size 4rem)

       (header
        :margin-bottom 2rem
        :font-family ,(css-var :content-font-family)

        (.title :font-size 2rem
                :margin 0
                :margin-bottom 0.4rem)

        (.subtitle :font-size 1.4rem
                   :color ,(css-color :dim-text)))

       (footer :color ,(css-color :dim-text)
               :margin-top 1rem)))))
  :render
  (with-html
    (:html
     (:head
      (:title title)
      (:meta :name "viewport" :content  "width=device-width, initial-scale=1")
      (:style (:raw (compile-and-write-lass-blocks (styles-of journeys-listing-page)))))
     (:body
      (render navbar-widget)
      (:article.main
       (:h1.title title)
       (:p "A journey is a commitment to move forward. Sometimes to reach a
            destination/goal, and sometimes just for the sake of movement.")

       (:ul.main-list
        (:li.journey
         (:section.primary
          (:header (:h2.title (:a :href "/journeys/infosec" "Infosec"))
                   (:p.subtitle "Information security from attacker's perspective"))
          (:article (:p "I have desired for long to explore infosec in depth. Finally, I have decided to
                        commit and invest in it.")))
         (:section.health
          (:div.grade (:span "A+"))
          (:ul.activity
           (:li "Very good health")
           (:li "7 day streak")
           (:li "0 companions")))))

       (:div.description
        (or description
            (:p "Criteria I use to decide what qualifies as a journey:")
            (:ol
             (:li (:p "An objective which need a long term commitment")
                  (:p "A journey is more than just exploring a topic for a few days/weeks. A
        journey starts when I have done the exploration and am ready to make a
        commitment."))
             (:li (:p "A feasible method of measuring progress")
                  (:p "It is really a wish if we can't track whether any progress is being made on
        the journey or not."))))))
      (render footer-widget)))))
