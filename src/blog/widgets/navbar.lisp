(in-package #:cl-ownblog)

(defwidget navbar-w () nil
  (:nav :class "top-nav"
        (:div :class "brand"
              (:a :href "/"
                  (:img :class "brand-avatar"
                        :src "/images/avatar.png"
                        :alt "Brand")))
        (:div
         (:ul (:li (:a :href "/blog" "Blog"))
              (:li (:a :href "/poems" "Poems"))
              (:li (:a :href "/projects" "Projects"))))))

(defmethod lass-of ((w navbar-w))
  (tagged-lass
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
             :font-family Roboto sans-serif
             :color "#000")

          ("a:hover" :color "#444"))

      ("li:last-child" :padding 0)

      (.brand :width 60px
              :height 60px
              :align-self center
              :flex-grow 1)

      (.brand-avatar :height 100%)))

   :sm `((.top-nav :padding-right 1em
                   (a :font-size 1.4em)))

   :dark `((.top-nav (ul (li (a :color red)))))))
