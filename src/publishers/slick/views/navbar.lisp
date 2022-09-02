(in-package :clown-slick.views)

(defun navbar-dom ()
  `(:nav :class "top-nav"
         (:div :class "brand"
               (:a :href "/"
                   (:img :class "brand-avatar"
                         :src ,(conf :avatar)
                         :alt ,(conf :author))))
         (:div
          (:ul (:li (:a :href "/blog" "Blog"))
               (:li (:a :href "/poems" "Poems"))
               (:li (:a :href ,(conf :github) "Projects"))))))

(defun navbar-css ()
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
    ,@(clown-slick:adjustable-width-css ".top-nav")))
