(in-package #:default-theme)

(defwidget project-widget (project)
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

      (.elsewhere-buttons
       :list-style-type none
       :display flex
       :margin-top 3rem

       (li :margin-right 1.4rem))

      ((:or .btn-github .btn-issues .btn-docs)
       :padding 0 1rem
       :font-size 1.2rem
       :color ,(css-color :secondary)

       (.icon :background-size contain
              :display inline-block
              :width 1.4rem
              :height 1.4rem
              :margin-right 0.4rem))

      (.btn-github (.icon :background-image (url "/images/icons/github.svg")))
      (.btn-issues (.icon :background-image (url "/images/icons/issues.svg")))
      (.btn-docs (.icon :background-image (url "/images/icons/docs.svg"))))))
  :render
  (with-html
    (:html
     (:head (:title "Spookfox - @bitspook's project")
            (:style (:raw (compile-and-write-lass-blocks (styles-of project-widget)))))
     (:body
      (render navbar-widget)
      (:section.container
       (:header.main
        (:h1.title "Spookfox")
        (:p.subtitle "Tinkerer's bridge between Emacs to Firefox"))
       (:article.main
        (:p "Spookfox is a Firefox extension and an Emacs package, which allow Emacs and
             Firefox to communicate with each other. Its primary goal is to
             offer an Emacs tinkerer similar (to Emacs) framework to tinker
             their browser.")
        (:p "I use Spookfox as my daily driver to enable a number of workflow enhancements,
             e.g capturing articles I read and Youtube videos I watch, and also
             to organize hundreds tabs using org-mode.")
        (:ul.elsewhere-buttons
         (:li (:a.btn.btn-github :href (conf :github) (:i.icon) (:span "Source Code")))
         (:li (:a.btn.btn-issues :href (conf :github) (:i.icon) (:span "Issue tracker")))
         (:li (:a.btn.btn-docs :href (conf :github) (:i.icon) (:span "Documentation"))))

        (:div#oracle-nav
         (render oracle-nav-widget))

        (:div#use
         (:h2.title "Usage")
         (:div "Use this in emacs. Use this in Firefox." ))

        (:div#explore
         (:h2.title "Related blog posts")
         (render posts-listing-widget)))
       (render footer-widget))))))
