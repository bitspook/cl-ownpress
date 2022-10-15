(in-package #:default-theme)

(defwidget projects-widget ()
  :styles (concatenate
           'list
           (top-level-css)
           (footer-css)
           `((.top-nav :padding 0)
             (.postamble :padding 0)
             (.main
              :position relative
              :margin 0 auto
              :max-width 1080px
              :font-family ,(css-var :content-font-family)

              (:header :font-family ,(css-var :title-font-family))
              (.title :font-size 2.5rem)
              (.description :font-size 1.4rem)

              (.projects-list :list-style-type none
                              :margin 4rem 0
                              :min-height 40rem)

              (.project
               :margin 1rem 0
               :padding 1rem
               :border 1px solid ,(css-color :separator-light)
               :border-radius 1rem

               (header
                :margin-bottom 2rem
                :font-family ,(css-var :content-font-family)

                (.project-title :font-size 2rem
                                :margin 0
                                :margin-bottom 0.4rem)
                (.subtitle :font-size 1.2rem
                           :color ,(css-color :dim-text))

                (.languages :width 100%
                            :margin-top 1.4rem
                            :margin-bottom 1rem

                            (.lang :display inline-flex
                                   :align-items center
                                   :margin-right 0.8rem)

                            (i :border-radius 80%
                               :width 0.8rem
                               :height 0.8rem
                               :margin 0 0.4rem)))

               (article
                :font-size 1.4rem
                (p :margin 1rem 0))

               (footer :color ,(css-color :dim-text)
                       :margin-top 1rem)))))
  :render
  (with-html
    (:html
     (:head
      (:title "@bitspook's projects")
      (:style (:raw (compile-and-write-lass-blocks (styles-of projects-widget)))))
     (:body
      (navbar-dom)
      (:article.main
       (:h1.title "Featured Projects")
       (:div.description "Listed on this page are projects which I find most interesting, want to
showcase, or are close to my heart. You can find a complete list of all my open
source work on " (:a :href (conf :github) "my github profile") ".")
       (:ul.projects-list
        (:li.project
         (:header (:h2.project-title (:a :href "/projects/spookfox" "Spookfox"))
                  (:p.subtitle "Tinkerer's bridge between Emacs and Firefox.")
                  (:div.languages
                   (:span.lang.emacs-lisp
                    (:i :style "background: #952994")
                    (:span.lang-name "Emacs Lisp"))
                   (:span.lang.typescript
                    (:i :style "background-color: #452134")
                    (:span.lang-name "Typescript"))))
         (:article
          (:p "Spookfox is a Firefox extension and an Emacs package, which allow Emacs and
Firefox to communicate with each other. Its primary goal is to offer an Emacs
tinkerer similar (to Emacs) framework to tinker their browser.")
          (:p "I use Spookfox as my daily driver to enable a number of workflow enhancements,
e.g capturing articles I read and Youtube videos I watch, and also to organize
hundreds tabs using org-mode."))
         (:footer
          (:div.last-update "Last updated on Oct 12, 2022")))))
      (footer-dom)))))
