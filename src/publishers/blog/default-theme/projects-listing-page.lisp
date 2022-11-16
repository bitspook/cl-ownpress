(in-package #:default-theme)

(defwidget projects-listing-page (projects title description)
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
      (:title title)
      (:meta :name "viewport" :content  "width=device-width, initial-scale=1")
      (:style (:raw (compile-and-write-lass-blocks (styles-of projects-listing-page)))))
     (:body
      (render navbar-widget)
      (:article.main
       (:h1.title title)
       (:div.description
        (or description
            (:p "Listed on this page are projects which I find most interesting, want to
             showcase, or are close to my heart. You can find a complete list of
             all my open source work on " (:a :href (conf :github) "my github profile") ".")))
       (:ul.projects-list
        (dolist (project projects)
          (:li.project
           (:header (:h2.project-title (:a :href (project-public-path project) (project-name project)))
                    (when (project-tagline project) (:p.subtitle (project-tagline project)))
                    (:div.languages
                     (dolist (lang (project-languages project))
                       (:span.lang
                        (:i :style (format nil "background: ~a" (gethash lang +gh-lang-colors+)))
                        (:span.lang-name lang)))))
           (:article (:raw (project-html-description project)))
           (:footer
            (when (project-updated-at project)
              (:div.last-update "Last updated on " (project-updated-at project))))))))
      (render footer-widget)))))
