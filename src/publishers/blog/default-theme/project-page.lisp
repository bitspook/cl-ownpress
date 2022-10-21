(in-package #:default-theme)

(defwidget project-page (project related-posts)
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
    (:doctype)
    (:html
     (:head (:title "Spookfox - @bitspook's project")
            (:style (:raw (compile-and-write-lass-blocks (styles-of project-page)))))
     (:body
      (render navbar-widget)
      (with-accessors ((name project-name)
                       (tagline project-tagline)
                       (html-description project-html-description)
                       (html-content project-html-content)
                       (source-code project-source-code)
                       (issue-tracker project-issue-tracker)
                       (docs project-docs)
                       (oracle-spec project-oracle-spec)) project
        (:section.container
         (:header.main
          (:h1.title name)
          (when tagline (:p.subtitle tagline)))
         (:article.main
          (:raw html-description)
          (:ul.elsewhere-buttons
           (when source-code (:li (:a.btn.btn-github :href source-code (:i.icon) (:span "Source Code"))))
           (when issue-tracker (:li (:a.btn.btn-issues :href issue-tracker (:i.icon) (:span "Issue tracker"))))
           (when docs (:li (:a.btn.btn-docs :href docs (:i.icon) (:span "Documentation")))))

          (render oracle-nav-widget :spec oracle-spec :realm "#oracle-realm")

          (:div#oracle-realm
           (dolist (section (html-sections html-content "outline-2"))
             (:raw section))

           (:div#explore
            (:h2.title "Related content")
            (render posts-listing-widget :posts related-posts))))
         (render footer-widget)))))))
