(defpackage com.example.website
  (:use #:cl #:serapeum/bundle)
  (:import-from #:spinneret :with-html)
  (:import-from #:in.bitspook.cl-ownpress :css-file-artifact)
  (:local-nicknames (:clown #:in.bitspook.cl-ownpress)))

(in-package #:com.example.website)

(clown:defwidget navbar-w (links)
  (clown:tagged-lass
   `((.top-nav
      :min-height (var --size-16)
      :max-width (var --width-lg)
      :margin 0 auto
      :display flex
      :padding (var --size-2)
      :flex-direction column
      :position relative
      :justify-content space-between

      (.brand
       :width (var --size-16)
       :height (var --size-16)
       :align-self center
       :flex-grow 1

       (img :width (var --size-16)))

      (ul.nav
       :list-style-type none
       :padding 0
       :display flex
       :flex-wrap wrap
       :justify-content center
       :align-items center

       (li :padding 0 (var --size-4) (var --size-4)))))

   :sm  `((.top-nav
           :padding-right 1em
           :flex-direction row

           (a :font-size 1em)

           (ul.nav (li :padding 0 (var --size-4))))))

  (let ((links (or links '(("Home" "/")
                           ("Blog" "/blog")
                           ("Projects" "/projects")
                           ("About me" "/about")))))
    (:nav :class "top-nav"
          (:div :class "brand" (:a :href "/" "Home"))
          (:ul :class "nav"
               (loop :for link :in links
                     :do (:li (:a :href (second link) (first link))))))))

(clown:defwidget home-page-w (title css-file-artifact)
  (clown:tagged-lass
   `((.home
      :display flex
      :flex-direction column

      (.main
       :width 100%
       :position relative
       :padding 20rem)

      (.header
       :padding 1rem
       :margin 1rem 0)

      (.title :font-family "monospace"
              :font-size 4rem
              :padding-bottom 2rem
              :border-bottom 1px solid "#222"))))
  (:doctype)
  (:html
   (:head (:title title)
          (:meta :name "viewport" :content "width=device-width, initial-scale=1")
          (:link :rel "stylesheet" :href (clown:embed-artifact-as css-file-artifact 'clown:link)))
   (:body
    (clown:render 'navbar-w :links nil)
    (:article.home
     (:div
      :class "main" (:h1 title))))))

(defparameter *page* (clown:make-html-page-artifact
                      :location "/index.html"
                      :css-location "/css/home.css"
                      :root-widget (make 'home-page-w
                                         :title "Hello world!")))

(let ((clown:*already-published-artifacts* nil)
      (www "/tmp/build"))
  (clown:publish-artifact *page* www))
