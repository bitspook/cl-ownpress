(in-package #:cl-ownblog)

(defparameter font-lass
  '((@font-face
     :font-family "Alfa Slab One"
     :src "url('/fonts/alfaslabone-regular-webfont.woff2') format('woff2'), url('/fonts/alfaslabone-regular-webfont.woff') format('woff')"
     :font-weight normal
     :font-style normal)

    (@font-face
     :font-family "Fira Sans"
     :src "url('/fonts/firasans-bold-webfont.woff2') format('woff2'), url('/fonts/firasans-bold-webfont.woff') format('woff')"
     :font-weight bold
     :font-style bold)

    (@font-face
     :font-family "Fira Sans"
     :src "url('/fonts/firasans-heavy-webfont.woff2') format('woff2'), url('/fonts/firasans-heavy-webfont.woff') format('woff')"
     :font-weight 900
     :font-style bold)

    (@font-face
     :font-family "Fira Sans"
     :src "url('/fonts/firasans-heavyitalic-webfont.woff2') format('woff2'), url('/fonts/firasans-heavyitalic-webfont.woff') format('woff')"
     :font-weight bold
     :font-style italic)

    (@font-face
     :font-family "Fira Sans"
     :src "url('/fonts/firasans-italic-webfont.woff2') format('woff2'), url('/fonts/firasans-italic-webfont.woff') format('woff')"
     :font-weight normal
     :font-style italic)

    (@font-face
     :font-family "Fira Sans"
     :src "url('/fonts/firasans-regular-webfont.woff2') format('woff2'), url('/fonts/firasans-regular-webfont.woff') format('woff')"
     :font-weight normal
     :font-style normal)

    (@font-face
     :font-family "Fira Sans"
     :src "url('/fonts/firasans-thin-webfont.woff2') format('woff2'), url('/fonts/firasans-thin-webfont.woff') format('woff')"
     :font-weight 300
     :font-style normal)

    (@font-face
     :font-family "Fira Sans"
     :src "url('/fonts/firasans-thinitalic-webfont.woff2') format('woff2'), url('/fonts/firasans-thinitalic-webfont.woff') format('woff')"
     :font-weight 300
     :font-style italic)))

(defparameter global-css-vars
  '(:--font-sans "Fira Sans"
    :--font-title "Alfa Slab One"))

(defparameter base-lass
  (tagged-lass
   normalize-lass
   font-lass

   `((":root" ,@pollen-vars
              ,@global-css-vars))

   `((body :font-size 16px
           :font-family (var --font-sans)
           :color (var --color-grey-900))

     (h1 :font-family (var --font-title)
         :margin 0
         :padding 0
         :letter-spacing 2px)

     (a :color (var --color-grey))

     (img :width 100%))

   :dark `((body :color (var --color-grey-300)
                 :background (var --color-grey-900)))))
