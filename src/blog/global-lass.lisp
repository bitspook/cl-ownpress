(in-package #:cl-ownblog)

(defparameter font-lass
  '((@font-face
     :font-family "Fira Sans"
     :src "url('/fonts/firasans-thin-webfont.woff2') format('woff2'), url('/fonts/firasans-thin-webfont.woff') format('woff')"
     :font-weight 300
     :font-style normal)

    (@font-face
     :font-family "Fira Sans"
     :src "url('/fonts/firasans-thinitalic-webfont.woff2') format('woff2'), url('/fonts/firasans-thinitalic-webfont.woff') format('woff')"
     :font-weight 300
     :font-style italic)

    (@font-face
     :font-family "Alfa Slab One"
     :src "url('/fonts/alfaslabone-regular-webfont.woff2') format('woff2'), url('/fonts/alfaslabone-regular-webfont.woff') format('woff')"
     :font-weight 400
     :font-style normal)

    (@font-face
     :font-family "Fira Sans"
     :src "url('/fonts/firasans-bold-webfont.woff2') format('woff2'), url('/fonts/firasans-bold-webfont.woff') format('woff')"
     :font-weight 700
     :font-style bold)

    (@font-face
     :font-family "Fira Sans"
     :src "url('/fonts/firasans-heavy-webfont.woff2') format('woff2'), url('/fonts/firasans-heavy-webfont.woff') format('woff')"
     :font-weight 800
     :font-style bold)

    (@font-face
     :font-family "Fira Sans"
     :src "url('/fonts/firasans-heavyitalic-webfont.woff2') format('woff2'), url('/fonts/firasans-heavyitalic-webfont.woff') format('woff')"
     :font-weight 700
     :font-style italic)

    (@font-face
     :font-family "Fira Sans"
     :src "url('/fonts/firasans-italic-webfont.woff2') format('woff2'), url('/fonts/firasans-italic-webfont.woff') format('woff')"
     :font-weight 400
     :font-style italic)

    (@font-face
     :font-family "Fira Sans"
     :src "url('/fonts/firasans-regular-webfont.woff2') format('woff2'), url('/fonts/firasans-regular-webfont.woff') format('woff')"
     :font-weight 400
     :font-style normal)))

(defparameter global-css-vars
  '(:--font-sans "Fira Sans"
    :--font-title "Alfa Slab One"))

(defparameter base-lass
  (tagged-lass
   `((":root" ,@pollen-vars
              ,@global-css-vars))

   normalize-lass
   font-lass

   `((body :font-size 16px
           :font-family (var --font-sans)
           :color (var --color-grey-900))

     ((:or h1 h2 h3 h4 h5 h6)
      :margin 0
      :padding 0
      :font-size (var --scale-1)
      :font-weight (var --weight-black)
      :letter-spacing 2px)

     ((:or blockquote dl figure form ol p pre table ul)
      :margin-bottom (var --scale-2))

     ((:or td th)
      :padding 12px 15px
      :text-align left
      :border-bottom 1px solid (var --color-grey-300))

     (h1 :font-size (var --scale-5))
     (h2 :font-size (var --scale-4))
     (h3 :font-size (var --scale-3))
     (h4 :font-size (var --scale-2))
     (h5 :font-size (var --scale-1))

     (h6 :font-size (var --scale-0))

     (a :color (var --color-grey-800))

     (img :width 100%)

     (blockquote
      :margin (var --size-2) 0
      :padding 0 (var --scale-2)
      :border-left (var --size-2) solid (var --color-grey-300))

     ((:or code kbd samp pre)
      :font-size (var --scale-0)
      :padding (var --size-1)
      :font-family (var --font-mono)
      :background-color (var --color-grey-200)
      :border 1px solid (var --color-grey-300)
      :border-radius (var --radius-xs)))

   :dark `((body :color (var --color-grey-300)
                 :background (var --color-grey-900))

           (a :color (var --color-grey-300))

           ((:or code kbd samp pre)
            :background-color (var --color-grey-800)
            :border 1px solid (var --color-grey-900)
            :border-radius (var --radius-xs))

           ((:or input textarea)
            :background-color (var --color-grey-700)
            :color inherit))))
