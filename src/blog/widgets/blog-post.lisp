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

(defun bp-lass ()
  (tagged-lass
   normalize-lass
   font-lass

   `((":root" ,@pollen-vars))

   `((body :font-size 16px
           :font-family "Fira Sans")

     (h1 :font-family "Alfa Slab One"))))

(defwidget blog-post-w (post)
    (bp-lass)
  (with-slots (title created-at tags body) post
    (:div
     (render 'navbar-w :links '(("Home" "/blog/home")
                                ("Blog" "/blog/home")
                                ("Poems" "/blog/home")
                                ("Projects" "/blog/home")
                                ("Experiments" "/blog/home")
                                ("About me" "/blog/home")))
     (:section
      :class "content"
      (:header
       :class "content-header"
       (:h1 title)
       (:span
        :class "content-meta"
        (:span
         :class "meta-item date"
         (local-time:format-timestring
          nil created-at
          :format '(:long-month " " :day ", " :year)))
        (when-let ((tags tags))
          (:ul
           :class "meta-item tags"
           (dolist (tag tags)
             (:li.tag
              (:a :href
                  (str:concat "/tags/" tag)
                  (str:concat "#" (str:downcase tag)))))))))
      (:article :class "main-article"
                (:raw body)))
     (render 'footer-w :post post))))
