(defpackage :clown-slick
  (:use :cl :cl-ownpress.db
        :clown-slick.config
        :clown-slick.views.home)
  (:import-from :spinneret :with-html-string)
  (:export build))
(in-package :clown-slick)

(defun navbar-tree ()
  `(:nav :class "top-nav"
         (:div :class "brand"
               (:a :href "/"
                   (:img :class "brand-avatar"
                         :src (conf 'avatar)
                         :alt (conf 'author))))
         (:div
          (:ul (:li (:a :href "/blog" "Blog"))
               (:li (:a :href "/poems" "Poems"))
               (:li (:a :href (conf 'github) "Projects"))))))

(defun navbar-defs ()
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
    ,@(adjustable-width ".top-nav")))

(defun write-html-to-file (dest html &key clean-urls?)
  "Write HTML to DEST. If CLEAN-URL?, write as dest/index.html"
  (let ((dest (if (and clean-urls? (not (string= (ppath:basename dest) "index.html")))
                  (str:concat dest "/index.html")
                  dest))
        (dest-dirs (ppath:dirname dest)))
    (unless (or (string= dest-dirs ".") (str:emptyp dest-dirs))
      (ensure-directories-exist dest-dirs))
    (str:to-file dest html)))

(defun copy-dirs (src dest)
  (uiop:run-program (format nil "cp -r ~a ~a" (ppath:join src "*") dest)
                    :output *standard-output*
                    :error-output *standard-output*))

(defun build (dest &key clean-dest? clean-urls?)
  "Use slick publisher to build a publishable bundle to DEST"
  (let ((dest (if (str:ends-with? "/" dest) dest (str:concat dest "/")))
        (assets-dir "./assets/")
        (dest-assets-dir dest)
        (html (home-html)))
    (when (uiop:directory-exists-p dest)
      (if (not clean-dest?)
          (error (format nil "Destination already exists. Aborting [dest=~a]" dest))
          (osicat:delete-directory-and-files dest)))
    (ensure-directories-exist dest)
    (ensure-directories-exist dest-assets-dir)
    (copy-dirs assets-dir dest-assets-dir)

    (write-html-to-file
     (ppath:join dest "index.html") html :clean-urls? clean-urls?)))
