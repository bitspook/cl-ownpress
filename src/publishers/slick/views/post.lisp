(in-package :clown-slick.views)

(defun post-css ()
  `(,@(navbar-css)
    (.content :font-family "Cantarell, sans-serif"
              :font-style normal
              :font-weight 500
              :font-size 1.5rem
              :line-height 1.4
              :max-width 70%
              :margin 0 auto
              :padding 0 124px)
    (.content-header (h1 :margin-bottom 1rem))
    (.content-meta :margin-top 0
                   :margin-bottom 2rem
                   :font-family "Roboto, sans-serif"
                   :color ,(css-color :secondary)
                   :display flex

                   (.meta-item :border-right ,(format nil "1px solid ~a" (css-color :separator))
                               :line-height 1
                               :padding 0 1rem
                               :font-size 0.8rem
                               :display flex)
                   ((:and .meta-item :last-child) :padding-right 0
                                                  :border none)
                   (.date :padding 0
                          :padding-right 1rem)

                   (.tags (ul :list-style-type none
                              :display flex
                              :flex-wrap wrap)
                          (li :padding-right 1rem)))
    (.main-article :font-family "Cantarell, sans-serif"
                   :min-height 500px

                   ((:or strong b) :font-weight bold)
                   (p :font-family inherit
                      :font-size inherit
                      :margin 1rem 0)
                   (h3 :margin 0.4rem 0)
                   (a :text-decoration underline)
                   (ul :margin-left 1.4rem)
                   (li :margin 0.7rem
                       :margin-right 0))))

(defun post-dom (post)
  (with-slots ((title clown:title)) post
    `(:div ,(navbar-dom)
           (:section :class "content"
                     (:header :class "content-header"
                              (:h1 ,title)
                              (:span :class "content-meta"
                                     (:span :class "meta-item date"
                                            "Jan 01, 1992")
                                     (:span :class "meta-item tags"
                                            (:ul (:li :class "tag" "Emacs")))))
                     (:article :class "main-article"
                               "Lol bro what's up?")))))

(defun post-html (post)
  (let ((title (slot-value post 'clown:title))
        (styles (list (font-defs)
                      (top-level-defs)
                      (post-css)))
        (dom (post-dom post)))
    (html-str :title title :cssom styles :dom dom)))
