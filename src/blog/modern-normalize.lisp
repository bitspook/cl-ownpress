(in-package #:cl-ownblog)

;; https://github.com/sindresorhus/modern-normalize
(defparameter normalize-lass
  '(
    ((:or "*"
      "::before"
      "::after")
     :box-sizing border-box)

    (html
     :font-family "system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji'"
     :line-height 1.15
     :-webkit-text-size-adjust 100%
     :-moz-tab-size 4
     :tab-size 4)

    (body :margin 0)

    (hr :height 0
        :color inherit)

    ("abbr[title]" :text-decoration "underline dotted")

    ((:or b strong) :font-weight bolder)

    ((:or code kbd samp pre)
     :font-family ui-monospace "SFMono-Regular" "Consolas" "Liberation Mono" "Menlo" monospace
     :font-size 1em)

    (small :font-size 80%)

    ((:or sub sup)
     :font-size 75%
     :line-height 0
     :position relative
     :vertical-align baseline)

    ((:or sub sup) )

    (sub :bottom -0.25em)
    (sup :top -0.5em)

    (table :text-indent 0
           :border-color inherit)

    ((:or button input optgroup select textarea)
     :font-family inherit
     :font-size 100%
     :line-height 1.15
     :margin 0)

    ((:or button select) :text-transform none)

    ((:or button "[type='button']" "[type='reset']" "[type='submit']")
     :-webkit-appearance button)

    ("::-moz-focus-inner" :border-style none
                          :padding 0)

    (":-moz-focusring" :outline 1px dotted "ButtonText")

    (":-moz-ui-invalid" :box-shadow none)

    (legend :padding 0)
    (progress :vertical-align baseline)

    ((:or "::-webkit-inner-spin-button" "::-webkit-outer-spin-button")
     :height auto)

    ("[type='search']" :-webkit-appearance textfield
                       :outline-offset -2px)

    ("::-webkit-search-decoration" :-webkit-appearance none)

    ("::-webkit-file-upload-button" :-webkit-appearance button
                                    :font inherit)

    (summary :display list-item)))
