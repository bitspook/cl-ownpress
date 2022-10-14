(in-package #:default-theme)

(defwidget oracle-nav ()
  :styles `((.oracle-container
             :margin 2rem 0

             (select
                 :border none
               :background white
               :margin 0 1rem
               :font-size 1.2rem
               :padding 0.4rem 0
               :border-bottom 2px solid ,(css-color :primary-text)
               :min-width 10rem)))
  :render
  (with-html
    (:div.oracle-container
     :style "display:none;"
     (:span.placeholder "I am")
     (:select :onchange (ps:ps (handle-select-role (ps:@ event target value)))
       (:option :value "universal" "one with the universe")
       (:option :value "developer" "a developer")
       (:option :value "surfer" "web surfer"))
     (:span#developer-placeholder.placeholder "who wants to")

     (:span#surfer-placeholder.placeholder "who wants to")
     (:select#surfer-intent
      :onchange (ps:ps (handle-select-intent (ps:@ event target value)))
      (:option :value "know-more" "know more about Spookfox"))

     (:select#developer-intent
      :onchange (ps:ps (handle-select-intent (ps:@ event target value)))
      (:option :value "know-more" "know more about Spookfox")
      (:option :value "use" "use Spookfox")
      (:option :value "develop" "develop Spookfox")
      (:option :value "hack" "hack with Spookfox")
      (:option :value "contribute" " contribute to Spookfox"))
     (:script
      (:raw
       (ps:ps
         (defun dom-el (selector &optional select-all)
           ((ps:@ document
                  (if select-all
                      'query-selector-all
                      'query-selector))
            selector))
         (defun display (el)
           (ps:@ el style display))
         (defun (setf display) (val el)
           (setf (ps:@ el style display) val))

         (defmacro with-elements (selector-forms &body body)
           `(let ,(loop :for form :in selector-forms
                        :collect `(,(first form) (dom-el ,(second form))))
              ,@body))
         (defmacro with-all-elements (selector-forms &body body)
           `(with-elements ,selector-forms
              (when (and ,@(mapcar #'first selector-forms)) ,@body)))

         (defvar +roles+ '(developer surfer universal))
         (defvar +intents+ '(universal use develop contribute explore feature-request bug-report))
         (defvar +role-intents+
           (ps:create
            surfer '(explore contribute)
            universal '(universal)
            developer  '(use develop contribute bug-report feature-request)))
         (defparameter *state* (ps:create))

         (defun activate-role (role)
           (dolist (r +roles+)
             (with-all-elements ((el (+ "#" r "-intent"))) (setf (display el) "none"))
             (with-all-elements ((el (+ "#" r "-placeholder"))) (setf (display el) "none")))
           (with-elements ((intent-el (+ "#" role "-intent"))
                           (ph-el (+ "#" role "-placeholder")))
             (when intent-el (setf (display intent-el) "inline-block"))
             (when ph-el (setf (display ph-el) "inline-block")))
           (setf (ps:@ *state* "active-role") role))

         (defun active-intents ()
           (ps:@ +role-intents+ (ps:@ *state* "active-role")))

         (defun activate-intent (intent)
           (dolist (i +intents+)
             (with-all-elements ((el (+ "#" i)))
               (setf (display el) "none"))
             (dolist (i (active-intents))
               (with-all-elements ((el (+ "#" i)))
                 (setf (display el) "block")))))

         (defun handle-select-role (role)
           (activate-role role)
           (activate-intent (ps:@ (ps:getprop +role-intents+ role) 0)))

         (defun handle-select-intent (intent)
           (activate-intent intent))

         (defun init ()
           (with-all-elements ((el ".oracle-container"))
             (setf (display el) "flex"))
           (activate-role 'surfer))

         (init)))))))
