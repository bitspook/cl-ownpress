(in-package #:default-theme)

(ps:import-macros-from-lisp 'op)
(defwidget oracle-nav-widget (spec realm)
  :styles `((.oracle-container
             :margin 2rem 0
             :margin-top 4rem

             (select
                 :border none
               :background white
               :margin 0 1rem
               :font-size 1.2rem
               :padding 0.4rem 0
               :border-bottom 2px solid ,(css-color :primary-text)
               :min-width 10rem)))
  :render
  (labels ((role-intents (role) (cdr (assoc role spec :test 'equal))))
    (let ((roles (mapcar #'first spec))
          (ps:*js-target-version* "7.0.0"))
      (with-html
        (:div.oracle-container
         :style "display:none;"
         (:span.placeholder "I am")
         (:select#roles :onchange (ps:ps (handle-select-role (ps:@ event target value)))
                        (dolist (role roles)
                          (:option :value role role)))

         (:span.placeholder "who wants to")

         (dolist (role roles)
           (:select.intents
            :data-role role
            :onchange (ps:ps (handle-select-intent (ps:@ event target value)))
            (dolist (intent (role-intents role))
              (:option :value (first intent) (first intent)))))

         (:script
          (:raw (ps:ps* ps:*ps-lisp-library*))
          (:raw (ps:ps* `(ps:var +spec+ (ps:[] ,@spec))))
          (:raw
           (ps:ps
             (defun is-array (obj)
               ((ps:chain -array is-array) obj))

             (defun display (el)
               (ps:@ el style display))

             (defun (setf display) (val el)
               (setf (ps:@ el style display) val))

             (defmacro dom-select (selector &optional (parent 'document))
               `((ps:chain ,parent query-selector) ,selector))

             (defmacro dom-select-all (selector &optional (parent 'document))
               `((ps:chain -array from) ((ps:chain ,parent query-selector-all) ,selector)))

             (defun first (list)
               (when (and list (> (ps:chain list length) 0))
                 (ps:@ list 0)))
             (ps:var car first)

             (defun cdr (list)
               (when (and list (> (ps:chain list length) 1))
                 ((ps:chain list slice) 1)))

             (defun cdadr (list)
               (cdr (car (cdr list))))

             (defun find-if (pred seq)
               ((ps:chain seq find) pred))

             (defun c-log (&rest args)
               (apply (ps:chain console log) args))

             (defun array-from (obj)
               ((ps:chain -array from) obj))

             (defun roles ()
               (loop :for item :in +spec+
                     :collect (first item)))

             (defun role-intents (role)
               (loop :for item :in +spec+
                     :when (equal (first item) role)
                       :return (cdr item)))

             (defun active-role ()
               (ps:@ ((ps:chain document get-element-by-id) "roles") value))

             (defun activate-role (role)
               (let ((els (dom-select-all ".intents")))
                 (dolist (el els) (setf (display el) "none")))
               (let ((intent-els (dom-select-all (+ ".intents[data-role='" role "']"))))
                 (dolist (el intent-els) (setf (display el) "inline-block"))))

             (defun activate-intent (intent-spec)
               (let* ((realm-el (dom-select (ps:lisp realm)))
                      (realm-children (array-from (ps:@ realm-el children))))
                 (dolist (el realm-children)
                   (setf (display el) "none"))

                 (dolist (sel (cdadr intent-spec))
                   (case sel
                     ("all" (dolist (el realm-children)
                              (setf (display el) "block")))
                     (t (dolist (el (dom-select-all sel realm-el))
                          (setf (display el) "block")))))))

             (defun handle-select-role (role)
               (activate-role role)
               (dom-reset-intents-el))

             (defun handle-select-intent (intent)
               (let ((intent-spec (find-if
                                   (op (equal (car _) intent))
                                   (role-intents (active-role)))))
                 (activate-intent intent-spec)))

             (defun dom-reset-select-el (selector)
               (let ((el (dom-select selector)))
                 (setf (ps:@ el selected-index) 0)
                 ((ps:@ el dispatch-event) (ps:new (-event "change")))))

             (defun dom-reset-intents-el ()
               (dom-reset-select-el (+ ".intents[data-role='" (active-role) "']")))

             (defun init ()
               (let ((els (dom-select-all ".oracle-container")))
                 (dolist (el els) (setf (display el) "flex")))
               ;; Timeout because this widget might render before the `realm' is
               ;; rendered in DOm
               (set-timeout
                (op
                  ;; We need to reset the select elements in DOM manually.
                  ;; Otherwise browser shows previously selected values but
                  ;; don't trigger 'onchange' event for the select element;
                  ;; putting Javascript code out of sync.
                  (dom-reset-select-el "#roles")
                  (dom-reset-intents-el))
                0))

             (init)))))))))
