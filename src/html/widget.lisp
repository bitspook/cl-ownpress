(in-package #:in.bitspook.cl-ownpress)

(export-always 'widget)
(defclass widget (artifact) nil
  (:documentation "A widget produces fragments of a web page. You can think of them as Web components.
A widget is made up of spinneret Dom forms and lass Css forms. Use `defwidget' macro to create a new
widget."))

(defun widgetp (w)
  (subtypep (type-of w) 'widget))

(export-always 'dom-of)
(defgeneric dom-of (widget)
  (:documentation "Provide spinneret DOM for the WIDGET.")
  (:method ((widget widget)) nil))

(export-always 'lass-of)
(defgeneric lass-of (widget)
  (:documentation "Provide a list of lass blocks for WIDGET")
  (:method ((widget widget)) nil))

(export-always 'css-of)
(defgeneric css-of (widget)
  (:documentation "Return css string for WIDGET. Uses LASS-OF internally.")
  (:method ((widget widget))
    (let ((lass:*pretty* *print-pretty*))
      (lass:write-sheet
       (apply #'lass:compile-sheet (lass-of widget))))))

(export-always '*self*)
(defvar *self* nil
  "*self* is a hack. Its purpose is to identify the current widget being built in `defwidget'.
You can use `render' in `defwidget' and never need *SELF*.")

(export-always 'defwidget)
(defmacro defwidget (name args lass &body dom)
  "Create a widget (instance of `widget') named NAME.
ARGS is the arguments received by `dom-of' and `lass-of' functions.
*self* is set to current widget in dom-of/lass-of methods."
  `(progn
     (defclass ,name (widget)
       ,(mapcar
         (lambda (arg) `(,arg :initarg ,(make-keyword arg)))
         args))

     (defmethod dom-of ((self ,name))
       (let ((*self* self))
         (with-slots ,args self
           (spinneret:with-html ,@dom))))

     (defmethod lass-of ((widget ,name))
       (let ((*self* widget))
         (with-slots ,args widget
           ,lass)))

     ',name))

(export-always 'render)
(defmacro render (widget &rest args)
  "A convenient macro to provide single API to consume WIDGET.
Enable consuming WIDGET in following ways:
1. Add WIDGET as a child of another widget
  1. Add WIDGET as dependency of *self*
  2. Instantiate WIDGET if it is a symbol
2. Directly obtain WIDGET's dom."
  (with-gensyms (instance)
    `(let ((,instance (if (eq 'symbol (class-name-of ,widget))
                          (make-instance ,widget ,@args)
                          ,widget)))
       (when *self* (add-dep *self* ,instance))
       (dom-of ,instance))))

(export-always 'rendered-css)
(defun rendered-css (widget)
  "Return CSS rendered for WIDGET.
Make sure widget has been `render'ed. A WIDGET's dependencies are resolved when it is `render'ed."
  (let* ((lass:*pretty* *print-pretty*)
         (all-deps (append (list widget) (all-deps widget)))
         (css-deps (remove-if-not #'widgetp all-deps))
         (css-fragments (mapcar #'css-of (append1 (nreverse css-deps) widget)))
         (unique-css-fragments (nreverse (remove-duplicates css-fragments :test #'equal))))
    (str:join (if *print-pretty* #\NewLine "") unique-css-fragments)))


(export-always '*lass-tags*)
(defparameter *lass-tags*
  '((:sm "(min-width: 640px)" :media-query)
    (:md "(min-width: 768px)" :media-query)
    (:lg "(min-width: 1024px)" :media-query)
    (:xl "(min-width: 1280px)" :media-query)
    (:2xl "(min-width: 1536px)" :media-query)
    (:dark ".dark" :selector))
  "mobile-first tags can be used to define breakpoints and other media queries to ergonomically add
specialized lass using MOBILE-FIRST-LASS.")

(defun apply-lass-tags (tag-labels styles)
  "Apply TAG-LABELS *LASS-TAGS* with STYLES to create final Lass."
  (let* ((tags (mapcar (op (assoc _ *lass-tags*)) tag-labels))
         (m-queries
           (str:join
            " or "
            (mapcar
             #'second
             (remove-if-not (op (eq (third _) :media-query)) tags))))
         (selectors (str:join " " (mapcar #'second (remove-if-not (op (eq (third _) :selector)) tags))))
         (l2-lass (if (str:emptyp selectors) styles
                      `((,selectors ,@styles))))
         (l1-lass (if (str:emptyp m-queries) l2-lass
                      `((:media ,m-queries ,@l2-lass)))))
    (car l1-lass)))

(export-always 'tagged-lass)
(defun tagged-lass (&rest styles)
  "Ergonomically add Lass for different cases based on *LASS-TAGS*."
  (let ((lass nil)
        (tag-labels nil))
    (loop :for style :in styles
          :do (if (keywordp style)
                  (push style tag-labels)
                  (progn
                    (if tag-labels
                        (push (apply-lass-tags tag-labels style) (cdr (last lass)))
                        (setf lass (concatenate 'list lass style)))
                    (setf tag-labels nil))))
    (concatenate 'list lass)))
