(in-package #:in.bitspook.cl-ownpress/publisher)

(export-always 'widget)
(defclass widget (artifact) nil
  (:documentation "A widget produces fragments of a web page. You can think of them as Web components.
A widget is made up of spinneret Dom forms and lass Css forms. Use `defwidget' macro to create a new
widget."))

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

(export-always 'dep)
(defmethod embed-as ((w widget) (as (eql 'dep)) &key of)
  (add-dep of w)
  (dom-of w))

(export-always '*self*)
(defvar *self* nil
  "*self* is a hack. Its purpose is to identify the current widget being built in `defwidget'.
You can use `emdep' in `defwidget' and never need *SELF*.")

(export-always 'defwidget)
(defmacro defwidget (name args lass  &body dom)
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

(export-always 'emdep)
(defmacro emdep (widget &rest args)
  "A convenient macro to add child-widgets. Features:
1. Add WIDGET as dependency of *self*
2. Instantiate WIDGET if it is a symbol"
  (with-gensyms (instance)
    `(let ((,instance (if (eq 'symbol (class-name-of ,widget))
                          (make-instance ,widget ,@args)
                          ,widget)))
       (embed-as ,instance 'dep :of *self*))))

(defvar *render-stack* nil
  "A list to keep track of widgets that are getting rendered. This is used/useful to determine which
widget's CSS should be included in the final artifact.")

(export-always '*render-stack*)
(export-always 'render)
;; This is a good place for extension. I'd eventually like to have the ability
;; to override dom/css/js of any individual widget rendered in an HTML page. To
;; achieve that, we can add Emacs style hooks which are executed before/after a
;; widget is rendered/instantiated, so dom/css of not only the class of a
;; widget, but also that of a particular instance can be manipulated by the end
;; user.
(defmacro render (widget &rest args)
  "Instantiate WIDGET with ARGS (if it is a symbol), add it to *RENDER-STACK* and return its dom.

This is recommended API for:
1. Obtaining the dom of a widget for generating html
2. Adding child widget(s)

It adds the instance to *RENDER-STACK*, which can then be used to determine what
was rendered. This can be useful e.g to determine which widgets' css should be
generated. Make sure to wrap calls to RENDER in a lexical scope which sets
*RENDER-STACK*."
  `(let ((instance (if (eq 'symbol (class-name-of ,widget))
                       (make-instance ,widget ,@args)
                       ,widget)))
     (push instance *render-stack*)
     (dom-of instance)))

(export-always 'rendered-css)
(defun rendered-css ()
  "Return CSS for all the widgets rendered so far.
Rendered widgets are determined from *render-stack*."
  (let ((lass:*pretty* *print-pretty*)
        ;; This part will get tricky when we add the ability to change css of each individual
        ;; instance.
        (unique-widgets
          (nreverse
           (remove-duplicates
            *render-stack*
            :test (lambda (a b)
                    (eql (class-name-of a)
                         (class-name-of b)))))))
    (mapconcat
     (lambda (widget)
       (apply #'lass:compile-and-write (lass-of widget)))
     unique-widgets
     (if *print-pretty* #\NewLine ""))))
