(in-package :clown-blog.theme)

(defclass widget ()
  ((children :initarg :children :accessor children))
  (:documentation "A widget can be rendered on a web page.
A widget is made up of spinneret DOM forms, and lass CSS forms. Use `defwidget'
macro to create a new widget."))

(defgeneric render (widget &key)
  (:documentation "Provide spinneret DOM for the WIDGET."))

(defgeneric styles-of (widget)
  (:documentation "Provide a list of lass blocks for WIDGET"))

(defmethod render ((widget widget) &key) nil)

(defmethod styles-of ((widget widget))
  "Last call to `styles-of' a WIDGET provides a list of all the lass blocks that
belong to children of the WIDGET.

It can be used in the final widget (i.e a widget which makes the entire HTML
document) to render styles for all the widgets used in it."
  (concatenate 'list (mapcan (lambda (w) (styles-of w)) (children widget))))

(defun used-child-widgets (dom)
  "Walk spinneret DOM to figure out which widgets are used in in it.
A used widget is determined by a call to `render'."
  (let ((children nil))
    (walk-tree
     (lambda (cell)
       (when (and (listp cell)
                  (eq (car cell) 'render))
         (let ((child (cadr cell)))
           (when (not (find child children))
             (push child children)))))
     dom)
    children))

(defmacro defwidget (name args &key styles render)
  "Create a widget (instance of `widget') named NAME.
ARGS is the arguments received by `render' function.
STYLES is a list of lass blocks.
RENDER is the body of `render' function."
  (let ((instance name)
        (children (used-child-widgets render)))
    `(progn
       (defparameter ,name
         (make-instance 'widget :children (list ,@children)))

       (defmethod styles-of ((widget (eql ,instance)))
         (concatenate 'list ,styles (call-next-method)))

       (defmethod render ((widget (eql ,instance)) &key ,@args)
         ,render))))
