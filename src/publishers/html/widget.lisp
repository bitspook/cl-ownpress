(in-package :clown-publishers)

(export-always 'widget)
(defclass widget ()
  nil
  (:documentation "A widget produces fragments of a web page. You can think of them as Web components.
A widget is made up of spinneret Dom forms, and lass Css forms. Use `defwidget'
macro to create a new widget."))

(export-always 'dom-of)
(defgeneric dom-of (widget &key)
  (:documentation "Provide spinneret DOM for the WIDGET."))

(export-always 'lass-of)
(defgeneric lass-of (widget)
  (:documentation "Provide a list of lass blocks for WIDGET"))

(defmethod dom-of ((widget widget) &key) nil)

(defmethod lass-of ((widget widget))
  "Last call to `lass-of' a WIDGET provides a list of all the lass blocks that
belong to children of the WIDGET.

It can be used in the final widget (i.e a widget which makes the entire HTML
document) to dom-of styles for all the widgets used in it."
  (concatenate 'list (mapcan (lambda (w) (lass-of w)) (used-child-widgets widget))))

(defun used-child-widgets (dom)
  "Walk spinneret DOM to figure out which widgets are used in in it.
A used widget is determined by a call to `dom-of'."
  (let ((children nil))
    (walk-tree
     (lambda (cell)
       (when (and (listp cell)
                  (eq (car cell) 'dom-of))
         (let ((child (cadr cell)))
           (when (not (find child children))
             (push child children)))))
     dom)
    children))

(export-always 'defwidget)
(defmacro defwidget (name args lass  &body dom)
  "Create a widget (instance of `widget') named NAME.
ARGS is the arguments received by `dom-of' function."
  `(let ((,name (make-instance 'widget)))
     (defmethod dom-of ((widget (eql ,name)) &key ,@args)
       (spinneret:with-html ,@dom))

     (defmethod lass-of ((widget (eql ,name)))
       ,lass)

     ,name))
