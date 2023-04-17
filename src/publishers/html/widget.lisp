(in-package :clown-publishers)

(export-always 'widget)
(export-always 'widget-children)
(defclass widget ()
  ((children :initarg :children
             :initform nil
             :accessor widget-children
             ;; TODO: Figure out how to find children of a widget after it is
             ;; created (i.e from outside defwidget macro). I tried, but my CL
             ;; skills aren't good enough to figure it out yet. So I am settling
             ;; for this API for now.
             :documentation "List of widgets used in the DOM-OF this widget. This list is automatically
created when using DEFWIDGET to create a widget. But when a WIDGET's dom is modified,
this list need to be manually modified, or other methods should be used to compensate.
For example when using `defmethod :around' to add extra html to DOM-OF a WIDGET, either
 add new widgets (if any) to this list, or also extend LASS-OF the WIDGET."))
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
  nil)

(export-always 'children-of)
(defun children-of (dom)
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
  `(let ((,name (make-instance
                 'widget
                 :children (children-of ',dom))))
     (defmethod dom-of ((widget (eql ,name)) &key ,@args)
       (spinneret:with-html ,@dom))

     (defmethod lass-of ((widget (eql ,name)))
       ,lass)

     ,name))
