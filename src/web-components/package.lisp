(defpackage in.bitspook.web-components
  (:use :cl :serapeum/bundle)
  (:import-from :spinneret :with-html-string :with-html)
  (:import-from :lass :compile-sheet :write-sheet)
  (:export
   ;; component
   #:component
   #:dom-of
   #:lass-of
   #:css-of
   #:defcomponent
   ;; rendering
   #:*self*
   #:render
   #:rendered-css
   ;; lass conveniences
   #:*lass-tags*
   #:tagged-lass))

(in-package #:in.bitspook.web-components)

(defclass component ()
  ((dependencies :initform nil
                 :documentation "List of dependency components. Used to decide final CSS."))
  (:documentation "A component produces HTML+CSS fragments. You can think of them as Web components, minus the
Javascirpt. A component is made up of spinneret Dom forms and lass Css forms. Use `defcomponent'
macro to create a new component."))

(defgeneric add-dependency (parent dependency)
  (:documentation "Add dependency component to COMP")
  (:method ((parent component) (dependency component))
    (pushnew dependency (slot-value parent 'dependencies))))

(defun all-dependencies (comp)
  (declare (component comp))
  (let ((dependencies (slot-value comp 'dependencies)))
    (nub (flatten (concatenate 'list dependencies (mapcar #'all-dependencies dependencies))))))

(defun componentp (w)
  (subtypep (type-of w) 'component))

(defgeneric dom-of (component)
  (:documentation "Provide spinneret DOM for the COMPONENT.")
  (:method ((component component)) nil))

(defgeneric lass-of (component)
  (:documentation "Provide a list of lass blocks for COMPONENT")
  (:method ((component component)) nil))

(defgeneric css-of (component)
  (:documentation "Return css string for COMPONENT. Uses LASS-OF internally.")
  (:method ((component component))
    (let ((lass:*pretty* *print-pretty*))
      (write-sheet
       (apply #'compile-sheet (lass-of component))))))

(defvar *self* nil
  "*self* is a hack. Its purpose is to identify the current component being rendered in `defcomponent'.
You can use `render' in `defcomponent' and never need *SELF*.")

(defmacro defcomponent (name args lass &body dom)
  "Create a component (instance of `component') named NAME.
ARGS is the arguments received by `dom-of' and `lass-of' functions.
*self* is set to current component in dom-of/lass-of methods."
  `(progn
     (defclass ,name (component)
       ,(mapcar
         (lambda (arg) `(,arg :initarg ,(make-keyword arg)))
         args))

     (defmethod dom-of ((self ,name))
       (let ((*self* self))
         (with-slots ,args self
           (spinneret:with-html ,@dom))))

     (defmethod lass-of ((component ,name))
       (let ((*self* component))
         (with-slots ,args component
           ,lass)))

     ',name))

(defmacro render (component &rest args)
  "A convenient macro to provide single API to consume a COMPONENT.
Enable consuming COMPONENT in following ways:
1. Add COMPONENT as a child of another component
  1. Add COMPONENT as dependency of *self*
  2. Instantiate COMPONENT if it is a symbol
2. Directly obtain COMPONENT's dom."
  (with-gensyms (instance)
    `(let ((,instance (if (eq 'symbol (class-name-of ,component))
                          (make-instance ,component ,@args)
                          ,component)))
       (when *self* (add-dependency *self* ,instance))
       (dom-of ,instance))))

(defun rendered-css (component)
  "Return CSS rendered for COMPONENT.
Make sure component has been `render'ed. A COMPONENT's dependencies are resolved when it is `render'ed."
  (let* ((lass:*pretty* *print-pretty*)
         (all-dependencies (append (list component) (all-dependencies component)))
         (css-dependencies (remove-if-not #'componentp all-dependencies))
         (css-fragments (mapcar #'css-of (append1 (nreverse css-dependencies) component)))
         (unique-css-fragments (nreverse (remove-duplicates css-fragments :test #'equal))))
    (str:join (if *print-pretty* #\NewLine "") unique-css-fragments)))


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
