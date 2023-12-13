(in-package #:in.bitspook.cl-ownpress/publisher)

(export-always 'html-publisher)
(defclass html-publisher (publisher)
  ((asset-pub :initarg :asset-pub
              :documentation
              "`asset-publisher' that will be used to publish produced assets (Html files, Css,
              fonts, images etc)"))
  (:documentation "Publish `widget's as Html files along with their dependent assets.
(Although it is named `html-publisher', it publishes both Html and Css files. It's called
`html-publisher' because HTML is the primary artifact.)"))

(defmethod publish ((pub html-publisher) &key page-builder root-widget path)
  "Publish ROOT-WIDGET at PATH using PAGE-BUILDER.
PAGE-BUILDER gets `css-file' and `html' as keyword arguments. `css-file' is path to the created css
file. `html' is html string of rendered widgets that should go in `<body>` of the page.

PAGE-BUILDER is a quick-fix for the chicken-and-egg problem of rendering a `widget' to conclude
what css to render, and needing the rendered css to create css-file with content-hash which should
then be linked in final html page. I wish we could produce the final HTML from `root-widget' itself
and not need a `page-builder'."
  (let ((*render-stack* nil)
        (ass (slot-value pub 'asset-pub)))

    (push root-widget *render-stack*)
    (let* ((html (with-html-string (dom-of root-widget)))
           (css-file (publish ass :path #p"styles.css"
                              :content (rendered-css)
                              :hash-artifact-name-p t))
           (html (with-html-string (funcall page-builder :css-file css-file :html html)))
           (html-file-publisher (make 'asset-publisher :dest (publisher-dest pub))))

      (publish html-file-publisher :path path :content html))))

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
