(in-package :clown-publishers)

(export-always 'html-publisher)
(defclass html-publisher (publisher)
  ((asset-pub :initarg :asset-pub
              :documentation
              "`asset-publisher' that will be used to publish produced assets (Html files, Css,
              fonts, images etc)"))
  (:documentation "Publish `widget's as Html files along with their dependent assets.
(Although it is named `html-publisher', it publishes both Html and Css files. It's called
`html-publisher' because HTML is the primary artifact.)"))

(export-always 'html-page)
(defclass html-page ()
  ((title :initarg :title
          :accessor :html-page-title
          :documentation "Title of the Html page")))

(defmethod publish ((pub html-publisher) &key page-builder root-widget path)
  "Publish ROOT-WIDGET at PATH using PAGE-RENDERER."
  (let* ((*render-stack* nil)
         (ass (slot-value pub 'asset-pub)))

    (push root-widget *render-stack*)

    (let* ((html (with-html-string (dom-of root-widget)))
           (css-file (publish ass :path #p"styles.css"
                              :content (rendered-css)
                              :hash-artifact-name-p t))
           (html (with-html-string (funcall page-builder :css-file css-file :html html))))

      ;; TODO Create a new instance of asset-publisher for publishing the Html file. Use `asset-pub'
      ;; only for css/js. Do this when asset-publisher has been updated to configure hash-in-name
      ;; setting at instance level instead of function call level.
      (publish ass :path path :content html))))
