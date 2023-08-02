(in-package :clown-publishers)

(export-always 'html-publisher)
(defclass html-publisher (publisher)
  ((asset-pub :initarg :asset-pub
              :accessor html-asset-publisher
              :documentation
              "`asset-publisher' that will be used to publish produced assets (Html files, Css,
              fonts, images etc)"))
  (:documentation "Publish `widget's as Html files along with their dependent assets.
(Although it is named `html-publisher', it publishes both Html and Css files. It's called
`html-publisher' because HTML is the primary artifact.)"))

(defmethod publish ((pub html-publisher) &key widget path)
  "Publish WIDGET at PATH. PATH is relative to DEST. If CLEAN-URL-P is non-nil, final destination is
DEST/PATH/index.html, else DEST/PATH.html"
  ;; (format nil "PUBLISING ~a TO ~a" widget path)
  (let* ((*render-stack* nil)
         (html (progn
                (push widget *render-stack*)
                (with-html-string (dom-of widget))))
         (ass (html-asset-publisher pub)))
    (publish ass :path path :content html)))
