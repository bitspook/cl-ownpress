(in-package :clown-publishers)

(defclass html-publisher (publisher)
  ((widgets :initarg :widgets
            :initform nil
            :accessor publisher-widgets
            :documentation  "List of widgets which this publisher will use to produce Html/Css/Js"))
  (:documentation "Base class for publishers which produce Html artifacts."))
