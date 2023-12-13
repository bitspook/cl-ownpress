(uiop:define-package #:in.bitspook.cl-ownblog
  (:use #:cl #:serapeum/bundle #:in.bitspook.cl-ownpress/publisher #:in.bitspook.cl-ownpress/provider)
  (:import-from #:slug :slugify))

(defgeneric from (obj to)
  (:documentation "Convert OBJ object to instance of class represented symbol TO"))
