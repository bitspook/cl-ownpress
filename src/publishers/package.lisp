(defpackage #:clown-publishers
  (:use #:cl #:serapeum/bundle #:clown #:40ants-doc)
  (:import-from #:slug :slugify))
(in-package #:clown-publishers)

(defsection @publishers (:title "Publishers")
  "A publisher publishes content provided by one or more providers.

Publishers create artifacts which can then be published. For example, a
publisher might create a website (a bunch of Html, Css and Js files), or a pdf
file.

Here is a list of publishers packaged with cl-ownpress.

## API"

  (clown-publishers package)

  (publisher class)

  (publish generic-function)
  (public-path generic-function)

  "### Available publishers
Following publishers are provided with cl-ownpress:"
  (asset-publisher class)
  (publish (method () (asset-publisher)))

  (blog-post-publisher class))

(defclass publisher ()
  ((dest :documentation "Directory in which publishable artifacts are created."
         :initform (error "dest is required")
         :initarg :dest
         :accessor publisher-dest))
  (:documentation "A publisher creates publishable artifacts. For example Html+Css+Js files for a
website."))

(export-always 'publish)
(defgeneric publish (publisher &key)
  (:documentation "Instruct the PUBLISHER to create artifacts needed for publishing."))

(defgeneric public-path (publisher &key)
  (:documentation "Provide the public path of artifacts produced by PUBLISHER."))

(export-always 'artifact-already-exists)
(define-condition artifact-already-exists (error)
  ((path :initarg :path :reader artifact-path))
  (:documentation "Condition indicating that an artifact at the same location already exists."))
