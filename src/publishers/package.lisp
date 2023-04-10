(defpackage #:clown-publishers
  (:use #:cl #:serapeum/bundle #:clown #:40ants-doc))
(in-package #:clown-publishers)

(defsection @publishers (:title "Publishers")
  "A publisher publishes content provided by one or more providers.

Publishers are free to publish content in any shape and form. They are supposed
to create artifacts which can then be published. For example, a publisher might
create a website, or a pdf file.

Here is a list of publishers packaged with cl-ownpress.

## Publishers packaged with cl-ownpress

- [clown-blog](/publishers-clown-blog) <br /> Creates a static Html site which can be hosted on platforms
  like Github pages. Meant for publishing personal blogs.

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

(defclass html-publisher (publisher)
  nil
  (:documentation "Publisher whose primary artifact is Html meant to be published on websites."))

(defgeneric global-lass-for (publisher)
  (:documentation "Returned lass blocks are added as global Css for generated Html artifacts."))

(export-always 'artifact-already-exists)
(define-condition artifact-already-exists (error)
  ((path :initarg :path :reader artifact-path))
  (:documentation "Condition indicating that an artifact at the same location already exists."))
