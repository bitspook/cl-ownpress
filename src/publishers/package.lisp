(defpackage #:clown-publishers
  (:use #:cl #:serapeum/bundle #:clown #:40ants-doc))
(in-package #:clown-publishers)

(defsection @publishers (:title "Publishers")
  "A publisher publishes content collected in clown db.

Publishers are free to publish content in any shape and form. They are supposed
to create artifacts which can then be published. For example, a publisher might
create a website, or a pdf file.

Here is a list of publishers packaged with cl-ownpress.

## Publishers packaged with cl-ownpress

- [clown-blog](/clown-blog-publisher) <br /> Creates a static HTML site which can be hosted on platforms
  like Github pages. Meant for publishing personal blogs.

## API"

  (clown-publishers package))
