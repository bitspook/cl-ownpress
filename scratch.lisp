(asdf:load-system :cl-ownpress)
(ql:quickload :40ants-doc-full)
(in-package :cl-user)

(defparameter docs-dir (asdf:system-relative-pathname :cl-ownpress #p"docs/"))

(defun build-doc ()
  (let ((sections (list cl-ownpress::@index
                        clown-providers::@providers
                        clown-publishers::@publishers
                        clown-blog::@publishers-clown-blog
                        clown-blog.theme::@blog-themes)))
    (loop :for section :in sections
          :do (40ants-doc-full/builder:render-to-files
               section :base-dir docs-dir))))

(build-doc)
