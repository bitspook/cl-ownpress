(ql:quickload '(:in.bitspook.cl-ownblog))

(in-package :in.bitspook.cl-ownblog)

(defparameter *notes* nil)

(let ((notes-provider (make-instance 'denote-provider)))
  (setf *notes* (provide-all notes-provider)))

(defun build-test-blog ()
  (let* ((www #p"/tmp/www/")
         (static (asdf:system-relative-pathname :in.bitspook.cl-ownblog "src/blog/static/"))
         (author (make-instance 'persona
                                :name "Charanjit Singh"
                                :handles '(("Mastodon" "bitspook" "https://infosec.exchange/@bitspook"))))
         (asset-pub (make-instance 'asset-publisher
                                   :dest www))
         (post-pub (make-instance 'blog-post-publisher
                                  :asset-pub asset-pub
                                  :dest www)))

    (uiop:delete-directory-tree www :validate t :if-does-not-exist :ignore)
    (publish asset-pub :content static)
    (loop :for post :in (mapcar (op (from _ 'blog-post)) *notes*)
          :do (let ((root-w (make 'blog-post-w :post post)))
                (setf (post-author post) author)
                (publish post-pub :post post :layout root-w)))))

(build-test-blog)

;; quick hack to auto-build
;; elisp
;; (defun build-blog (successp notes buffer loadp)
;;   (sly-eval '(in.bitspook.cl-ownblog::build-test-blog) "cl-ownblog"))
;; (add-hook 'sly-compilation-finished-hook #'build-blog)
