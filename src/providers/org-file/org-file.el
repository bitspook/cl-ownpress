;-*- lexical-binding: t; -*-

(defconst *provider-name* "org-file")
(defvar *provider-dir* (file-name-directory load-file-name))
(defvar *cask-bundle* nil)
(defvar *setup-cask* nil)

(require 'cask (format "%scask.el" (getenv "CASK_DIR")))

(setq user-emacs-directory (expand-file-name "./" *provider-dir*)
      *cask-bundle* (cask-initialize)
      load-path (cask-load-path *cask-bundle*))

(when *setup-cask*
  (cask-install *cask-bundle*))

(require 'seq)
(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'jsonrpc)
(require 'htmlize)
(require 's)

(defun clown--get-org-file-props (filename)
  "Get file-level org props for FILENAME."
  (with-temp-buffer
    (insert-file filename)
    (org-element-map (org-element-parse-buffer 'greater-element)
        '(keyword)
      (lambda (kwd)
        (let ((data (cadr kwd)))
          (list (plist-get data :key)
                (plist-get data :value)))))))

(defun clown--get-post-meta (org-file)
  "Get post metadata for org file with ORG-FILE published to PUBLISHED-FILE."
  (let* ((props (clown--get-org-file-props org-file))
         (props (seq-map
                 (lambda (pcell)
                   (let ((key (downcase (car pcell)))
                         (val (cadr pcell)))
                     (pcase key
                       ("date" (cons 'date (format-time-string "%Y-%m-%d %H:%M:%S" (org-time-string-to-time val))))
                       ("filetags" (cons 'tags (split-string val " " t "[ \t]")))
                       (_ (cons (intern key) val)))))
                 props)))

    (when (not (assq 'date props))
      (push (cons 'date (format-time-string "%Y-%m-%d %H:%M:%S" (current-time))) props))

    props))

(defun clown--org-to-html (filename)
  "Return content of FILENAME named org file as HTML."
  (let ((org-export-with-section-numbers nil)
        (org-export-with-toc nil)
        (org-src-fontify-natively t)
        (org-html-htmlize-output-type 'inline-css)
        (org-content (org-file-contents filename))
        (org-html-head-include-default-style nil)
        (org-export-show-temporary-export-buffer nil))
    (with-temp-buffer
      (insert org-content)
      (org-export-as 'html nil nil t))))

(defun clown--file-slug (file)
  "Slug for the file named FILE."
  (s-replace "_" "-" (file-name-base file)))

(defun clown--org-file-to-post (file)
  (let ((meta (clown--get-post-meta file)))
    `(:id ,(or (alist-get 'slug meta) (clown--file-slug file))
      :slug ,(or (alist-get 'slug meta) (clown--file-slug file))
      :title ,(alist-get 'title meta)
      :tags ,(json-encode-list (alist-get 'tags meta))
      :metadata ,(json-encode-alist meta)
      :published-at ,(alist-get 'date meta)
      :content_raw ,(org-file-contents file)
      :content_html ,(clown--org-to-html file))))

(defun main ()
  (defvar conn (make-network-process :name "clown-rpc"
                                     :buffer nil
                                     :host "localhost"
                                     :service 8192))

  (load-theme 'leuven)

  (let ((files argv))
    (cl-dolist (file files)
      (process-send-string conn (json-encode (clown--org-file-to-post file)))
      (process-send-string conn "<<<<RPC-EOM>>>>")))

  (process-send-string conn "DONE")
  (process-send-string conn "<<<<RPC-EOM>>>>")

  (delete-process conn))

(main)
