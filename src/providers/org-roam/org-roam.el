;-*- lexical-binding: t; -*-

(defconst *provider-name* "org-roam")
(defvar *provider-dir* (file-name-directory load-file-name))
(defvar *cask-bundle* nil)
(defvar *setup-cask* nil)
(setq org-directory "~/Documents/org")
(defvar org-roam-directory (concat org-directory "/notes/"))
(defvar org-roam-db-location "~/.emacs.d/org-roam.db")
(defvar org-roam-tag-sources '(prop))
(defvar org-roam-v2-ack t)

(require 'cask (format "%scask.el" (getenv "CASK_DIR")))

(setq user-emacs-directory (expand-file-name "./" *provider-dir*)
      *cask-bundle* (cask-initialize)
      load-path (cask-load-path *cask-bundle*))

(when *setup-cask*
  (cask-install *cask-bundle*))

(require 'seq)
(require 'cl-lib)
(require 'org-roam)
(require 'org)
(require 'jsonrpc)

(defvar *clown-control-tags* '("blog-post" "draft" "published")
  "Tags meant for controlling publishing.
Which themselves should be not be published.")

(defun clown--roam-nodes-with-tags (tags)
  "Find all org-roam nodes which have all TAGS."
  (let ((nodes (seq-filter
                (lambda (node)
                  (= (seq-length tags)
                     (seq-length
                      (seq-intersection (org-roam-node-tags node) tags))))
                (org-roam-node-list))))
    nodes))

(defun clown--node-slug (node)
  "Get slug for org-roam NODE."
  (string-replace "_" "-" (org-roam-node-slug node)))

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

(defun clown--collect-node (node)
  "Collect a single org-roam NODE."
  (let ((meta (clown--get-post-meta (org-roam-node-file node))))
    `(:id ,(org-roam-node-id node)
      :slug ,(or (alist-get 'slug meta) (clown--node-slug node))
      :title ,(alist-get 'title meta)
      :tags ,(json-encode-list (alist-get 'tags meta))
      :metadata ,(json-encode-alist meta)
      :published-at ,(alist-get 'date meta)
      :content ,(org-file-contents (org-roam-node-file node)))))

(defun clown--collect ()
  "Collect all the org-roam notes which should be published.
Along with their dependencies."
  (let* ((blog-posts-to-publish (clown--roam-nodes-with-tags '("blog-post" "published"))))
    (mapcar #'clown--collect-node blog-posts-to-publish)))

(defun main ()
  (defvar conn (make-network-process :name "clown-rpc"
                                     :buffer nil
                                     :host "localhost"
                                     :service 8192))


  (let ((inputs (clown--collect)))
    (cl-dolist (input inputs)
      (process-send-string conn (json-encode input))
      (process-send-string conn "<<<<RPC-EOM>>>>")))

  (process-send-string conn "DONE")
  (process-send-string conn "<<<<RPC-EOM>>>>")

  (delete-process conn))

(main)
