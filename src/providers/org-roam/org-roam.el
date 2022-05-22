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
(require 'emacsql)
(require 'emacsql-sqlite)
(require 'org)

(defvar clown--control-tags '("blog-post" "draft" "published")
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

(defun clown--collect-node (node)
  "Collect a single org-roam NODE."
  `((id . ,(org-roam-node-id node))
    (slug . ,(clown--node-slug node))
    (title . ,(org-roam-node-title node))
    (tags . ,(org-roam-node-tags node))
    (content . ,(org-file-contents (org-roam-node-file node)))))

(clown--collect-node
 (car (seq-take (clown--roam-nodes-with-tags '("blog-post")) 1)))

(defun clown--collect ()
  "Collect all the org-roam notes which should be published.
Along with their dependencies."
  (let* ((blog-posts-to-publish (clown--roam-nodes-with-tags '("blog-post" "published"))))
    (mapcar #'clown--collect-node blog-posts-to-publish)))

(let* ((db-name (car argv))
       (db (emacsql-sqlite db-name))
       (values (mapcar
                (lambda (row)
                  (vector (alist-get 'id row)
                          (alist-get 'slug row)
                          (alist-get 'title row)
                          (alist-get 'content row)
                          (alist-get 'tags row)
                          *provider-name*))
                (clown--collect))))
  (emacsql db [:insert
               :or :replace
               :into inputs [id slug title content tags provider]
               :values $v1]
           values)
  (message "Done! org-roam blog posts are now in %s" db-name))
