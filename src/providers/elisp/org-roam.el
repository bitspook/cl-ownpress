(require 'seq)
(require 'cl-lib)
(require 'org-roam)
(require 'org)
(require 'jsonrpc)
(require 'htmlize)

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

(defun clown--org-to-html (filename)
  "Return content of FILENAME named org file as HTML."
  (let ((org-export-with-section-numbers nil)
        (org-export-with-toc nil)
        (org-src-fontify-natively t)
        (org-html-htmlize-output-type 'inline-css)
        (org-content (org-file-contents filename))
        (org-html-head-include-default-style nil)
        (org-export-with-properties nil)
        (org-export-with-drawers nil)
        (org-export-show-temporary-export-buffer nil)
        (org-export-use-babel nil))
    (with-temp-buffer
      (insert org-content)
      (org-export-as 'html nil nil t))))

(defun clown--collect-node (node)
  "Collect a single org-roam NODE."
  (let* ((file (org-roam-node-file node))
         (meta (clown--get-post-meta file))
         (cat (or (alist-get "category" meta) "blog")))
    (push `("category" . ,cat) meta)

    `(:id ,(org-roam-node-id node)
          :metadata ,(json-encode-alist meta)
          :body_raw ,(org-file-contents file)
          :body_html ,(clown--org-to-html file))))

(defun clown--collect (tags)
  "Collect all the org-roam notes which have TAGS."
  (mapcar #'clown--collect-node (clown--roam-nodes-with-tags tags)))

(defun clown--main (&key tags)
  "Main function called from cl-ownpress for providing notes with TAGS."
  (let ((conn (make-network-process
               :name "clown-rpc"
               :buffer nil
               :host "localhost"
               :service 8192))
        (notes (clown--collect tags)))

    (cl-dolist (note notes)
      (process-send-string conn (json-encode note))
      (process-send-string conn "<<<<RPC-EOM>>>>"))

    (process-send-string conn "DONE")
    (process-send-string conn "<<<<RPC-EOM>>>>")

    (delete-process conn)))
