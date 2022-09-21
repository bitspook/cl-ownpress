(require 'seq)
(require 'cl-lib)
(require 'org-roam)
(require 'org)
(require 'jsonrpc)
(require 'htmlize)

(defvar *clown--roam-node-category-fn* nil)

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

(defun clown--node-public-path (node)
  (let ((cat (funcall *clown--roam-node-category-fn* node)))
    (format "/%s/%s" cat (clown--node-slug node))))

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

(defun clown--export-org-id-link (path desc backend)
  "Handle exporting links for inserted org-roam nodes.
Requirements: A variable named *linked-roam-nodes* must be a list
and present in scope. All the linked org-roam nodes are pushed
into this list."
  (cond
   ((eq backend 'html)
    (let* ((node (org-roam-node-from-id path))
           (public-path (when node (clown--node-public-path node))))

      (if (not public-path) desc
        (push node *linked-roam-nodes*)
        (format "<a href=\"%s\" title=\"%s\">%s</a>" public-path desc desc))))))

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
  (cl-block 'clown--collect-node
    (let* ((file (org-roam-node-file node))
           (meta (clown--get-post-meta file))
           (cat (or (alist-get "category" meta)
                    (funcall *clown--roam-node-category-fn* node)))
           (*linked-roam-nodes* nil)
           (id (org-roam-node-id node))
           (body-html nil))
      (push `(category . ,cat) meta)
      (when (not (alist-get 'slug meta))
        (push `(slug . ,(clown--node-slug node)) meta))

      (when (seq-contains-p *collected-ids* id)
        (cl-return-from 'clown--collect-node))

      (setf body-html (clown--org-to-html file))
      (push id *collected-ids*)

      (cl-concatenate
       'list
       (list (list
              :id id
              :metadata (json-encode-alist meta)
              :body_raw (org-file-contents file)
              :body_html body-html))
       (mapcan #'clown--collect-node
               *linked-roam-nodes*)))))

(defun clown--collect (tags)
  "Collect all the org-roam notes which have TAGS."
  (let ((original-id-exporter (org-link-get-parameter "id" :export))
        *collected-ids*
        collected-notes)
    ;; Handle linked notes
    (org-link-set-parameters "id" :export #'clown--export-org-id-link)

    (setf collected-notes (mapcan #'clown--collect-node (clown--roam-nodes-with-tags tags)))
    (org-link-set-parameters "id" :export original-id-exporter)
    collected-notes))

(cl-defun clown--main (&key tags listed-category unlisted-category)
  "Main function called from cl-ownpress for providing notes with TAGS.
LISTED-CATEGORY and UNLISTED-CATEGORY are used to mark
notes which should be published and those which are only linked
from published notes. They are also used to create links for
linked notes following the url scheme: /<category>/<slug>"
  (setf *clown--roam-node-category-fn*
        (lambda (node)
          (cl-labels ((contains-subseq-p (seq1 seq2)
                        (eq
                         (length (seq-intersection seq1 seq2))
                         (length seq2))))
            (if (contains-subseq-p (org-roam-node-tags node) tags)
                listed-category
              unlisted-category))))

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
