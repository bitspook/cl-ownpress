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
    (org-mode)
    (let ((props (org-element-map (org-element-parse-buffer 'greater-element)
                     '(keyword)
                   (lambda (kwd)
                     (let ((data (cadr kwd)))
                       (cons (downcase (plist-get data :key))
                             (plist-get data :value))))))
          (description (clown--org-buffer-description)))
      (push (cons "description" description) props)
      props)))

(defun clown--org-buffer-description ()
  "Get description for `current-buffer'."
  (org-forward-paragraph)
  (let ((begin (point))
        (end (progn
               (search-forward-regexp (rx line-start "*") nil t)
               (beginning-of-line)
               (point))))
    (buffer-substring begin end)))

(defun clown--get-post-meta (org-file)
  "Get post metadata for org file with ORG-FILE published to PUBLISHED-FILE."
  (let* ((props (clown--get-org-file-props org-file)))
    (cl-dolist (pcell props)
      (let ((key (downcase (car pcell)))
            (val (cdr pcell)))
        (pcase key
          ("date" (push (cons 'date (format-time-string "%Y-%m-%d %H:%M:%S" (org-time-string-to-time val))) props))
          ("filetags" (push (cons 'tags (split-string val " " t "[ \t]")) props))
          ("description"
           (push
            (cons 'description_html (clown--org-to-html val))
            props)))))

    (when (not (assq 'date props))
      (push (cons 'date (format-time-string "%Y-%m-%d %H:%M:%S" (current-time))) props))

    props))

(defun clown--org-to-html (org-content)
  "Return ORG-CONTENT as HTML."
  (let ((org-export-with-section-numbers nil)
        (org-export-with-toc nil)
        (org-src-fontify-natively t)
        (org-html-htmlize-output-type 'inline-css)
        (org-html-head-include-default-style nil)
        (org-export-with-properties nil)
        (org-export-with-drawers nil)
        (org-export-show-temporary-export-buffer nil)
        (org-export-use-babel nil))
    (with-temp-buffer
      (insert org-content)
      (org-mode)
      (org-export-as 'html nil nil t))))

(defun clown--org-file-to-msg (file)
  "Convert org FILE to msg to be send to cl-ownpress."
  (let ((meta (clown--get-post-meta file))
        (org-content (org-file-contents file)))
    (list
     :id (alist-get 'slug meta)
     :filepath file
     :metadata (json-encode-alist meta)
     :body_raw org-content
     :body_html (clown--org-to-html org-content))))

(defun clown--main (&key files)
  "Main function called by cl-ownpress with FILES."
  (let ((conn (make-network-process
               :name "clown-rpc"
               :buffer nil
               :host "localhost"
               :service 8192)))

    (cl-dolist (file files)
      (process-send-string conn (json-encode (clown--org-file-to-msg file)))
      (process-send-string conn "<<<<RPC-EOM>>>>"))

    (process-send-string conn "DONE")
    (process-send-string conn "<<<<RPC-EOM>>>>")

    (delete-process conn)))
