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
        (org-export-with-properties nil)
        (org-export-with-drawers nil)
        (org-export-show-temporary-export-buffer nil)
        (org-export-use-babel nil))
    (with-temp-buffer
      (insert org-content)
      (org-export-as 'html nil nil t))))

(defun clown--file-slug (file)
  "Slug for the file named FILE."
  (s-replace "_" "-" (file-name-base file)))

(defun clown--org-file-to-post (file)
  (let ((meta (clown--get-post-meta file)))
    `(
      :id ,(or (alist-get 'slug meta) (clown--file-slug file))
      :filepath ,file
      :metadata ,(json-encode-alist meta)
      :body_raw ,(org-file-contents file)
      :body_html ,(clown--org-to-html file))))

(defun clown--main (&key files)
  "Main function called by cl-ownpress with FILES."
  (let ((conn (make-network-process
               :name "clown-rpc"
               :buffer nil
               :host "localhost"
               :service 8192)))

    (cl-dolist (file files)
      (process-send-string conn (json-encode (clown--org-file-to-post file)))
      (process-send-string conn "<<<<RPC-EOM>>>>"))

    (process-send-string conn "DONE")
    (process-send-string conn "<<<<RPC-EOM>>>>")

    (delete-process conn)))
