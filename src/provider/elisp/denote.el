;;; org-file -- Import org files into cl-ownpress
;;; Commentary:
;;; Code:

(require 'seq)
(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'jsonrpc)
(require 'htmlize)
(require 's)

(load-file (expand-file-name "./clown-common.el" (file-name-directory load-file-name)))

(defun clown-get-post-meta (org-file)
  "Get post metadata for org file with ORG-FILE published to PUBLISHED-FILE."
  (let* ((props (cl-ownpress--get-org-file-props org-file)))
    (cl-dolist (pcell props)
      (let ((key (downcase (car pcell)))
            (val (cdr pcell)))
        (pcase key
          ("date"
           (setf props (cl-remove-if (lambda (cell) (equal (car cell) "date")) props))
           (push (cons 'date (format-time-string "%Y-%m-%d %H:%M:%S" (org-time-string-to-time val))) props))
          ("filetags" (push (cons 'tags (split-string val "[ :]" t "[ \t]")) props)))))

    (when (not (assq 'date props))
      (push (cons 'date (format-time-string "%Y-%m-%d %H:%M:%S" (current-time))) props))

    props))

(defun clown-org-to-html (org-content)
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

(defun clown-org-file-to-msg (file)
  "Convert org FILE to msg to be send to cl-ownpress."
  (let ((meta (clown-get-post-meta file))
        (org-content (org-file-contents file)))
    (list
     :id (alist-get 'slug meta)
     :filepath file
     :metadata (json-encode-alist meta)
     :body_raw org-content
     :body_html (clown-org-to-html org-content))))

(defun main (&rest _args)
  "Main function called by cl-ownpress."
  (let ((conn (clown-rpc-server))
        (files (cl-remove-if-not
                (lambda (fname) (seq-contains-p (denote-extract-keywords-from-path fname) "blog-post" #'equal))
                (denote-all-files))))
    (cl-dolist (file files)
      (jsonrpc-notify conn :event (clown-org-file-to-msg file)))

    (jsonrpc-notify conn :done nil)))

;;; org-file.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("clown" . "cl-ownpress-"))
;; End:
