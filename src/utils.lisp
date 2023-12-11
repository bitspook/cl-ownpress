(in-package #:in.bitspook.cl-ownpress)

(defun join-paths (&rest frags)
  "Join FRAGS together with `/` ensuring to add `/` b/w FRAGS only if necessary."
  (str:join
   "/" (loop
         :with sep := "/"
         :with max-idx := (1- (length frags))
         :for idx :from 0
         :for f :in frags
         :when (not (equal f ""))
         :collect
         (let* ((f (if (and (not (eq 0 idx)) (str:starts-with-p sep f))
                       (str:substring 1 (length f) f)
                       f))
                (f (if (and (not (eq max-idx idx)) (str:ends-with-p sep f))
                       (str:substring 0 (1- (length f)) f)
                       f)))
           f))))

(defun system-local (path)
  (namestring (asdf:system-relative-pathname "in.bitspook.cl-ownpress" path)))

(defun recursive-directory-files (start-dir)
  (let ((files (uiop:directory-files start-dir))
        (subdirs (uiop:subdirectories start-dir)))
    (concatenate 'list files (mapcan #'recursive-directory-files subdirs))))
