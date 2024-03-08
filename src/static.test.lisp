(in-package #:in.bitspook.cl-ownpress/tests)

(defparameter *test-src-dir* #p"/tmp/clown-test-src/"
  "Directory where source files to be used in tests should be stored.")

(defun setup ()
  (uiop:delete-directory-tree *test-dir* :validate t :if-does-not-exist :ignore)
  (uiop:delete-directory-tree *test-src-dir* :validate t :if-does-not-exist :ignore))

(define-test "publish-static")

(define-test "when CONTENT is a string" :parent "publish-static"
  (define-test "publish CONTENT as a file at PATH"
    (setup)
    (publish-static :dest-dir *test-dir* :content "Test" :path "css/styles.scss")

    (let ((expected-file (path-join *test-dir* "css/styles.scss")))
      (true (uiop:file-exists-p expected-file))
      (true (string= (str:from-file expected-file) "Test"))))

  (define-test "when a file at PATH already exists"
    (define-test "raise file-already-exists error if file at PATH already exists"
        (setup)
      (let* ((target "css/styles.css"))

        (publish-static :dest-dir *test-dir* :content "Test" :path target)
        (fail (publish-static :dest-dir *test-dir* :content "Test" :path target)
              'file-already-exists)))))

(define-test "when CONTENT is a path to a file" :parent "publish-static"
  (define-test "copy file at CONTENT to PATH"
    (setup)
    (uiop:with-temporary-file (:pathname src-file) :directory *test-src-dir*
      (let* ((path "images/lol.png")
             (dest-file (path-join *test-dir* path)))
        (str:to-file src-file "TEST")
        (publish-static :dest-dir (namestring *test-dir*) :path path :content src-file)
        (true (string= (str:from-file dest-file) "TEST")))))

  (define-test "when PATH already exists"
      (define-test "signal file-already-exists error"
        (setup)
        (uiop:with-temporary-file (:pathname src-file) :directory *test-src-dir*
          (let* ((path "images/lol.png")
                 (dest-file (path-join *test-dir* path)))
            (str:to-file src-file "TEST")
            (publish-static :dest-dir *test-dir*
                            :path dest-file
                            :content src-file)
            (fail (publish-static :dest-dir *test-dir* :path dest-file :content src-file)
                'file-already-exists))))))

(define-test "when CONTENT is path to a directory" :parent "publish-static"
  (define-test "recursively copy dir at CONTENT to PATH"
    (setup)
    ;; Setup recursive src dirs
    (let* ((src-level1 (path-join *test-src-dir* "level1/"))
           (src-level2 (path-join src-level1 "level2/")))
      (uiop:ensure-all-directories-exist (list src-level1 src-level2))
      (str:to-file (path-join src-level1 "l1.css") "LEVEL1 CSS")
      (str:to-file (path-join src-level2 "l2.css") "LEVEL2 CSS")

      (let* ((dest-dir (base-path-join *test-dir* "css/"))
             (dest-level1 (base-path-join dest-dir "level1/"))
             (dest-level2 (base-path-join dest-level1 "level2/")))
        (publish-static :dest-dir *test-dir*
                        :path "css/"
                        :content *test-src-dir*)

        (is string= (str:from-file (path-join dest-level1 "l1.css"))
            "LEVEL1 CSS")
        (is string= (str:from-file (path-join dest-level2 "l2.css"))
            "LEVEL2 CSS"))))

  (define-test "when PATH already exists"
    (define-test "signal file-already-exists error"
      (setup)
      (let ((existing-dir (path-join *test-dir* "l1/")))
        (ensure-directories-exist existing-dir)
        (uiop:with-temporary-file (:pathname temp-file :directory existing-dir)
          (declare (ignore temp-file))
          (fail (publish-static :dest-dir *test-dir*
                                :path "l1/"
                                :content existing-dir)
              'file-already-exists)))))

  (define-test "when HASH-FILE-NAME-P is non-nil"
    (define-test "throw error if  CONTENT is a path to a directory"
      (setup)
      (let ((content-dir (path-join *test-dir* "some-dir/")))
        (ensure-directories-exist content-dir)
        (handler-case (publish-static :dest-dir *test-dir*
                                      :path "l1"
                                      :content content-dir
                                      :hash-file-name-p t)
          (error (e)
            (true (string=
                   (format nil "~a" e)
                   "HASH-FILE-NAME-P can not be used to publish a directory"))))))

    (define-test "include content hash of artifact in published artifact's name"
      (define-test "when CONTENT is a string"
        (setup)
        (let* ((src-filename "css/lol.css")
               (content "body{background:red}")
               (expected-filename (append-content-hash
                                   src-filename
                                   (md5:md5sum-string content))))
          (publish-static :dest-dir *test-dir*
                          :path src-filename
                          :content content
                          :hash-file-name-p t)
          (true (uiop:file-exists-p (path-join *test-dir* expected-filename)))))

      (define-test "when CONTENT is pathname to a file"
        (setup)
        (let* ((src-filename #p"lol.css")
               (content "body{background:red}")
               (expected-filename (append-content-hash
                                   (namestring src-filename)
                                   (md5:md5sum-string content))))
          (ensure-directories-exist *test-src-dir*)
          (str:to-file (path-join *test-src-dir* src-filename) content)

          (publish-static :dest-dir *test-dir*
                          :path src-filename
                          :content (path-join *test-src-dir* src-filename)
                          :hash-file-name-p t)

          (true (uiop:file-exists-p (path-join *test-dir* expected-filename))))))))

(define-test "return PATH (relative to DEST) of published artifact" :parent "publish-static"
  (setup)
  (let* ((src-filename "css/lol.css")
         (content "body{background:red}")
         (expected-filename (append-content-hash
                             src-filename
                             (md5:md5sum-string content)))
         (returned-filename (publish-static
                             :dest-dir *test-dir*
                             :path src-filename :content content
                             :hash-file-name-p t)))

    (true (equal (namestring expected-filename)
                 (namestring returned-filename)))))
