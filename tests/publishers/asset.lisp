(in-package #:cl-ownpress/tests)

(use-package :clown-publishers)

(define-test "Clown publishers")

(defparameter *ass* (make-instance 'cpub:asset-publisher
                                   :dest *test-dir*))

(defparameter *test-src-dir* #p"/tmp/clown-test-src/"
  "Directory where source files to be used in tests should be stored.")

(defun setup ()
  (uiop:delete-directory-tree *test-dir* :validate t :if-does-not-exist :ignore)
  (uiop:delete-directory-tree *test-src-dir* :validate t :if-does-not-exist :ignore))

(define-test "asset-publisher" :parent "Clown publishers"
  (define-test .publish
    (define-test "when CONTENT is a string or stream"
      (define-test "publish CONTENT as a file at PATH"
        (setup)
        (cpub:publish *ass* :content "Test" :path "css/styles.scss")

        (let ((expected-file (path-join *test-dir* "css/styles.scss")))
          (true (uiop:file-exists-p expected-file))
          (true (string= (str:from-file expected-file) "Test"))))

      (define-test "when a file at PATH already exists"
        (define-test "raise artifact-already-exists error if file at PATH already exists"
          (setup)
          (let* ((target "css/styles.css"))

            (cpub:publish *ass* :content "Test" :path target)
            (fail (cpub:publish *ass* :content "Test" :path target)
                'cpub:artifact-already-exists)))))

    (define-test "when CONTENT is a path to a file"
      (define-test "copy file at CONTENT to PATH"
        (setup)
        (uiop:with-temporary-file (:pathname src-file) :directory *test-src-dir*
          (let ((dest-file (path-join *test-dir* "images/lol.png")))
            (str:to-file src-file "TEST")
            (cpub:publish *ass* :path dest-file :content src-file)
            (true (string= (str:from-file dest-file) "TEST")))))

      (define-test "when PATH already exists"
        (define-test "signal artifact-already-exists error"
          (setup)
          (uiop:with-temporary-file (:pathname src-file) :directory *test-src-dir*
            (let ((dest-file (path-join *test-dir* "images/lol.png")))
              (str:to-file src-file "TEST")
              (cpub:publish *ass* :path dest-file :content src-file)
              (fail (cpub:publish *ass* :path dest-file :content src-file)
                  'cpub:artifact-already-exists))))))

    (define-test "when CONTENT is path to a directory"
      (define-test "recursively copy dir at CONTENT to PATH"
        (setup)
        ;; Setup recursive src dirs
        (let* ((src-level1 (path-join *test-src-dir* "level1/"))
               (src-level2 (path-join src-level1 "level2/")))
          (uiop:ensure-all-directories-exist (list src-level1 src-level2))
          (str:to-file (path-join src-level1 "l1.css") "LEVEL1 CSS")
          (str:to-file (path-join src-level2 "l2.css") "LEVEL2 CSS")

          (let* ((dest-dir (path-join *test-dir* "css/"))
                 (dest-level1 (path-join dest-dir "level1/"))
                 (dest-level2 (path-join dest-level1 "level2/")))
            (cpub:publish *ass* :path dest-dir :content *test-src-dir*)

            (is string= (str:from-file (path-join dest-level1 "l1.css"))
                "LEVEL1 CSS")
            (is string= (str:from-file (path-join dest-level2 "l2.css"))
                "LEVEL2 CSS"))))

      (define-test "when PATH already exists"
        (define-test "signal artifact-already-exists error"
          (setup)
          (let ((existing-dir (path-join *test-dir* "l1/")))
            (ensure-directories-exist existing-dir)
            (uiop:with-temporary-file (:pathname temp-file :directory existing-dir)
              (declare (ignore temp-file))
              (fail (cpub:publish *ass* :path "l1/" :content existing-dir)
                  'cpub:artifact-already-exists))))))))
