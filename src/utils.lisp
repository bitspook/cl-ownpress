(in-package :clown)

(defun join-paths (&rest frags)
  "Join FRAGS together with `/` ensuring to add `/` b/w FRAGS only if necessary."
  (str:join
   "/" (loop
         :with sep := "/"
         :with max-idx := (1- (length frags))
         :for idx :from 0
         :for f :in frags
         :collect
         (let* ((f (if (and (not (eq 0 idx)) (str:starts-with-p sep f))
                       (str:substring 1 (length f) f)
                       f))
                (f (if (and (not (eq max-idx idx)) (str:ends-with-p sep f))
                       (str:substring 0 (1- (length f)) f)
                       f)))
           f))))

(defmacro make-conf (initial-value)
  "Create a trivial configuration management functionality.

In the context of current package, it creates:

1. A variable named `*conf*' which should be a plist initialized with INITIAL-VALUE
2. A function `conf' for getting/setting a configuration value saved in a KEY
3. A non-destructive function `conf-merge' for merging a subset of configuration
with present configuration"
  (let ((global-conf (intern "*CONF*" (sb-int:sane-package)))
        (conf (intern "CONF" (sb-int:sane-package)))
        (conf-merge (intern "CONF-MERGE" (sb-int:sane-package)))
        (key (gensym))
        (val (gensym)))
    `(progn
       (defparameter ,global-conf ,initial-value)

       (defun ,conf (,key)
         "Get configuration value corresponding to KEY."
         (getf ,global-conf ,key))

       (defun (setf ,conf) (,val ,key)
         (setf (getf ,global-conf ,key) ,val))

       (defun ,conf-merge (,val)
         "Merge NEW-CONF into default `*conf*' and return the result.

## Example

```lisp
(let ((*conf* (conf-merge `((:site-url \"https://mysite.com/\")))))
  (build))
```"
         (concatenate 'list ,val ,global-conf)))))
