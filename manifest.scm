(use-modules (guix packages)
             (gnu packages base)
             (gnu packages lisp))

(packages->manifest (list sbcl gnu-make))
