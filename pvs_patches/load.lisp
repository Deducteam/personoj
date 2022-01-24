(in-package #:cl-user)

;; Load PVS to Dedukti translator
(dolist (m '("pvs2dk/utils.lisp"
             "pvs2dk/pp-dk3.lisp"
             "pvs2dk/pvs.lisp"))
  (load m))

;; Works properly on Allegro Lisp only
#+allegro
(progn
  (load "proof-hooks.lisp")
  (load "proveit.lisp"))
