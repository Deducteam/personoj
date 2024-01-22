;;; Load this file to load all Personoj at once

(in-package #:cl-user)

;; Load PVS to Dedukti translator
(dolist (m '("pvs2dk/utils.lisp"
             "pvs2dk/pp-dk3.lisp"
             "pvs2dk/pvs.lisp"))
  (load m))
