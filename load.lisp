(dolist (m '("utils"
             "packages"
             "dklog"
             "dk-sig"
             "dk-recursive"
             "pp-dk3"
             "pvs"))
  (load (concatenate 'string "specs/src/" m ".lisp")))

(load "proofs/src/add-hooks.lisp")
