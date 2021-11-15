(dolist (m '("utils"
             "packages"
             "dklog"
             "dk-sig"
             "dk-recursive"
             "pp-dk3"
             "pvs"))
  (load (concatenate 'string "specs/src/" m ".lisp")))

(dolist (m '("proof-common" "tptp" "proof-json" "proof-dk" "add-hooks"))
  (load (concatenate 'string "proofs/src/" m ".lisp")))
