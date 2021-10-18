(dolist (m '("utils"
             "packages"
             "dklog"
             "dk-sig"
             "dk-recursive"
             "pp-dk3"
             "pvs"))
  (load (concatenate 'string "specs/src/" m ".lisp") ))

(dolist (m '("tptp" "proof-json"))
  (load (concatenate 'string "proofs/src/" m ".lisp")))
