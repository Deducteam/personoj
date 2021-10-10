;;;; Place this at the top of "~/.pvs.lisp"

(defparameter *pvs-dedukti-path* "PVSDKPATH")
(dolist (m '("utils" "packages" "dklog" "dk-sig" "dk-recursive" "pp-dk3" "pvs"))
  (load (concatenate 'string *pvs-dedukti-path* "/exporter/" m ".lisp")))
