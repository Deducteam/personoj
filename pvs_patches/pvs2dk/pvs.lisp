;;; This file exports a function suitable for the Emacs interface
(in-package :pvs)

(export '(prettyprint-dedukti))

(defun prettyprint-dedukti (theoryref out &optional without-proofs)
  "Print theory THEORYREF to output file OUT (which must be an absolute file
path). Proofs are not translated if WITHOUT-PROOFS is `t'."
  (let ((theory (get-typechecked-theory theoryref)))
    (with-open-file (stream out :direction :output :if-exists :supersede)
      (pp-dk stream theory without-proofs))))
