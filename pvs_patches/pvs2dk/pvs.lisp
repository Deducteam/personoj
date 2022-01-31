(in-package :pvs)

(export '(prettyprint-dedukti))

(defun prettyprint-dedukti (theoryref out &optional without-proofs)
  "Print theory THEORYREF to output file OUT (which must be an absolute file
path). Proofs are not translated if WITHOUT-PROOFS is T."
  (with-pvs-file (fname thname) theoryref
    (let ((theory (get-typechecked-theory (or thname fname))))
      (with-open-file (stream out :direction :output :if-exists :supersede)
        (pp-dk stream theory without-proofs)))))
