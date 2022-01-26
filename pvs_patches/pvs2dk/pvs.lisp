(in-package :pvs)

(export '(prettyprint-dedukti))

(defun prettyprint-dedukti (theoryref out)
  "Print theory THEORYREF to output file OUT (which must be an absolute file
path)."
  (with-pvs-file (fname thname) theoryref
    (let* ((theory (get-typechecked-theory (or thname fname))))
      (with-open-file (stream file :direction :output :if-exists :supersede)
        (pp-dk-top stream theory)))))
