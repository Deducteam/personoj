(in-package :pvs)

(export '(prettyprint-dedukti))

(defun prettyprint-dedukti (theoryref out &optional suppress-msg)
  "Print theory THEORYREF to output file OUT (which must be an absolute file
path). PVS shuts up if SUPPRESS-MSG is T."
  (let ((*suppress-msg* suppress-msg))
    (with-pvs-file (fname thname) theoryref
      (let ((theory (get-typechecked-theory (or thname fname))))
        (with-open-file (stream out :direction :output :if-exists :supersede)
          (pp-dk stream theory))))))
