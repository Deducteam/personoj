(in-package :pvs)

(export '(prettyprint-dedukti))

(declaim (ftype (function (string string) *) prettyprint-dedukti))
(defun prettyprint-dedukti (theoryref out)
  "Print theory THEORYREF to output file OUT (which must be an absolute file
path)."
  (with-pvs-file (fname thname) theoryref
    (let ((*no-comments* nil)
          (*xt-periods-allowed* t))
      (let* ((theory (get-typechecked-theory (or thname fname)))
             (*current-context* (saved-context theory)))
        (to-dk3 theory out)))))
