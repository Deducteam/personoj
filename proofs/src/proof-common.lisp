;; Common utilites for proof exports

(in-package :pvs)

(defun pp-path (stream ps &optional colon-p at-sign-p)
  "Print the path from the top state to proof state PS on stream STREAM."
  (declare (ignore colon-p at-sign-p))
  (let ((pth (path-from-top ps)))
    (if (endp pth)
        (princ "root" stream)
        (format stream "~{~a~^.~}" pth))))

(defun pprint-rec-sep (s)
  "Output a record jar separator %%."
  (format s "~&%%"))

(defun pprint-record (s label fmt &rest args)
  "Print format string FMT with arguments ARGS as passed to format t
fmt args but prepended by \"label:\", all that on stream S."
  (fresh-line s)
  (pprint-logical-block (s nil)
    ;; Indent by one space newlines to conform to record jar format
    (pprint-indent :current 1 s)
    (princ label s)
    (princ ": " s)
    (apply #'format s fmt args)))
