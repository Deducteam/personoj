(in-package :pvs)

(defun pp-path (stream ps &optional colon-p at-sign-p)
  "Print the path from the top state to proof state PS on stream STREAM."
  (declare (ignore colon-p at-sign-p))
  (let ((pth (path-from-top ps)))
    (if (endp pth)
        (princ "root" stream)
        (format stream "~{~a~^.~}" pth))))
