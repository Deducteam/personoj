(in-package :pvs)

(export '(double mkstr symb))
(proclaim '(inline double))

(defun double (lst)
  (and (consp lst) (singleton? (cdr lst))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))
