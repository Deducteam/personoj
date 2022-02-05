(in-package :pvs)

(export '(double mkstr symb aif))
(proclaim '(inline double))

(defun double (lst)
  (and (consp lst) (singleton? (cdr lst))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defmacro aif (test-form then-form &optional else-form)
  "Anaphoric `if' using IT as the result of the test form."
  (let ((it (intern (symbol-name 'it)))) ;so that the macro can be exported
    `(let ((,it ,test-form))
       (declare (ignorable ,it))
       (if ,it ,then-form ,else-form))))
