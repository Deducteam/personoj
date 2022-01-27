(in-package :pvs)

(export '(append1 double mkstr symb lrec aif acond))
(proclaim '(inline append1 double))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun double (lst)
  (and (consp lst) (singleton? (cdr lst))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun lrec (rec &optional base)
  "List recursor, because `reduce' seem to be suited to operator taking two
arguments of the same type. The base case BASE may be a function or a value, the
recursive case REC is a function of two arguments, the first is the `car' of the
list traversed and the second is a thunk that performs the recursion on its
`cdr'."
  (labels ((self (lst)
             (if (null lst)
                 (if (functionp base) (funcall base) base)
                 (funcall rec (car lst) (lambda () (self (cdr lst)))))))
    #'self))

(defmacro aif (test-form then-form &optional else-form)
  "Anaphoric `if' using IT as the result of the test form."
  (let ((it (intern (symbol-name 'it)))) ;so that the macro can be exported
    `(let ((,it ,test-form))
       (declare (ignorable ,it))
       (if ,it ,then-form ,else-form))))

(defmacro acond (&rest clauses)
  "Anaphoric `cond' that bind IT as the result of the test."
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (sym (gensym))
            (it (intern (symbol-name 'it))))
        `(let* ((,sym ,(car cl1)))
           (if ,sym
               (let ((,it ,sym))
                 (declare (ignorable ,it))
                 ,@(cdr cl1))
               (acond ,@(cdr clauses)))))))
