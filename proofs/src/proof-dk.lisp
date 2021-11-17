;; Use `pp-dk' to print the sequent of proof states as Dedukti propositions.
(in-package :pvs)

(defun ps->dk (ps)
  "Transform the proofstate PS to a proposition."
  (with-slots ((goal current-goal) (ctx context) (crule current-rule)) ps
    (let* ((forms (mapcar #'formula (s-forms goal)))
           (formula (make!-disjunction* forms)))
      (format nil "Prf ~:/pvs:pp-dk/" formula))))
