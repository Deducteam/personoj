;; Use `pp-dk' to print the sequent of proof states as Dedukti propositions.
(in-package :pvs)

(defparameter *counter* 0 "Number of sequents generated.")

(defun ps->dk (ps)
  (with-slots ((goal current-goal) (lab label) (pps parent-proofstate)
               (ctx context) (crule current-rule)) ps
    (let* ((name (format nil "{|~a~32r|}" lab (setf *counter* (1+ *counter*))))
           (forms (mapcar #'formula (s-forms goal)))
           (formula (make!-disjunction* forms)))
      (pprint-record *standard-output* "dk" "symbol ~a: Prf ~:/pvs:pp-dk/;" name formula))))
