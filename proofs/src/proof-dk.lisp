(in-package :pvs)

(defun ps->dk (ps)
  (with-slots ((goal current-goal) (lab label) (pps parent-proofstate)
               (ctx context) (crule current-rule)) ps
    (let* ((name (format nil "{|~a ~/pvs:pp-path/|}" lab ps))
           (forms (mapcar #'formula (s-forms goal)))
           (formula (make!-disjunction* forms)))
      (format t "Translating~%")
      (format t "symbol ~a: Prf ~:/pvs:pp-dk/;" name formula))))

(pushnew #'ps->dk *proofstate-hooks*)
(pushnew #'ps->dk *success-proofstate-hooks*)
