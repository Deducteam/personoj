;;; This module provides a compiler from PVS recursive functions to Dedukti
;;; recursors using System T like recursors.

(in-package :pvs)

;; Recursive functions in PVS are always defined using an IF THEN ELSE or a
;; CASES construct. The decreasing argument can thus be found in the condition
;; of the IF and in the measure given.

(defgeneric nat-rec-p (meas ex))
(defmethod nat-rec-p ((meas name-expr) ex)
  "Look if the expression EX with decreasing measure MEAS destructures a natural
number. If so, the destructured variable is returned."
  (declare (ignore ex))
  (if (natural-expr? meas) meas))

(declaim (ftype (function (stream * expr * type-expr) *) pp-dk-recursor))
(defun pp-dk-recursor (s id ex m ty)
  "Print the term TE as a recursive definition of function ID of type TY with
measure M on stream S."
  (acond
    ((nat-rec-p m ex) (pp-nat-recursor s id ex it ty))
    (t
     (format *error-output*
             "Unknown measure [~a] (of class ~a, type ~a)" m
             (class-of m)
             (type m))
     (error "faiil"))))

(defgeneric recursive-call-p (id ex)
  (:documentation "Return true if expression EX is a recursive call to function
whose identifier is ID.")
  (:method (id ex)
    (declare (ignore id ex))
    nil))
(defmethod recursive-call-p (id (ex application))
  (equalp (id (operator* ex)) id))

(declaim (ftype (function (stream symbol expr name type-expr) *) pp-nat-recursor))
(defun pp-nat-recursor (s id te dec ty)
  "Print a recursive function with recursion on a natural number. Is usually of
the form IF n = 0 THEN base ELSE e where N is the measure. DEC is the
destructured argument."
  (let* ((base (then-part te))
         (rec (gensubst (else-part te)
                        (lambda (&rest args)
                          (declare (ignore args))
                          (make-new-variable-name-expr '|rec| *naturalnumber*))
                        (lambda (te) (recursive-call-p id te)))))
    (format s "nrec {~/pvs:pp-dk/} ~:/pvs:pp-dk/ ~:/pvs:pp-dk/ ~
(λ (n: Nat) (rec: ~/pvs:pp-type/), ~/pvs:pp-dk/)"
            ty dec base ty rec)))
