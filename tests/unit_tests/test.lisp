;;;
;;; Unit tests for PRETTYPRINT-LAMBDAPI
;;;
(in-package #:pvs)

(defparameter *theories*
  '("naturals"
    "simple"
    "eqtype"
    "decvar"
    "import"
    "call_import"
    "prenex"
    "thyp_val"
    "thyp_type_val"
    "mixedup"
    "apply_theories"
    "subtypethy"
    "NatArray"
    "vector"
    "tuples"
    "tup_patmatch"
    "linear_form"
    "tfrom"
    "instantiate_subtype_formal"
    "constant_param"
    "exprastype"
    "depsubtype"
    "expandeddefs"
    "letin")
  "List of theories in the file `simple.pvs`.")

(defun promote (theory)
  "Promote the actual result of the translation of THEORY as the expected
result."
  (let ((out (format nil "~a.lp" theory))
        (expected (format nil "~a.lp.expected" theory)))
   (uiop:run-program `("cp" "-f" ,out ,expected))))

(defun runtest (theory &optional non-interactive-p)
  "Translate the theory THEORY from the file `simple.pvs` to the file
`THEORY.lp` and compare its output with `THEORY.lp.expected`. If there's a
difference and NON-INTERACTIVE-P is NIL, ask the user to promote the current
output as new expectation (i.e. replace `THEORY.lp.expected` with the current
output). If NON-INTERACTIVE-P is true and there's a difference, the process
exits with status code 1."
  (let* ((*suppress-msg* t)
         (source (namestring (uiop:truename* "./simple.pvs")))
         (thyref (format nil "~a#~a" source theory))
         (out (format nil "~a.lp" theory))
         (expected (format nil "~a.lp.expected" theory)))
    (prettyprint-lambdapi thyref out t)
    (handler-case
        (uiop:run-program `("diff" "-u" "--color=always" ,expected ,out)
                          :output t :error-output t)
      (uiop:subprocess-error (err)
        (declare (ignore err))
        (when non-interactive-p
          (uiop:quit 1))
        (when (y-or-n-p "Promote ~S?" out)
          (promote theory))))))

(defun runall (&key (theories *theories*) non-interactive-p)
  "Run RUNTEST on all theories in the list THEORIES. NON-INTERACTIVE-P is used
as argument to RUNTEST."
  (mapc (lambda (th) (runtest th non-interactive-p)) theories))
