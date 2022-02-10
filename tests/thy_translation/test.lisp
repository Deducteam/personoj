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
    "letin"))

(defun promote (theory)
  "Promote the actual result of the translation of THEORY as the expected
result."
  (let ((out (format nil "~a.lp" theory))
        (expected (format nil "~a.lp.expected" theory)))
   (uiop:run-program `("cp" "-f" ,out ,expected))))

(defun runtest (theory &optional non-interactive-p)
  "Translate the THEORY from the file `simple.pvs`. If the resulting file
`THEORY.lp` differs from `THEORY.lp.expected`, prompt the user to promote the
resulting file. If NON-INTERACTIVE-P is true, there is no prompt and the process
exits sbcl with status code 1."
  (let* ((*suppress-msg* t)
         (source (namestring (uiop:truename* "./simple.pvs")))
         (thyref (format nil "~a#~a" source theory))
         (out (format nil "~a.lp" theory))
         (expected (format nil "~a.lp.expected" theory)))
    (prettyprint-dedukti thyref out t)
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
  (mapc (lambda (th) (runtest th non-interactive-p)) theories))
