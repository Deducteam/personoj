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
  (let ((out (format nil "~a.lp" theory))
        (expected (format nil "~a.lp.expected" theory)))
   (uiop:run-program `("cp" "-f" ,out ,expected))))

(defun runtest (theory)
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
        (unless non-interactive-p
          (uiop:quit 1))
        (when (y-or-n-p "Promote ~S?" out)
          (promote theory))))))

(defun runall (&key (theories *theories*) non-interactive-p)
  (mapc #'runtest theories))
