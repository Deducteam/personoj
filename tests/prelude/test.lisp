;;;
;;; Translate and typecheck the Prelude.
;;;
(in-package #:pvs)

(defun theory-select (src)
  "Read the JSON file SRC that describe the content of a PVS file and outputs
theory names on STREAM.  The input SRC must be of the form

{ \"source\": FILE, \"theories\": THEORIES }

where FILE is the name of the PVS file containing the theories, and THEORIES
is an array of theories, where each theory is of the form

{ \"name\": THEORY, \"disabled\": DISABLED }

where THEORY is the name of a Prelude theory and DISABLED is true if the theory
should *not* be exported. The key/value pair DISABLED is optional, and `nil' by
default.

The argument COMMAND may be
- :DISABLED, all theories for which DISABLED is true are printed
- :ENABLED, all theories for which DISABLED is NIL are printed
- :ALL, all theories are printed."
  (with-open-file (s src)
    (loop for theory in (cdr (assoc :theories (json:decode-json s))) collect
          `(:name ,(cdr (assoc :name theory))
            :disabledp ,(cdr (assoc :disabled theory))))))

(defun runtest
    (name
     &key disabledp without-proof-p if-exists no-check-p lp-out lp-err
       (lp-flags '("--gen-obj" "-w")))
  "Translate theory NAME from the prelude to `NAME.lp`, then execute the file
`NAME.lp.sh` if it is present and finally, if NO-CHECK-P is not set, typecheck
the translation running `lambdapi check` on `NAME.lp`. If DISABLEDP is true,
ignore the theory and create an empty file.  Proofs are translated unless
WITHOUT-PROOF-P is true.  Argument IF-EXISTS is passed to OPEN when creating
`NAME.lp`.

The binary lambdapi is called using UIOP:RUN-PROGRAM with parameters :OUTPUT set
to LP-OUT and :ERROR-OUTPUT set to LP-ERR. LP-FLAGS may contain flags passed to
`lambdapi check`."
  (let ((out (format nil "~a.lp" name))
        (script (format nil "~a.lp.sh" name)))
    (with-open-file (s out :direction :output :if-exists if-exists
                           :if-does-not-exist :create)
      (if disabledp
          (format s "// Dummy theory~&")
          (pp-dk s (get-theory name) without-proof-p)))
    (when (uiop:file-exists-p script)
      (uiop:run-program `("sh" ,script)))
    (unless no-check-p
      (handler-case
          (uiop:run-program `("lambdapi" "check" ,@lp-flags ,out)
                            :output lp-out :error-output lp-err)
        (uiop:subprocess-error (c)
          (declare (ignore c))
          (format *error-output* "Failed to type check theory ~a." name)
          (uiop:quit 1))))))

(defun runall (&rest test-pairs &key (json "theories.json") &allow-other-keys)
  "Test theories as specified in the JSON. Additional keyword arguments are
transmitted to RUNTEST."
  (mapc (lambda (thy)
          (apply #'runtest
                 (getf thy :name)
                 :disabledp (getf thy :disabledp)
                 :allow-other-keys t
                 test-pairs))
        (theory-select json))
  (uiop:quit 0))
