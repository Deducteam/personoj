(in-package #:pvs)

(defun theory-select (src)
  "Read a a JSON SRC that describe the content of a PVS file and outputs theory
names on STREAM.  The input must be of the form

{ \"source\": string, \"theories\": [TH1, ...] }

where the source is the name of the PVS file containing the theories, and each
THi is an object of the form

{ \"name\": string, \"disabled\": bool }

where NAME is the name of a theory and DISABLED is true if the theory should not
be exported. The key/value pair DISABLED is optional, and `nil' by default.

If COMMAND is `:disabled' then all theories for which DISABLED is `t' are
printed. If COMMAND is `:enabled', all theories for which DISABLED is `nil' are
printed. If COMMAND is `:all', all theories are printed."
  (with-open-file (s src)
    (loop for theory in (cdr (assoc :theories (json:decode-json s))) collect
          `(:name ,(cdr (assoc :name theory))
            :disabledp ,(cdr (assoc :disabled theory))
            :with-proof-p ,(cdr (assoc :with-proof theory))))))

(defun runtest
    (name
     &key disabledp without-proof-p if-exists lp-out lp-err
       (lp-flags '("--gen-obj" "-w")))
  "Translate and typecheck theory NAME.  If DISABLEDP is true, then translate as
an empty theory that can still be loaded. If WITHOUT-PROOF-P is true, proofs are
not exported. Argument IF-EXISTS behaves as in `open' and concerns the creation
of the translated file. LP-OUT and LP-ERR control the `:output' and
`:error-output' of the process calling lambadpi. LP-FLAGS may contain parameters
to be passed to lambdapi."
  (let ((out (format nil "~a.lp" name))
        (script (format nil "~a.lp.sh" name)))
    (with-open-file (s out :direction :output :if-exists if-exists
                           :if-does-not-exist :create)
      (if disabledp
          (format s "// Dummy theory~&")
          (pp-dk s (get-theory name) without-proof-p)))
    (when (uiop:file-exists-p script)
      (uiop:run-program `("sh" ,script)))
    (uiop:run-program `("lambdapi" "check" ,@lp-flags ,out)
                      :output lp-out :error-output lp-err)))

(defun runall (&rest test-pairs &key (json "theories.json") &allow-other-keys)
  (mapc (lambda (thy)
          (apply #'runtest
                 (getf thy :name)
                 :disabledp (getf thy :disabledp)
                 :allow-other-keys t
                 test-pairs))
        (theory-select json)))
