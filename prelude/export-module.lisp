(defun theory-select (src command &optional (stream *standard-output*))
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
    (loop for theory in (cdr (assoc :theories (json:decode-json s))) do
      (let ((name (cdr (assoc :name theory)))
            (disabledp (cdr (assoc :disabled theory))))
        (cond
          ((and (eq command :disabled) disabledp)
           (format stream "~&~a" name))
          ((and (eq command :enabled) (not disabledp)
                (format stream "~&~a" name)))
          ((eq command :all) (format stream "~&~a" name))))))
  (fresh-line stream))

(defun prelude-export (theory out &optional without-proof)
  "Export theory THEORY from prelude to file OUT."
  (with-open-file
      (s (uiop:parse-unix-namestring out) :direction :output
                                          :if-exists :supersede
                                          :if-does-not-exist :create)
    ;; Let it crash and stay in debugger.
    (pp-dk s (get-theory theory) without-proof)
    (format t "~&Theory ~a translated.~%" theory))
  (sb-ext:exit))
