(defun prelude-export (theory out)
  "Export theory THEORY from prelude to file OUT."
  (let ((mod (gethash (intern theory :pvs) *prelude*)))
    (with-open-file
        (s (uiop:parse-unix-namestring out) :direction :output
                                            :if-exists :supersede
                                            :if-does-not-exist :create)
      (handler-bind
          ((simple-error
             (lambda (err)
               (format *error-output* "~&Translation of theory ~a failed:~&~a." theory err)
               (sb-ext:exit :code 1))))
        (pp-dk s mod)
        (format *standard-output* "~&Theory ~a translated.~%" theory))))
  (sb-ext:exit))
