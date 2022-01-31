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
