;;;
;;; This script sets up PVS to allow it to call functions from personoj. To
;;; use it, (load "tools/personoj.lisp") from the root of the repository.
;;;
(in-package #:cl-user)

(with-open-file
    (pvs.lisp #P"~/.pvs.lisp" :direction :output :if-exists :append :if-does-not-exist :create)
  (format pvs.lisp ";;; The following lines were added by personoj~%")
  (with-open-file
      (load-personoj #P"tools/load-personoj.lisp" :direction :input :if-does-not-exist :error)
    (uiop:copy-stream-to-stream load-personoj pvs.lisp :linewise t))
  (format pvs.lisp "~%(load-personoj \"~a\")~%" (sb-posix:getcwd))
  (format pvs.lisp ";;; The previous lines were added by personoj~%"))

;; Select a suitable make binary
(defvar *make*
  (case (uiop:operating-system)
    (:LINUX "bmake")
    (otherwise "make")))

;; NOTE: uiop:with-current-directory does not work
(uiop:run-program (list *make* "-C" "encoding" "install")
                  :output *standard-output*)

(sb-ext:quit)
