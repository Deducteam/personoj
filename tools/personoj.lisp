;;;
;;; This script sets up PVS to allow it to call functions from personoj. To
;;; use it, (load "tools/personoj.lisp") from the root of the repository.
;;;
(in-package #:cl-user)

;; Fail on debugger
(setf *debugger-hook*
      (lambda (c h)
        (declare (ignore c h))
        (uiop:quit 1)))

(unless (string-equal (car (last (pathname-directory (uiop:getcwd))))
                      "personoj")
  (error "This script should be run from the root of the personoj repository."))

;; TODO check if the lines have already been appended

(if (or (not (interactive-stream-p *standard-input*))
        (yes-or-no-p "Can I edit ~~/.pvs.lisp?"))
    (with-open-file
        (pvs.lisp #P"~/.pvs.lisp"
                  :direction :output
                  :if-exists :append
                  :if-does-not-exist :create)
      (format pvs.lisp ";;; The following lines were added by personoj~%")
      (with-open-file
          (load-personoj #P"tools/load-personoj.lisp"
                         :direction :input
                         :if-does-not-exist :error)
        (uiop:copy-stream-to-stream load-personoj pvs.lisp :linewise t))
      (format pvs.lisp "~%(load-personoj \"~a\")~%" (sb-posix:getcwd))
      (format pvs.lisp ";;; The previous lines were added by personoj~%")))

;; Select a suitable make binary
(defvar *make*
  (case (uiop:operating-system)
    (:LINUX "bmake")
    (otherwise "make")))

(defun binary-exists-p (bin)
  (multiple-value-bind (out err ret)
      (uiop:run-program (list "command" "-v" bin) :ignore-error-status t)
    (declare (ignore out err))
    (= ret 0)))

(unless (binary-exists-p *make*)
  (error "No suitable make command found. Please install BSD make."))

(defun install-lp-encoding ()
  ;; NOTE: uiop:with-current-directory does not work
  (uiop:run-program (list *make* "-C" "encoding" "install")
                    :output *standard-output*))

(if (binary-exists-p "lambdapi")
    (install-lp-encoding)
    (error "Cannot find executable \"lambdapi\", skipping encoding installation.~%"))
(uiop:quit)
