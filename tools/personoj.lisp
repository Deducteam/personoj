;;;
;;; This script sets up PVS to allow it to call functions from personoj. To
;;; use it, (load "tools/personoj.lisp") from the root of the repository.
;;;
(in-package #:cl-user)

;; Fail on debugger
(setf *debugger-hook*
      (lambda (c h)
        (declare (ignore h))
        (format *error-output* "Something went wrong during the install procedure.~
You may report it.")
        (princ c *error-output*)
        (uiop:quit 1)))

(unless (string-equal (car (last (pathname-directory (uiop:getcwd))))
                      "personoj")
  (error "This script should be run from the root of the personoj repository."))

;; If prettyprint-lambdapi isn't available, it means personoj isn't loaded
(unless (handler-case (and (find-package 'pvs)
                           (functionp (find-symbol "prettyprint-lambdapi" :pvs)))
          (undefined-function (c)
            (declare (ignore c))
            nil))
  (format *query-io* "Personoj not loaded, setting up autoload.")
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
        (format pvs.lisp ";;; The previous lines were added by personoj~%"))))

;; Select a suitable make binary
(defvar *make*
  (case (uiop:operating-system)
    (:LINUX "bmake")
    (otherwise "make")))

(defun binary-exists-p (bin)
  (multiple-value-bind (out err ret)
      (uiop:run-program (list "which" bin) :ignore-error-status t)
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
    (error "Cannot find executable \"lambdapi\", skipping encoding installation."))
(uiop:quit)
