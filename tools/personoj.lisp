;;;
;;; This script sets up PVS to allow it to call functions from personoj. To
;;; use it, (load "tools/personoj.lisp") from the root of the repository.
;;;
(in-package #:cl-user)

(unless (string-equal (car (last (pathname-directory (uiop:getcwd))))
                      "personoj")
  (format *error-output*
          "This script should be run from the root of the personoj repository.")
  (uiop:quit 1))

;; TODO check if the lines have already been appended

(if (yes-or-no-p "Can I edit ~~/.pvs.lisp?")
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
  ;; TODO check if the binary exists
  (case (uiop:operating-system)
    (:LINUX "bmake")
    (otherwise "make")))

(defun install-lp-encoding ()
  ;; NOTE: uiop:with-current-directory does not work
  (uiop:run-program (list *make* "-C" "encoding" "install")
                    :output *standard-output*))

(multiple-value-bind (out err ret)
    (uiop:run-program "command -v lambdapi" :ignore-error-status t)
  (declare (ignore out))
  (declare (ignore err))
  (if (= ret 0)
      (install-lp-encoding)
      (format *error-output*
              "Cannot find executable \"lambdapi\", skipping encoding installation.~%")))

(uiop:quit)
