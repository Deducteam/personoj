(in-package :dklog)

(defvar *log-file* "/tmp/pp-dk3.log" "File used for logging and debugging.")

(defun dk-log (tag format-str &rest args)
  "Like format *log-file* FORMAT-STR ARGS adding timestamp, informative tag TAG
at the beginning of line and terminating line."
  (with-open-file
      (out *log-file* :direction :output :if-exists :append
                      :if-does-not-exist :create)
    (multiple-value-bind (second minute hour date month year dow dst-p tz)
        (get-decoded-time)
      (declare (ignore date month year dow dst-p tz))
      (format out "[~d:~d:~d] " hour minute second))
    (if tag (format out "[~a] " tag))
    (let ((pvs::*pp-compact* t)
          (pvs::*pp-no-newlines?* t))
     (apply #'format out format-str args))
    (terpri out)))

(defun top (format-str &rest args)
  (apply #'dk-log nil format-str args))
(defun sign (format-str &rest args)
  (apply #'dk-log "sign" format-str args))
(defun decl (format-str &rest args)
  (apply #'dk-log "decl" format-str args))
(defun expr (format-str &rest args)
  (apply #'dk-log "expr" format-str args))
(defun type (format-str &rest args)
  (apply #'dk-log "type" format-str args))
