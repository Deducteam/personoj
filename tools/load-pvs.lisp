(defun load-pvs (&optional pvspath)
  "Load PVS into the current lisp environment. Argument PVSPATH is the filespec
to the sources of PVS. If not provided, it is fetched from the environment
variables PVSPATH."
  (let ((pvspath
          (cond
            (pvspath (uiop:ensure-pathname pvspath))
            ((uiop:getenvp "PVSPATH") (uiop:getenv-pathname "PVSPATH"))
            (t (error "Cannot load PVS: PVSPATH not set.")))))
    (uiop:with-current-directory (pvspath)
      (load "pvs.system")
      (uiop:symbol-call :make "OPERATE-ON-SYSTEM" :pvs :load))))
