(defun load-personoj (&optional pth)
  "Load Personoj. If given, PTH is the path to the root of the repository
  containing the PVS patches and the Lambdapi/Dedukti encodings. Otherwise, the
  root is taken from the environment variable PERSONOJPATH."
  (let* ((pth (cond
                (pth (uiop:ensure-pathname pth :ensure-directory t))
                ((uiop:getenvp "PERSONOJPATH")
                 (uiop:getenv-pathname "PERSONOJPATH" :ensure-directory t))
                (t (error "Cannot load personoj: PERSONOJPATH not set."))))
         (pth (merge-pathnames #P"pvs_patches/" pth)))
    (uiop:with-current-directory (pth)
      (load "pp-lp.lisp"))))
