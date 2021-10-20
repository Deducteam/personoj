(defun load-personoj (&optional pth)
  "Load Personoj specification exporter and proof exporter. If given, PTH is the
path to the root of the repository. Otherwise, the root is taken from the
environment variable PERSONOJPATH."
  (let ((pth (cond
               (pth (uiop:ensure-pathname pth :ensure-directory t))
               ((uiop:getenvp "PERSONOJPATH")
                (uiop:getenv-pathname "PERSONOJPATH" :ensure-directory t))
               (t (error "Cannot load personoj: PERSONOJPATH not set.")))))
    (uiop:with-current-directory (pth)
      (load "load.lisp"))))
