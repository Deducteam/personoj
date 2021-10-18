;;;; Call (load-personoj) from ~/.pvs.lisp to load Personoj automatically into
;;;; PVS

(defmacro load-personoj (&optional pth)
  "Load Personoj specification exporter and proof exporter. If given, PTH is the
path to the root of the repository. Otherwise, the root is taken from the
environment variable PERSONOJ"
  (let ((pth (cond
               (pth pth)
               ((uiop:getenvp "PERSONOJPATH")
                (uiop:getenv-pathname "PERSONOJPATH" :ensure-directory t))
               (t (error "Cannot load personoj: PERSONOJPATH not set.")))))
    `(uiop:with-current-directory (,pth)
       (load "load.lisp"))))
