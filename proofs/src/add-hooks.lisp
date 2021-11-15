;; Add hooks to PVS to actually use the functions

(defun add-ps-hook (fun)
  (pushnew fun *proofstate-hooks*)
  (pushnew fun *success-proofstate-hooks*))
(add-ps-hook (lambda (&rest args)
                (declare (ignore args))
                (pprint-rec-sep *standard-output*)))
(add-ps-hook #'output-json-proofstate-to-stream)
(add-ps-hook #'ps->dk)
(add-ps-hook (lambda (ps)
               (pprint-record *standard-output* "path"
                              "~/pvs:pp-path/" ps)))
