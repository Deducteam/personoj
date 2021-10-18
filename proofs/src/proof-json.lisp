;;; JSON creation: output proofstates as json structures.

(in-package :pvs)

;; Note that this has the side effect of setting the view of the sform,
;; Which is a cons of the string and its view (computed lazily).
(defun pvs2json-sform (sform fnum par-sforms)
  (let* ((nf (formula sform))
         (frm (if (negation? nf) (args1 nf) nf)))
    (unless (view sform)
      (multiple-value-bind (frmstr frmview)
          (pp-with-view frm *proofstate-indent* *proofstate-width*)
        (setf (view sform) (list frmstr frmview))))
    (let ((names-info (names-info-proof-formula sform)))
      `(("labels" . ,(cons fnum (label sform)))
        ("changed" . ,(if (memq sform par-sforms) "false" "true"))
        ("formula" . ,(car (view sform)))))))

(defun pvs2json-sforms (sforms neg? par-sforms)
  (let ((c 0))
    (mapcar #'(lambda (sf)
                (let* ((fnum (if neg? (- (incf c)) (incf c))))
                  (pvs2json-sform sf fnum par-sforms)))
            sforms)))

(defmethod pvs2json-seq (seq parent-ps)
  (let* ((par-sforms (when parent-ps
                       (s-forms (current-goal parent-ps))))
         (hidden-s-forms (hidden-s-forms seq))
         (hn-sforms (neg-s-forms* hidden-s-forms))
         (hp-sforms (pos-s-forms* hidden-s-forms)))
    (make-seqstruct
     :antecedents (pvs2json-sforms (neg-s-forms seq) t par-sforms)
     :succedents (pvs2json-sforms (pos-s-forms seq) nil par-sforms)
     :hidden-antecedents (pvs2json-sforms hn-sforms t par-sforms)
     :hidden-succedents (pvs2json-sforms hp-sforms nil par-sforms)
     :info nil)))

(defmethod pvs2json ((ps proofstate))
  (with-slots (label comment current-goal (pps parent-proofstate)) ps
    (let ((sequent (pvs2json-seq current-goal pps))
          (prev-cmd (let ((wish-rule (wish-current-rule ps))
                          (parent-ps (parent-proofstate ps)))
                      (cond (wish-rule (format nil "~s" wish-rule))
                            (parent-ps (format nil "~s" (current-rule parent-ps)))
                            (t nil)))))
      `(("label" . ,label)
        ,@(when prev-cmd `(("prev-cmd" . ,prev-cmd)))
        ("path" . ,(format nil "~{~a~^.~}" (path-from-top ps)))
        ("sequent" . ,sequent)))))

(setq *suppress-msg* t)
(setq *suppress-printing* t)
(setq *pvs-message-hook* nil)

(defun rerun-prove (decl)
  (if (and *noninteractive*
           (integerp *pvs-verbose*)
           (> *pvs-verbose* 2))
      (let ((*suppress-printing* t) ;; <- printing supressed even when *noninteractive* and verbose
            (*proving-tcc* t))
        (prove-decl decl :strategy `(then (then (rerun) (postpone)) (quit))))
      (let ((*suppress-printing* t)
            (*printproofstate* nil)
            (*proving-tcc* t))
        (prove-decl decl :strategy `(then (then (rerun) (postpone)) (quit))))))

(defun output-json-proofstate-to-stream (ps)
  (let* ((json:*lisp-identifier-name-to-json* #'identity)
         (ps-json (pvs:pvs2json ps)))
    (format t "~&~a~%" (json:encode-json-to-string ps-json))
    (setq *prover-commentary* nil)))

(pushnew 'output-json-proofstate-to-stream *proofstate-hooks*)
