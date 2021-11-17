;; Add hooks to transform proofstates into Dedukti propositions and print them
;; as JSON object.

(in-package :pvs)

#+allegro
(defun prefix-no-dot (s)
  "Take the longest prefix of S without dot."
  (car (excl:split-re "\\." s)))

#-allegro
(require :cl-ppcre)
#-allegro
(defun prefix-no-dot (s)
  "Take the longest prefix of S without dot."
  (car (cl-ppcre:split "\\." s)))

;; prefix-no-dot is used to retrieve the name of the formula declaration from a
;; proofstate.

;; (neg-s-forms seq) to get the antecedants
;; (pos-s-forms seq) to get the succedents
;; (collect-skolem-constants) gathers all skolem constant declarations
;; (remove-skolem-constants expr) seem to universally quantify over skolem constant declarations

(defun pprint-ps-dk (ps &optional (stream *standard-output*))
  "Transform the proofstate PS into a Dedukti proposition."
  (with-slots ((goal current-goal) (ctx context) (crule current-rule)) ps
    (let* ((forms (mapcar #'formula (s-forms goal)))
           (formula (make!-disjunction* forms)))
      (format stream "Prf ~:/pvs:pp-dk/" formula))))

(defparameter *ps-counter* 0
  "Number of proofstates encountered. Used to create identifiers.")

(defmacro incr (var)
  "The resulting form increments VAR and returns its value (post incrementation)."
  `(setf ,var (1+ ,var)))

(defun pprint-path (ps &optional (stream *standard-output*))
  "Print the path from the top state to proof state PS on stream STREAM."
  (let ((pth (path-from-top ps)))
    (if (endp pth)
        (princ "root" stream)
        (format stream "~{~a~^.~}" pth))))

;; Remove messages of PVS

(setq *suppress-msg*        t)
(setq *suppress-printing*   t)
(setq *pvs-message-hook*  nil)
(setq *prover-commentary* nil)

(defun pprint-ps (ps &optional (stream *standard-output*))
  "Print the proofstate PS as a JSON object on stream STREAM

{ \"name\": name of the proposition,
  \"incr\": a unique integer to distinguish the sequents between them,
  \"path\": the path from the root to the current proofstate,
  \"dk\": the proposition in Dedukti,
  \"tac\": some information on the tactic used }"
  (let ((elts
          `((name . ,(prefix-no-dot (label ps)))
            (incr . ,(incr *ps-counter*))
            (path . ,(with-output-to-string (s) (pprint-path ps s)))
            (dk   . ,(with-output-to-string (s) (pprint-ps-dk ps s)))
            (tac  . ,(wish-current-rule ps))))
        (json:*lisp-identifier-name-to-json* #'identity))
    (fresh-line stream)
    (json:encode-json elts stream)))

(pushnew #'pprint-ps *proofstate-hooks*)
(pushnew #'pprint-ps *success-proofstate-hooks*)
