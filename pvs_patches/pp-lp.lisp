;;;
;;; Export to Dedukti.
;;;
;;; This module provides the function ‘pp-lp’ exports PVS structures to
;;; Dedukti.
;;;

(in-package :pvs)

(export '(prettyprint-lambdapi))

;;;
;;; Missing features:
;;;
;;; TODO recursive functions
;;; TODO inductive types
;;; TODO records
;;; TODO abstract datatypes
;;; TODO properly handling bounded quantification

(defparameter *without-proofs* nil
  "If true, do not print proofs.")

(defparameter *var-count* 0
  "Number of generated variables. Used to create fresh variable names.")

;;; Utilities

(proclaim '(inline double))

(defun double (lst)
  (and (consp lst) (singleton? (cdr lst))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

;;; Printing macros

(defmacro let-duet ((x y) lst &body body)
  "Bind variables X and Y to be the `car' and the `cadr' respectively of list
LST into BODY."
  `(let ((,x (car ,lst))
         (,y (cadr ,lst)))
     ,@body))

(defmacro with-parens ((stream &optional (wrap t)) &body body)
  "Wraps body BODY into parentheses (printed on stream STREAM) if WRAP is true."
  `(progn
     (when ,wrap (format ,stream "("))
     ,@body
     (when ,wrap (format ,stream ")"))))

(defmacro wrap ((&key (stream '*standard-output*) impl) &body body)
  `(progn
     (princ (if ,impl #\[ #\() ,stream)
     ,@body
     (princ (if ,impl #\] #\)) ,stream)))

(defmacro with-comment (stream &body body)
  `(progn
     (princ "/* " ,stream)
     ,@body
     (princ " */" ,stream)))

(defmacro with-binapp-args ((larg rarg binapp) &body body)
  "Binds the left (resp. right) argument of binary application BINAPP to LARG
(resp. RARG) in body BODY."
  `(destructuring-bind (,larg ,rarg &rest _) (exprs (argument ,binapp))
     (declare (ignore _))
     ,@body))

(defgeneric tag (thing)
  (:documentation "Get a tagged identifier out of THING. The tag allows to
differentiate several resolutions (typically to resolve overloading)."))

(defmethod tag ((thing declaration))
  (let ((index (xml-declaration-index thing)))
    (if (= 0 index)
        (id thing)
        (symb (id thing) #\! index))))

(defmethod tag ((thing name))
  (with-slots (id resolutions) thing
    (assert (singleton? resolutions))
    (let ((decl (declaration (car resolutions))))
      (if (and (module decl)
               (memq decl (all-decls (module decl))))
          (tag decl)
          id))))

;;; Some definitions and functions

(defparameter +lp-syms+
  '((|boolean| . "prop") (|bool| . "prop") (true . "true") (false . "false")
    (|type| . "Set" )
    (|restrict| . "restrict") (|extend| . "extend")
    ;; We add equality to avoid printing it as "equalities.="
    (|=| . "="))
  "Maps PVS names to names of the encoding. It is also used to avoid prepending
the symbols with a module id.")

(defun fresh-var (&key (prefix ""))
  "Provide a fresh variable name."
  (let ((var-name (format nil "_~av~36r" prefix *var-count*)))
    (incf *var-count*)
    var-name))

(defgeneric fundomains (ex)
  (:documentation "Return the list of domains of EX if it is a type or of its
type."))

(defmethod fundomains ((ty funtype))
  (cons (domain ty) (fundomains (range ty))))

(defmethod fundomains ((ty type-expr))
  nil)

(defmethod fundomains ((ex expr))
  (fundomains (type ex)))

(defgeneric cast-required-p (should is)
  (:documentation "Return T if a cast is required from type IS to
SHOULD. Casts are required when a there is a formal subtype declaration [S:
TYPE FROM T]. In that case, we have no syntactic information to insert coercions
properly so we rely on an abstract cast operator."))

(defmethod cast-required-p :around (should is)
  (assert (and should is))
  (unless (tc-eq should is)
    (call-next-method)))

(defmethod cast-required-p ((should dep-binding) is)
  (cast-required-p (type should) is))

(defmethod cast-required-p (should (is dep-binding))
  (cast-required-p should (type is)))

(defmethod cast-required-p (should (is type-name))
  (declare (ignore should))
  (let ((resolution (car (resolutions is))))
    (formal-subtype-decl? (declaration resolution))))

(defmethod cast-required-p (should (is subtype))
  ;; In some cases, an a type such as (surjective?[S,R]) is typechecked as a
  ;; `subtype', with an `expr-as-type' as `print-type'. In that case, we don't
  ;; want to look at the `expr-as-type'
  (or (and (print-type is) (type-name? (print-type is))
           (cast-required-p should (print-type is)))
      (cast-required-p should (supertype is))))

(defmethod cast-required-p ((should tupletype) (is tupletype))
  (some (lambda (c e) (cast-required-p c e))
        (types should) (types is)))

(defmethod cast-required-p ((should funtype) (is funtype))
  (cast-required-p (range should) (range is)))

(defmethod cast-required-p (should is)
  "Default case is to ignore and don't cast."
  (declare (ignore should is))
  nil)

;;; Formals and parameters
;;;
;;; Formals appear in theories as well as declarations. In a declaration
;;; g(x,y)(z) = e, the formals are given as a list of list ((x y) (z)).
;;; It is the same for parameters, in an expression g(e1, e2)(e3),
;;; the parameters can be retrieved by the `arguments' functions in utils.lisp.

(defun context-formals (&optional (context *current-context*))
  "Get the theory formals of context CONTEXT."
  (assert context)
  (formals-sans-usings (theory context)))

(defun arg-p (thing) (and (listp thing) (every #'expr? thing)))
(deftype arg () '(satisfies arg-p))

(declaim (ftype (function (arg) expr) normalise-arg))
(defun normalise-arg (arg)
  "Transform an argument ARG (or parameter, that is the term being applied to
something) into a proper term."
  (cond
    ((atom arg) arg)
    ((singleton? arg) (car arg))
    (t (make!-tuple-expr arg))))

;;; Printing functions

;; Declaring functions beforehand silences warnings due to load order
(declaim (ftype function pp-lp* pprint-proof pprint-proof*))

(defun pprint-cast (arg target-type &key (stream *standard-output*) wrap)
  "Print an abstract cast for ARG to TARGET-TYPE."
  (with-parens (stream wrap)
    (format stream
            ;; "cast ~:/pvs:pp-lp*/ ~:pvs:pp-lp*/ _ ~:/pvs:pp-lp*/"
            ;; arg-type target-type arg
            ;; HACK see the file `cast.lp' of the encoding
            "cast ~:/pvs:pp-lp*/ ~:/pvs:pp-lp*/ ~
(cast-proof ~:/pvs:pp-lp*/ ~:/pvs:pp-lp*/) ~:/pvs:pp-lp*/"
            (type arg) target-type (type arg) target-type arg)))

;;; Specialised printing functions

(defparameter +lp-id-forbidden+
  (list #\Newline #\Space #\Rubout #\Tab
        #\: #\. #\, #\; #\` #\/ #\\ #\| #\" #\@ #\$
        #\( #\) #\{ #\} #\[ #\])
  "List of characters that are forbidden inside Dedukti identifiers.")

(defun lp-id-char-p (thing)
  "True if THING is a character allowed in Dedukti identifiers."
  (and (characterp thing) (not (member thing +lp-id-forbidden+))))

(defgeneric pp-ident (s id &optional colon-p at-sign-p)
  (:documentation "Print identifier ID on stream S so that it can be parsed by
Dedukti (see https://lambdapi.readthedocs.io/en/latest/terms.html#identifiers
for valid identifiers)."))
(defmethod pp-ident (s (id string) &optional colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (if (every #'lp-id-char-p id)
      (princ id s)
      (format s "{|~a|}" id)))
(defmethod pp-ident (s (id symbol) &optional colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (aif (assoc id +lp-syms+)
       (princ (cdr it) s)
       (pp-ident s (mkstr id))))

;;; Printing bindings and formals as bindings

(defgeneric pp-binding (stream bd &optional colon-p at-sign-p)
  (:documentation "Print BD as a binding of the form (x: a) or [x: a] if COLON-P
is `t'. A dedicated function is used because `dep-binding' may be either printed
as types with `pp-lp*' or as bindings.."))

(defmethod pp-binding (stream (bd binding) &optional colon-p at-sign-p)
  (declare (ignore at-sign-p))
  (with-slots (id type) bd
    (wrap (:stream stream :impl colon-p)
      (format stream "~/pvs:pp-ident/: El ~:/pvs:pp-lp*/" id type))))

(defmethod pp-binding (stream (fm formal-type-decl) &optional colon-p at-sign-p)
  (declare (ignore at-sign-p))
  (wrap (:stream stream :impl colon-p)
    (format stream "~/pvs:pp-ident/: Set" (id fm))))

(defmethod pp-binding (stream (fm formal-subtype-decl) &optional colon-p at-sign-p)
  (declare (ignore at-sign-p))
  (wrap (:stream stream :impl colon-p)
    (format stream "~/pvs:pp-ident/: Set" (id fm))))

(defmethod pp-binding (stream (fm formal-const-decl) &optional colon-p at-sign-p)
  (declare (ignore at-sign-p))
  (with-slots (id type) fm
   (wrap (:stream stream :impl colon-p)
     (format stream "~/pvs:pp-ident/: El ~:/pvs:pp-lp*/" id type))))

(defun pprint-binders
    (bind-sym body bindings &key (stream *standard-output*) wrap impl)
  "Print the BODY with BINDINGS. BINDINGS may be a list or a single binding.
BODY is a thunk (or lazy computation). The first IMPL bindings are printed as
implicit bindings. The symbol BIND-SYM is used as binder."
  (if (null bindings)
      (funcall body)
      (with-parens (stream wrap)
        (format stream "~a " bind-sym)
        (let ((binding (or (and (listp bindings) (car bindings)) bindings))
              (implp (or (and (numberp impl) (> impl 0)) impl))
              (impl-next (or (and (numberp impl) (1- impl)) impl)))
          (pp-binding stream binding implp)
          (princ #\, stream)
          (pprint-binders bind-sym body (and (listp bindings) (cdr bindings))
                          :stream stream :impl impl-next)))))

(defmacro abstract-over
    ((bindings &key (stream '*standard-output*) wrap impl) &body body)
  "Abstract over BINDINGS as lambdas. If IMPL is a number, then the first IMPL
  bindings are implicits. If it evaluates to `t', then all bindings are
  implicit. Otherwise none of them is implicit."
  `(pprint-binders "λ" (lambda () ,@body) ,bindings
                   :stream ,stream
                   :wrap ,wrap
                   :impl ,impl))

(defmacro abstract-over@
    ((bindings &key (stream '*standard-output*) wrap impl) &body body)
  "Abstract over bindings as products. IMPL may be a number, nil or :all. If it
is :all, all bindings are implicits."
  `(pprint-binders "Π" (lambda () ,@body) ,bindings
                   :stream ,stream
                   :wrap ,wrap
                   :impl ,impl))

(defun pprint-quantification
    (quantification body bindings &key (stream *standard-output*) wrap)
  "Execute BODY quantifying over BINDINGS either universally if QUANTIFICATION
is `:all' or existentially if QUANTIFICATION is `:exists'."
  (if (endp bindings)
      (funcall body)
      (with-parens (stream wrap)
        (let ((x (car bindings))
              (qu-char (case quantification
                         (:all #\∀)
                         (:exists #\∃))))
          (format stream "~a [~/pvs:pp-lp*/] " qu-char (type x))
          (abstract-over (x :stream stream :wrap t)
            (pprint-quantification quantification body (cdr bindings)
                                   :stream stream))))))

(defmacro forall ((bindings &key (stream '*standard-output*) wrap) &body body)
  `(pprint-quantification :all (lambda () ,@body) ,bindings
                          :stream ,stream :wrap ,wrap))

(defmacro exists ((bindings &key (stream '*standard-output*) wrap) &body body)
  `(pprint-quantification :exists (lambda () ,@body) ,bindings
                          :stream ,stream :wrap ,wrap))

(defun pprint-formals (body formals &key (stream *standard-output*) wrap in-type)
  "Abstracts over formals FORMALS and finally call BODY. FORMALS is a list of
lists: each singleton list is simply abstracted over, each non singleton list
(more than one element) is considered as a pattern matched tuple: a new binding
is created that stand for that tuple, and the matching operator is used to bind
the components."
  (cond
    ((endp formals)
     (funcall body))
    ((and (consp formals) (singleton? (car formals)))
     (assert (expr? (caar formals)))
     (with-slots (id type) (caar formals)
       (abstract-over ((make-bind-decl id type) :stream stream :wrap wrap)
         (pprint-formals body (cdr formals) :stream stream))))
    ((consp formals)
     (assert (listp (car formals)))
     (let* ((bindings (mapcar (lambda (e) (mk-dep-binding (id e) (type e))) (car formals)))
            (btype (make-tupletype bindings))
            (fresh (make-new-bind-decl btype))
            ;; Make a fresh binding that stand for the tuple
            (matchop (if in-type "TL.match*" "TL.match")))
       (abstract-over (fresh :stream stream :wrap wrap)
         (format stream "~a ~/pvs:pp-ident/ " matchop (id fresh))
         (let ((bdecls (mapcar (lambda (fm)
                                 (make!-bind-decl (id fm) (type fm)))
                               (car formals))))
           (abstract-over (bdecls :stream stream :wrap t)
             (pprint-formals body (cdr formals) :stream stream :wrap t))))))))

(defmacro with-formals
    ((formals &key (stream '*standard-output*) wrap in-type) &body body)
  `(pprint-formals (lambda () ,@body) ,formals
                   :stream ,stream :wrap ,wrap :in-type ,in-type))

;;; Main printing

(defgeneric pp-lp* (stream obj &optional colon-p at-sign-p)
  (:documentation "Prints object OBJ to stream STREAM. This function can be used
in `format'. The colon modifier specifies whether arguments should be wrapped
into parentheses.")
  (:method (stream obj &optional colon-p at-sign-p)
    (declare (ignore stream colon-p at-sign-p))
    (error "Unexpected element: ~a of type ~a." obj (class-of obj))))

(defmethod pp-lp* (stream (thing null) &optional colon-p at-sign-p)
  (declare (ignore stream colon-p at-sign-p))
  (error "Invalid null argument passed to pp-lp*: ~a" thing))

(defmethod pp-lp* (stream (mod module) &optional colon-p at-sign-p)
  "Print the declarations of module MOD."
  (declare (ignore colon-p at-sign-p))
  (with-slots (id theory (fsu formals-sans-usings) saved-context) mod
    (assert saved-context)
    (let ((*current-context* saved-context))
      (format stream
              "~&require open personoj.lhol personoj.logical personoj.pvs_cert
personoj.eq personoj.restrict personoj.coercions;
require personoj.telescope as TL;
require personoj.extra.arity-tools as A;
require open personoj.nat;
require personoj.int as int;
symbol Int ≔ int.Int;
symbol int#o ≔ int.int#o;")
      (unless *without-proofs*
        (format stream "~&require personoj.proofs as P;"))
      ;; Could be opened only if there is a formal-subtype-decl
      (format stream "~&require open personoj.cast;")
      (format stream "~&// Theory ~a" id)
      (let ((prelude (mapcar #'id *prelude-theories*)))
        (loop for m in (list-upto prelude id) do
          (format stream "~&require pvs.prelude.~a as ~a;~%" m m)))
      ;; Loop through the declarations to print them.
      ;; NOTE that because the predicate of a `type-from-decl' appears after the
      ;; type of the definition, we need to switch these declarations.
      (loop with skip = nil             ;t to skip the declaration
            for decls on theory
            when skip
              do (setf skip nil)
            else
              do (if (type-from-decl? (car decls))
                     (progn
                       (setf skip t)    ;skip next declaration
                       (format stream "~&~/pvs:pp-lp*/~&~/pvs:pp-lp*/~&~%"
                               (cadr decls) (car decls)))
                     (format stream "~&~/pvs:pp-lp*/~&~%" (car decls)))
            end))))

(defmethod pp-lp* (stream (imp importing) &optional colon-p at-sign-p)
  "Prints importing declaration IMP."
  (declare (ignore colon-p at-sign-p))
  (with-slots (theory-name) imp
    (format stream "require ~a;" theory-name)))

;;; Declarations

(defmethod pp-lp* (s (decl var-decl) &optional colon-p at-sign-p)
  "Variable declarations x: VAR int are not printed because variables are added
to the context."
  (declare (ignore colon-p at-sign-p))
  nil)

(defmethod pp-lp* (stream (decl type-decl) &optional colon-p at-sign-p)
  "t: TYPE."
  (declare (ignore colon-p at-sign-p))
  (format stream "constant symbol ~/pvs:pp-ident/: " (tag decl))
  (abstract-over@ ((context-formals) :stream stream :impl t)
    (princ "Set" stream))
  (princ #\; stream))

(defmethod pp-lp* (stream (decl type-eq-decl) &optional colon-p at-sign-p)
  "t: TYPE = x, but also domain(f): TYPE = D"
  (declare (ignore colon-p at-sign-p))
  (with-slots (id type-expr formals) decl
    (format stream "symbol ~/pvs:pp-ident/: " (tag decl))
    (let ((fm-types (mapcar (lambda (fm)
                              (if (singleton? fm)
                                  (type (car fm))
                                  (make-tupletype (mapcar #'type fm))))
                            formals)))
      (abstract-over@ ((context-formals) :stream stream :impl t)
        (format stream "~{El ~:/pvs:pp-lp*/~^ ~~> ~}" fm-types)
        (unless (null fm-types) (princ " → " stream))
        (princ "Set" stream))
      (princ " ≔ " stream)
      (abstract-over ((context-formals) :stream stream :impl t)
        (with-formals (formals :stream stream :in-type t)
          (pp-lp* stream type-expr))))
    (princ " begin admitted;" stream)))

(defmethod pp-lp* (stream (decl type-from-decl) &optional colon-p at-sign-p)
  "t: TYPE FROM s"
  (declare (ignore colon-p at-sign-p))
  (with-slots (id predicate supertype) decl
    ;; PREDICATE is a type declaration
    (format stream "symbol ~/pvs:pp-ident/: " (tag decl))
    (abstract-over@ ((context-formals) :stream stream :impl t)
      (princ "Set" stream))
    (princ " ≔ " stream)
    (abstract-over ((context-formals) :stream stream :impl t)
      ;; Build properly the subtype expression for printing
      (pp-lp* stream (mk-subtype supertype (mk-name-expr (id predicate)))))
    (princ #\; stream)))

(defmethod pp-lp* (stream (decl formula-decl) &optional colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (with-slots (spelling id (cdefn closed-definition) definition) decl
    (let ((axiomp (member spelling '(AXIOM POSTULATE)))
          ;; Make the universal closure of the definition if it isn't already
          ;; done
          (defn (if cdefn cdefn
                    (let ((*generate-tccs* 'all))
                      (universal-closure definition)))))
      (assert defn)
      (unless axiomp (princ "opaque " stream))
      (format stream "symbol ~/pvs:pp-ident/ : " (tag decl))
      (abstract-over@ ((context-formals) :stream stream :impl t)
        (format stream "Prf ~:/pvs:pp-lp*/" defn))
      (unless axiomp
        (princ " ≔ " stream)
        (unless *without-proofs*
          (abstract-over ((context-formals) :stream stream)
            (pprint-proof decl stream))))
      (princ " begin admitted;" stream))))

(defmethod pp-lp* (stream (decl const-decl) &optional colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (with-slots (id type definition formals) decl
    (unless definition
      (princ "constant " stream))
    (format stream "symbol ~/pvs:pp-ident/: " (tag decl))
    (if definition
        (progn
          (abstract-over@ ((context-formals) :stream stream :impl t)
            (format stream "El ~:/pvs:pp-lp*/" type))
          (princ " ≔ " stream)
          (abstract-over ((context-formals) :stream stream :impl t)
            (with-formals (formals :stream stream)
              (pp-lp* stream definition))))
        (progn
          (abstract-over@ ((context-formals) :stream stream :impl t)
            (format stream "El ~:/pvs:pp-lp*/" type))))
    (princ " begin admitted;" stream)))

(defmethod pp-lp* (stream (decl macro-decl) &optional colon-p at-sign-p)
  "Ignore macro definitions, they are expanded anyway."
  (declare (ignore stream colon-p at-sign-p))
  nil)

;; REVIEW: a lot of duplication between inductive-decl and const-decl, but
;; inductive decl is not yet handled as it should
(defmethod pp-lp* (stream (decl inductive-decl) &optional colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (with-slots (id type definition formals) decl
    (format stream "symbol ~/pvs:pp-ident/:" (tag decl))
    (abstract-over@ ((context-formals) :stream stream :impl t)
      (format stream "El ~:/pvs:pp-lp*/" type))
    ;; TODO inductive definitions are not handled yet, they are axiomatised
    (with-comment stream
      (princ " ≔ " stream)
      (abstract-over ((context-formals) :stream stream :impl t)
        (with-formals (formals :stream stream)
          (pp-lp* stream definition))))
    (princ " begin admitted;" stream)))

(defmethod pp-lp* (stream (decl def-decl) &optional colon-p at-sign-p)
  ;; `declared-type' is the range while `type' is the type of the symbol
  (declare (ignore colon-p at-sign-p))
  (with-accessors ((id id) (fm formals) (m declared-measure) (defn definition)
                   (range declared-type) (ty type)) decl
    (format stream "symbol ~/pvs:pp-ident/:" (tag decl))
    (abstract-over@ ((context-formals) :stream stream :impl t)
      (format stream "El ~:/pvs:pp-lp*/" ty))
    ;; TODO: translate the recursive definition
    (princ " begin admitted;" stream)))

(defmethod pp-lp* (stream (decl conversion-decl) &optional colon-p at-sign-p)
  "CONVERSION elt, there are conversion(plus|minus)-decl as well."
  (declare (ignore stream colon-p at-sign-p))
  nil)

(defmethod pp-lp* (stream (decl auto-rewrite-decl) &optional colon-p at-sign-p)
  "AUTO_REWRITE, there are auto-rewrite-(plus|minus)-decl as well."
  (declare (ignore stream colon-p at-sign-p))
  nil)

;;; Judgements

(defmethod pp-lp* (stream (decl name-judgement) &optional colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (with-slots (name id (ty type) formals) decl
    (princ "assert ⊢ " stream)
    (abstract-over ((context-formals) :stream stream)
      (pp-lp* stream name))
    (princ ": " stream)
    (abstract-over@ ((context-formals) :stream stream)
      (format stream "El ~:/pvs:pp-lp*/" ty))
    (princ ";" stream)))

(defmethod pp-lp* (stream (decl application-judgement)
                   &optional colon-p at-sign-p)
  "Print the judgement. A TCC is generated with the same `id'.
See parse.lisp:826"
  (declare (ignore stream colon-p at-sign-p))
  nil)


(defmethod pp-lp* (stream (decl expr-judgement) &optional colon-p at-sign-p)
  (declare (ignore stream colon-p at-sign-p))
  ;; See classes-decl.lisp:656
  nil)

(defmethod pp-lp* (stream (decl subtype-judgement) &optional colon-p at-sign-p)
  (declare (ignore stream colon-p at-sign-p))
  nil)

;;; Type expressions

(defmethod pp-lp* :around (stream (te type-expr) &optional colon-p at-sign-p)
  "Prints the printable type of type expr TE if it exists, or hand over to next
method. If we do not use the PRINT-TYPE field, all predicate subtypes
definitions are expanded, and the translation becomes too large."
  (aif (print-type te)
       (pp-lp* stream it colon-p at-sign-p)
       (call-next-method)))

(defmethod pp-lp* (stream (te dep-binding) &optional colon-p at-sign-p)
  "Print the dependent binding as a type, not as a binding. To print it as a
binding, use `pp-binding'."
  (pp-lp* stream (type te) colon-p at-sign-p))

(defun pp-telescope (stream types &optional colon-p at-sign-p)
  "Print a telescope of types (mainly for tuple types)."
  (declare (ignore at-sign-p))
  (with-parens (stream colon-p)
    (cond
      ((or (endp types) (singleton? types))
       (error "A tupletype must have at least two components"))
      ((and (double types) (dep-binding? (car types)))
       (let-duet (x y) types
         (declare (ignore x y))
         (format stream "TL.&double! ~:/pvs:pp-lp*/ " (car types))
         (abstract-over ((car types) :stream stream :wrap t)
           (pp-lp* stream (cadr types)))))
      ((double types)
       (let-duet (x y) types
         (format stream "TL.double! ~:/pvs:pp-lp*/ ~:/pvs:pp-lp*/" x y)))
      ((dep-binding? (car types))
       (format stream "TL.&cons! ~:/pvs:pp-lp*/ " (type (car types)))
       (abstract-over ((car types) :stream stream :wrap t)
         (pp-telescope stream (cdr types))))
      (t
       (format stream "TL.cons! ~:/pvs:pp-lp*/ ~:/pvs:pp-telescope/"
               (car types) (cdr types))))))

(defmethod pp-lp* (stream (te tupletype) &optional colon-p at-sign-p)
  "[A, B], but also the domain of [A, B -> C]"
  (declare (ignore at-sign-p))
  (with-parens (stream colon-p)
    (format stream "TL.code ~:/pvs:pp-telescope/" (types te))))

(defmethod pp-lp* (stream (te subtype) &optional colon-p at-sign-p)
  "{n: nat | n /= zero} or (x | p(x)), see classes-decl.lisp:824"
  (declare (ignore at-sign-p))
  (with-slots (supertype predicate) te
    (with-parens (stream colon-p)
      (format stream "psub [~/pvs:pp-lp*/] ~:/pvs:pp-lp*/" supertype predicate))))

(defmethod pp-lp* (stream (te expr-as-type) &optional colon-p at-sign-p)
  "Used in e.g. (equivalence?), that is, a parenthesised expression used as a
type."
  (declare (ignore at-sign-p))
  (let ((tte
          ;; The supertype of TE may be `nil' if it has not been typechecked
          (if (or (null (supertype te)) (null (predicate te)))
              (let ((*generate-tccs* 'all))
                (typecheck te))
              te)))
    (with-slots (supertype predicate) tte
      (with-parens (stream colon-p)
        (format stream "psub [~/pvs:pp-lp*/] ~:/pvs:pp-lp*/" supertype predicate)))))

(defmethod pp-lp* (stream (te simple-expr-as-type) &optional colon-p at-sign-p)
  "Used in e.g. (equivalence?) without inheriting subtypes. I don't know when it
can be used."
  (with-slots (expr) te
    (pp-lp* stream expr colon-p at-sign-p)))

(defmethod pp-lp* (stream (te type-application) &optional colon-p at-sign-p)
  "Prints type application TE to stream STREAM. Used for instance with dependent
(sub)types `subt(i)` where `subt(i) = {x: ... | f(i)}`."
  (declare (ignore at-sign-p))
  (with-slots (type parameters) te
    (with-parens (stream colon-p)
      (format stream
              "~:/pvs:pp-lp*/ ~:/pvs:pp-lp*/"
              type (normalise-arg parameters)))))

(defgeneric pprint-funtype (domain range stream &optional wrap)
  (:documentation "Print function type from type DOMAIN to type RANGE on stream
STREAM."))

(defmethod pprint-funtype ((domain dep-binding) range stream &optional wrap)
  (with-parens (stream wrap)
    (format stream "arrd ~:/pvs:pp-lp*/ " (type domain))
    (abstract-over (domain :stream stream :wrap t)
      (pp-lp* stream range))))

(defmethod pprint-funtype (domain range stream &optional wrap)
  (with-parens (stream wrap)
    (format stream "~:/pvs:pp-lp*/ ~~> ~/pvs:pp-lp*/" domain range)))

(defmethod pp-lp* (stream (te funtype) &optional colon-p at-sign-p)
  "Prints function type TE to stream STREAM."
  (declare (ignore at-sign-p))
  (with-slots (domain range) te
    (pprint-funtype domain range stream colon-p)))

;;; Expressions

(defmethod pp-lp* :around (s (name name-expr) &optional colon-p at-sign-p)
  "It may happen that some name is not typechecked: we typecheck it to ensure a
resolution is always available (and then we resolve overloading)."
  (declare (ignore s colon-p at-sign-p))
  (unless (singleton? (resolutions name))
    (let ((*generate-tccs* 'all))
      (typecheck name)))
  ;; The resolution is set in-place, so it must be available now
  (call-next-method))

(defmethod pp-lp* (s (name name) &optional colon-p at-sign-p)
  "Print NAME handling resolutions. A name is always qualified with its theory,
and has its actuals applied. The application is wrapped if COLON-P is true.

If NAME refers to a symbol that is overloaded inside a single theory, then a
disambiguating suffix is appended."
  (assert *current-context*)
  (with-slots (id mod-id actuals resolutions) name
    (assert (singleton? resolutions))
    (let* ((resolution (car resolutions))
           (decl (declaration resolution))
           (mod (or mod-id (id (module-instance resolution))))
           (actuals (or actuals (actuals (module-instance resolution)))))
      (assert mod)
      (cond
        ((or (formal-decl? decl) (binding? decl)) ;bound variable
         (pp-ident s id))
        ((skolem-const-decl? decl)
         ;; Skolem constants are bound by a lambda abstraction. To be neat,
         ;; skolem constants should by substituted by these variables.
         (pp-ident s id))
        ((assoc id +lp-syms+)
         ;; Symbol of the encoding
         (pp-ident s id colon-p at-sign-p))
        ((equalp mod (id (theory-name *current-context*)))
         ;; Symbol of the current theory
         (with-parens (s (consp (context-formals)))
           (pp-ident s (tag name) colon-p at-sign-p)
           (when (context-formals)
             ;; Apply theory arguments (as implicit args) to symbols of signature
             (format s "~{ [~/pvs:pp-ident/]~}" (mapcar #'id (context-formals))))))
        (t
         (assert (tag name))
         ;; Symbol from elsewhere
         (with-parens (s (and colon-p (consp actuals)))
           (format s "~/pvs:pp-ident/.~/pvs:pp-ident/~{ [~/pvs:pp-lp*/]~}"
                   mod (tag name) actuals)))))))

(defmethod pp-lp* (s (name modname) &optional colon-p at-sign-p)
  (pp-ident s (id name) colon-p at-sign-p))

(defmethod pp-lp* (stream (ex lambda-expr) &optional colon-p at-sign-p)
  "LAMBDA (x: T): t. The expression LAMBDA x, y: x binds a tuple of two elements
to its first element. Abstractions are always wrapped because if they appear as
head of application, they must be (although usually ((f x) y) = (f x y) but (\x,
x y) != ((\x, x) y)"
  (declare (ignore colon-p at-sign-p))
  (with-slots (bindings expression) ex
    (with-formals ((list bindings) :stream stream :wrap t)
      (pp-lp* stream expression))))

(defmethod pp-lp* (stream (ex forall-expr) &optional colon-p at-sign-p)
  (declare (ignore at-sign-p))
  (with-slots (bindings expression) ex
    (forall (bindings :stream stream :wrap colon-p)
      (pp-lp* stream expression))))

(defmethod pp-lp* (stream (ex exists-expr) &optional colon-p at-sign-p)
  (declare (ignore at-sign-p))
  (with-slots (bindings expression) ex
    (exists (bindings :stream stream :wrap colon-p)
      (pp-lp* stream expression))))

(defmethod pp-lp* (stream (ex application) &optional colon-p at-sign-p)
  "Print application EX. The expression EX ``f(e1,e2)(g1,g2)'' will be printed
as ``f (σ (e1 ^^ e2)) (σ (g1 ^^ g2))''."
  (declare (ignore at-sign-p))
  (let ((op (operator* ex))
        (args (mapcar #'normalise-arg (arguments* ex))))
    (assert (every (lambda (e) (atom e)) args))
    (with-parens (stream colon-p)
      (pp-lp* stream op)
      (let ((doms (fundomains op)))
        ;; The procedure is the following: for each argument, look if we could
        ;; get an appropriate domain type from the operator (this is not that
        ;; clear). If that's the case, then call `cast-required-p' to know
        ;; whether the argument must use an abstract cast operator.
        (loop for arg in args do
          (princ #\Space stream)
          (let ((dom (unless (endp doms) (pop doms))))
            (if (and dom (cast-required-p dom (type arg)))
                (pprint-cast arg dom :stream stream :wrap t)
                (pp-lp* stream arg t))))))))

;; LET IN expression are processed by the application case

(defmethod pp-lp* (stream (ex projection-application)
                   &optional colon-p at-sign-p)
  "For projections of the form t`2."
  (declare (ignore at-sign-p))
  (with-parens (stream colon-p)
    (with-slots (id index argument) ex
      (format stream "TL.nth (A.pure ~d) ~/pvs:pp-lp*/" (1- index) argument))))

(defun pp-tuple (stream exs &optional colon-p at-sign-p)
  "Print expressions EXS as components of a tuple on stream STREAM."
  (declare (ignore at-sign-p))
  (with-parens (stream colon-p)
    (if (double exs)
        (let-duet (ex1 ex2) exs
          (assert (not (dep-binding? (type ex1))))
          (format stream "@TL.double~{ ~:/pvs:pp-lp*/~}"
                  (list (type ex1) (type ex2) ex1 ex2)))
        (progn
          (assert (not (dep-binding? (type (car exs)))))
          (format stream "@TL.cons (A.pure ~d) ~:/pvs:pp-lp*/ ~:/pvs:pp-telescope/ ~:/pvs:pp-lp*/ "
                  (length (cdr exs)) (type (car exs))
                  (mapcar #'type (cdr exs))
                  (car exs))
          (pp-tuple stream (cdr exs) t)))))

(defmethod pp-lp* (stream (ex tuple-expr) &optional colon-p at-sign-p)
  (declare (ignore at-sign-p))
  (pp-tuple stream (exprs ex) colon-p))

(defmethod pp-lp* (stream (ex branch) &optional colon-p at-sign-p)
  "IF(a,b,c)"
  (declare (ignore at-sign-p))
  (with-parens (stream colon-p)
    ;; Always print the common supertype
    (format stream "@if ~:/pvs:pp-lp*/ ~:/pvs:pp-lp*/ "
            (type ex) (condition ex))
    (format stream "(λ ~a: Prf ~:/pvs:pp-lp*/, ~/pvs:pp-lp*/)"
            (fresh-var) (condition ex) (then-part ex))
    (princ #\Space stream)
    (format stream "(λ ~a: Prf (¬ ~:/pvs:pp-lp*/), ~/pvs:pp-lp*/)"
            (fresh-var) (condition ex) (else-part ex))))

(defun pprint-eq (ty argl argr &key neqp (stream *standard-output*) wrap)
  "Print the equation between ARGL and ARGR both typed as TY. If NEQP is `t'
then a disequation is written."
  (let ((rel (if neqp "!=" "=")))
   (with-parens (stream wrap)
     (format stream "~a [~/pvs:pp-lp*/] " rel ty)
     (with-parens (stream)
       (format stream "TL.double [~/pvs:pp-lp*/] [~/pvs:pp-lp*/] " ty ty)
       (if (cast-required-p ty (type argl))
           (pprint-cast argl ty :stream stream :wrap t)
           (pp-lp* stream argl t))
       (princ #\Space stream)
       (if (cast-required-p ty (type argr))
           (pprint-cast argr ty :stream stream :wrap t)
           (pp-lp* stream argr t))))))

(defmethod pp-lp* (stream (ex equation) &optional colon-p at-sign-p)
  "=(A,B) arguments of equality are always typed as their top-type."
  (declare (ignore at-sign-p))
  (let* ((eq-ty (type (operator ex)))
         (dom (types (domain eq-ty)))
         (tyl (car dom))
         (tyr (cadr dom)))
    (assert (tc-eq tyl tyr))
    (with-binapp-args (argl argr ex)
      (pprint-eq tyl argl argr :stream stream :wrap colon-p))))

(defmethod pp-lp* (stream (ex disequation) &optional colon-p at-sign-p)
  "/=(A, B)"
  (declare (ignore at-sign-p))
  (let* ((eq-ty (type (operator ex)))
         (dom (types (domain eq-ty)))
         (tyl (car dom))
         (tyr (cadr dom)))
    (assert (tc-eq tyl tyr))
    (with-binapp-args (argl argr ex)
      (pprint-eq tyl argl argr :neqp t :stream stream :wrap colon-p))))

(defmethod pp-lp* (stream (ex conjunction) &optional colon-p at-sign-p)
  "AND(A, B)"
  (declare (ignore at-sign-p))
  (with-parens (stream colon-p)
    (with-binapp-args (argl argr ex)
      (format
       stream
       "~:/pvs:pp-lp*/ ∧ (λ ~a: Prf ~:/pvs:pp-lp*/, ~/pvs:pp-lp*/)"
       argl (fresh-var) argl argr))))

(defmethod pp-lp* (stream (ex disjunction) &optional colon-p at-sign-p)
  "OR(A, B)"
  (declare (ignore at-sign-p))
  (with-parens (stream colon-p)
    (with-binapp-args (argl argr ex)
      (format
       stream
       "~:/pvs:pp-lp*/ ∨ (λ ~a: Prf (¬ ~:/pvs:pp-lp*/),~/pvs:pp-lp*/)"
       argl (fresh-var) argl argr))))

(defmethod pp-lp* (stream (ex implication) &optional colon-p at-sign-p)
  "IMPLIES(A, B)"
  (declare (ignore at-sign-p))
  (with-parens (stream colon-p)
    (with-binapp-args (argl argr ex)
      (format
       stream
       "~:/pvs:pp-lp*/ ⇒ (λ ~a: Prf ~:/pvs:pp-lp*/,~/pvs:pp-lp*/)"
       argl (fresh-var) argl argr))))

(defmethod pp-lp* (stream (ex iff) &optional colon-p at-sign-p)
  "IFF(A, B)"
  (declare (ignore at-sign-p))
  (with-parens (stream colon-p)
    (with-binapp-args (argl argr ex)
      (format stream "iff ~:/pvs:pp-lp*/ ~:/pvs:pp-lp*/" argl argr))))

(defmethod pp-lp* (stream (ex negation) &optional colon-p at-sign-p)
  "NOT(A), there is also a `unary-negation' that represents NOT A."
  (declare (ignore at-sign-p))
  (with-parens (stream colon-p)
    (format stream "¬ ~:/pvs:pp-lp*/" (argument ex))))

(defmethod pp-lp* (stream (ex number-expr) &optional colon-p at-sign-p)
  ;; PVS uses bignum while lambdapi is limited to 2^30 - 1
  (declare (ignore colon-p at-sign-p))
  (format stream "~d" ex))

(defmethod pp-lp* (stream (ex int-expr) &optional colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (if (>= (number ex) 0)
      (format stream "(int.positive ~d)" (number ex))
      (format stream "(int.negative ~d)" (abs (number ex)))))

(defmethod pp-lp* (stream (ac actual) &optional colon-p at-sign-p)
  "Formal parameters of theories, the `t' in `pred[t]'."
  (pp-lp* stream (expr ac) colon-p at-sign-p))

(defmethod pp-lp* (stream (ex bitvector) &optional colon-p at-sign-p)
  (declare (ignore stream colon-p at-sign-p))
  (error "Bitvectors not handled yet."))

;;; Proofs

(defmacro with-bound-prf ((var prop &optional (stream '*standard-output*)) &body body)
  "Execute BODY after having bound a fresh variable to VAR both in body, and in
the printed code with a \"let VAR : PROP ≔ _ in\""
  `(let ((,var (fresh-var)))
     (format ,stream "~&let ~/pvs:pp-ident/ : Prf ~:/pvs:pp-lp*/ ≔ _ in" ,var ,prop)
     ,@body))

(defun pprint-proof (formref &optional (stream *standard-output*))
  "Print the proof of the formula designated by FORMREF on STREAM. If the proof
  is of the form

             P
            ---
       Q     R
     -----------
          S

  Then the generated code will be
  ```
  let v0 : Prf P := _ in
  let v1 : Prf (P => R) := _ in
  let v2 : Prf Q := _ in
  let v3 : Prf (Q => R => S) := _ in
  v3 v2 (v1 v0)
  ```
  where each variable represents the justification of an inference step.
  "
  (let ((*suppress-printing* t)
        (*suppress-msg* t)
        (*multiple-proof-default-behavior* :noquestions))
    (prove-formula formref :rerun? t)
    (pprint-proof* *last-proof* stream)))

(declaim (ftype (function (sequent) expr) sequent-formula))
(defun sequent-formula (seq)
  "Return the implication of the conjunction of the antescedents to the
disjunction of the succedents.

a1, ..., an |- s1, ..., sn is translated to (a1 /\ ... /\ an => s1 \/ ... \/ sn)"
  ;; It's more robust to call `neg-s-forms' than getting the slot `n-sforms'
  (let ((antes (mapcar (lambda (f) (negate (formula f))) (neg-s-forms seq)))
        (succs (mapcar #'formula (pos-s-forms seq))))
    (make!-implication (make!-conjunction* antes)
                       (make!-disjunction* succs))))

(declaim (ftype (function (proofstate) expr) close-conclusion))
(defun close-conclusion (ps)
  "Return the closure of the goal of PS wrt. skolem constants."
  (with-slots (current-goal context) ps
    (let ((skolems
            (let ((*current-context* context))
              (collect-skolem-constants))))
      (if (endp skolems)
          (sequent-formula current-goal)
          ;; Calling make!-forall-expr with NIL would create a forall with no
          ;; bindings (and mess up parentheses in particular)
          (let ((bindings (mapcar (lambda (d)
                                    (make!-bind-decl (id d) (type d)))
                                  skolems)))
            (make!-forall-expr bindings (sequent-formula current-goal)))))))

(defun imply (exprs conc)
  "Return the implication of elements of EXPRS to CONC. If EXPRS is (E1 E2 E3),
it returns E1 => E2 => E3 => CONC."
  (labels ((rec (exprs)
             (if (endp exprs) conc
                 (make!-implication (car exprs) (rec (cdr exprs))))))
    (rec exprs)))

(declaim (ftype (function (proofstate) expr) inference-formula))
(defun inference-formula (ps)
  "Return the implication of the conjunction of the closed premises to the
conclusion, where C(x) is the closure of x wrt to its skolem constants

p1 ... pn
--------- is translated to (C(p1) => ... => C(pn) => C(c))
    c"
  (with-slots ((goal current-goal) (subgoals done-subgoals)) ps
    (let ((conc (close-conclusion ps))
          (premises (mapcar #'close-conclusion subgoals)))
      (imply premises conc))))

(defgeneric pp-proof-term (stream proof &optional colon-p at-sign-p)
  (:documentation "Print proof term PROOF on STREAM."))

(defmethod pp-proof-term (s (x symbol) &optional colon-p at-sign-p)
  (pp-ident s x colon-p at-sign-p))

(defmethod pp-proof-term (s (x string) &optional colon-p at-sign-p)
  (pp-ident s x colon-p at-sign-p))

(defmethod pp-proof-term (s (x list) &optional colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (format s "(~{~/pvs:pp-proof-term/~^ ~})" x))

(defgeneric pprint-proof* (ps &optional stream)
  (:documentation "Build the complete proof of PS bottom-up. If PS is a leaf of
the tree, simply bind a variable (in the printed code and in lisp) to the proof
of its sequent. Otherwise, build the implication from the premises to the
conclusion of the proof state, bind that proof to a variable X, print
recursively proofs for the hypotheses and build a proof term (X Y1 ... YN) where
X is the fresh bound variable and Yi are the proof terms of the subgoals
obtained by recursion."))

(defmethod pprint-proof* ((ps top-proofstate) &optional (stream *standard-output*))
  (with-slots ((subgoals done-subgoals)) ps
    ;; In top proof state, the subgoals contains the current goal only
    (assert (singleton? subgoals))
    (let ((proof (pprint-proof* (car subgoals) stream)))
      (fresh-line stream)
      (pp-proof-term stream proof))))

(defmethod pprint-proof* ((ps proofstate) &optional (stream *standard-output*))
  (with-slots ((goal current-goal) (subgoals done-subgoals)) ps
    (assert (endp (pending-subgoals ps)))
    (assert (endp (remaining-subgoals ps)))
    (if (endp subgoals)
        (with-bound-prf (x (close-conclusion ps) stream)
          x)
        (with-bound-prf (prf-step (inference-formula ps) stream)
          (let ((subproofs (mapcar (lambda (g)
                                     (pprint-proof* g stream))
                                   subgoals)))
            ;; Apply the proof step to all the hypotheses
            `(,prf-step ,@subproofs))))))

(defun pp-lp (s x &optional without-proofs)
  "Print object X using the syntax of Dedukti to stream S. If WITHOUT-PROOFS is
  T, proofs of formulaes are admitted and not printed."
  (let ((*print-escape* nil)
        (*print-pretty* nil)            ;slows down printing when t
        (*print-right-margin* 78)       ;used when *print-pretty* is t
        (*without-proofs* without-proofs)
        (*var-count* 0))
    (pp-lp* s x)))

(defun prettyprint-lambdapi (theoryref out &optional without-proofs)
  "Print theory THEORYREF to output file OUT (which must be an absolute file
path). Proofs are not translated if WITHOUT-PROOFS is `t'."
  (let ((theory (get-typechecked-theory theoryref)))
    (with-open-file (stream out :direction :output :if-exists :supersede)
      (pp-lp stream theory without-proofs))))
