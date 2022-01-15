;;; Export to Dedukti.
;;; This module provides the function ‘to-dk3’ which exports a PVS theory to a
;;; Dedukti3 file.
;;; TODO recursive functions
;;; TODO assuming sections
;;; TODO dependent pairs
;;; TODO records

(in-package :pvs)
(export '(to-dk3))

;;; Macros

(defmacro with-parens ((stream &optional (wrap t)) &body body)
  "Wraps body BODY into parentheses (printed on stream STREAM) if WRAP is true."
  `(progn
     (when ,wrap (format ,stream "("))
     ,@body
     (when ,wrap (format ,stream ")"))))

(defmacro with-cbraces ((stream &optional (impl t)) &body body)
  "Wrap BODY into curly braces printed on stream STREAM if IMPL is true."
  `(progn
     (when ,impl (princ #\{ ,stream))
     ,@body
     (when ,impl (princ #\} ,stream))))

(defmacro commented (stream &body body)
  `(progn
     (princ "/* " ,stream)
     ,@body
     (princ " */" ,stream)))

(defmacro with-binapp-args ((larg rarg binapp) &body body)
  "Binds the left (resp. right) argument of binary application BINAPP to LARG
(resp. RARG) in body BODY."
  `(destructuring-bind (,larg ,rarg &rest _) (exprs (argument ,binapp))
     ,@body))

;;; Dedukti Signature handling
;;; Signatures are used to record declared symbols. It is mainly used to resolve
;;; overloading.

(defparameter *workdir* nil "Directory where signatures are saved and read.")

(defparameter *theory-name* nil
  "Name of the exported theory.")

;;; Name resolution in general (solve overloading and such)

(defun tag-id (id index)
  "Tag identifier ID with an INDEX if needed."
  (if (= 0 index) id (symb id #\! index)))

(defmacro aresolve ((decl) &body body)
  "Bind the resolved indentifier of declaration DECL to `it' in BODY."
  `(let ((it (tag-id (id ,decl) (xml-declaration-index ,decl))))
     ,@body))

(defun ends_pred-p (s)
  "Return T if S is of the form \"foo_pred\"."
  ;; (ppcre:scan ".*_pred$" (string s)) would do the same, but loading ppcre
  ;; does not work well in PVS
  (let* ((s (string s))
         (start (search "_pred" s)))
    (string= (concatenate 'string (subseq s 0 start) "_pred") s)))

(defgeneric get-resolution (name)
  (:documentation "Fetch a resolution for NAME. In particular, if NAME is something
like `number_field_pred' which has no resolution, it fetches the resolution of
`number_field'"))
(defmethod get-resolution ((name name))
  (with-slots (resolutions id) name
    (cond
      ((singleton? resolutions) (car resolutions))
      ((ends_pred-p id)
       (let* ((strid (string id))
              (end (- (length strid) 5))
              (dom (pc-parse (subseq strid 0 end) 'type-expr)))
         ;; Typechecking sets resolutions on `dom'
         (pc-typecheck dom)
         (get-resolution dom))))))

(defgeneric module-of (name)
  (:documentation "Return the module name of NAME (as a symbol)."))
(defmethod module-of ((name name))
  (with-slots (id resolutions mod-id) name
    (if mod-id mod-id
        (aif (get-resolution name)
             (with-slots (declaration module-instance) it
               (assert module-instance)
               (check-type module-instance modname)
               (id module-instance))))))

(defgeneric index-of (name)
  (:documentation "Return an index for NAME that is different across overloaded
instances."))
(defmethod index-of ((name name))
  (aif (get-resolution name)
       (with-slots (declaration) it
         (if (and (module declaration)
                  (memq declaration (all-decls (module declaration))))
             (xml-declaration-index declaration)
             0))))

(defun to-dk3 (obj file)
  "Export PVS object OBJ to Dedukti file FILE using Dedukti3 syntax."
  (dklog:top "Translating ~s" file)
  (let* ((path (uiop:parse-unix-namestring file :want-absolute t))
         (*print-pretty* nil)            ;slows down printing when t
         (*print-right-margin* 78)
         (*workdir* (uiop:pathname-directory-pathname path)))
    (with-open-file (stream file :direction :output :if-exists :supersede)
      (princ "require open personoj.lhol personoj.tuple personoj.sum
personoj.logical personoj.pvs_cert personoj.eq personoj.restrict;
require open personoj.nat personoj.coercions;
require personoj.extra.arity-tools as A;" stream)
      (fresh-line stream)
      (pp-dk stream obj))))

;;; Some definitions and functions

(declaim (type (cons (cons symbol string) list) +dk-sym-map+))
(defparameter +dk-sym-map+
  '((|boolean| . "prop") (|bool| . "prop") (true . "true") (false . "false")
    (|type| . "Set" )
    (|restrict| . "restrict") (|extend| . "extend"))
  "Maps PVS names to names of the encoding. It is also used to avoid prepending
the symbols with a module id.")

(declaim (type type-name *type*))
(defparameter *type* (mk-type-name '|type|)
  "Symbol that represents TYPE in PVS which is translated as Set.")
(defgeneric sortp (ex)
  (:documentation "Return true if EX is the top sort TYPE."))
(defmethod sortp ((tex type-expr))
  (equalp tex *type*))
(defmethod sortp ((tex dep-binding))
  (or (sortp (type tex)) (sortp (declared-type tex))))

(declaim (type integer *var-count*))
(defparameter *var-count* 0
  "Number of generated variables. Used to create fresh variable names.")

(declaim (ftype (function (string) string) fresh-var))
(defun fresh-var (&key (prefix ""))
  "Provide a fresh variable name."
  (let ((var-name (format nil "_~av~36r" prefix *var-count*)))
    (incf *var-count*)
    var-name))

(defgeneric fundomains (ty)
  (:documentation "Return the list of domains of TY."))
(defmethod fundomains ((ty funtype))
  (cons (domain ty) (fundomains (range ty))))
(defmethod fundomains ((ty type-expr))
  (declare (ignore ty))
  nil)

;;; Theory formals

(defun thy-binding-p (thing)
  (and (bind-decl? thing) (symbolp (id thing)) (declared-type thing)))
(defun thy-bindings-p (thing)
  (and (listp thing) (every #'thy-binding-p thing)))
(deftype thy-bindings ()
  '(and list (satisfies thy-bindings-p)))

(declaim (type thy-bindings *thy-bindings*))
(defparameter *thy-bindings* nil
  "Bindings of the theory. This list is not updated using dynamic scoping
because elements are never removed from it. Formals are printed as implicit
arguments.")

(declaim (ftype (function (symbol type-expr thy-bindings) thy-bindings)))
(defun add-thy-binding (va ty bds)
  "Add variable VA of type TY to theory bindings BDS."
  (cons (make-bind-decl va ty) bds))

;;; Handling formal declarations

(defgeneric handle-tformal (formal &optional tb ctx)
  (:documentation "Add the theory formal FORMAL in the relevant context: theory
bindings TB, theory subtypes TS and context CTX."))

(defmethod handle-tformal
    ((fm formal-subtype-decl)
     &optional (tb *thy-bindings*) (ctx *ctx*))
  "Add the formal FM as a new type in TB and add a cons cell formed with FM and
the predicate associated to FM (since it's a subtype declaration) to TS."
  (values (add-thy-binding (id fm) *type* tb) ctx))

(defmethod handle-tformal ((fm formal-type-decl)
                           &optional (tb *thy-bindings*)
                             (ctx *ctx*))
  "Add type declaration FM to TB only."
  (values (add-thy-binding (id fm) *type* tb) ctx))

(defmethod handle-tformal ((fm formal-const-decl)
                           &optional (tb *thy-bindings*) (ctx *ctx*))
  "Add the constant declaration FM to TB and to CTX."
  (with-accessors ((id id) (dty declared-type)) fm
    (values (add-thy-binding id dty tb) (cons (make!-bind-decl id dty) ctx))))

(defun handle-tformals (formals &optional tb ctx)
  "Process theory formals FORMALS to produce the list of theory bindings TB, and
the initial context CTX."
  (if (null formals)
      (values tb ctx)
      (multiple-value-bind (tb ctx) (handle-tformal (car formals) tb ctx)
        (handle-tformals (cdr formals) tb ctx))))

;;; Contexts
;;;
;;; Contexts are  global variables that are  filled during the export.  They are
;;; filled  using  dynamic   scoping  so  that  the   variables  introduced  are
;;; automatically  removed  when  we  escape  their lexical  scope  in  the  PVS
;;; specification. Contexts are  always reversed wrt their  definition: the most
;;; recent binding (that may depend on older bindings) is on top of the list.

(defparameter *ctx* nil
  "Contains bindings of theory formals and symbol declaration formals. Theory
formals are never removed from the context.  It should be extended using
`with-extended-context' and inspected with `ctx-find' or
`type-with-ctx'.")

(declaim (ftype (function (symbol &optional *) (or type-expr null)) ctx-find))
(defun ctx-find (x &optional (ctx *ctx*))
  "Return the type of X if it's bound into context CTX. Else return `nil'."
  (aif (find-if (lambda (e) (eq x (id e))) ctx)
       (type it)))

(defgeneric type-with-ctx (thing &optional ctx)
  (:documentation "Type THING resorting to context CTX if needed."))
(defmethod type-with-ctx ((thing simple-decl) &optional (ctx *ctx*))
  (with-accessors ((id id) (dty declared-type) (ty type)) thing
    (or dty ty (ctx-find id) (error "Cannot type expression ~a"))))

(defgeneric extend-dk-ctx (v ctx)
  (:documentation "Extend context CTX with variable V or the variables that V
defines."))
(defmethod extend-dk-ctx ((bd binding) ctx)
  (cons bd ctx))

(defmacro with-extended-context ((bd &optional (ctx '*ctx*)) &body body)
  "Run BODY in the context CTX extended with binding BD. If the binding defines
a sub context, the context is extended with this sub context."
  `(let ((,ctx (extend-dk-ctx ,bd ,ctx)))
     ,@body))

(in-package :dklog)

(defun contexts (ind &optional obj)
  "Prints debug information on standard output with IND an indication (typically
a function name from where the debug is called)."
  (dk-log nil "~a:" ind)
  (dk-log nil "~a:" obj)
  (dk-log nil "  thf:~i~<~a~:>" (list pvs::*thy-bindings*))
  (dk-log nil "  ctx:~i~<~a~:>" (list pvs::*ctx*)))

(in-package :pvs)

;;; Formals and parameters
;;;
;;; Formals appear in theories as well as declarations. In a declaration
;;; g(x,y)(z) = e, the formals are given as a list of list ((x y) (z)).
;;; It is the same for parameters, in an expression g(e1, e2)(e3),
;;; the parameters can be retrieved by the `arguments' functions in utils.lisp.

(defun arg-p (thing) (and (listp thing) (every #'expr? thing)))
(deftype arg () '(satisfies arg-p))

(declaim (ftype (function (arg) (or type-expr dep-binding)) type-formal))
(defun type-formal (arg &optional (ctx *ctx*))
  "Give the type of argument ARG."
  (if (singleton? arg)
      (type-with-ctx (car arg) ctx)
      (make-tupletype (mapcar (lambda (e) (type-with-ctx e ctx)) arg))))

;; TODO rename into normalise-parameter
(defun normalise-arg (arg)
  "Transform an argument ARG (or parameter, that is the term applied to
something) into a proper term."
  (cond
    ((atom arg) arg)
    ((and (consp arg) (not (singleton? arg))) (make!-tuple-expr arg))
    ((singleton? arg) (car arg))
    (t (error "Ill-formed argument ~a" arg))))

(declaim (ftype (function (var-decl) *) handle-var-decl))
(defun handle-var-decl (vd)
  "Variable declarations are ignored, the type of the variable end up in their
`type' slot anyway."
  (declare (ignore vd))
  nil)

;;; Specialised printing functions

(defparameter +dk-id-forbidden+
  (list #\Newline #\Space #\Rubout #\Tab #\: #\, #\; #\`
        #\( #\) #\{ #\} #\[ #\])
  "List of characters that are forbidden inside Dedukti identifiers.")

(defun dk-id-char-p (thing)
  "True if THING is a character allowed in Dedukti identifiers."
  (and (characterp thing) (not (member thing +dk-id-forbidden+))))

(defgeneric pp-ident (s id &optional colon-p at-sign-p)
  (:documentation "Print identifier ID on stream S so that it can be parsed by
Dedukti (see https://lambdapi.readthedocs.io/en/latest/terms.html#identifiers
for valid identifiers)."))
(defmethod pp-ident (s (id string) &optional colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (if (every #'dk-id-char-p id)
      (princ id s)
      (format s "{|~a|}" id)))
(defmethod pp-ident (s (id symbol) &optional colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (aif (assoc id +dk-sym-map+)
       (princ (cdr it) s)
       (pp-ident s (mkstr id))))

(declaim (ftype (function (stream * &optional boolean boolean) *) pp-type))
(defun pp-type (stream tex &optional wrap at-sign-p)
  "Print `Set' if TEX is `*type*', or prefix TEX by `El'."
  (if (sortp tex) (princ "Set" stream)
      (with-parens (stream wrap)
        (format stream "El ~:/pvs:pp-dk/" tex))))

(declaim (ftype (function (stream binding &optional boolean *) *) pp-binding))
(defun pp-binding (s bd &optional impl at-sign-p)
  "Print binding BD as (x: T) or {x: T} if IMPL is true."
  (with-cbraces (s impl)
    (with-slots (id (dty declared-type) (ty type)) bd
      (format s "~/pvs:pp-ident/: ~/pvs:pp-type/" id (or dty ty)))))

(declaim
 (ftype
  (function
   (string function list stream &key (wrap boolean) (impl integer)) *)
  pprint-binders))
(defun pprint-binders (bind-sym body bindings stream &key wrap (impl 0))
  "Print the BODY with BINDINGS. BINDINGS may be a list or a single binding that
can be printed by `pp-binding'. BODY is a thunk (or lazy computation). The
first IMPL bindings are printed as implicit bindings.  The symbol BIND-SYM is
used as binder."
  (if (endp bindings)
      (funcall body)
      (with-parens (stream wrap)
        (multiple-value-bind (hd tl)
            (if (listp bindings)
                (values (car bindings) (cdr bindings))
                (values bindings nil))
          (format stream "~a " bind-sym)
          (pp-binding stream hd (> impl 0))
          (princ #\, stream)
          (with-extended-context (hd)
            (pprint-binders bind-sym body tl stream :impl (- impl 1)))))))

;; The following functions are for convenience, to be able to write
;; (with-abstractions (s)
;;   (foo)
;;   (bar))
;; rather than (pprint-abstractions s (lambda () (foo) (bar))

(defmacro with-abstractions ((stream &key wrap (impl 0)) bindings &body body)
  `(pprint-binders "λ" (lambda () ,@body) ,bindings ,stream
                   :wrap ,wrap :impl ,impl))

(defmacro with-products ((stream &key wrap (impl 0)) bindings &body body)
  `(pprint-binders "Π" (lambda () ,@body) ,bindings ,stream
                   :wrap ,wrap :impl ,impl))

(defmacro with-products-thy-formals (stream &body body)
  `(with-products (,stream :impl (length *thy-bindings*)) *thy-bindings*
     ,@body))

(declaim
 (ftype
  (function (function list list stream &key (wrap boolean) (in-type boolean)) *)
  pprint-formals))
(defun pprint-formals (body formals fmtypes stream &key wrap in-type)
  "Abstracts over formals FORMALS and finally call BODY. FMTYPES contains the
types of formals. FORMALS is a list of lists, with length formals = length
fmtypes. For any i, if the ith element of FORMALS is a list of length l, then
the ith element of FMTYPES is a tuple type of length l."
  (if (endp formals)
      (funcall body)
      (if (singleton? (car formals))
          (progn
            (assert (expr? (caar formals)))
            (with-abstractions
                (stream :wrap wrap)
                (list (make-bind-decl (id (caar formals))
                                      (car fmtypes)))
              (pprint-formals body (cdr formals) (cdr fmtypes) stream)))
          (let ((fresh (make-new-bind-decl (car fmtypes)))
                (fm-ext (mapcar #'list (car formals)))
                (fmtypes-ext (progn
                               (assert (tupletype?
                                        (if (dep-binding? (car fmtypes))
                                            (type (car fmtypes))
                                            (car fmtypes))))
                               (types (car fmtypes))))
                ;; Use match* if the matching is operated in a type
                (match (if in-type "match*" "match")))
            (with-abstractions (stream :wrap wrap) (list fresh)
              (format stream "~a ~/pvs:pp-ident/ " match (id fresh))
              (pprint-formals body (append fm-ext (cdr formals))
                              (append fmtypes-ext (cdr fmtypes)) stream
                              :wrap t))))))

(defmacro with-formals ((stream &key wrap in-type) formals fmtypes &body body)
  `(pprint-formals (lambda () ,@body) ,formals ,fmtypes ,stream
                   :wrap ,wrap :in-type ,in-type))

(defun pprint-require (mod &optional (s *standard-output*))
  (format s "require pvs.prelude.~a as ~a;" mod mod))

;;; Main printing

(defgeneric pp-dk (stream obj &optional colon-p at-sign-p)
  (:documentation "Prints object OBJ to stream STREAM. This function can be used
in `format' funcall `~/pvs:pp-dk3/'. The colon modifier specifies whether
arguments should be wrapped into parentheses.")
  (:method (stream obj &optional colon-p at-sign-p)
    (declare (ignore colon-p at-sign-p))
    (error "Unexpected element: ~a of type ~a." obj (class-of obj))))

(defmethod pp-dk (stream (mod module) &optional colon-p at-sign-p)
  "Print the declarations of module MOD."
  (labels
      ((pprint-decls (decls)
         "Print declarations DECLS to stream STREAM. We use a special function
(rather than a `map') because PVS places the declaration of predicates *after*
the declaration of TYPE FROM."
         (declare (type list decls))
         (assert (every (lambda (e) (or (importing? e) (declaration? e)))
                        decls))
         (when (not (null decls))
           (cond
             ((var-decl? (car decls))
              (handle-var-decl (car decls))
              (pprint-decls (cdr decls)))
             ((type-from-decl? (first decls))
              ;; In this case (TYPE FROM declaration), the predicate appears
              ;; after the type declaration
              (assert (>= (length decls) 2))
              (pp-dk stream (second decls))
              (fresh-line stream)
              (pp-dk stream (first decls))
              (fresh-line stream)
              (terpri stream)
              (pprint-decls (cddr decls)))
             (t (pp-dk stream (car decls))
                (fresh-line stream)
                (terpri stream)
                (pprint-decls (cdr decls)))))))
    (with-accessors ((id id) (th theory) (fsu formals-sans-usings)) mod
      (setf *theory-name* id)
      (format stream "// Theory ~a~%" id)
      (let ((prelude (mapcar #'id *prelude-theories*)))
        (loop for m in (list-upto prelude id) do
          (pprint-require m stream)
          (fresh-line stream)))
      (multiple-value-bind (tb ctx) (handle-tformals fsu)
        ;; No need for dynamic scoping here since theory formals are never
        ;; removed from contexts
        (psetf *thy-bindings* (nreverse tb)
               ;; Reverse theory bindings to be able to print them directly
               ;; (print oldest binding first)
               *ctx* ctx))
      (pprint-decls th))))

(defmethod pp-dk (stream (imp importing) &optional colon-p at-sign-p)
  "Prints importing declaration IMP."
  (with-slots (theory-name) imp
    (format stream "require ~a;" theory-name)))

;;; Declarations

(defmethod pp-dk (stream (decl type-decl) &optional colon-p at-sign-p)
  "t: TYPE."
  (dklog:decl "type decl ~S" (id decl))
  (aresolve (decl)
    (format stream "constant symbol ~/pvs:pp-ident/: " it)
    (with-products-thy-formals stream (pp-dk stream *type*))
    (princ #\; stream)))

(defmethod pp-dk (stream (decl type-eq-decl) &optional colon-p at-sign-p)
  "t: TYPE = x, but also domain(f): TYPE = D"
  (dklog:decl "type-eq-decl ~a" decl)
  (with-slots (id type-expr formals) decl
    (aresolve (decl)
      (format stream "symbol ~/pvs:pp-ident/: " it)
      (let ((fm-types (mapcar #'type-formal formals)))
        (with-products-thy-formals stream
          (format stream "~{~:/pvs:pp-type/~^ ~~> ~}" fm-types)
          (unless (endp fm-types) (princ " → " stream))
          (pp-dk stream *type*))
        (princ " ≔ " stream)
        (with-abstractions (stream :impl (length *thy-bindings*)) *thy-bindings*
          (with-formals (stream :in-type t) formals fm-types
            (pp-dk stream type-expr))))
      (princ " begin admitted;" stream))))

(defmethod pp-dk (stream (decl type-from-decl) &optional colon-p at-sign-p)
  "t: TYPE FROM s"
  (dklog:contexts "type from" decl)
  (dklog:decl "type-from-decl ~S" (id decl))
  (with-slots (id predicate supertype) decl
    (aresolve (decl)
      ;; PREDICATE is a type declaration
      (format stream "symbol ~/pvs:pp-ident/: " it)
      (with-products-thy-formals stream (pp-dk stream *type*))
      (princ " ≔ " stream)
      (with-abstractions (stream :impl (length *thy-bindings*)) *thy-bindings*
        ;; Build properly the subtype expression for printing
        (pp-dk stream (mk-subtype supertype (mk-name-expr (id predicate)))))
      (princ #\; stream))))

(defmethod pp-dk (stream (decl formula-decl) &optional colon-p at-sign-p)
  (dklog:decl "formula: ~S" (id decl))
  (with-slots (spelling id (cdefn closed-definition) definition
               (proof default-proof)) decl
    (aresolve (decl)
      (let ((axiomp (member spelling '(AXIOM POSTULATE)))
            ;; Make the universal closure of the definition if it isn't already
            ;; done
            (defn (if cdefn cdefn
                      (let ((*generate-tccs* 'all))
                        (universal-closure definition)))))
        (assert defn)
        (unless axiomp (princ "opaque " stream))
        (format stream "symbol ~/pvs:pp-ident/ : " it)
        (with-products-thy-formals stream
          (format stream "Prf ~:/pvs:pp-dk/" defn))
        (unless axiomp
          (princ " ≔ " stream)
          (when proof
            (commented stream
              (princ (script proof) stream))))
        (princ " begin admitted;" stream)))))

(defmethod pp-dk (stream (decl const-decl) &optional colon-p at-sign-p)
  (dklog:decl "const: ~S (id ~d)" (id decl) (xml-declaration-index decl))
  (dklog:contexts "const-decl")
  (with-slots (id type definition formals) decl
    (format stream "// Constant declaration ~a~%" id)
    (aresolve (decl)
      (if definition
          (progn
            (format stream "symbol ~/pvs:pp-ident/: " it)
            (with-products-thy-formals stream
              (format stream "El ~:/pvs:pp-dk/" type))
            (princ " ≔ " stream)
            (with-abstractions (stream :impl (length *thy-bindings*)) *thy-bindings*
              (with-formals (stream) formals (fundomains type)
                (pp-dk stream definition))))
          (progn
            (format stream "constant symbol ~/pvs:pp-ident/: " it)
            (with-products-thy-formals stream
              (format stream "El ~:/pvs:pp-dk/" type))))
      (princ " begin admitted;" stream))))

(defmethod pp-dk (stream (decl macro-decl) &optional colon-p at-sign-p)
  "Ignore macro definitions, they are expanded anyway."
  (declare (ignore stream decl colon-p at-sign-p))
  nil)

;; REVIEW: a lot of duplication between inductive-decl and const-decl, but
;; inductive decl is not yet handled as it should
(defmethod pp-dk (stream (decl inductive-decl) &optional colon-p at-sign-p)
  (dklog:decl "inductive: ~S" (id decl))
  (with-slots (id type definition formals) decl
    (format stream "// Inductive definition ~a~%" id)
    (aresolve (decl)
      (format stream "symbol ~/pvs:pp-ident/:" it)
      (with-products-thy-formals stream
        (format stream "El ~:/pvs:pp-dk/" type))
      ;; TODO inductive definitions are not handled yet, they are axiomatised
      (commented stream
        (princ " ≔ " stream)
        (with-abstractions (stream :impl (length *thy-bindings*)) *thy-bindings*
          (with-formals (stream) formals (fundomains type)
            (pp-dk stream definition))))
      (princ " begin admitted;" stream))))

(defmethod pp-dk (stream (decl def-decl) &optional colon-p at-sign-p)
  ;; `declared-type' is the range while `type' is the type of the symbol
  (with-accessors ((id id) (fm formals) (m declared-measure) (defn definition)
                   (range declared-type) (ty type)) decl
    (aresolve (decl)
      (format stream "symbol ~/pvs:pp-ident/:" it)
      (with-products-thy-formals stream (format stream "El ~:/pvs:pp-dk/" ty))
      (princ " ≔ " stream)
      (with-abstractions (stream :impl (length *thy-bindings*)) *thy-bindings*
        (with-formals (stream) fm (fundomains ty)
          (pp-dk-recursor stream id defn m range)))
      (princ " begin admitted;" stream))))

(defmethod pp-dk (stream (decl conversion-decl) &optional colon-p at-sign-p)
  "CONVERSION elt, there are conversion(plus|minus)-decl as well."
  (dklog:decl "conversion")
  (dklog:contexts "conversion-decl")
  (with-slots (id) decl
    (format stream "// Conversion: ~/pvs:pp-ident/" id)))

(defmethod pp-dk (stream (decl auto-rewrite-decl) &optional colon-p at-sign-p)
  "AUTO_REWRITE, there are auto-rewrite-(plus|minus)-decl as well."
  (format stream "// Auto rewrite ~/pvs:pp-ident/" (id decl)))

;;; Judgements
;; TODO: they are only comments for now

(defmethod pp-dk (stream (decl name-judgement) &optional colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (with-slots (name id (ty type) formals) decl
    (format stream "// Name judgement ~a~%" id)
    (princ "assert ⊢ " stream)
    (let ((fm-types (mapcar #'type-formal formals)))
      (with-abstractions (stream) *thy-bindings* (pp-dk stream name))
      (princ ": " stream)
      (with-products (stream) *thy-bindings*
        (format stream "El ~:/pvs:pp-dk/" ty))
      (princ ";" stream))))

(defmethod pp-dk (stream (decl application-judgement)
                  &optional colon-p at-sign-p)
  "Print the judgement. A TCC is generated with the same `id'.
See parse.lisp:826"
  (dklog:decl "application judgement")
  (dklog:contexts "application-judgement" decl)
  (with-slots (id formals declared-type judgement-type name) decl
    (format stream "// Application judgement \"~a\"~%" id)))


(defmethod pp-dk (stream (decl expr-judgement) &optional colon-p _at-sign-p)
  (dklog:contexts "expr-judgement")
  (with-slots (id) decl
    ;; See classes-decl.lisp:656
    (format stream "// expr-judgement: ~/pvs:pp-ident/" id)))

(defmethod pp-dk (stream (decl subtype-judgement) &optional colon-p _at-sign-p)
  (with-slots (id declared-subtype subtype) decl
    (format stream "// Subtype judgment,~%")
    (format stream "// ~/pvs:pp-ident/: ~/pvs:pp-dk/ has type ~/pvs:pp-dk/"
            id subtype declared-subtype)))

(defmethod pp-dk :after
    (stream (decl existence-tcc) &optional colon-p at-sign-p)
  ;; Only add a comment after the formula
  (format stream "// ^^ Existence TCC~&"))

(defmethod pp-dk :after
    (stream (decl subtype-tcc) &optional colon-p at-sign-p)
  (format stream "// ^^ Subtype TCC~&"))

;;; Type expressions

(defmethod pp-dk :around (stream (te type-expr) &optional colon-p at-sign-p)
  "Prints the printable type of type expr TE if it exists, or hand over to next
method. If we do not use the PRINT-TYPE field, all predicate subtypes
definitions are expanded, and the translation becomes too large."
  (aif (print-type te)
       (pp-dk stream it colon-p at-sign-p)
       (call-next-method)))

(defmethod pp-dk :before (stream (te type-expr) &optional colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (dklog:type "Element \"~a\" of class: \"~a\"" te (class-of te)))

(defmethod pp-dk (stream (te dep-binding) &optional colon-p at-sign-p)
  (pp-dk stream (or (type te) (declared-type te)) colon-p at-sign-p))

(declaim (ftype (function (list stream) *) pprint-telescope))
(defun pprint-telescope (types stream)
  (if (endp types)
      (princ "&nil" stream)
      (if (dep-binding? (car types))
          (progn
            (format stream "~:/pvs:pp-dk/ & " (type (car types)))
            (with-abstractions (stream) (list (car types))
              (pprint-telescope (cdr types) stream)))
          (progn
            (format stream "~:/pvs:pp-dk/ & " (car types))
            (pprint-telescope (cdr types) stream)))))

(defmethod pp-dk (stream (te tupletype) &optional colon-p at-sign-p)
  "[A, B], but also the domain of [A, B -> C]"
  (declare (ignore at-sign-p))
  (with-parens (stream colon-p)
    (princ "σ " stream)
    (with-parens (stream)
      (pprint-telescope (types te) stream))))

(defmethod pp-dk (stream (te subtype) &optional colon-p at-sign-p)
  "{n: nat | n /= zero} or (x | p(x)), see classes-decl.lisp:824"
  (declare (ignore at-sign-p))
  (with-slots (supertype predicate) te
    (with-parens (stream colon-p)
      (format stream "psub {~/pvs:pp-dk/} ~:/pvs:pp-dk/" supertype predicate))))

(defmethod pp-dk (stream (te expr-as-type) &optional colon-p at-sign-p)
  "Used in e.g. (equivalence?), that is, a parenthesised expression used as a
type."
  (with-slots (expr supertype predicate top-type) te
    (declare (ignore predicate top-type))
    (with-parens (stream colon-p)
      (let ((super
              ;; Try to fetch the supertype
              (if supertype supertype
                  (let ((ty (type expr)))
                    (if (funtype? ty) (domain ty))))))
        (if super
            (format stream "@psub ~:/pvs:pp-dk/ ~:/pvs:pp-dk/" super expr)
            (format stream "psub ~:/pvs:pp-dk/" expr))))))

(defmethod pp-dk (stream (te simple-expr-as-type) &optional colon-p at-sign-p)
  "Used in e.g. (equivalence?) without inheriting subtypes. I don't know when it
can be used."
  (with-slots (expr) te
    (pp-dk stream expr colon-p at-sign-p)))

(defmethod pp-dk (stream (te type-application) &optional colon-p at-sign-p)
  "Prints type application TE to stream STREAM. Used for instance with dependent
(sub)types `subt(i)` where `subt(i) = {x: ... | f(i)}`."
  (with-slots (type parameters) te
    (with-parens (stream colon-p)
      (format stream
              "~:/pvs:pp-dk/ ~:/pvs:pp-dk/"
              type (normalise-arg parameters)))))

(defgeneric pprint-funtype (domain range stream &optional wrap)
  (:documentation "Print function type from type DOMAIN to type RANGE on stream
STREAM."))

(defmethod pprint-funtype ((domain dep-binding) range stream &optional wrap)
  (with-parens (stream wrap)
    (format stream "arrd ~:/pvs:pp-dk/ " (declared-type domain))
    (with-abstractions (stream :wrap t) (list domain) (pp-dk stream range))))

(defmethod pprint-funtype (domain range stream &optional wrap)
  (with-parens (stream wrap)
    (format stream "~:/pvs:pp-dk/ ~~> ~/pvs:pp-dk/" domain range)))

(defmethod pp-dk (stream (te funtype) &optional colon-p _at-sign-p)
  "Prints function type TE to stream STREAM."
  (with-slots (domain range) te
    (pprint-funtype domain range stream colon-p)))

;;; Expressions

(defmethod pp-dk :before (stream (ex expr) &optional colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (dklog:expr "Element \"~a\" of class \"~a\"" ex (class-of ex)))

(defmethod pp-dk (s (name (eql *type*)) &optional colon-p at-sign-p)
  "Print the `*type*' constant as Set."
  (declare (ignore colon-p at-sign-p))
  (princ "Set" s))

(defmethod pp-dk (s (name name) &optional colon-p at-sign-p)
  "Print NAME handling resolutions. A name is always qualified with its theory,
and has its actuals applied. The application is wrapped if COLON-P is true.

If NAME refers to a symbol that is overloaded inside a single theory, then a
disambiguating suffix is appended."
  (with-slots (id mod-id actuals) name
    (let ((mod (module-of name))
          (index (index-of name)))
      (assert mod)
      (acond
       ((ctx-find id *ctx*)             ;bound variable
        (pp-ident s id))
       ((assoc id +dk-sym-map+)
        ;; Symbol of the encoding
        (pp-ident s id colon-p at-sign-p))
       ((equalp mod *theory-name*)
        ;; Symbol of the current theory
        (dklog:sign "Symbol \"~a\" from current theory (index ~d)" id index)
        (with-parens (s (consp *thy-bindings*))
          (pp-ident s (tag-id id index) colon-p at-sign-p)
          (when *thy-bindings*
            ;; Apply theory arguments (as implicit args) to symbols of signature
            (format s "~{ {~/pvs:pp-ident/}~}" (mapcar #'id *thy-bindings*)))))
       (t
        ;; Symbol from elsewhere
        (dklog:sign "Symbol \"~a\" from another theory" id)
        (with-parens (s (and colon-p (consp actuals)))
          (format s "~/pvs:pp-ident/.~/pvs:pp-ident/~{ {~/pvs:pp-dk/}~}"
                  mod (tag-id id index) actuals)))))))

(defmethod pp-dk (s (name modname) &optional colon-p at-sign-p)
  (pp-ident s (id name) colon-p at-sign-p))

(defmethod pp-dk (stream (ex lambda-expr) &optional colon-p _at-sign-p)
  "LAMBDA (x: T): t. The expression LAMBDA x, y: x binds a tuple of two elements
to its first element."
  (with-slots (bindings expression) ex
    (if
     (singleton? bindings)
     ;; If there is only one binding, it represents a variable
     (with-abstractions (stream :wrap colon-p) bindings
       (pp-dk stream expression))
     ;; Otherwise, each variable of the binding is the component of a tuple
     (let ((fm-type (type-formal bindings)))
       (with-formals (stream :wrap colon-p) (list bindings) (list fm-type)
         (pp-dk stream expression))))))

(defmethod pp-dk (stream (ex quant-expr) &optional wrap at-sign-p)
  (declare (ignore at-sign-p))
  (with-parens (stream wrap)
    (princ (cond ((forall-expr? ex) #\∀) ((exists-expr? ex) #\∃)) stream)
    (princ #\Space stream)
    (with-slots (bindings expression) ex
      (assert (listp bindings))
      (destructuring-bind (hd &rest tl) bindings
        (with-cbraces (stream) (pp-dk stream (type-with-ctx hd)))
        (let ((subex
                (cond
                  ((null tl) expression) ; No more quantification needed
                  ((forall-expr? ex) (make!-forall-expr tl expression))
                  ((exists-expr? ex) (make!-exists-expr tl expression))
                  (otherwise (error "Invalid expression ~S" ex)))))
          (with-abstractions (stream :wrap t) (list hd)
            (pp-dk stream subex)))))))

(defmethod pp-dk (stream (ex application) &optional colon-p at-sign-p)
  "Print application EX. The expression EX ``f(e1,e2)(g1,g2)'' will be printed
as ``f (σcons e1 e2) (σcons g1 g2)''."
  (declare (ignore at-sign-p))
  (let ((op (operator* ex))
        (args (mapcar #'normalise-arg (arguments* ex))))
    (with-parens (stream colon-p)
      (format stream "~:/pvs:pp-dk/ ~{~:/pvs:pp-dk/~^ ~}" op args))))

;; LET IN expression are processed by the application case

(defmethod pp-dk (stream (ex projection-application)
                  &optional colon-p at-sign-p)
  "For projections of the form t`2."
  (declare (ignore at-sign-p))
  (with-parens (stream colon-p)
    (with-slots (id index argument) ex
      (format stream "proj (A.pure ~d) ~/pvs:pp-dk/" (1- index) argument))))

;; TODO: print a dependent version with `consd' when needed
(defun pprint-tuple (exs stream)
  "Print expressions EXS as components of a tuple on stream STREAM."
  (if (double exs)
      (let ((ex1 (car exs)) (ex2 (cadr exs)))
        (format stream "@^^ ~{~:/pvs:pp-dk/~^ ~}"
                (list (type (car exs)) (type (cadr exs)) ex1 ex2)))
      (progn
        (format stream "@^ (A.pure ~d) ~:/pvs:pp-dk/ _ ~:/pvs:pp-dk/ "
                (length (cdr exs)) (type (car exs))
                (car exs))
        (pprint-tuple (cdr exs) stream))))

(defmethod pp-dk (stream (ex tuple-expr) &optional colon-p at-sign-p)
  (declare (ignore at-sign-p))
  (with-parens (stream colon-p)
    (pprint-tuple (exprs ex) stream)))

(defmethod pp-dk (stream (ex branch) &optional colon-p at-sign-p)
  "IF(a,b,c)"
  (with-parens (stream colon-p)
    ;; Always print the common supertype
    (format stream "@if ~:/pvs:pp-dk/ ~:/pvs:pp-dk/ "
            (type ex) (condition ex))
    (format stream "(λ ~a: Prf ~:/pvs:pp-dk/, ~/pvs:pp-dk/)"
            (fresh-var) (condition ex) (then-part ex))
    (princ #\Space stream)
    (format stream "(λ ~a: Prf (¬ ~:/pvs:pp-dk/), ~/pvs:pp-dk/)"
            (fresh-var) (condition ex) (else-part ex))))

;;; REVIEW: factorise disequation and equation

(defmethod pp-dk (stream (ex equation) &optional colon-p at-sign-p)
  "Translation of eq[T].=(A, B). The domain T is not translated because it is
always the topmost type common to A and B. Not translating it reduces the amount
of subtyping, and allows to type check more terms in presence of TYPE FROM.

For instance, in theory min_nat with theory parameters [T: TYPE FROM nat],
the last lemma invokes eq[number].=(min(S), a) with a: T. Because T is a subtype
of nat, a can be coerced to number, but with an abstract coercion (a coercion
with type variables). Therefore (@= number (min S) a) won't type check, because
the coercion problem a: T == number cannot be solved. But (= (min S) a) does
typecheck."
  (with-parens (stream colon-p)
    (let* ((eq-ty (type (operator ex)))
           (dom (types (domain eq-ty)))
           (tyl (car dom))
           (tyr (cadr dom)))
      (assert (equal tyl tyr))
      (with-binapp-args (argl argr ex)
        (format stream "= (~:/pvs:pp-dk/ ^^ ~:/pvs:pp-dk/)" argl argr)))))

(defmethod pp-dk (stream (ex disequation) &optional colon-p at-sign-p)
  "/=(A, B)"
  (with-parens (stream colon-p)
    (let* ((eq-ty (type (operator ex)))
           (dom (types (domain eq-ty)))
           (tyl (car dom))
           (tyr (cadr dom)))
      (assert (equal tyl tyr))
      (with-binapp-args (argl argr ex)
        (format stream "!= (~:/pvs:pp-dk/ ^^ ~:/pvs:pp-dk/)" argl argr)))))

(defmethod pp-dk (stream (ex conjunction) &optional colon-p at-sign-p)
  "AND(A, B)"
  (with-parens (stream colon-p)
    (with-binapp-args (argl argr ex)
      (format
       stream
       "~:/pvs:pp-dk/ ∧ (λ ~a: Prf ~:/pvs:pp-dk/, ~/pvs:pp-dk/)"
       argl (fresh-var) argl argr))))

(defmethod pp-dk (stream (ex disjunction) &optional colon-p at-sign-p)
  "OR(A, B)"
  (with-parens (stream colon-p)
    (with-binapp-args (argl argr ex)
      (format
       stream
       "~:/pvs:pp-dk/ ∨ (λ ~a: Prf (¬ ~:/pvs:pp-dk/),~/pvs:pp-dk/)"
       argl (fresh-var) argl argr))))

(defmethod pp-dk (stream (ex implication) &optional colon-p at-sign-p)
  "IMPLIES(A, B)"
  (dklog:expr "implication")
  (with-parens (stream colon-p)
    (with-binapp-args (argl argr ex)
      (format
       stream
       "~:/pvs:pp-dk/ ⇒ (λ ~a: Prf ~:/pvs:pp-dk/,~/pvs:pp-dk/)"
       argl (fresh-var) argl argr))))

(defmethod pp-dk (stream (ex iff) &optional colon-p at-sign-p)
  "IFF(A, B)"
  (with-parens (stream colon-p)
    (with-binapp-args (argl argr ex)
      (format stream "iff ~:/pvs:pp-dk/ ~:/pvs:pp-dk/" argl argr))))

(defmethod pp-dk (stream (ex negation) &optional colon-p _at-sign-p)
  "NOT(A), there is also a `unary-negation' that represents NOT A."
  (with-parens (stream colon-p)
    (format stream "¬ ~:/pvs:pp-dk/" (argument ex))))

(defmethod pp-dk (stream (ex number-expr) &optional colon-p at-sign-p)
  ;; PVS uses bignum while lambdapi is limited to 2^30 - 1
  (format stream "~d" ex))

(defmethod pp-dk (stream (ac actual) &optional colon-p at-sign-p)
  "Formal parameters of theories, the `t' in `pred[t]'."
  (pp-dk stream (expr ac) colon-p at-sign-p))

(defmethod pp-dk (stream (ex bitvector) &optional _colon-p _at-sign-p)
  (error "Bitvectors not handled yet."))
