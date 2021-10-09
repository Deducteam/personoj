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

(defmacro with-parens ((stream wrap) &body body)
  "Wraps body BODY into parentheses (printed on stream STREAM) if WRAP is true."
  `(progn
     (when ,wrap (format ,stream "("))
     ,@body
     (when ,wrap (format ,stream ")"))))

(defmacro with-implicitness ((impl stream) &body body)
  "Wrap BODY into curly braces printed on stream STREAM if IMPL is true."
  `(progn
     (when ,impl (princ #\{ ,stream))
     ,@body
     (when ,impl (princ #\} ,stream))))

(defmacro with-binapp-args ((larg rarg binapp) &body body)
  "Binds the left (resp. right) argument of binary application BINAPP to LARG
(resp. RARG) in body BODY."
  `(destructuring-bind (,larg ,rarg &rest _) (exprs (argument ,binapp))
     ,@body))

;;; Global printing parameters

(defparameter *print-domains* t
  "Whether to systematically print domain of abstractions.")

(defparameter *print-implicits* nil
  "Whether to systematically print implicit arguments.")

;;; Dedukti Signature handling

(defparameter *workdir* nil "Directory where signatures are saved and read.")

(declaim (type dksig:signature *signature*))
(defparameter *signature* (dksig:make-signature :theory 'dummy)
  "Signature of the currently exported theory.")

(defparameter *opened-signatures* nil
  "Signatures that are opened into the current one.")

(defun set-workdir (path)
  "Set PATH as the working directory (see `*workdir*')"
  (assert
   (uiop:directory-pathname-p path) (path)
   "Working directory must be a dirctory: ~a is not a directory." path)
  (when *workdir* (error "Working directory already set: ~a" *workdir*))
  (setf *workdir* path))

(defun dump-sig (&optional (sign *signature*))
  "Write the signature of the current theory to filename `theory.lisp'."
  (let* ((filename (string (dksig:signature-theory sign)))
         (path (pvs::merge-pathnames
                *workdir* (make-pathname :name filename :type "lisp"))))
    (with-open-file (s path :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
      (dksig:dump sign s))))

(defun open-sig (theory)
  "Import the defined symbols of THEORY into the current one."
  (let* ((filename (string theory))
         (path (pvs::merge-pathnames
                *workdir* (make-pathname :name filename :type "lisp"))))
    (if (uiop:file-exists-p path)
        (let ((newsig (with-open-file (s path :direction :input)
                        (dksig:open theory s))))
          (setf *opened-signatures* (cons newsig *opened-signatures*))))))

(defmacro with-sig-update ((bind sym ty sig &optional opened) &body body)
  "Bind BIND with the symbol name for ID which denotes a symbol of type TY that
will be added into signature SIG with opened signatures OPENED at the end of
BODY."
  (let ((newsig (gensym)))
    `(multiple-value-bind (,bind ,newsig) (dksig:add ,sym ,ty ,sig ,opened)
       ,@body
       (setf sig ,newsig))))

(declaim (ftype (function (syntax string) *) to-dk3))
(defun to-dk3 (obj file)
  "Export PVS object OBJ to Dedukti file FILE using Dedukti3 syntax."
  (dklog:top "Translating ~s" file)
  (let ((path (uiop:parse-unix-namestring file :want-absolute t))
        (*print-pretty* nil)            ;slows down printing when t
        (*print-right-margin* 78))
    (set-workdir (uiop:pathname-directory-pathname path))
    (with-open-file (stream file :direction :output :if-exists :supersede)
      (princ "require open personoj.lhol personoj.tuple personoj.sum
personoj.logical personoj.pvs_cert personoj.eqtup;
require open personoj.nat personoj.coercions;
require personoj.extra.arity-tools as A;" stream)
      (fresh-line stream)
      (pp-dk stream obj))))

;;; Some definitions and functions

(declaim (type (cons (cons symbol string) list) *dk-sym-map*))
(defparameter *dk-sym-map*
  '((|boolean| . "prop") (|bool| . "prop") (true . "true") (false . "false")
    (|type| . "Set" ))
  "Maps PVS names to names of the encoding. It is also used to avoid prepending
the symbols with a module id.")

(declaim (type type-name *type*))
(defparameter *type* (mk-type-name '|type|)
  "Symbol that represents TYPE in PVS which is translated as Set.")

(defgeneric is-*type*-p (ex)
  (:documentation "Return true if EX is the top sort TYPE."))
(defmethod is-*type*-p ((tex type-expr))
  (equalp tex *type*))
(defmethod is-*type*-p ((tex dep-binding))
  (or (is-*type*-p (type tex)) (is-*type*-p (declared-type tex))))

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

(declaim (ftype (function (list) list) normalise-tb))
(defun normalise-tb (tb)
  "Normalises theory bindings TB to add them to a signature."
  (flet ((norm-b (b)
           (with-accessors ((id id) (dty declared-type)) b
             (cons id dty))))
    (mapcar #'norm-b tb)))

(declaim (type list *ctx-thy-subtypes*))
(defparameter *ctx-thy-subtypes* nil
  "For each u TYPE FROM t present in the theory formals, a cons cell
(u . u_pred) is added to this context. When u is required, it will be printed as
psub u_pred. Both u and u_pred are symbols.")

;;; Handling formal declarations

(defgeneric handle-tformal (formal &optional tb ts ctx)
  (:documentation "Add the theory formal FORMAL in the relevant context: theory
bindings TB, theory subtypes TS and context CTX."))

(defmethod handle-tformal
    ((fm formal-subtype-decl)
     &optional (tb *thy-bindings*) (ts *ctx-thy-subtypes*) (ctx *ctx*))
  "Add the formal FM as a new type in TB and add a cons cell formed with FM and
the predicate associated to FM (since it's a subtype declaration) to TS."
  (let ((pred (predicate (type-value fm))))
    (assert (symbolp (id fm)))
    (assert (symbolp (id pred)))
    (values (add-thy-binding (id pred) (type pred) tb)
            (acons (id fm) (id pred) ts)
            ctx)))

(defmethod handle-tformal ((fm formal-type-decl)
                          &optional (tb *thy-bindings*) (ts *ctx-thy-subtypes*)
                            (ctx *ctx*))
  "Add type declaration FM to TB only."
  (values (add-thy-binding (id fm) *type* tb) ts ctx))

(defmethod handle-tformal ((fm formal-const-decl)
                          &optional (tb *thy-bindings*) (ts *ctx-thy-subtypes*)
                            (ctx *ctx*))
  "Add the constant declaration FM to TB and to CTX."
  (with-accessors ((id id) (dty declared-type)) fm
    (values (add-thy-binding id dty tb) ts (cons (make!-bind-decl id dty) ctx))))

(declaim (ftype (function (list &optional list list list)
                          (values list list list))
                handle-tformals))
(defun handle-tformals (formals &optional tb ts ctx)
  "Process theory formals FORMALS to produce the list of theory bindings TB, the
list of declared theory subtypes TS and the initial context CTX."
  (if (null formals)
      (values tb ts ctx)
      (multiple-value-bind (tb ts ctx) (handle-tformal (car formals) tb ts ctx)
        (handle-tformals (cdr formals) tb ts ctx))))

;;; Contexts
;;;
;;; Contexts are  global variables that are  filled during the export.  They are
;;; filled  using  dynamic   scoping  so  that  the   variables  introduced  are
;;; automatically  removed  when  we  escape  their lexical  scope  in  the  PVS
;;; specification. Contexts are  always reversed wrt their  definition: the most
;;; recent binding (that may depend on older bindings) is on top of the list.

(defun ty-context-p (thing)
  "List of `binding's."
  (when (listp thing) (every #'binding? thing)))

(deftype ty-context ()
  "A context is an association list mapping symbols to types."
  (and 'list '(satisfies ty-context-p)))

(declaim (type ty-context *ctx*))
(defparameter *ctx* nil
  "Contains bindings of theory formals and symbol declaration formals. Theory
formals are never removed from the context.  It should be extended using
`with-extended-context'.")

(declaim (ftype (function (symbol &optional ty-context) (or null binding))
                find-ty-context))
(defun find-ty-context (x &optional (ctx *ctx*))
  "Find a binding of X into context CTX."
  (find-if (lambda (e) (eq x (id e))) ctx))

(defgeneric type-with-ctx (thing &optional ctx)
  (:documentation "Type THING resorting to context CTX if needed."))
(defmethod type-with-ctx ((thing simple-decl) &optional (ctx *ctx*))
  (with-accessors ((id id) (dty declared-type) (ty type)) thing
    (or dty ty (aif (find-ty-context id ctx)
                    (or (declared-type it) (type it))
                    (error "No type available for expression ~a")))))

(defgeneric extend-dk-ctx (v ctx)
  (:documentation "Extend context CTX with variable V or the variables that V
defines."))
(defmethod extend-dk-ctx ((bd binding) ctx)
  (cons bd ctx))

(defmacro with-extended-context ((bd &optional (ctx *ctx*)) &body body)
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
  (dk-log nil "  tst:~i~<~a~:>" (list pvs::*ctx-thy-subtypes*))
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
  (if (single arg)
      (type-with-ctx (car arg) ctx)
      (make-tupletype (mapcar (lambda (e) (type-with-ctx e ctx)) arg))))

;; TODO rename into normalise-parameter
(defun normalise-arg (arg)
  "Transform an argument ARG (or parameter, that is the term applied to
something) into a proper term."
  (cond
    ((atom arg) arg)
    ((and (consp arg) (not (single arg))) (make!-tuple-expr arg))
    ((single arg) (car arg))
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

(declaim (ftype (function (* stream) *) pprint-ident))
(defgeneric pprint-ident (id stream)
  (:documentation "Transform identifier ID so that is can be read by Dedukti and
print it to stream STREAM."))
(defmethod pprint-ident ((id string) stream)
  "Print identifier IDENT to STREAM so that it can be read by Lambdapi."
  (if (every #'dk-id-char-p id)
      (princ id stream)
      (format stream "{|~a|}" id)))
(defmethod pprint-ident ((id symbol) stream)
  "Resolve symbol SYM, transform it to a Dedukti identifier and print it to
stream STREAM."
  (aif (assoc id *dk-sym-map*)
       (princ (cdr it) stream)
       (pprint-ident (mkstr id) stream)))
(defun pp-ident (stream sym &optional _colon-p _at-sign-p)
  "Wrapper of pprint-ident to be used in format strings."
  (pprint-ident sym stream))

(defun pp-type (stream tex &optional wrap at-sign-p)
  "Print `Set' if TEX is `*type*', or prefix TEX by `El'."
  (if (is-*type*-p tex) (princ "Set" stream)
      (with-parens (stream wrap)
        (format stream "El ~:/pvs:pp-dk/" tex))))

(declaim (ftype (function (binding stream &optional boolean) *) pprint-binding))
(defun pprint-binding (bd stream &optional impl)
  (with-implicitness (impl stream)
    (with-accessors ((id id) (dty declared-type) (ty type)) bd
      (pp-ident stream id)
      (princ #\: stream)
      (pp-type stream (or dty ty)))))

(declaim
 (ftype (function (* (or list atom) stream &key (wrap boolean) (impl integer))
                  null)
        pprint-abstraction))
(defun pprint-abstraction (ex bindings stream &key wrap (impl 0))
  "Print expression EX on stream STREAM abstracting arguments in BINDINGS (with
λs). The context `*ctx*' is extended on each printed binding. If IMPL is
provided, then the first IMPL bindings are made implicit."
  (if (null bindings)
      (if (functionp ex)
          (funcall ex stream wrap)
          (pp-dk stream ex wrap))
      (with-parens (stream wrap)
        (multiple-value-bind (hd tl)
            (if (listp bindings)
                (values (car bindings) (cdr bindings))
                (values bindings nil))
          (princ "λ " stream)
          (pprint-binding hd stream (> impl 0))
          (princ #\, stream)
          (with-extended-context (hd *ctx*)
            (pprint-abstraction ex tl stream :impl (- impl 1)))))))

(declaim (ftype (function (* symbol list stream *) null)
                pprint-product))
(defun pprint-product (tex kind bds stream &key wrap (impl 0))
  "Print `Π x1: t1, Π x2: t2, ..., Π xn: tn, ξ (TEX)' where `(xi, ti)' are the
bindings of BDS, and ξ is determined by KIND which may be :set, :prop or :kind.
The first IMPL arguments are made implicit (wrapped into curly brackets)."
  (if (endp bds)
      (if (functionp tex)
          (funcall tex stream wrap)
          (case kind
            ;; The only term of type Kind is "Set"
            (:kind (pp-dk stream tex))
            (:set (format stream "El ~:/pvs:pp-dk/" tex))
            (:prop (format stream "Prf ~:/pvs:pp-dk/" tex))))
      (destructuring-bind (bd &rest tl) bds
        (princ "Π " stream)
        (pprint-binding bd stream (> impl 0))
        (princ #\, stream)
        (with-extended-context (bd *ctx*)
          (pprint-product tex kind tl stream :impl (- impl 1))))))

(declaim (ftype (function (type-expr symbol stream *) null) pprint-thy-formals))
(defun pprint-thy-formals (tex kind stream &optional wrap)
  "Print type expression TEX prefixed by as many products as there are formals
in the theory. Formals are implicit. Parameter KIND behaves as in
`pprint-product'."
  (let* ((thy-bds (reverse *thy-bindings*))
         (len (length thy-bds)))
    (pprint-product tex kind thy-bds stream :wrap wrap :impl len)))

(declaim (ftype (function (* list list stream &key (wrap boolean)) *)
                pprint-formals))
(defun pprint-formals (ex formals fmtypes stream &key wrap in-type)
  "Abstracts over formals FORMALS and finally print EX. FMTYPES contains the
types of formals. FORMALS is a list of lists, with length formals = length
fmtypes. For any i, if the ith element of FORMALS is a list of length l, then
the ith element of FMTYPES is a tuple type of length l."
  (if (endp formals)
      (if (functionp ex) (funcall ex stream wrap) (pp-dk stream ex wrap))
      (if (single (car formals))
          (flet ((ppfm (s colon-p)
                   (pprint-formals ex (cdr formals) (cdr fmtypes) s)))
            (assert (expr? (caar formals)))
            (pprint-abstraction #'ppfm
                                (make-bind-decl (id (caar formals))
                                                (car fmtypes))
                                stream :wrap wrap))
          (let ((fresh (make-new-bind-decl (car fmtypes)))
                (fm-ext (mapcar #'list (car formals)))
                (fmtypes-ext (progn
                               (assert (tupletype?
                                        (if (dep-binding? (car fmtypes))
                                            (type (car fmtypes))
                                            (car fmtypes))))
                               (types (car fmtypes)))))
            (pprint-abstraction
             (lambda (s colon-p)
               (with-parens (stream colon-p)
                 ;; Use match* if the matching is operated in a type
                 (princ (if in-type "match*" "match") stream)
                 (format stream " ~/pvs:pp-ident/ " (id fresh))
                 (pprint-formals ex (append fm-ext (cdr formals))
                                 (append fmtypes-ext (cdr fmtypes)) s
                                 :wrap t)))
             fresh stream :wrap wrap)))))

(declaim (ftype (function (symbol stream string) *) pprint-reqopen))
(defun pprint-reqopen (mod stream &optional root)
  "Prints a ``require open'' directive opening module MOD with root path ROOT on
stream STREAM."
  (princ "require open" stream)
  (unless (null root)
    (princ #\space stream)
    (princ root stream)
    (princ #\. stream))
  (princ mod stream)
  (open-sig mod))

(declaim (ftype (function (stream obj * *) *) pp-impl))
(defun pp-impl (stream obj &optional colon-p at-sign-p)
  "Print object OBJ to stream STREAM if `*print-implicits*' is true."
  (declare (ignore colon-p))
  (when *print-implicits*
    (princ #\{ stream)
    (pp-dk stream obj nil at-sign-p)
    (princ #\} stream)))

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
      (setf *signature* (dksig:make-signature :theory id))
      (format stream "// Theory ~a~%" id)
      (let ((prelude (mapcar #'id *prelude-theories*)))
        (loop for m in (list-upto prelude id) do
          (pprint-reqopen m stream "pvs.prelude")
          (princ #\; stream)
          (fresh-line stream)))
      (multiple-value-bind (tb ts ctx) (handle-tformals fsu)
        ;; No need for dynamic scoping here since theory formals are never
        ;; removed from contexts
        (psetf *thy-bindings* tb
               *ctx-thy-subtypes* ts
               *ctx* ctx
               *signature* (dksig:make-signature :theory id
                                                 :context (normalise-tb tb))))
      (pprint-decls th)
      (dump-sig))))

(defmethod pp-dk (stream (imp importing) &optional colon-p at-sign-p)
  "Prints importing declaration IMP."
  (with-slots (theory-name) imp
    (format stream "require ~a;" theory-name)))

;;; Declarations

(defmethod pp-dk (stream (decl type-decl) &optional colon-p at-sign-p)
  "t: TYPE."
  (dklog:decl "type decl ~S" (id decl))
  (with-slots (id) decl
    (with-sig-update (newid id nil *signature* *opened-signatures*)
      (format stream "constant symbol ~/pvs:pp-ident/: " newid)
      (pprint-thy-formals *type* :kind stream t)
      (princ #\; stream))))

(defmethod pp-dk (stream (decl type-eq-decl) &optional colon-p at-sign-p)
  "t: TYPE = x, but also domain(f): TYPE = D"
  (dklog:decl "type-eq-decl ~a" decl)
  (with-slots (id type-expr formals) decl
    (with-sig-update (newid id nil *signature* *opened-signatures*)
      (format stream "symbol ~/pvs:pp-ident/: " newid)
      (let ((fm-types (mapcar #'type-formal formals)))
        (pprint-product (lambda (s &rest args)
                          (declare (ignore args))
                          (format s "~{~:/pvs:pp-type/~^ ~~> ~}" fm-types)
                          (unless (endp fm-types) (princ " → " s))
                          (pp-dk s *type*))
                        :kind (reverse *thy-bindings*) stream
                        :wrap t :impl (length *thy-bindings*))
        (princ " ≔ " stream)
        (flet ((ppfm (s &rest args)
                 (declare (ignore args))
                 (pprint-formals type-expr formals fm-types stream :in-type t)))
          (pprint-abstraction #'ppfm (reverse *thy-bindings*) stream
                              :impl (length *thy-bindings*))))
      (princ " begin admitted;" stream))))

(defmethod pp-dk (stream (decl type-from-decl) &optional colon-p at-sign-p)
  "t: TYPE FROM s"
  (dklog:contexts "type from" decl)
  (dklog:decl "type-from-decl ~S" (id decl))
  (with-slots (id predicate supertype) decl
    (with-sig-update (newid id nil *signature* *opened-signatures*)
      ;; PREDICATE is a type declaration
      (format stream "symbol ~/pvs:pp-ident/: " newid)
      (pprint-thy-formals *type* :kind stream t)
      (princ " ≔ " stream)
      (pprint-abstraction
       ;; Build properly the subtype expression for printing
       (mk-subtype supertype (mk-name-expr (id predicate)))
       (reverse *thy-bindings*)
       stream
       :impl (length *thy-bindings*))
      (princ #\; stream))))

(defmethod pp-dk (stream (decl formula-decl) &optional colon-p at-sign-p)
  (dklog:decl "formula: ~S" (id decl))
  (with-accessors ((sp spelling) (id id) (cdefn closed-definition)
                   (prf default-proof)) decl
    (format stream "// Formula declaration: ~a~&" sp)
    ;; TODO the type is for now `nil', something more meaningful must be used,
    ;; in accordance to what the type is when the name of the  formula is
    ;; printed
    (with-sig-update (newid id nil *signature* *opened-signatures*)
      (let ((axiomp (member sp '(AXIOM POSTULATE))))
        (unless axiomp (princ "opaque " stream))
        (format stream "symbol ~/pvs:pp-ident/ : " newid)
        (pprint-thy-formals cdefn :prop stream t)
        (unless axiomp
          (format stream " ≔ /* ~a */ " (script prf)))
        (princ " begin admitted;" stream)))))

(defmethod pp-dk (stream (decl const-decl) &optional colon-p at-sign-p)
  (dklog:decl "const: ~S" (id decl))
  (dklog:contexts "const-decl")
  (with-slots (id type definition formals) decl
    (format stream "// Constant declaration ~a~%" id)
    (with-sig-update (newid id type *signature* *opened-signatures*)
      (if definition
          (progn
            (format stream "symbol ~/pvs:pp-ident/: " newid)
            (pprint-thy-formals type :set stream t)
            (princ " ≔ " stream)
            (flet ((ppfm (s &rest args)
                     (declare (ignore args))
                     (pprint-formals definition formals (fundomains type) s)))
              (pprint-abstraction #'ppfm (reverse *thy-bindings*) stream
                                  :impl (length *thy-bindings*))))
          (progn
            (format stream "constant symbol ~/pvs:pp-ident/: " newid)
            (pprint-thy-formals type :set stream t)))
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
    (with-sig-update (newid id type *signature* *opened-signatures*)
      (format stream "symbol ~/pvs:pp-ident/:" newid)
      (pprint-thy-formals type :set stream t)
      (flet ((ppfm (s &rest args)
               (declare (ignore args))
               (pprint-formals definition formals (fundomains type) s)))
        ;; TODO inductive definitions are not handled yet, they are axiomatised
        (princ "/*" stream)              ;Comment definition
        (princ " ≔ " stream)
        (pprint-abstraction definition (reverse *thy-bindings*) stream
                            :impl (length *thy-bindings*)))
      (princ "*/" stream)             ;End of definition comment
      (princ " begin admitted;" stream))))

(defmethod pp-dk (stream (decl def-decl) &optional colon-p at-sign-p)
  ;; `declared-type' is the range while `type' is the type of the symbol
  (with-accessors ((id id) (fm formals) (m declared-measure) (defn definition)
                   (range declared-type) (ty type)) decl
    (with-sig-update (newid id ty *signature* *opened-signatures*)
      (format stream "symbol ~/pvs:pp-ident/:" newid)
      (pprint-thy-formals ty :set stream t)
      (princ " ≔ " stream)
      (let ((recursor-def (lambda (s &rest args)
                            (declare (ignore args))
                            (pp-dk-recursor s id defn m range))))
        (flet ((ppfm (s &rest args)
                 (declare (ignore args))
                 (pprint-formals recursor-def fm (fundomains ty) s)))
          (pprint-abstraction #'ppfm (reverse *thy-bindings*) stream
                              :impl (length *thy-bindings*))))
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
  (with-slots (name) decl
    (format stream "// Name judgement \"~a\"." name)))

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
            (pprint-abstraction (lambda (s &rest args)
                                  (declare (ignore args))
                                  (pprint-telescope (cdr types) s))
                                (car types) stream :wrap nil))
          (progn
            (format stream "~:/pvs:pp-dk/ & " (car types))
            (pprint-telescope (cdr types) stream)))))

(defmethod pp-dk (stream (te tupletype) &optional colon-p at-sign-p)
  "[A, B], but also the domain of [A, B -> C]"
  (declare (ignore at-sign-p))
  (with-parens (stream colon-p)
    (princ "σ " stream)
    (with-parens (stream t)
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
      (flet ((super ()
               ;; Try to fetch the supertype and return it
               (if supertype supertype
                   (let ((ty (type expr)))
                     (if (funtype? ty) (domain ty))))))
        (aif (and *print-implicits* (super))
             (format stream "@psub ~:/pvs:pp-dk/ ~:/pvs:pp-dk/" it expr)
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
    (pprint-abstraction range domain stream :wrap t)))

(defmethod pprint-funtype (domain range stream &optional wrap)
  (with-parens (stream wrap)
    (format stream "~:/pvs:pp-dk/ ~~> ~/pvs:pp-dk/" domain range)))

(defmethod pp-dk (stream (te funtype) &optional colon-p _at-sign-p)
  "Prints function type TE to stream STREAM."
  (with-slots (domain range) te
    (pprint-funtype domain range stream colon-p)))

;;; Expressions

(declaim
 (ftype (function (symbol
                   (or type-expr null)
                   stream
                   &key (mod-id *) (actuals list) (wrap boolean)) *)
        pprint-name))
(defun pprint-name (id ty stream &key mod-id actuals wrap)
  "Print identifier ID of module MOD-ID to stream STREAM with ACTUALS applied.
If WRAP is true, then the application of ID to ACTUALS is wrapped between
parentheses. The type TY of the symbol represented by ID may be used to resolve
overloading."
  (acond
   ((find-ty-context id *ctx*) ;bound variable
    (pp-ident stream id))
   ;; The symbol is a type declared as TYPE FROM in theory parameters,
   ;; we print the predicate associated
   ((assoc id *ctx-thy-subtypes*)
    (format stream "~:/pvs:pp-ident/" (cdr it)))
   ;; Symbol of the encoding
   ((assoc id *dk-sym-map*) (pp-ident stream id))
   ;; Symbol from the current signature
   ((dksig:find id ty *signature*)
    (dklog:sign "Symbol \"~a: ~a\" found as \"~a\" in current signature."
                id ty it)
    (with-parens (stream (consp *thy-bindings*))
      (pprint-ident it stream)
      (when *thy-bindings*
        ;; Apply theory arguments (as implicit args) to symbols of signature
        (format
         stream "~{ {~/pvs:pp-ident/}~}"
         (mapcar #'id (reverse *thy-bindings*))))))
   ;; Symbol from an opened signature
   ((dksig:find id ty *opened-signatures*)
    (dklog:sign "Symbol \"~a: ~a\" found as \"~a\" in opened signatures."
                id ty it)
    (with-parens (stream (and wrap (consp actuals)))
      (format stream "~/pvs:pp-ident/~{ {~/pvs:pp-dk/}~}" it actuals)))
   ;; Symbol from an imported theory
   (t
    (dklog:sign "Symbol \"~a: ~a\" not found in signatures." id ty)
    (with-parens (stream (consp actuals))
      (when mod-id
        (pp-ident stream mod-id)
        (princ #\. stream))
      (format stream "~/pvs:pp-ident/~{ {~/pvs:pp-dk/}~}" id actuals)))))

(defmethod pp-dk :before (stream (ex expr) &optional colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (dklog:expr "Element \"~a\" of class \"~a\"" ex (class-of ex)))

(defmethod pp-dk (stream (ex name-expr) &optional colon-p _at-sign-p)
  "Print name NAME applying theory formal parameters if needed. Takes care of
name resolution"
  (with-slots (id type mod-id actuals) ex
    (pprint-name id type stream :mod-id mod-id :actuals actuals :wrap colon-p)))

(defmethod pp-dk (stream (ex type-name) &optional colon-p _at-sign-p)
  (with-slots (id mod-id actuals) ex
    (pprint-name id nil stream :mod-id mod-id :actuals actuals
                                  :wrap colon-p)))

(defmethod pp-dk (stream (ex lambda-expr) &optional colon-p _at-sign-p)
  "LAMBDA (x: T): t. The expression LAMBDA x, y: x binds a tuple of two elements
to its first element."
  (with-slots (bindings expression) ex
    (if
     (single bindings)
     ;; If there is only one binding, it represents a variable
     (pprint-abstraction expression bindings stream :wrap colon-p)
     ;; Otherwise, each variable of the binding is the component of a tuple
     (let ((fm-type (type-formal bindings)))
       (pprint-formals expression (list bindings) (list fm-type) stream
                       :wrap colon-p)))))

(defmethod pp-dk (stream (ex quant-expr) &optional wrap at-sign-p)
  (declare (ignore at-sign-p))
  (with-parens (stream wrap)
    (princ (cond ((forall-expr? ex) #\∀) ((exists-expr? ex) #\∃)) stream)
    (princ #\Space stream)
    (with-slots (bindings expression) ex
      (destructuring-bind (hd &rest tl) bindings
        (pp-impl stream (type-with-ctx hd))
        (let ((subex
                (cond
                  ((null tl) expression) ; No more quantification needed
                  ((forall-expr? ex) (make!-forall-expr tl expression))
                  ((exists-expr? ex) (make!-exists-expr tl expression))
                  (otherwise (error "Invalid expression ~S" ex)))))
          (pprint-abstraction subex hd stream :wrap t))))))

(defmethod pp-dk (stream (ex application) &optional colon-p at-sign-p)
  "Print application EX. The expression EX ``f(e1,e2)(g1,g2)'' will be printed
as ``f (σcons e1 e2) (σcons g1 g2)''."
  (declare (ignore at-sign-p))
  (let ((op (operator* ex))
        (args (mapcar #'normalise-arg (arguments* ex))))
    (with-parens (stream colon-p)
      (format stream "~/pvs:pp-dk/ ~{~:/pvs:pp-dk/~^ ~}" op args))))

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
      (if *print-implicits*
          (progn
            (format stream "@^ (A.pure ~d) ~:/pvs:pp-dk/ _ ~:/pvs:pp-dk/ "
                    (length (cdr exs)) (type (car exs))
                    (car exs))
            (pprint-tuple (cdr exs) stream))
          (progn
            (pp-dk stream (car exs) t)
            (princ " ^ " stream)
            (pprint-tuple (cdr exs) stream)))))

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
    (format stream "(λ ~a~:[~*~;: Prf ~:/pvs:pp-dk/~], ~/pvs:pp-dk/)"
            (fresh-var) *print-domains* (condition ex) (then-part ex))
    (princ #\Space stream)
    (format stream "(λ ~a~:[~*~;: Prf (¬ ~:/pvs:pp-dk/)~], ~/pvs:pp-dk/)"
            (fresh-var) *print-domains* (condition ex) (else-part ex))))

;;; REVIEW: factorise disequation and equation

(defmethod pp-dk (stream (ex disequation) &optional colon-p at-sign-p)
  "/=(A, B)"
  (with-parens (stream colon-p)
    (let* ((eq-ty (type (operator ex)))
           (dom (types (domain eq-ty)))
           (tyl (car dom))
           (tyr (cadr dom)))
      (assert (equal tyl tyr))
      (with-binapp-args (argl argr ex)
        (format stream "@neq ~:/pvs:pp-dk/ ~:/pvs:pp-dk/"
                tyl (make!-tuple-expr (list argl argr)))))))

(defmethod pp-dk (stream (ex equation) &optional colon-p at-sign-p)
  "=(A, B)"
  (with-parens (stream colon-p)
    (let* ((eq-ty (type (operator ex)))
           (dom (types (domain eq-ty)))
           (tyl (car dom))
           (tyr (cadr dom)))
      (assert (equal tyl tyr))
      (with-binapp-args (argl argr ex)
        (format stream "@eq ~:/pvs:pp-dk/ ~:/pvs:pp-dk/"
                tyl (make!-tuple-expr (list argl argr)))))))

(defmethod pp-dk (stream (ex conjunction) &optional colon-p at-sign-p)
  "AND(A, B)"
  (with-parens (stream colon-p)
    (with-binapp-args (argl argr ex)
      (format
       stream
       "~:/pvs:pp-dk/ ∧ (λ ~a~:[~*~;: Prf ~:/pvs:pp-dk/~], ~/pvs:pp-dk/)"
       argl (fresh-var) *print-domains* argl argr))))

(defmethod pp-dk (stream (ex disjunction) &optional colon-p at-sign-p)
  "OR(A, B)"
  (with-parens (stream colon-p)
    (with-binapp-args (argl argr ex)
      (format
       stream
       "~:/pvs:pp-dk/ ∨ (λ ~a~:[~*~;: Prf (¬ ~:/pvs:pp-dk/)~],~/pvs:pp-dk/)"
       argl (fresh-var) *print-domains* argl argr))))

(defmethod pp-dk (stream (ex implication) &optional colon-p at-sign-p)
  "IMPLIES(A, B)"
  (dklog:expr "implication")
  (with-parens (stream colon-p)
    (with-binapp-args (argl argr ex)
      (format
       stream
       "~:/pvs:pp-dk/ ⇒ (λ ~a~:[~*~;: Prf ~:/pvs:pp-dk/~],~/pvs:pp-dk/)"
       argl (fresh-var) *print-domains* argl argr))))

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