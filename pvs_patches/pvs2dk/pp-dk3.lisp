(in-package :pvs)
;;; Export to Dedukti.
;;; This module provides the function ‘pp-dk’ exports PVS structures to Dedukti.
;;; TODO recursive functions, inductive types
;;; TODO dependent pairs
;;; TODO records

(defun pp-dk (s x)
  (let ((*print-escape* nil)
        (*print-pretty* nil)            ;slows down printing when t
        (*print-right-margin* 78))      ;used when *print-pretty* is t
    (pp-dk* s x)))

;;; Printing macros

(defmacro with-parens ((stream &optional (wrap t)) &body body)
  "Wraps body BODY into parentheses (printed on stream STREAM) if WRAP is true."
  `(progn
     (when ,wrap (format ,stream "("))
     ,@body
     (when ,wrap (format ,stream ")"))))

(defmacro with-brackets ((stream &optional (impl t)) &body body)
  "Wrap BODY into curly braces printed on stream STREAM if IMPL is true."
  `(progn
     (when ,impl (princ #\[ ,stream))
     ,@body
     (when ,impl (princ #\] ,stream))))

(defmacro with-comment (stream &body body)
  `(progn
     (princ "/* " ,stream)
     ,@body
     (princ " */" ,stream)))

(defmacro with-binapp-args ((larg rarg binapp) &body body)
  "Binds the left (resp. right) argument of binary application BINAPP to LARG
(resp. RARG) in body BODY."
  `(destructuring-bind (,larg ,rarg &rest _) (exprs (argument ,binapp))
     ,@body))

(defparameter *theory-name* nil
  "Name of the exported theory.")

;;; Name resolution in general (solve overloading and such)

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

(defgeneric tag (thing)
  (:documentation "Get a tagged identifier out of THING. The tag allows to
differentiate several resolutions."))
(defmethod tag ((thing declaration))
  (let ((index (xml-declaration-index thing)))
    (if (= 0 index)
        (id thing)
        (symb (id thing) #\! index))))
(defmethod tag ((thing name))
  (aif (get-resolution thing)
       (with-slots (declaration) it
         (if (and (module declaration)
                  (memq declaration (all-decls (module declaration))))
             (let ((index (xml-declaration-index declaration)))
               (if (= 0 index)
                   (id thing)
                   (symb (id thing) #\! index)))
             (id name)))))

;;; Some definitions and functions

(defparameter +dk-syms+
  '((|boolean| . "prop") (|bool| . "prop") (true . "true") (false . "false")
    (|type| . "Set" )
    (|restrict| . "restrict") (|extend| . "extend")
    ;; We add equality to avoid printing it as "equalities.="
    (|=| . "="))
  "Maps PVS names to names of the encoding. It is also used to avoid prepending
the symbols with a module id.")

(declaim (type type-name +type+))
(defparameter +type+ (mk-type-name '|type|)
  "Symbol that represents TYPE in PVS which is translated as Set.")

(declaim (type integer *var-count*))
(defparameter *var-count* 0
  "Number of generated variables. Used to create fresh variable names.")

(declaim (ftype (function (string) string) fresh-var))
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
  (declare (ignore ty))
  nil)
(defmethod fundomains ((ex expr))
  (fundomains (type ex)))

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
  (find (id is) *thy-subtype-vars*))

(defmethod cast-required-p (should (is subtype))
  (or (aif (print-type is) (cast-required-p should it))
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

;;; Theory formals

(declaim (type list *thy-bindings*))
(defparameter *thy-bindings* nil
  "Bindings of the theory (as a list of `bind-decl'). This list is not updated
using dynamic scoping because elements are never removed from it. Formals are
printed as implicit arguments when applied to functions. It's not necessary to
add formals to `*ctx*' because on each declarations, all theory bindings are put
into it by abstracting over them with `abstract-over'.")

(declaim (type list *thy-subtype-vars*))
(defparameter *thy-subtype-vars* nil
  "A set contataining the id of formals declared as theory subtypes.")

(defgeneric handle-tformal (formal)
  (:documentation "Add the theory formal FORMAL in the relevant structure:
`*thy-bindings*' or `*thy-subtype-vars*'."))

(defmethod handle-tformal ((fm formal-subtype-decl))
  (push (id fm) *thy-subtype-vars*)
  (push (make-bind-decl (id fm) +type+) *thy-bindings*))

(defmethod handle-tformal ((fm formal-type-decl))
  (push (make-bind-decl (id fm) +type+) *thy-bindings*))

(defmethod handle-tformal ((fm formal-const-decl))
  (with-slots (id (dty declared-type)) fm
    (push (make!-bind-decl id dty) *thy-bindings*)))

(defmethod handle-tformal ((fms list))
  (mapc #'handle-tformal fms))

(defmethod handle-tformal ((fm null))
  (declare (ignore fm))
  nil)

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
      (type (car arg))
      (make-tupletype (mapcar #'type arg))))

;; TODO rename into normalise-parameter
(declaim (ftype (function (arg) expr) normalise-arg))
(defun normalise-arg (arg)
  "Transform an argument ARG (or parameter, that is the term being applied to
something) into a proper term."
  (cond
    ((atom arg) arg)
    ((and (consp arg) (not (singleton? arg))) (make!-tuple-expr arg))
    ((singleton? arg) (car arg))
    (t (error "Ill-formed argument ~a" arg))))

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
  (aif (assoc id +dk-syms+)
       (princ (cdr it) s)
       (pp-ident s (mkstr id))))

(defgeneric pp-as-type (s ty &optional colon-p at-sign-p)
  (:documentation "Print element TY as a type on stream S."))

(defmethod pp-as-type (s (ty (eql +type+)) &optional colon-p at-sign-p)
  "If TY is `+type+', then print Set"
  (declare (ignore colon-p at-sign-p))
  (princ "Set" s))

(defmethod pp-as-type (s (ty type-expr) &optional colon-p at-sign-p)
  "If TY is a `type-expr', just apply El on it."
  (declare (ignore colon-p at-sign-p))
  (format s "El ~:/pvs:pp-dk*/" ty))

(defparameter *ctx* nil
  "Contains variables bound by products and lambda abstractions. It is not used
to type things, for this we rely on PVS facilities. It is extended using dynamic
scoping with the `with-extended-context' macro. Stack-like structure: the most
recent binding is on top.")

(defmacro with-extended-context ((bd &optional (ctx '*ctx*)) &body body)
  "Run BODY in the context CTX extended with binding BD. Since dynamic scoping
is used, BD is removed from CTX after BODY."
  `(let ((,ctx (cons (id ,bd) ,ctx)))
     ,@body))

(declaim (ftype (function (stream binding &optional boolean *) *) pp-binding))
(defun pp-binding (s bd &optional impl at-sign-p)
  "Print binding BD as (x: T) or {x: T} if IMPL is true."
  (declare (ignore at-sign-p))
  (with-brackets (s impl)
    (with-slots (id (dty declared-type) (ty type)) bd
      (format s "~/pvs:pp-ident/: ~/pvs:pp-as-type/" id (or dty ty)))))

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

(defmacro abstract-over ((bindings stream &key wrap (impl 0)) &body body)
  "Abstract over BINDINGS as lambdas. IMPL may be a number, nil or :all. If it
is :all, all bindings are implicits."
  `(pprint-binders "λ" (lambda () ,@body) ,bindings ,stream
                   :wrap ,wrap
                   :impl ,(if (eql :all impl)
                              `(length ,bindings)
                              impl)))

(defmacro abstract-over@ ((bindings stream &key wrap (impl 0)) &body body)
  "Abstract over bindings as products. IMPL may be a number, nil or :all. If it
is :all, all bindings are implicits."
  `(pprint-binders "Π" (lambda () ,@body) ,bindings ,stream
                   :wrap ,wrap
                   :impl ,(if (eql :all impl)
                              `(length ,bindings)
                              impl)))

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
            (abstract-over
                ((list (make-bind-decl (id (caar formals)) (car fmtypes)))
                 stream :wrap wrap)
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
            (abstract-over ((list fresh) stream :wrap wrap) (list fresh)
              (format stream "~a ~/pvs:pp-ident/ " match (id fresh))
              (pprint-formals body (append fm-ext (cdr formals))
                              (append fmtypes-ext (cdr fmtypes)) stream
                              :wrap t))))))

(defmacro with-formals ((stream &key wrap in-type) formals fmtypes &body body)
  `(pprint-formals (lambda () ,@body) ,formals ,fmtypes ,stream
                   :wrap ,wrap :in-type ,in-type))

;;; Main printing

(defgeneric pp-dk* (stream obj &optional colon-p at-sign-p)
  (:documentation "Prints object OBJ to stream STREAM. This function can be used
in `format' funcall `~/pvs:pp-dk*3/'. The colon modifier specifies whether
arguments should be wrapped into parentheses.")
  (:method (stream obj &optional colon-p at-sign-p)
    (declare (ignore colon-p at-sign-p))
    (error "Unexpected element: ~a of type ~a." obj (class-of obj))))

(defmethod pp-dk* (stream (thing null) &optional colon-p at-sign-p)
  (declare (ignore stream thing colon-p at-sign-p))
  (error "Invalid null argument passed to pp-dk*: ~a" thing))

(defmethod pp-dk* (stream (mod module) &optional colon-p at-sign-p)
  "Print the declarations of module MOD."
  (with-slots (id theory (fsu formals-sans-usings) saved-context) mod
    (assert saved-context)
    (let ((*current-context* saved-context))
      (format stream
              "~&require open personoj.lhol personoj.tuple personoj.sum
personoj.logical personoj.pvs_cert personoj.eq personoj.restrict;
require open personoj.nat personoj.coercions;
require personoj.extra.arity-tools as A;")
      (setf *theory-name* id)
      (handle-tformal fsu)
      ;; All we need from theory bindings is to iterate though them to print
      ;; them
      (setf *thy-bindings* (nreverse *thy-bindings*))
      ;; `handle-tformal' must be called beforehand to setup
      ;; `*thy-subtype-vars*'
      (unless (endp *thy-subtype-vars*)
        (format stream "~&require open personoj.cast;"))
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
                       (format stream "~&~/pvs:pp-dk*/~&~/pvs:pp-dk*/~&~%"
                               (cadr decls) (car decls)))
                     (format stream "~&~/pvs:pp-dk*/~&~%" (car decls)))
            end))))

(defmethod pp-dk* (stream (imp importing) &optional colon-p at-sign-p)
  "Prints importing declaration IMP."
  (with-slots (theory-name) imp
    (format stream "require ~a;" theory-name)))

;;; Declarations

(defmethod pp-dk* (s (decl var-decl) &optional colon-p at-sign-p)
  "Variable declarations x: VAR int are not printed because variables are added
to the context."
  (declare (ignore s decl colon-p at-sign-p))
  nil)

(defmethod pp-dk* (stream (decl type-decl) &optional colon-p at-sign-p)
  "t: TYPE."
  (format stream "constant symbol ~/pvs:pp-ident/: " (tag decl))
  (abstract-over@ (*thy-bindings* stream :impl :all)
    (pp-dk* stream +type+))
  (princ #\; stream))

(defmethod pp-dk* (stream (decl type-eq-decl) &optional colon-p at-sign-p)
  "t: TYPE = x, but also domain(f): TYPE = D"
  (with-slots (id type-expr formals) decl
    (format stream "symbol ~/pvs:pp-ident/: " (tag decl))
    (let ((fm-types (mapcar #'type-formal formals)))
      (abstract-over@ (*thy-bindings* stream :impl :all)
        (format stream "~{~:/pvs:pp-as-type/~^ ~~> ~}" fm-types)
        (unless (endp fm-types) (princ " → " stream))
        (pp-dk* stream +type+))
      (princ " ≔ " stream)
      (abstract-over (*thy-bindings* stream :impl :all)
        (with-formals (stream :in-type t) formals fm-types
          (pp-dk* stream type-expr))))
    (princ " begin admitted;" stream)))

(defmethod pp-dk* (stream (decl type-from-decl) &optional colon-p at-sign-p)
  "t: TYPE FROM s"
  (with-slots (id predicate supertype) decl
    ;; PREDICATE is a type declaration
    (format stream "symbol ~/pvs:pp-ident/: " (tag decl))
    (abstract-over@ (*thy-bindings* stream :impl :all)
      (pp-dk* stream +type+))
    (princ " ≔ " stream)
    (abstract-over (*thy-bindings* stream :impl :all)
      ;; Build properly the subtype expression for printing
      (pp-dk* stream (mk-subtype supertype (mk-name-expr (id predicate)))))
    (princ #\; stream)))

(defmethod pp-dk* (stream (decl formula-decl) &optional colon-p at-sign-p)
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
      (abstract-over@ (*thy-bindings* stream :impl :all)
        (format stream "Prf ~:/pvs:pp-dk*/" defn))
      (unless axiomp
        (princ " ≔ " stream)
        (with-comment stream (princ "TODO proof" stream)))
      (princ " begin admitted;" stream))))

(defmethod pp-dk* (stream (decl const-decl) &optional colon-p at-sign-p)
  (with-slots (id type definition formals) decl
    (unless definition
      (princ "constant " stream))
    (format stream "symbol ~/pvs:pp-ident/: " (tag decl))
    (if definition
        (progn
          (abstract-over@ (*thy-bindings* stream :impl :all)
            (format stream "El ~:/pvs:pp-dk*/" type))
          (princ " ≔ " stream)
          (abstract-over (*thy-bindings* stream :impl :all)
            (with-formals (stream) formals (fundomains type)
              (pp-dk* stream definition))))
        (progn
          (abstract-over@ (*thy-bindings* stream :impl :all)
            (format stream "El ~:/pvs:pp-dk*/" type))))
    (princ " begin admitted;" stream)))

(defmethod pp-dk* (stream (decl macro-decl) &optional colon-p at-sign-p)
  "Ignore macro definitions, they are expanded anyway."
  (declare (ignore stream decl colon-p at-sign-p))
  nil)

;; REVIEW: a lot of duplication between inductive-decl and const-decl, but
;; inductive decl is not yet handled as it should
(defmethod pp-dk* (stream (decl inductive-decl) &optional colon-p at-sign-p)
  (with-slots (id type definition formals) decl
    (format stream "symbol ~/pvs:pp-ident/:" (tag decl))
    (abstract-over@ (*thy-bindings* stream :impl :all)
      (format stream "El ~:/pvs:pp-dk*/" type))
    ;; TODO inductive definitions are not handled yet, they are axiomatised
    (with-comment stream
      (princ " ≔ " stream)
      (abstract-over (*thy-bindings* stream :impl :all)
        (with-formals (stream) formals (fundomains type)
          (pp-dk* stream definition))))
    (princ " begin admitted;" stream)))

(defmethod pp-dk* (stream (decl def-decl) &optional colon-p at-sign-p)
  ;; `declared-type' is the range while `type' is the type of the symbol
  (with-accessors ((id id) (fm formals) (m declared-measure) (defn definition)
                   (range declared-type) (ty type)) decl
    (format stream "symbol ~/pvs:pp-ident/:" (tag decl))
    (abstract-over@ (*thy-bindings* stream :impl :all)
      (format stream "El ~:/pvs:pp-dk*/" ty))
    ;; TODO: translate the recursive definition
    (princ " begin admitted;" stream)))

(defmethod pp-dk* (stream (decl conversion-decl) &optional colon-p at-sign-p)
  "CONVERSION elt, there are conversion(plus|minus)-decl as well."
  (declare (ignore stream decl colon-p at-sign-p))
  nil)

(defmethod pp-dk* (stream (decl auto-rewrite-decl) &optional colon-p at-sign-p)
  "AUTO_REWRITE, there are auto-rewrite-(plus|minus)-decl as well."
  (declare (ignore stream decl colon-p at-sign-p))
  nil)

;;; Judgements

(defmethod pp-dk* (stream (decl name-judgement) &optional colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (with-slots (name id (ty type) formals) decl
    (princ "assert ⊢ " stream)
    (let ((fm-types (mapcar #'type-formal formals)))
      (abstract-over (*thy-bindings* stream)
        (pp-dk* stream name))
      (princ ": " stream)
      (abstract-over@ (*thy-bindings* stream)
        (format stream "El ~:/pvs:pp-dk*/" ty))
      (princ ";" stream))))

(defmethod pp-dk* (stream (decl application-judgement)
                   &optional colon-p at-sign-p)
  "Print the judgement. A TCC is generated with the same `id'.
See parse.lisp:826"
  (declare (ignore stream decl colon-p at-sign-p))
  nil)


(defmethod pp-dk* (stream (decl expr-judgement) &optional colon-p at-sign-p)
  (declare (ignore stream decl colon-p at-sign-p))
  ;; See classes-decl.lisp:656
  nil)

(defmethod pp-dk* (stream (decl subtype-judgement) &optional colon-p at-sign-p)
  (declare (ignore stream decl colon-p at-sign-p))
  nil)

;;; Type expressions

(defmethod pp-dk* :around (stream (te type-expr) &optional colon-p at-sign-p)
  "Prints the printable type of type expr TE if it exists, or hand over to next
method. If we do not use the PRINT-TYPE field, all predicate subtypes
definitions are expanded, and the translation becomes too large."
  (aif (print-type te)
       (pp-dk* stream it colon-p at-sign-p)
       (call-next-method)))

(defmethod pp-dk* (stream (te dep-binding) &optional colon-p at-sign-p)
  (pp-dk* stream (or (type te) (declared-type te)) colon-p at-sign-p))

(declaim (ftype (function (list stream) *) pprint-telescope))
(defun pprint-telescope (types stream)
  (if (endp types)
      (princ "&nil" stream)
      (if (dep-binding? (car types))
          (progn
            (format stream "~:/pvs:pp-dk*/ & " (type (car types)))
            (abstract-over ((list (car types)) stream)
              (pprint-telescope (cdr types) stream)))
          (progn
            (format stream "~:/pvs:pp-dk*/ & " (car types))
            (pprint-telescope (cdr types) stream)))))

(defmethod pp-dk* (stream (te tupletype) &optional colon-p at-sign-p)
  "[A, B], but also the domain of [A, B -> C]"
  (declare (ignore at-sign-p))
  (with-parens (stream colon-p)
    (princ "σ " stream)
    (with-parens (stream)
      (pprint-telescope (types te) stream))))

(defmethod pp-dk* (stream (te subtype) &optional colon-p at-sign-p)
  "{n: nat | n /= zero} or (x | p(x)), see classes-decl.lisp:824"
  (declare (ignore at-sign-p))
  (with-slots (supertype predicate) te
    (with-parens (stream colon-p)
      (format stream "psub [~/pvs:pp-dk*/] ~:/pvs:pp-dk*/" supertype predicate))))

(defmethod pp-dk* (stream (te expr-as-type) &optional colon-p at-sign-p)
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
            (format stream "psub [~/pvs:pp-dk*/] ~:/pvs:pp-dk*/" super expr)
            (format stream "psub ~:/pvs:pp-dk*/" expr))))))

(defmethod pp-dk* (stream (te simple-expr-as-type) &optional colon-p at-sign-p)
  "Used in e.g. (equivalence?) without inheriting subtypes. I don't know when it
can be used."
  (with-slots (expr) te
    (pp-dk* stream expr colon-p at-sign-p)))

(defmethod pp-dk* (stream (te type-application) &optional colon-p at-sign-p)
  "Prints type application TE to stream STREAM. Used for instance with dependent
(sub)types `subt(i)` where `subt(i) = {x: ... | f(i)}`."
  (with-slots (type parameters) te
    (with-parens (stream colon-p)
      (format stream
              "~:/pvs:pp-dk*/ ~:/pvs:pp-dk*/"
              type (normalise-arg parameters)))))

(defgeneric pprint-funtype (domain range stream &optional wrap)
  (:documentation "Print function type from type DOMAIN to type RANGE on stream
STREAM."))

(defmethod pprint-funtype ((domain dep-binding) range stream &optional wrap)
  (with-parens (stream wrap)
    (format stream "arrd ~:/pvs:pp-dk*/ " (declared-type domain))
    (abstract-over ((list domain) stream :wrap t)
      (pp-dk* stream range))))

(defmethod pprint-funtype (domain range stream &optional wrap)
  (with-parens (stream wrap)
    (format stream "~:/pvs:pp-dk*/ ~~> ~/pvs:pp-dk*/" domain range)))

(defmethod pp-dk* (stream (te funtype) &optional colon-p _at-sign-p)
  "Prints function type TE to stream STREAM."
  (with-slots (domain range) te
    (pprint-funtype domain range stream colon-p)))

;;; Expressions

(defmethod pp-dk* (s (name (eql +type+)) &optional colon-p at-sign-p)
  "Print the `+type+' constant as Set."
  (declare (ignore colon-p at-sign-p))
  (princ "Set" s))

(defmethod pp-dk* (s (name name) &optional colon-p at-sign-p)
  "Print NAME handling resolutions. A name is always qualified with its theory,
and has its actuals applied. The application is wrapped if COLON-P is true.

If NAME refers to a symbol that is overloaded inside a single theory, then a
disambiguating suffix is appended."
  (with-slots (id mod-id actuals) name
    (let ((mod (module-of name)))
      (assert mod)
      (acond
       ((find id *ctx*) ;bound variable
        (pp-ident s id))
       ((assoc id +dk-syms+)
        ;; Symbol of the encoding
        (pp-ident s id colon-p at-sign-p))
       ((equalp mod *theory-name*)
        ;; Symbol of the current theory
        (with-parens (s (consp *thy-bindings*))
          (pp-ident s (tag name) colon-p at-sign-p)
          (when *thy-bindings*
            ;; Apply theory arguments (as implicit args) to symbols of signature
            (format s "~{ [~/pvs:pp-ident/]~}" (mapcar #'id *thy-bindings*)))))
       (t
        ;; Symbol from elsewhere
        (with-parens (s (and colon-p (consp actuals)))
          (format s "~/pvs:pp-ident/.~/pvs:pp-ident/~{ [~/pvs:pp-dk*/]~}"
                  mod (tag name) actuals)))))))

(defmethod pp-dk* (s (name modname) &optional colon-p at-sign-p)
  (pp-ident s (id name) colon-p at-sign-p))

(defmethod pp-dk* (stream (ex lambda-expr) &optional colon-p _at-sign-p)
  "LAMBDA (x: T): t. The expression LAMBDA x, y: x binds a tuple of two elements
to its first element. Abstractions are always wrapped because if they appear as
head of application, they must be (although usually ((f x) y) = (f x y) but (\x,
x y) != ((\x, x) y)"
  (with-slots (bindings expression) ex
    (if
     (singleton? bindings)
     ;; If there is only one binding, it represents a variable
     (abstract-over (bindings stream :wrap t)
       (pp-dk* stream expression))
     ;; Otherwise, each variable of the binding is the component of a tuple
     (let ((fm-type (type-formal bindings)))
       (with-formals (stream :wrap t) (list bindings) (list fm-type)
         (pp-dk* stream expression))))))

(defmethod pp-dk* (stream (ex quant-expr) &optional wrap at-sign-p)
  (declare (ignore at-sign-p))
  (with-parens (stream wrap)
    (princ (cond ((forall-expr? ex) #\∀) ((exists-expr? ex) #\∃)) stream)
    (princ #\Space stream)
    (with-slots (bindings expression) ex
      (assert (listp bindings))
      (destructuring-bind (hd &rest tl) bindings
        (with-brackets (stream)
          (pp-dk* stream (type hd)))
        (let ((subex
                (cond
                  ((endp tl) expression) ; No more quantification needed
                  ((forall-expr? ex) (make!-forall-expr tl expression))
                  ((exists-expr? ex) (make!-exists-expr tl expression))
                  (otherwise (error "Invalid expression ~S" ex)))))
          (abstract-over ((list hd) stream :wrap t)
            (pp-dk* stream subex)))))))

(defmethod pp-dk* (stream (ex application) &optional colon-p at-sign-p)
  "Print application EX. The expression EX ``f(e1,e2)(g1,g2)'' will be printed
as ``f (σ (e1 ^^ e2)) (σ (g1 ^^ g2))''."
  (declare (ignore at-sign-p))
  (let ((op (operator* ex))
        (args (mapcar #'normalise-arg (arguments* ex))))
    (assert (every (lambda (e) (atom e)) args))
    (with-parens (stream colon-p)
      (pp-dk* stream op)
      (let ((doms (fundomains op)))
        ;; The procedure is the following: for each argument, look if we could
        ;; get an appropriate domain type from the operator (this is not that
        ;; clear). If that's the case, then call `cast-required-p' to know
        ;; whether the argument must use an abstract cast operator.
        (loop for arg in args do
          (princ #\Space stream)
          (let ((dom (unless (endp doms) (pop doms))))
            (if (and dom (cast-required-p dom (type arg)))
                (format stream
                        "(cast ~:/pvs:pp-dk*/ ~:/pvs:pp-dk*/ _ ~:/pvs:pp-dk*/)"
                        (type arg) dom arg)
                (pp-dk* stream arg t))))))))

;; LET IN expression are processed by the application case

(defmethod pp-dk* (stream (ex projection-application)
                   &optional colon-p at-sign-p)
  "For projections of the form t`2."
  (declare (ignore at-sign-p))
  (with-parens (stream colon-p)
    (with-slots (id index argument) ex
      (format stream "proj (A.pure ~d) ~/pvs:pp-dk*/" (1- index) argument))))

;; TODO: print a dependent version with `consd' when needed
(defun pprint-tuple (exs stream)
  "Print expressions EXS as components of a tuple on stream STREAM."
  (if (double exs)
      (let ((ex1 (car exs)) (ex2 (cadr exs)))
        (format stream "@^^ ~{~:/pvs:pp-dk*/~^ ~}"
                (list (type (car exs)) (type (cadr exs)) ex1 ex2)))
      (progn
        (format stream "@^ (A.pure ~d) ~:/pvs:pp-dk*/ _ ~:/pvs:pp-dk*/ "
                (length (cdr exs)) (type (car exs))
                (car exs))
        (pprint-tuple (cdr exs) stream))))

(defmethod pp-dk* (stream (ex tuple-expr) &optional colon-p at-sign-p)
  (declare (ignore at-sign-p))
  (with-parens (stream colon-p)
    (pprint-tuple (exprs ex) stream)))

(defmethod pp-dk* (stream (ex branch) &optional colon-p at-sign-p)
  "IF(a,b,c)"
  (with-parens (stream colon-p)
    ;; Always print the common supertype
    (format stream "@if ~:/pvs:pp-dk*/ ~:/pvs:pp-dk*/ "
            (type ex) (condition ex))
    (format stream "(λ ~a: Prf ~:/pvs:pp-dk*/, ~/pvs:pp-dk*/)"
            (fresh-var) (condition ex) (then-part ex))
    (princ #\Space stream)
    (format stream "(λ ~a: Prf (¬ ~:/pvs:pp-dk*/), ~/pvs:pp-dk*/)"
            (fresh-var) (condition ex) (else-part ex))))

;;; REVIEW: factorise disequation and equation

(defmethod pp-dk* (stream (ex equation) &optional colon-p at-sign-p)
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
        (format stream "= (~:/pvs:pp-dk*/ ^^ ~:/pvs:pp-dk*/)" argl argr)))))

(defmethod pp-dk* (stream (ex disequation) &optional colon-p at-sign-p)
  "/=(A, B)"
  (with-parens (stream colon-p)
    (let* ((eq-ty (type (operator ex)))
           (dom (types (domain eq-ty)))
           (tyl (car dom))
           (tyr (cadr dom)))
      (assert (equal tyl tyr))
      (with-binapp-args (argl argr ex)
        (format stream "!= (~:/pvs:pp-dk*/ ^^ ~:/pvs:pp-dk*/)" argl argr)))))

(defmethod pp-dk* (stream (ex conjunction) &optional colon-p at-sign-p)
  "AND(A, B)"
  (with-parens (stream colon-p)
    (with-binapp-args (argl argr ex)
      (format
       stream
       "~:/pvs:pp-dk*/ ∧ (λ ~a: Prf ~:/pvs:pp-dk*/, ~/pvs:pp-dk*/)"
       argl (fresh-var) argl argr))))

(defmethod pp-dk* (stream (ex disjunction) &optional colon-p at-sign-p)
  "OR(A, B)"
  (with-parens (stream colon-p)
    (with-binapp-args (argl argr ex)
      (format
       stream
       "~:/pvs:pp-dk*/ ∨ (λ ~a: Prf (¬ ~:/pvs:pp-dk*/),~/pvs:pp-dk*/)"
       argl (fresh-var) argl argr))))

(defmethod pp-dk* (stream (ex implication) &optional colon-p at-sign-p)
  "IMPLIES(A, B)"
  (with-parens (stream colon-p)
    (with-binapp-args (argl argr ex)
      (format
       stream
       "~:/pvs:pp-dk*/ ⇒ (λ ~a: Prf ~:/pvs:pp-dk*/,~/pvs:pp-dk*/)"
       argl (fresh-var) argl argr))))

(defmethod pp-dk* (stream (ex iff) &optional colon-p at-sign-p)
  "IFF(A, B)"
  (with-parens (stream colon-p)
    (with-binapp-args (argl argr ex)
      (format stream "iff ~:/pvs:pp-dk*/ ~:/pvs:pp-dk*/" argl argr))))

(defmethod pp-dk* (stream (ex negation) &optional colon-p _at-sign-p)
  "NOT(A), there is also a `unary-negation' that represents NOT A."
  (with-parens (stream colon-p)
    (format stream "¬ ~:/pvs:pp-dk*/" (argument ex))))

(defmethod pp-dk* (stream (ex number-expr) &optional colon-p at-sign-p)
  ;; PVS uses bignum while lambdapi is limited to 2^30 - 1
  (format stream "~d" ex))

(defmethod pp-dk* (stream (ac actual) &optional colon-p at-sign-p)
  "Formal parameters of theories, the `t' in `pred[t]'."
  (pp-dk* stream (expr ac) colon-p at-sign-p))

(defmethod pp-dk* (stream (ex bitvector) &optional _colon-p _at-sign-p)
  (error "Bitvectors not handled yet."))
