(in-package :dksig)

(defun some-pvs-type-p (thing)
  (or (null thing) (pvs::type-expr? thing)))

(deftype some-pvs-type ()
  "A PVS type or `nil'."
  '(or null (satisfies some-pvs-type-p)))

(defstruct variant
  "A variant of a symbol. It is identified by its type, and has a suffix to be
appended to the symbol."
  (type nil :type some-pvs-type) (suffix "" :type string))

(deftype variants ()
  "A list of variants."
  '(and list (satisfies variants-p)))

(defun variants-p (thing)
  (and (listp thing) (every #'variant-p thing)))

(defstruct signature
  "A signature is made of the identifier of its theory and a hash table mapping
symbols as they appear in PVS to a list of variants."
  theory
  (context '() :type list)
  (decls (make-hash-table) :type hash-table))

(declaim (ftype (function (symbol list) integer) count-definitions))
(defun count-definitions (sym sigs)
  "Count the number of definitions of symbol SYM inside signatures SIGS"
  (flet ((count (sig) (aif (gethash sym (signature-decls sig)) (length it) 0)))
    (funcall (lrec (lambda (sig f) (+ (count sig) (funcall f))) 0) sigs)))

(declaim (ftype (function (integer) string) mksuffix))
(defun mksuffix (n)
  (format nil "~36r" n))

(declaim (ftype (function (some-pvs-type some-pvs-type) *) some-pvs-type-eq))
(defun some-pvs-type-eq (x y)
  "PVS equality on optional terms. X and Y are equal if they are both `nil' or
if they are `pvs:ps-eq'. Behaviour on open term is undefined."
  (or (and (null x) (null y)) (and x y (pvs:ps-eq x y))))

;;; Adding and finding symbols

(declaim
 (ftype (function (symbol some-pvs-type signature list)
                  (values symbol signature)) add))
(defun add (sym ty sig opened)
  "Add the declaration of symbol SYM of type TY to the signature SIG considering
that signatures OPENED are opened, and return the new identifier that is to be
used in place of SYM and the new signature.  Destructive on SIG."
  (assert (every #'signature-p opened))
  (let ((ndefs (count-definitions sym opened)))
    (aif (gethash sym (signature-decls sig))
         (let ((suffix (mksuffix (+ (length it) ndefs))))
           (setf (gethash sym (signature-decls sig))
                 (append1 it (make-variant :type ty :suffix suffix)))
           (values (symb sym suffix) sig))
         (let* ((suffix (if (= ndefs 0) "" (mksuffix ndefs)))
                (initial (list (make-variant :type ty :suffix suffix))))
           (setf (gethash sym (signature-decls sig)) initial)
           (values (symb sym suffix) sig)))))

(declaim
 (ftype (function (symbol some-pvs-type signature) (or null symbol)) find1))
(defun find1 (sym ty sig)
  "Get the appropriate identifier for PVS symbol identified with symbol SYM of
type TY among defined symbols of signature SIG."
  (aif (gethash sym (signature-decls sig))
       (aif (cl:find ty it :test #'some-pvs-type-eq :key #'variant-type)
            (symb sym (variant-suffix it)))))

(declaim (ftype (function (symbol some-pvs-type list) (or null symbol))))
(defun find* (sym ty sigs)
  "Search for symbol SYM of type TY among signatures SIGS."
  (if (consp sigs)
      (aif (find1 sym ty (car sigs)) it (find* sym ty (cdr sigs)))))

(defun find (sym ty sig)
  "Find symbol SYM of type TY in signature(s) SIG."
  (if (listp sig) (find* sym ty sig) (find1 sym ty sig)))

;;; Printing signatures
;;;
;;; Signatures are transformed into simple lisp expressions (made of lists,
;;; symbols and strings) via the `normalise-*' functions, and then printed using
;;; the lisp printer.

(declaim (ftype (function (variant) cons) normalise-variant))
(defun normalise-variant (v)
  (with-slots (type suffix) v
    (cons (mkstr type) suffix)))

(declaim (ftype (function (symbol list) cons) normalise-decl))
(defun normalise-decl (s vs)
  (assert (every #'variant-p vs))
  (cons (mkstr s) (mapcar #'normalise-variant vs)))

(declaim (ftype (function (hash-table) list) normalise-decls))
(defun normalise-decls (d)
  (let ((acc nil))
    (maphash (lambda (s vs) (push (normalise-decl s vs) acc)) d)
    (nreverse acc)))

(declaim (ftype (function (list) list) normalise-context))
(defun normalise-context (c)
  (assert (every (lambda (e)
                   (and (consp e) (symbolp (car e)) (pvs::type-expr? (cdr e))))
                 c))
  (mapcar (lambda (x) (cons (car x) (mkstr (cdr x)))) c))

(declaim (ftype (function (signature) list) normalise-signature))
(defun normalise-signature (s)
  (with-slots (theory context decls) s
    (declare (ignore theory))
    (list :context (normalise-context context)
          :decls (normalise-decls decls))))

(declaim (ftype (function (signature stream) *) dump))
(defun dump (sig stream)
  "Write signature SIG to stream STREAM."
  (let ((pvs::*pp-no-newlines?* t)
        (pvs::*pp-compact* t)
        (pvs::*pp-print-pretty* nil))
    (print (normalise-signature sig) stream)))

;;; Rules to parse saved to disk signatures
;;; Signatures are saved in the following format
;;; (:context '()
;;;  :decls ("s0" . ("ty0,0" . "d0,0") ("ty0,1" . "d0,1"))
;;;         ("s1" . ("ty1,0" . "d1,0") ("ty1,1" . "d1,1")))
;;; where s0, s1 are the symbols from PVS, tyi,j are the types of these symbols,
;;; there may be several types when the symbol is overloaded. Types are between
;;; carets to ease parsing. And di,j are the symbols used into the Dedukti
;;; translation, to bypass overloading.

(declaim (ftype (function (string) some-pvs-type) open-some-pvs-type))
(defun open-some-pvs-type (pty)
  (if (string/= pty "NIL") (pvs:pc-parse pty 'pvs:type-expr)))

(declaim (ftype (function (cons) variant) open-variant))
(defun open-variant (ts)
  (destructuring-bind (ty . suff) ts
    (make-variant :type (open-some-pvs-type ty) :suffix suff)))

(defun open* (s)
  "Read stream S and preprocess the lisp expression. Return two values, the
context of the theory and the declarations. S may be either a string or a
stream."
  (let*
      ((presig
         (cond ((stringp s) (with-input-from-string (is s) (read is)))
               ((streamp s) (read s))
               (t (error "Cannot read from ~a." s)))))
    (destructuring-bind (&key context decls) presig
      (assert (listp decls))
      (assert (every #'consp decls))
      (assert (every (lambda (sv) (stringp (car sv)) (listp (cdr sv))) decls))
      (values
       context
       (mapcar
        (lambda (sv) (cons (symb (car sv)) (mapcar #'open-variant (cdr sv))))
        decls)))))

(defun open (theory s)
  (let ((ht (make-hash-table)))
    (multiple-value-bind (ctx decls) (open* s)
      (declare (ignore ctx))
      (assert (every #'variants-p (mapcar #'cdr decls)))
      (assert (every #'symbolp (mapcar #'car decls)))
      (mapc (lambda (d) (setf (gethash (car d) ht) (cdr d))) decls)
      (make-signature :theory theory :decls ht))))
