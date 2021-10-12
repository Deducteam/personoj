(defvar *tptp-id-counter*)
(defvar *tptp-ite-env*)  

(newcounter *tptp-id-counter*)

(defparameter *metit-interpreted-names* 
  '((FALSE (|booleans| . $false))
    (TRUE (|booleans| . $true))
    (IMPLIES (|booleans| . =>))
    (=> (|booleans| . =>))
    (AND (|booleans| . &))
    (& (|booleans| . &))
    (OR (|booleans| . \|)) 
    (NOT (|booleans| . ~))
    (IFF (|booleans| . <=>))
    (<=> (|booleans| . <=>))
    (= (|equalities| . =))
    (/= (|notequal| . /=))
    (< (|reals| . <))
    (<= (|reals| . <=))
    (> (|reals| . >))
    (>= (|reals| . >=))
    (+  (|number_fields| . +))
    (- (|number_fields| . -))
    (* (|number_fields| .  *))
    (/ (|number_fields| . /))
    (^ (|exponentiation| . ^))
    (sin (|sincos_def| . sin) (|trig_basic| . sin)) 
    (cos (|sincos_def| . cos) (|trig_basic| . cos))
    (tan (|sincos_def| . tan) (|trig_basic| . tan))
    (pi (|atan| . pi) (|trig_basic| . pi))
    (e  (|ln_exp| . "exp(1)"))
    (asin (|asin| . arcsin))
    (atan (|atan| . arctan))
    (acos (|acos| . arccos))
    (sqrt (|sqrt| . sqrt))
    (sq (|sq| . sq))
    (ln (|ln_exp| . ln))
    (exp (|ln_exp| . exp))
    (sinh (|hyperbolic| . sinh))
    (cosh (|hyperbolic| . cosh))
    (tanh (|hyperbolic| . tanh))
    (abs (|real_defs| . abs))
    (\#\# (|interval| . \#\#))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Input is a name-expr and a list of bindings of the form 
;; (("Y" . Y2) ("X" . X1)). The bindings are set in translate-metit-bindings
;; Since the metit-named bounded variables travel in the bindings list, when we get
;; the named variable in an expression return the cdr that 
;; holds the variable name. Otherwise we have a constant symbol (such as pi) and call 
;; metit-interpretation.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter tptp-escape nil) 

(defun downcase-tptp-id (id)
  (setq tptp-escape nil)
  (format nil "~{~a~}"
	 (loop for char across (format nil "~a" id)
	       collect
	       (cond ((alpha-char-p char)
		      (string (char-downcase char)))
		     ((and (digit-char-p char) tptp-escape)
		      (format nil "0")
		      (format nil "~c" char)
		      (setq tptp-escape nil))
		     ((and (digit-char-p char) (not tptp-escape))
		      (format nil "~c" char))
		     ((equal char #\_)
		      (setq tptp-escape t)
		      (format nil "_"))
		     (t
		      (format nil "_~d_" (char-int char)))))))

(defun upcase-tptp-id (id)
  (setq tptp-escape nil)
  (format nil "~{~a~}"
	 (loop for char across (format nil "~a" id)
	       collect
	       (cond ((alpha-char-p char)
		      (string-upcase (format nil "~c" char)))
		     ((and (digit-char-p char) tptp-escape)
		      (format nil "0")
		      (format nil "~c" char)
		      (setq tptp-escape nil))
		     ((and (digit-char-p char) (not tptp-escape))
		      (format nil "~c" char))
		     ((equal char #\_)
		      (setq tptp-escape t)
		      (format nil "_"))
		     (t
		      (format nil "_~d_" (char-int char)))))))

(defmethod translate-to-tptp* ((expr name-expr) env)
  (or (cond ((find (id expr) env)
	     (format nil "~a" (upcase-tptp-id (id expr))))
	    ;; ((is-const-decl-expr expr '("pi" "e"))
	    ;;  (metit-interpretation expr))
	    ((assoc (id expr) *metit-interpreted-names*)
	     (metit-interpretation expr))
	    ;; ((is-variable-expr expr)
	    ;;  (cdr (assoc (id expr) bindings :test #'string=)))
	    (t
	     (downcase-tptp-id (format nil "~a" (id expr)))))
      (error "constant/variable ~a cannot be handled 1." expr)))

;; (defmethod translate-to-tptp* ((expr fieldappl))
;;   (or (when (is-variable-expr expr)
;; 	(cdr (assoc (format nil "~a" expr)  bindings :test #'string=)))
;;       (error "field application ~a cannot be handled." expr)))

;; (defmethod translate-to-tptp* ((expr projappl))
;;   (or (when (is-variable-expr expr)
;; 	(cdr (assoc (format nil "~a" expr)  bindings :test #'string=)))
;;       (error "projection ~a cannot be handled." expr)))

(defmethod translate-to-tptp* ((expr decimal) env)
  (declare (ignore env))
  (format nil "(~a / ~a)" (args1 expr) (args2 expr)))

(defmethod translate-to-tptp* ((expr rational-expr) env)
  (declare (ignore env))
  (if (number-expr? expr)
      (number expr)
    (let ((rat (number expr)))
      (format nil "(~a / ~a)" (numerator rat) (denominator rat)))))

(defmethod translate-to-tptp* ((expr string-expr) env)
  (declare (ignore env))
  (error "string ~a cannot be handled" expr))

;;
;; The PVS variables are converted into a Tptp representation (uppercase) and are
;; made distinct by appending *metit-id-counter*. This is required because a user
;; might use both cases in a specification and we need to differentiate between x and X.
;; In this case they would be converted to X1 and X2.
;;

(defun tptp-id-name ()
  (format nil "ite~a" (funcall *tptp-id-counter*)))

;;
;; (argument expr) return a tuple of the arguments of expr
;; (args1 expr) (args2 expr) returns the first and second parts of the expr tuple
;;

(defun translate-to-tptp-list (exprs env)
  (cond ((null exprs)
	 "")
	(t
	 (format nil "~a"
		 (translate-to-tptp-list* exprs env)))))

(defun translate-to-tptp-list* (exprs env)
  (cond ((null (cdr exprs))
	 (translate-to-tptp* (car exprs) env))
	(t
	 (format nil "~a, ~a"
		 (translate-to-tptp* (car exprs) env)
		 (translate-to-tptp-list* (cdr exprs) env)))))

(defmethod translate-to-tptp* ((expr tuple-expr) env)
  (translate-to-tptp-list (exprs expr) env))

(defmethod translate-to-tptp* ((expr application) env)
  (with-slots (operator argument) expr
    (if (name-expr? operator)	      
       (let* ((op-id (metit-interpretation operator)))
	 (case op-id
	   (~ (format nil "(~~ ~a)" (translate-to-tptp* (argument expr) env)))
	   ((<=>)
	    (let ((arg1 (translate-to-tptp* (args1 expr) env))
		  (arg2 (translate-to-tptp* (args2 expr) env)))
	      (format nil "((~a => ~a) & (~a => ~a))" arg1 arg2 arg2 arg1)))
	   (^
	    (format nil "~a~a~a" (translate-to-tptp* (args1 expr) env) 
		    op-id (translate-to-tptp* (args2 expr) env)))
	   ((sq)
	    (format nil "~a^2" (translate-to-tptp* (argument expr) env)))
	   ((sin cos tan sqrt tanh cosh sinh ln exp abs arctan)
	    (format nil "~a(~a)" op-id (translate-to-tptp* (argument expr) env)))
	   (-
	    (if (unary-application? expr)
		(format nil "-~a" (translate-to-tptp* (argument expr) env))
	      (format nil "(~a ~a ~a)"
		      (translate-to-tptp* (args1 expr) env)
		      op-id
		      (translate-to-tptp* (args2 expr) env))))
	   ((/=)
	    (format nil "(~a < ~a | ~a > ~a)"
		    (translate-to-tptp* (args1 expr) env)
		    (translate-to-tptp* (args2 expr) env)
		    (translate-to-tptp* (args1 expr) env)
		    (translate-to-tptp* (args2 expr) env)))
	   ((=)
	    (if (and (type-name? (car (types (domain (type (operator expr))))))
		     (or (eq (id (car (types (domain (type (operator expr)))))) 'bool)
			 (eq (id (car (types (domain (type (operator expr)))))) 'boolean)))
		(format nil "(~a ~a ~a)"
			(translate-to-tptp* (args1 expr) env)
			'<=>
			(translate-to-tptp* (args2 expr) env))
	      (format nil "(~a ~a ~a)"
		      (translate-to-tptp* (args1 expr) env)
		      op-id
		      (translate-to-tptp* (args2 expr) env))))
	   ((< <= > >= + * / => & \|)
	    (format nil "(~a ~a ~a)"
		    (translate-to-tptp* (args1 expr) env)
		    op-id
		    (translate-to-tptp* (args2 expr) env)))
	   ((\#\#)
	    (format nil "(~a <= ~a & ~a <= ~a)"
		    (translate-to-tptp* (args1 (args2 expr)) env)
		    (translate-to-tptp* (args1 expr) env)
		    (translate-to-tptp* (args1 expr) env)
		    (translate-to-tptp* (args2 (args2 expr)) env)))		    
	   (t
	    (cond ((eq (id operator) 'IF)
		   (translate-to-tptp* (mk-if-expr (car (exprs (argument expr)))
						   (cadr (exprs (argument expr)))
						   (caddr (exprs (argument expr))))
					   env))
		  ((and (name-expr? (operator expr))
		     (find (id (operator expr)) env))
		   (format nil "app(~a)"
			(translate-to-tptp-list (list (operator expr) (argument expr)) env)))
		  (t
		   (format nil "~a(~a)"
		      (translate-to-tptp* (operator expr) env)
		      (translate-to-tptp* (argument expr) env))))
	    ;; (error "expression ~a cannot be handled." expr)
	    )))
      (error "expression ~a cannot be handled 2." operator))))

;;
;; translate-metit-bindings : Go through a list of bind declarations and create a
;; bind list with their new metit names. tc-eq ensures we only have real variables.
;;

;; (defun translate-metit-bindings (bind-decls bindings accum)
;;   (cond ((consp bind-decls)
;; 	 (if (tc-eq (type (car bind-decls)) *real*)
;; 	     (let ((yname (metit-id-name (id (car bind-decls)))))
;; 	       (translate-metit-bindings (cdr bind-decls) ;;making bindings here
;; 					 (cons (cons (string (id (car bind-decls)))
;; 						     yname)
;; 					      )
;; 					 (cons yname accum)))
;; 	   (error "type of ~a must be real." (id (car bind-decls)))))
;; 	(t (values bindings (nreverse accum)))))

;;
;; metit-interpretation : Translate pvs symbol to the Tptp representation. Ensures
;; that the resolution (the real meaning of the symbol) is what we want. This is due
;; to the massive overloading in PVS (anything can be overloaded). 
;; Answers the question: is + actually the + for the reals?
;;

(defun metit-interpretation (name-expr)
  (assert (name-expr? name-expr))
  (let* ((id-assoc (cdr (assoc (id name-expr) *metit-interpreted-names*)))
	 (mod-assoc (cdr (assoc (id (module-instance
				     (resolution name-expr)))
				id-assoc))))
    mod-assoc))
 
(defmethod translate-to-tptp* ((expr binding-expr) env)
  (declare (ignore env))
  (error "expression ~a cannot be handled 3." expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; lift-predicates-in-quantifier takes the constraints on the variables (p1 : posreal)
;; and converts it into the proper logical form p1 > 0 & p1 >= 0 and returns
;; a new expression with these propositional atoms in the antecedent
;;
;; The we recursively call translate-metit-bindings to build a cons list of bindvars
;; ((p1 : real. P11))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod translate-to-tptp* ((expr quant-expr) env)
  ;; (let ((new-expr (lift-predicates-in-quantifier expr (list *real*))))
  (with-slots
   (bindings expression) expr
   ;; (multiple-value-bind (newbindings bindvars)
   ;; 	 (translate-metit-bindings expr-bindings bindings nil)
   (let ((yexpression (translate-to-tptp* expression (append (mapcar #'id bindings) env))))
     (cond ((forall-expr? expr)
	    (format nil "(![~{~a~^, ~}]: ~a)" (mapcar #'upcase-tptp-id (mapcar #'id bindings))
		    yexpression))
	   ((exists-expr? expr)
	    (format nil "(?[~{~a~^, ~}]: ~a)" (mapcar #'upcase-tptp-id (mapcar #'id bindings))
		    yexpression)))))) ;;no else case

(defmethod translate-to-tptp* ((expr if-expr) env)
  (if (and (type-name? (type expr))
	   (or (eq (id (type expr)) 'bool)
	       (eq (id (type expr)) 'boolean)))
      (let ((condexpr (translate-to-tptp* (nth 0 (arguments expr)) env)))
	(format nil "((~a => ~a) & ((~~ ~a) => ~a))"
		condexpr
		(translate-to-tptp* (nth 1 (arguments expr)) env)
		condexpr
		(translate-to-tptp* (nth 2 (arguments expr)) env)))
    (if (assoc expr *tptp-ite-env* :test #'tc-eq)
	(cdr (assoc expr *tptp-ite-env* :test #'tc-eq))
      (let ((newsym (tptp-id-name)))
	(setq *tptp-ite-env* (cons (cons expr newsym) *tptp-ite-env*))
	newsym))))

(defmethod translate-to-tptp* :around (expr env)
  (declare (ignore expr env))
  ;; (format t "TEST: ~a~%~%" expr)
  (call-next-method))

(defun translate-to-tptp (expr source)
  (setq *tptp-ite-env* nil)
  (if (or ;; (eq source 'decision-procedure)
	  (eq source 'lift-if)
	  (eq source 'typepred))
      (format nil "%fof(pvs2metit,conjecture, ~a)." 
	      (translate-to-tptp* expr nil))
    (format nil "fof(pvs2metit,conjecture, ~a)." 
	    (translate-to-tptp* expr nil))))
