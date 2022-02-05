(in-package :pvs)

(defun prove-formula (formref &optional rerun?)
  "Starts a proof with formula given by formref."
  (when *in-checker*
    (pvs-error "Prove-formula error" "Must exit the prover first"))
  (let ((fdecls (collect-formula-decls formref)))
    (cond ((null fdecls)
	   (error "No declarations found matching ~a" formref))
	  ((cdr fdecls)
	   (error "~a has ~d matching declarations:~{~%  ~a~}"
		  formref (length fdecls) (mapcar #'make-formref fdecls)))
	  (t (let ((fdecl (car fdecls)))
	       (with-workspace (context-path (module fdecl))
		 (let* ((strat (when rerun? '(rerun))))
		   (read-strategies-files)
		   (setq *last-proof* (prove fdecl :strategy strat)))))))))

(defun make-formref (fdecl)
  (let ((th (module fdecl)))
    (format nil "~a~a.pvs#~a#~a"
      (context-path th) (filename th) (id th) (id fdecl))))

(defun collect-formula-decls (formref)
  "Returns all formulas visible to the current context that satisfy formref.
the idea being that you should be able to start the prover on any formula
specifying as little as possible."
  (if (formula-decl? formref)
      (list formref)
      (multiple-value-bind (pvsfile lib ext thname fmname)
	  (parse-referent formref)
	;; if only one of pvsfile thname fmname is set, it is the fmid If
	;; two, thname is nil; hence if the file isn't found, it should be
	;; treated as a theory name for matching purposes.
	(let ((fmid (or fmname thname pvsfile))
	      (thid (when fmname thname))
	      (file (when (or fmname thname) pvsfile))
	      (fdecls nil))
	  (unless (or thname fmname (and (null lib) (null ext)))
	    (error "formula name not specified: ~a" formref))
	  (flet ((add-matching (th)
		   (when (and (module? th)
			      (or (null thid) (string= thid (id th))))
		     (dolist (fmla (all-formulas th))
		       ;; Could allow regexps here
		       (when (string= fmid (id fmla))
			 (push fmla fdecls))))))
	    (if lib
		(let* ((path (if (char= (uiop:last-char lib) #\@)
				 (or (get-library-reference lib)
				     (error "Library ~a not found" lib))
				 lib))
		       (ws (find path *all-workspace-sessions*
				 :key #'path :test #'pathname-equal)))
		  (unless (uiop:directory-exists-p path)
		    (error "~a not found" path))
		  (cond (ws
			 (do-theories #'add-matching (pvs-theories ws)))
			((pathname-equal (format nil "~a/lib/" *pvs-path*) lib)
			 (do-theories #'add-matching *prelude*))
			(t (let ((theories (typecheck-file (format nil "~a~a.pvs" path file))))
			     (dolist (th theories) (add-matching th))))))
		(do-all-theories #'add-matching))
	    fdecls)))))

(defun parse-referent (refobj)
  "Given a ref string of the form dir/fname.ext#id1#id2, this returns the
values fname dir ext id1 id2. Note that any of them may be missing down to a
single name, which will be returned as the fname. Examples:
  dir/fname.ext#id1#id2 => fname dir/ ext id1 id2
  lib@fname.ext#id1#id2 => fname ldir/ ext id1 id2   ldir from PVS_LIBRARY_PATH
  lib@fname.ext#id1#id2 => fname lib@ ext id1 id2    with '@' attached
  fname#id1#id2         => fname nil nil id1 id2
  fname#id1             => fname nil nil id1
  fname                 => fname
"
  (let* ((ref (typecase refobj
		(symbol (string refobj))
		(pathname (namestring refobj))
		(t refobj)))
	 (args (split ref #\#))
	 (path (car args))
	 (dir (cond ((find #\/ path) ;; always a directory - note that it could contain '@'
		     (subseq path 0 (1+ (position #\/ path :from-end t))))
		    ((find #\@ path) ;; lib@file - try to look it up in PVS_LIBRARY_PATH
		     (or (get-library-reference
			  (subseq path 0 (position #\@ path)))
			 (subseq path 0 (1+ (position #\@ path)))))))
	 (file (cond ((find #\/ path)
		      (subseq path (1+ (position #\/ path :from-end t))))
		     ((find #\@ path)
		      (subseq path (1+ (position #\@ path))))
		     (t path)))
	 (epos (search ".pvs" file :from-end t :test #'string-equal))
	 (name (if (and epos
			(= epos (- (length file) 4)))
		   (subseq file 0 epos)
		   file))
	 (ext (when (and epos
			 (= epos (- (length file) 4)))
		"pvs")))
    (assert name)
    (unless (or (null ext) (string-equal ext "pvs"))
      ;; Not a pvs extension, we push it back to the name in case
      ;; name.ext.pvs is the actual filename, and "pvs" was omitted.
      (setq name (format nil "~a.~a" name ext))
      (setq ext nil))
    (values-list (cons name (cons dir (cons ext (cdr args)))))))

#-sbcl
(defun pathname-equal (p1 p2)
  (uiop:pathname-equal p1 p2))

#+sbcl
(defun pathname-equal (p1 p2)
  ;; Fails in comparing "~/foo" to "/home/user/foo" in SBCL
  ;; We don't use truename, as that assumes the paths exist
  (uiop:pathname-equal (uiop:native-namestring p1) (uiop:native-namestring p2)))
