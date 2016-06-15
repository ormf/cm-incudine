;;; **********************************************************************
;;; Copyright (C) 2006 Rick Taube
;;; This program is free software; you can redistribute it and/or   
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************

;;; $Revision: 1560 $
;;; $Date: 2008-01-22 22:11:54 +0100 (Tue, 22 Jan 2008) $

(in-package cm)

					; (progn (cd "/Lisp/sal/") (load "parse.lisp") (load "sal.lisp"))

(defgrammer sexpr
    '((:co )				; comma token
      (:lc )				; left curly token
      (:rc)
      (:lp)				; left paren token
      (:rp)
      (:lb)
      (:rb)
      ;; basic datatypes
      (:int ) (:float ) (:ratio ) (:string ) (:id ) (:key ) (:bool )
      ;; placeholder for #a[...] constructor expressions, which are
      ;; parsed at emit time...
      (:object ) (:class)
      (:number (or :int :float :ratio))
      ;; atomic data
      (:atom (or :int :float :ratio :id :bool))
      ;; quoted lists
      (:list (:lc (* :elt) :rc)  parse-qlist)
      (:elt (or :atom :list :string))
      (:aref (:id :lb :pargs :rb)  parse-aref)
      ;; the :? token class is created by #?
      (:ifexpr (:? :lp :sexpr :co :sexpr (@ :co :sexpr) :rp)
       parse-ifexpr)
      ;; funcall and funargs
      (:funcall (:id :funargs)  parse-funcall)
      (:funargs (or (:lp :rp)
		 (:lp :pargs :rp)
		 (:lp :kargs :rp)
		 (:lp :pargs :co :kargs :rp))
       parse-funargs)
      (:pargs (or (:sexpr (+ :co :sexpr))
	       :sexpr)
       parse-parg)
      (:kargs (or (:key :sexpr (+ :co :key :sexpr))
	       (:key :sexpr))
       parse-karg)
      ;; if token class
      (:?) 
      ;; math ops
      (:+ ) (:-) (:*) (:/) (:%) (:^)
      ;; relations
      (:= ) (:!= ) (:<) (:>) (:<=) (:>=) (:~=)
      ;; logic
      (:! ) (:& ) (:\|) 
      ;; ops
      (:op (or :+ :- :* :/ :% :^
	    := :!= :< :> :<= :>= :~=
	    :! :& :\|
	    ))
      (:mexpr (:term (* :op :term)) parse-mexpr)
      (:term (or (:- :term)  (:! :term) (:lp :mexpr :rp)
	      :ifexpr :funcall :aref :atom 
	      ;; added for general equality
	      :list :string)
       parse-term)
      ;;(:rel (or := :!= :< :> :<= :>=))
      ;;(:relation (:term :rel :term))
      (:sexpr
       (or :mexpr :ifexpr :funcall :aref :list :string :object :class :atom))
      )
  :literals '(:+ :- :* :/ := :!= :< :> :<= :>= :~= :! :& :\| ) ; :? not a literal
  )

(defmacro emiterr (errf at str &rest args)
  `(funcall ,errf
    (make-instance 'sal-error :type ':read
     :text ,(if args `(format nil ,str ,@args) str)
     :start ,at)))

(defmethod emit ((tok token) &optional info errf)
  info errf
  (flet ((id->symbol (tok str err)
	   (if (not str) (setq str (token-string tok)))
	   ;; see if symbol has package qualifier
	   (let ((pkg (cdr (assoc ':pkg (token-info tok)))))
	     (if (not pkg)
		 (read-from-string str)
		 (let* ((nam (nstring-upcase (subseq str 0 pkg)))
			(sym (nstring-upcase (subseq str (+ pkg 1)))))
		   (if (find-package nam)
		       (or (find-symbol sym nam)
			   (intern sym nam))
		       (emiterr err (token-start tok) 
				"No package named ~A" nam)))))))
    (case (token-type tok)
      ((:string )
       (token-string tok))
      ((:key )
       (intern (symbol-name (read-from-string (token-string tok)))
	       ':keyword))
      ((:bool ) 
       (token-lisp tok))
      ;;((:object )
      ;; ;(emit-object-constructor tok info errf)
      ;; ;(token-info tok)
      ;; (token-lisp tol)
      ;; )
      (( :class )
       (let ((name (id->symbol tok nil errf)))
	 (or (find-class name nil)
	     (emiterr errf  (token-start tok)
		      "No class named ~A" name))))
      ((:id )
       ;; check for slot ref id...
       (let* ((info (token-info tok))
	      (name (token-string tok))
	      (dot? (cdr (assoc ':slot info))))
	 (if dot?
	     (let ((inst (id->symbol tok (subseq name 0 dot?) errf))
		   (slot (id->symbol tok (subseq name (+ 1 dot?)) errf)))
	       `(slot-value ,inst ',slot))
	     (id->symbol tok name errf))))
      (t 
       (if (slot-boundp tok 'lisp)
	   (token-lisp tok)
	   (id->symbol tok nil errf)
	   )))))

(defclass list-unit (parse-unit) ())

(defun parse-qlist (args errf)
  errf
  (make-instance 'list-unit :parsed (second args)))

(defmethod emit ((unit list-unit) &optional info errf)
  (let ((lev (get-emit-info ':list info)) ; t if we are sublist
	(pat (get-emit-info ':pattern info))
	(sub (parse-unit-parsed unit)))
    (if (AND (not lev) (not pat))
	;; we are at top level of a list not inside a pattern
	;; really needs anaysis to see what context the list is in
	;; (funcall arg, varibale binding etc). for now we make a null
	;; top-level {} emit (list) to create the list.
	(if (null sub)
	    (list 'list)
	    (let ((l (emit sub (add-emit-info ':list t info) errf)))
	      ;; if in a bindings copy the constant list to avoid user
	      ;; side effecting constant (quoted) data.
	      (if (get-emit-info ':bindings info)
		  `(copy-list (quote ,l))
		  `(quote ,l))))
	(emit (parse-unit-parsed unit) info errf))))

(defclass aref-unit (parse-unit) ())

(defun parse-aref (args errf)
  errf
  ;; <id> <lb> <parg> <rb>
  ;;(print (list :aref-> l))
  (make-instance 'aref-unit
		 :parsed (cons (first args) (third args))))

(defmethod emit ((obj aref-unit) &optional info errf)
  (let ((args (emit (parse-unit-parsed obj) info errf)))
    ;; if only one arg use ELT so that lists can be referenced as
    ;; arrays
    (if (null (cddr args))
	(cons 'elt args)
	(cons 'aref args))))

;; ifexpr

(defun parse-ifexpr (args errf)
  errf
  ;; ? ( <sexpr> , <sexpr> [, <sexpr>])
  ;;(print (list :args-> args))
  (let ((test (third args))
	(then (fifth args))
	(else (sixth args)))
    (list 'if test then  (and else (second else)))))

(defun parse-parg (args errf)
  errf
  ;; args is 1 arg or (arg (:co arg ...))
  ;;(print (list :pargs-> args))
  (if (not (consp args))
      (list args)
      (if (and (consp (cdr args))
	       (consp (cadr args))
	       (token=? (car (cadr args)) ':co))
	  (cons (car args)
		(loop for x in (cadr args)
		      unless (token=? x ':co) 
		      collect x))
	  (list args))))

(defun parse-karg (args errf)
  errf
  ;;(print (list :kargs-> args))
  (list* (car args)
	 (cadr args)
	 (if (and (consp (cddr args))
		  (consp (caddr args))
		  (token=? (car (caddr args)) ':co))
	     (loop for x in (caddr args)
		   unless (token=? x ':co)
		   collect x)
	     (list))))

(defun parse-funargs (args errf)
  errf
  ;; args is (:lp ... :rp) where ... are pargs and/or kargs with :co
  ;; separater. omit lp/rp tokens 
  ;;(print (list :funargs-> args))
  (let ((args1 '()))
    (pop args)				; remove :lp
    (setq args1 (pop args))
    (if (token=? args1 ':rp)
	(list)
	(if (token=? (car args) ':co)
	    (append args1 (cadr args))
	    args1))))

(defclass funcall-unit (parse-unit) ())

(defun parse-funcall (args errf)
  errf
  ;; args is (<id> (<arg>...))
  (make-instance 'funcall-unit
		 :parsed (cons (car args) (cadr args))))

(defparameter *special-forms*
  ;; functions that receive special expansion
  '((make emit-make)))

(defmethod emit ((unit funcall-unit) &optional info errf)
  ;; rewrite special SAL funcalls
  ;; someday funarg analysis could catch common errors...
  (let* ((form (parse-unit-parsed unit))
	 (func (emit (car form) info errf))
	 (spec (assoc func *special-forms* ))
	 )
    (if spec
	(funcall (second spec) form info errf)
	(cons func (emit (cdr form) info errf)))))

(defun emit-make (form info errf)
  (let ((type (second form)))
    ;; if we have a <class> variable in first arg then check for legal
    ;; initargs else expand the form and catch errors at runtime
    (if (token=? type ':class)
	(let* ((class (emit type info errf))
	       (slots (class-slots class))
	       (name (class-name class))
	       (extern (list 'make-instance `(quote ,name))))
	  (loop for (keyw expr) on (cddr form) by #'cddr
		for init = (emit keyw info errf)
		unless (keywordp init)
		do (emiterr errf (+ (token-start type) 2
				    (length (token-string type)))
			    "Illegal initialization for class ~A" 
			    (token-string type))
		do (if (find init slots :test #'member 
			     :key #'slot-definition-initargs)
		       (nconc extern (list init (emit expr info errf)))
		       (emiterr errf (token-start keyw)
				"Invalid initialization for class ~A"
				(token-string type))))
	  extern)
	(cons 'make-instance (emit (cdr form) info errf)))))

(defun parse-mexpr (args errf)
  errf
  ;; args is either <term> or list (<term> (<op> <term> <op> <term>))
  ;;(print (list :mexpr-> args))
  (if (and (consp (cadr args))
	   (is-op? (car (cadr args))))
      (let ((infix (cons (car args) (cadr args))))
	(inf->pre infix))
      (car args)))

(defclass term-unit (parse-unit) ())

(defun parse-term (args errf)
  errf
  ;;(print (list :term-> args))
  ;; "tag" terms so mexpr can tell the difference between funcalls and mexprs
  (make-instance 'term-unit 
		 :parsed (if (consp args)
			     (if (token=? (car args) ':lp)
				 (second args)
				 args)
			     args)))

(defmethod emit ((unit term-unit) &optional info errf)
  (emit (parse-unit-parsed unit) info errf))

					;
;; bindings
					;

(defgrammer bindings
    '((:bindings (:bind (* :co :bind))
       parse-bindings)
      (:bind (or (:id := :sexpr)
	      :id))))

(defclass bindings-unit (parse-unit) ())

(defun map-comma-delimited (fn arg others)
  ;; arg = first datum 
  ;; others = (<,> <expr> ...)
  (cons (funcall fn arg)
	(loop for a in (cdr others) by #'cddr
	      collect (funcall fn a))))

(defun parse-bindings (args errf)
  errf
  (flet ((make-bind (x)
	   (if (consp x)
	       (list (first x) (third x))
	       (list x nil))))
    (make-instance 'bindings-unit
		   :parsed (map-comma-delimited #'make-bind
						(car args)
						(cadr args)))))

(defmethod emit ((unit bindings-unit) &optional info errf)
  (emit (parse-unit-parsed unit)
	;; tell subforms that the context is a let bindings
	(add-emit-info ':bindings t info)
	errf))

;;;
;;; language statement definitons. compound statements dont actually
;;; define the :statement rule itself, this is left up to the
;;; top-level grammers to define
;;;

;;;
;;; 'set' assignment
;;;

(defgrammer assignment
    '((:assignment (:set :assign (* :co :assign))
       parse-assignment)
      (:assign ((or :aref :id) :assigner :sexpr))
      (:assigner (or := :+= :*= :&= :@= :^= :<= :>=))
      (:set ) (:= ) (:+= ) (:*= ) (:&= ) (:@=) (:^=) (:<= ) (:>=) 
      ))

(defclass assignment-unit (parse-unit) ())

(defun parse-assignment (args errf)
  errf
  ;; args = (:set <:id> <:op> <:expr> ({:, <:id> <:op> <:expr>}*))
  ;;(print (list :assignment-> args))
  (pop args)
  (make-instance 'assignment-unit
		 :parsed
		 (map-comma-delimited (lambda (x) x)
				      (car args)
				      (cadr args))))

(defmethod emit ((unit assignment-unit) &optional info errf)
  ;; assigns = ( {(<:id> <:op> <:expr>)}+ )
  (let ((assigns (parse-unit-parsed unit)))
    (let* ((head (list 'progn))
	   (tail head))
      (do ((iter assigns (cdr iter)))
	  ((null iter)
	   ;; insure no value return 
	   ;;(setf (cdr tail) (list (list 'values)))
	   ;; head is (progn ...)
	   (if (null (cddr head)) (cadr head) head)
	   ;;head
	   )
	(let* ((asgn (car iter))
	       (vref (emit (first asgn) info errf))
	       (oper (second asgn))
	       (expr (emit (third asgn) info errf))
	       (form nil))
	  (case (token-type oper)
	    (( := ) (setq form expr))
	    (( :+= ) (setq form `(+ ,vref ,expr)))
	    (( :*= ) (setq form `(* ,vref ,expr)))
	    (( :&= ) (setq form `(nconc ,vref (list ,expr))))
	    (( :@= ) (setq form `(cons , expr ,vref)))
	    (( :^= ) (setq form `(nconc ,vref (copy-list ,expr))))
	    (( :<= ) (setq form `(min ,vref , expr)))
	    (( :>= ) (setq form `(max ,vref , expr)))
	    (t (emiterr errf (token-start oper)
			"unknown assigment operator ~A"
			(token-type oper)))
	    )
	  (setf form `(setf ,vref ,form))
	  (setf (cdr tail) (list form))
	  (setf tail (cdr tail)))))))

;;;
;;; context-sensitive statements like 'return' and 'wait'
;;;

(defgrammer special-statements
    '((:return-from (:return :sexpr (* :co :sexpr))
	parse-return-from)
      (:process-wait (:wait :sexpr)
       parse-process-wait)
      (:return ) (:wait )))

(defclass special-form-unit (parse-unit) ())

(defun parse-return-from (args errf)
  errf
  ;; args = (:return <:sexpr> ({<:,> <:sexpr>}*))
  ;; (print (list :return-> args))
  (make-instance 'special-form-unit
		 :parsed
		 (cons (car args)
		       (map-comma-delimited (lambda (x) x)
					    (cadr args)
					    (caddr args)))))

(defun parse-process-wait (args errf)
  errf
  (make-instance 'special-form-unit :parsed args))

;; infos:
;; (:syntax . {:cltl | :scheme})
;; ({:loop | :run} . t)
;; ({:process | :function} <name>)

(defmethod emit ((unit special-form-unit) &optional info errf)
  ;; retn = (:return {<:sexpr>}+)
  ;; retn = (:wait <:sexpr>)
  (let ((spec (parse-unit-parsed unit))
	(type (get-emit-info ':definition info)))
    ;;(print (list :emit-special-> spec))
    ;;;(setq foo (list spec info))
    (cond ((token=? (car spec) ':return)
	   ;; 'return' is an error if used outside function definition
	   (unless (eql type ':function)
	     (emiterr errf (token-start (car spec))
		      "return statement outside function"))
	   (let ((label (get-emit-info ':function info)))
	     (list 'return-from  label
		   (cons 'values (emit (cdr spec) info errf)))))
	  ((token=? (car spec) ':wait)
	   ;; 'wait' is an error if its called outside 'run' or within
	   ;; a 'loop' inside 'run'.
	   (unless (and (get-emit-info ':run info)
			(not (get-emit-info ':loop info)))
	     (emiterr errf (token-start (car spec))
		      "wait statement outside run block"))
	   (emit spec info errf)))))

;;;
;;; begin [with <var> [= <sexpr>] { , <var> [= <sexpr>]}* 
;;;       {<statement>}+
;;; end

(defgrammer blocks	
    '((:block (:begin (@ :with :bindings) (* :statement) :end)
	parse-block)
      (:begin ) (:with ) (:end )))

(defclass block-unit (parse-unit) ())

(defun parse-block (args errf)
  errf
  ;; args (:begin ([:with <bindings>]) (...) :end)
  ;;(print (list :block-> args ))
  (let ((vars (second args))
	(exec (third args)))
    ;; remove :with token
    (unless (null vars) (setq vars (cadr vars)))
    (make-instance 'block-unit :parsed (list* vars exec))))

(defmethod emit ((unit block-unit) &optional info errf)
  ;; ( <bindings> . <statements>)
  (let ((bloc (parse-unit-parsed unit)))
    (let ((vars (car bloc))		; #<bindings> or ()
	  (body (emit (cdr bloc) info errf)))
      ;; at some point this could pass var decls into (emit body) to catch
      ;; unknown variable errors etc.
      (if (null vars)
	  (cons 'progn body)
	  (list* 'let*
		 (emit vars info errf)
		 body)))))

;;;
;;; 'if' conditional
;;;

(defgrammer conditional
    '((:conditional (or	;; was just if
		     (:if :sexpr :then (@ :statement) (@ :else :statement))
		     (:when :sexpr :statement)
		     (:unless :sexpr :statement))
       parse-if)
      (:if ) (:then ) (:else ) 
      (:when ) (:unless ) ;; added
      ))

(defclass if-unit (parse-unit) ())
(defclass when-collect-unit (parse-unit) ())

(defun parse-if (args errf &optional type)
  errf
  ;; args is (:if <test> :then (...) [:else (...)] )
  ;; or (:when <test> (..)) or (:unless <test> (...))
  ;; (print (list :if-> args))
  (let ((oper (first args))
	(test (second args))
	(then (list))
	(else (list)))
    (if (token=? oper ':if)
	(progn (setq then (fourth args))
	       (setq else (let ((clause (fifth args)))
			    (if (null clause) nil (cdr clause)))))
	(progn
	  ;;(print (list :when-> args))
	  (setq then (third args))
	  ))
    (make-instance (or type 'if-unit)
		   :parsed (list oper test then else))))

(defun parse-if-collect (args errf)
  ;; add 'and' to conjoin individual collects to 'with'
  (let* ((p (parse-if args errf 'when-collect-unit))
	 (l (parse-unit-parsed p))
	 (c (third l)))
    (unless (null (cddr c))
      (setf (third l)
	    (list* (first c) (second c)
		   (loop for tail on (cddr c) by #'cddr
			 collect 'and collect (car tail)
			 collect (cadr tail)))))
    p))

(defmethod emit ((unit if-unit) &optional info errf)
  (let* ((expr (parse-unit-parsed unit))
	 (oper (emit (first expr) info errf))
	 (test (emit (second expr) info errf))
	 (then (third expr))
	 (else (fourth expr)))
    (if (member oper '(when unless))
	(progn
	  (setq then (emit then info errf))
	  (if (eql (car then) 'progn)
	      (list* oper test (cdr then))
	      (list oper test then)))
	(progn
	  (cond ((null then) (setq then nil))
		((null (cdr then)) (setq then (emit (car then) info errf)))
		(t (setq then (cons 'progn (emit then info errf)))))
	  (cond ((null else) (setq else nil))
		((null (cdr else)) (setq else (emit (car else) info errf)))
		(t (setq else (cons 'progn (emit else info errf)))))
	  (if else
	      `(if ,test ,then ,else)
	      `(if ,test ,then))))))

(defmethod emit ((unit when-collect-unit) &optional info errf)
  ;; this joins when, test, collection(s) into 1 flat list,
  ;; ie (when test collect x AND collect y ...)
  (let* ((expr (parse-unit-parsed unit))
	 (oper (emit (first expr) info errf))
	 (test (emit (second expr) info errf))
	 (then (emit (third expr) info errf)))
    ;; then = (:collect x {AND collect y}*)
    (list* oper test then)))

;;;
;;; 'loop' and 'run' iteration
;;;

(defgrammer iteration
    '((:loop-statement (:loop (@ :with :bindings ) 
			(* :stepping )
			(* :termination )
			(+ :statement )
			(@ :finally :statement)
			:end)
       parse-loop-run)
      (:run-statement (:run (@ :with :bindings)
		       (* :stepping)
		       (* :termination)
		       (+ :statement) 
		       (@ :finally :statement)
		       :end)
       parse-loop-run)
      (:stepping (or (:repeat :sexpr)
		  (:for :id := :sexpr (@ :then :sexpr))
		  (:for :id :in :sexpr )
		  (:for :id :over :sexpr (@ :by :sexpr))
		  ;;(:for :id (@ :from :sexpr) (or :below :to) :sexpr (@ :by :sexpr))
		  (:for :id :from :sexpr (@ (or :below :to :above :downto) :sexpr)
			(@ :by :sexpr))
		  (:for :id (or :below :to :above :downto) :sexpr (@ :by :sexpr))
		  )
       parse-stepping)
      (:termination (or (:while :sexpr) (:until :sexpr)))
      ;; terminals
      (:loop ) (:run ) (:repeat ) (:for ) (:from ) (:in ) (:below ) (:to )
      (:above) (:downto) (:by ) (:over) (:while) (:until) (:finally )
      ))

(defclass iteration-unit (parse-unit) ())
(defclass loop-run-unit (iteration-unit) ())
(defclass collecting-unit (iteration-unit) ())

(defun parse-loop-run (args errf )
  errf
  (make-instance 'loop-run-unit :parsed args))

(defun parse-collection (args errf)
  errf
  (let ((op (pop args)))
    (setq args (map-comma-delimited (lambda (x) x) (car args) (cadr args)))
    ;; have to make seperate collect statements for each expr!
    (loop for a in args collect op collect a)))

(defun parse-collecting (args errf)
  errf
  (make-instance 'collecting-unit :parsed args))

(defun parse-stepping (args errf)
  errf
  ;; (print (list :stepping-> args))
  ;; args = (:repeat ...) or (for ...)
  (cond ((or (token=? (first args) ':repeat)
	     (token=? (third args) ':in))
	 args)
	((or (token=? (third args) ':over) (token=? (third args) ':=))
	 ;; flatten optional (:then <sexpr>) into main list
	 ;; flatten optional (:by <sexpr>) into main list
	 (list* (first args) (second args) (third args) (fourth args)
		(fifth args)))
	(t
	 ;; else numeric. flatten optional clauses
	 ;;(loop for x in args if (consp x) nconc x else if (not (null x)) collect x)
	 ;; fourth arg is the first (required) sexpr
	 (list* (first args) (second args) (third args) (fourth args)
		(loop for x in (nthcdr 4 args) if (consp x)
		      append x else if (not (null x)) collect x)))))

(defmethod emit ((unit loop-run-unit) &optional info errf)
  ;; (:run|:loop (with...)((for...))((while...))(statement...)(finally state) :end)
  (let* ((iter (parse-unit-parsed unit))
	 (type (if (token=? (first iter) ':loop) ':loop ':run))
	 (sub? (or (get-emit-info ':run info) ; is THIS expr undeneath a run?
		   (get-emit-info ':loop info)))
	 (info (add-emit-info type t info)) ; add :loop or :run to info
	 (with (second iter))		; with decl
	 (fors (third iter))		; stepping
	 (stop (fourth iter))		; while|until
	 (body (fifth iter))		; actions
	 (done (sixth iter))		; finally clause
	 )
    ;;(print (list :loop-run-emit-> iter))
    ;; do some error checks on run blocks. they must be inside a process
    ;; and cannot be under a run or loop.
    (when (eql type ':run)
      (let ((dtyp (get-emit-info  ':definition info)))
	(unless (eql dtyp ':process)
	  (emiterr errf (token-start (first iter))
		   "run block outside of process")))
      (when sub?
	(emiterr errf (token-start (first iter)) 
		 "recursive run block (under loop or run)")))
    (unless (null with) 
      ;; collect all with bindings into separate 'with {var} = {form}'
      ;; clauses so that they will be sequentially bound by loop macro
      (setq with (loop for b in (emit (cadr with) info errf)
		       nconc (list 'with (first b) '= (second b)))))
    (unless (null fors)
      (setq fors (loop for f in fors nconc (emit f info errf))))
    (unless (null stop)
      (setq stop (loop for f in stop nconc (emit f info errf))))
    (setq body (list* 'do (emit body info errf)))
    (unless (null done)
      (setq done (emit done info errf)))
    (nconc (list (if (eql type ':loop) 'loop 'process))
	   with fors stop body done)))

;;
;;; patterns UNUSED
;;

(defgrammer patterns
    '((:pattern-statement (or (:cycle (* :var-decl)  (@ :of :item-type)
				      (+ :basic-item) (* :pattern-option))
			   (:heap (* :var-decl)  (@ :of :item-type) 
				  (+ :basic-item) (* :pattern-option))
			   (:palindrome (* :var-decl) (@ :of :item-type) 
					(+ :basic-item) (* :pattern-option))
			   (:line (* :var-decl) (@ :of :item-type)
				  (+ :basic-item) (* :pattern-option))
			   (:weighting (* :var-decl) (@ :of :item-type)
				       (+ :weighting-item) (* :pattern-option))
			   (:markov (* :var-decl) (* :of :item-type)
				    (+ :markov-item) (* :pattern-option))
			   (:graph (* :var-decl) (@ :of :item-type)
				   (+ :graph-item) (* :pattern-option))
			   (:rewrite (* :var-decl) (* :of :item-type)
				     (+ :rewrite-item) (* :pattern-option))
			   (:palindrome (* :var-decl) (@ :of :item-type) 
					(+ :basic-item) (* :pattern-option))
			   (:rotation (* :var-decl) (@ :of :item-type) 
				      (+ :basic-item) (* :pattern-option))
			   )
       parse-pattern-statement)
      (:var-decl (:var-type := :sexpr))
      (:var-type (or :with :alias))
      (:item-type (or :notes :keynums :rhythms :amplitudes))
      (:weighting-item (or (:lc :basic-item (* :weighting-option) :rc)
			:basic-item)
       parse-weighting-node)
      (:graph-item (:lc :basic-item (+ :graph-option) :rc)
       parse-graph-node)
      (:markov-item (:lc (* :atomic-item) :-> (+ :basic-item) :rc)
       parse-markov-node)
      (:rewrite-item (:lc (* :atomic-item) :-> (* :atomic-item) :rc)
       parse-rewrite-node)
      (:basic-item (or :atomic-item :list )) ;:string
      (:atomic-item (or :number :id))
      (:weighting-option (or (:weight :atomic-item)
			  (:min :atomic-item)
			  (:max :atomic-item)))
      (:graph-option (or (:to :atomic-item) (:id :atomic-item)))
      (:pattern-option (:pattern-init :sexpr))
      (:pattern-init (or :for :from :to :in :through :repeat :tempo :elide))
      ;; terminals
      (:with ) (:alias )
      (:cycle ) (:heap ) (:palindrome ) (:line ) (:weighting )
      (:markov) (:graph ) (:rewrite) (:rotation)
      (:notes ) (:keynums ) (:rhythms ) (:amplitudes )
      (:id ) (:weight ) (:min ) (:max ) (:->)
      (:for ) (:from ) (:of ) (:to ) (:in ) (:through ) (:repeat )
      (:initially) (:tempo ) (:elide ) (:rotations )
      ))

(defclass pattern-unit (parse-unit) ())

(defun parse-pattern-statement (args errf)
  errf
  (make-instance 'pattern-unit :parsed args))

(defmethod emit ((unit pattern-unit) &optional info errf)
  (let* ((expr (parse-unit-parsed unit))
	 (patn (emit (first expr) info errf))
	 (vars (emit (second expr) info errf))
	 (type (emit (third expr) info errf))
	 (data (emit (fourth expr) info errf))
	 (opts (emit (fifth expr) info errf)))
    (append '(pattern)
	    (apply #'append vars)
	    (list patn)
	    type
	    data
	    (apply #'append opts))))

(defun parse-weighting-node (a e) (parse-pattern-node a e 'weighting))
(defun parse-graph-node (a e) (parse-pattern-node a e 'graph))
(defun parse-markov-node (a e) (parse-pattern-node a e 'markov))
(defun parse-rewrite-node (a e) (parse-pattern-node a e 'rewrite))

(defun parse-pattern-node (args errf type)
  ;; strip :lc :rc from args so it becomes a lisp list.
  errf
  (if (consp args)
      ;; remove outer curly bracket tokens from node
      (let ((data (loop for a in (cdr args) until (token=? a ':rc) collect a)))
	(case type
	  ((weighting graph)
	   ;; weighting and graph: node options are a list of lists,
	   ;; flatten...
	   (list* (first data) (apply #'append (second data))))
	  ((markov rewrite)
	   ;; flatten left hand and right hand side around ->
	   (append (first data) (list (second data)) (third data)))))
      args))

(defun parse-pattern-constructor (str pos errf)
  pos
  (multiple-value-bind (bool form)
      (parse (list sexpr patterns ) :pattern-statement str
	     :info (add-emit-info ':pattern t (list)))
    (if (not bool) (funcall errf form)
	(make-instance 'token :type ':object
		       :string str :lisp form)))) 

;;;
;;; Statements
;;;

(defun restate (name args errf &optional (sw 1)) 
  errf
  ;;(print (list :args-> args))
  (case sw
    ((1) (cons name (cdr args)))
    ((2)
     ;; args = (<cmd> <sexpr> ({<,> <sexpr>}*)
     (let ((coms (cdr args)))
       (cons name (map-comma-delimited (lambda (x) x) (car coms)
				       (cadr coms)))))
    ((3 )
     ;; args = (<cmd> <sexpr> ({<,> <key> <sexpr>}*)
     (list* name 
	    (second args)
	    (loop for tail on (third args) by #'cdddr
		  collect (second tail) collect (third tail))))
    ((4 )
     ;; args = (<cmd> {<sexpr> | <key> <sexpr>} ({<,> <sexpr> | <key> <sexpr>})
     (cons name
	   (loop for a in (list* (second args) (third args))
		 unless (token=? a ':co)
		 if (consp a) append a else collect a)))))

(defgrammer statement
    `(
      (:print-statement (:print :sexpr (* :co :sexpr))
       ,(lambda (a e) (restate 'sal-print a e 2)))
      (:exec-statement (:exec :sexpr (* :co :sexpr))
       ,(lambda (a e) (restate 'progn a e 2) ))
      (:open-statement (:open :sexpr (* :co :key :sexpr))
       ,(lambda (a e) (restate 'sal-open a e 3)))
      (:sprout-statement (:sprout :sexpr (@ :co :sexpr)) 
       ,(lambda (a e) e (make-instance 'sprout-unit :parsed a)))
      (:output-statement (:output :sexpr) )
      (:load-statement (:load :sexpr) 
       ,(lambda (a e) (restate 'sal-load a e 1)))
      (:system-statement (:system :sexpr (* :co :key :sexpr))
       ,(lambda (a e) (restate 'sal-system a e 3)))
      (:chdir-statement (:chdir :sexpr)
       ,(lambda (a e) (restate 'sal-chdir a e 1)))
      (:play-statement (:play :sexpr) )
      (:plot-statement (:plot (or (:key :sexpr) :sexpr) (* :co (or (:key :sexpr) :sexpr)))
       , (lambda (a e) (restate 'sal-plot a e 4)))
      (:rts-statement (:rts :sexpr)
       ,(lambda (a e) (restate 'sal-rts a e 1)))
      (:define-statement (:define :declaration) parse-define-command)
      (:statement (or :block 
		   :conditional
		   :assignment 
		   :print-statement
		   :exec-statement
		   :open-statement
		   :sprout-statement
		   :output-statement
		   :load-statement
		   :system-statement
		   :chdir-statement
		   :play-statement
		   :rts-statement
		   :plot-statement
		   :loop-statement 
		   :return-from		; emit check legality
		   :process-wait	; emit checks legality
		   :define-statement
		   ))
      (:statement-sequence (+ :statement) 
       ,(lambda (a e) e 
		;; dont bother with an emit method, just wrap a progn around
		;; multiple commands
		(if (null (cdr a)) (car a) (list* 'progn a))))
      (:print ) (:open)  (:sprout ) (:output ) (:load) (:system) (:chdir ) 
      (:play ) (:exec ) (:plot)  (:rts) (:define )
      )
  )

(defclass sprout-unit (parse-unit) ())

(defmethod emit ((unit sprout-unit) &optional info errf)
  ;; args = (:sprout|:output <sexpr> ([:at <sexpr>]))
  (let* ((data (parse-unit-parsed unit))
	 (at (third data))
	 (fn (emit (first data) info errf)))
    ;; at some point this will check info to see if we are
    ;; emitting under a a defprocess and, if so, optimize the
    ;; output or sprout statements
    ;; sprout as top-level command calls sal-sprout.
    (when (and (eql fn 'sprout)
	       (not (get-emit-info ':definition info)))
      (setq fn 'sal-sprout))
    (if (null at)
	(emit (list fn (second data)) info errf)
	(emit (list fn (second data) 
		    ':at (second at))
	      info errf))))

;;;
;;; declarations
;;;

(defgrammer declarations
    '((:declaration (or :vardecl :fundecl :procdecl))
      (:vardecl (:variable :bindings)
       parse-variable)
      (:variable )
      (:fundecl (:function :id (or (:lp :rp)
				   (:lp :id (* :co :id) :rp))
		 :statement)
       parse-function)
      (:function )
      (:procdecl (:process :id (or (:lp :rp)
				   (:lp :id (* :co :id) :rp))
		  :process-body)
       parse-process)
      (:process-body
       (or :run-statement
	(:begin (@ :with :bindings) (* :statement) 
		:run-statement
		:end)))
      (:process )
      ))
     
(defclass declaration-unit (parse-unit) ())
(defclass function-decl (declaration-unit) ())
(defclass process-decl (declaration-unit) ())
(defclass variable-decl (declaration-unit) ())
;; others can be added:
					;(defclass pattern-decl (declaration-unit) ())
					;(defclass class-decl (declaration-unit) ())
					;(defclass command-decl (declaration-unit) ())
					;(defclass synthdef-decl (declaration-unit) ())
					;(defclass instrument-decl (declaration-unit) ())

(defgeneric declaration-type (unit))
(defmethod declaration-type ((u variable-decl)) ':variable)
(defmethod declaration-type ((u function-decl)) ':function)
(defmethod declaration-type ((u process-decl)) ':process)

(defun parse-variable (args errf)
  errf
  ;; args = (:variable (:bindings . (:id :sexpr) ...))
  ;;(print (list :variable-> args))
  (make-instance 'variable-decl :parsed (second args)))

(defmethod emit ((unit variable-decl) &optional info errf)
  (let ((bind (emit (parse-unit-parsed unit) 
		    (add-emit-info ':definition ':variable info)
		    errf)))
    (if (null (cdr bind))
	(cons 'defparameter (car bind) )
	(cons 'progn (loop for b in bind
			   collect (cons 'defparameter b))))))

(defun parse-function (args errf &optional (type 'function-decl))
  ;; args = (:function :id (:lp ... :rp) <statement>)
  ;;(print (list :function-> args))
  errf
  (let ((name (second args))
	(args (third args))
	(body (fourth args)))
    (pop args)				; remove :lp
    (make-instance type :parsed
		   (list 
		    name
		    (if (token=? (car args) ':rp)
			(list)
			(map-comma-delimited (lambda (x) x)
					     (car args)
					     (cadr args)))
		    body))))

(defmethod emit ((unit function-decl) &optional info errf)
  ;;(print (list :emit-func-> func))
  (let* ((func (parse-unit-parsed unit))
	 (name (emit (car func) info errf))
	 (args (emit (cadr func) info errf))
	 (body (emit (caddr func)
		     ;; add (:function . name) to info
		     (add-emit-info ':function name info)
		     errf)))
    `(defun ,name ,args ,body (values))))

;;;
;;; process
;;;

(defun parse-process (args errf)
  (parse-function args errf 'process-decl))

(defmethod emit ((unit process-decl) &optional info errf)
  (let* ((func (parse-unit-parsed unit))
	 (name (emit (car func) info errf))
	 (args (emit (cadr func) info errf))
	 (body (emit (caddr func)
		     (add-emit-info ':process name info)
		     errf)))
    ;;(print body)
    (if (eql (car body) 'process)
	`(defprocess ,name ,args ,body)
	;; (begin [(with ((v1 val) ...))] (...) (process... ) end)
	(let ( ;; optional binding list
	      (vars (second (second body)))
	      ;; optional forms before run expansion
	      (forms (third body)))
	  (if vars
	      `(defprocess ,name ,args (let* ,vars ,@forms ,(fourth body)))
	      `(defprocess ,name ,args ,@forms ,(fourth body)))))))

(defclass define-unit (parse-unit) ())

(defun parse-define-command (args errf)
  errf
  ;; args= (:define #<decl>)
  (make-instance 'define-unit :parsed (second args)))

(defmethod emit ((unit define-unit) &optional info errf)
  (let* ((decl (parse-unit-parsed unit))
	 (type (declaration-type decl)))
    ;; if we are already under a definition just return the lisp form...
    (if (get-emit-info ':definition info)
	(emit decl info errf)
	(let ((info (add-emit-info ':definition type info)))
	  ;; add def type to info
	  `(sal-define ',type ',(emit decl info errf))))))

(defparameter *sal-eval* t)
(defparameter *sal-trace* nil)
(defparameter *sal-input* nil)
(defparameter *sal-break* nil)
(defparameter *sal-grammer* 
  (list sexpr bindings conditional assignment ; execute 
	iteration 
	special-statements blocks
	declarations
					;command
	statement
	))

(defparameter *top-level-pattern*
  ;; default patterns we allow at top-level
  ':statement
					;  '(or :command :block :conditional :assignment ; :execute
					;    :loop-statement )
  )

;;;
;;; the main function. accepts string input, calls parser and then evals
;;;

(defun report-error (string)
  (if (find ':grace *features*) 
      (funcall (find-symbol "REPORT-ERROR" :grace) string)
      (write-string string)))

(defun sal (input &rest args &key (pattern *top-level-pattern*) 
	    (grammer *sal-grammer*) (expand nil) &allow-other-keys)
  ;; slime nonsense: package needs to be reset before input string is
  ;; read
  args
  (let ((*package* (find-package ':cm)))
    (setq *sal-input* input)
    (multiple-value-bind (a b c)
	(parse grammer pattern input)
      c
      (cond ((not a)
	     (if (typep b 'sal-error)
		 (progn
		   ;; add input text if error doesnt contain it
		   (unless (sal-error-line b)
		     (setf (sal-error-line b) input))
		   (report-error (pperror b nil))
		   ))
	     )
	    (expand
             (let ((*print-case* ':downcase))
	       (if (and (consp b) (eql (car b) 'sal-define))
		   (setf b (third b)))
	       (if (and (consp b) (eql (car b) 'quote))
		   (setf b (second b)))
	       (if (and (consp b) (eql (car b) 'progn)
			(null (cddr b)))
		   (setf b (second b)))
	       (pprint b)
	       (force-output *standard-output*)))
	    (t
	     (sal-eval b)))
      (values))))

(defun sal-eval (form &optional recursive?)
  (if *sal-eval*
      (if (or *sal-break* recursive?)
	  (eval form)
	  (handler-case (progn
			  #-sbcl (eval form)
			  #+sbcl
			  (handler-bind 
			      ((style-warning  #'muffle-warning)
			       (warning #'muffle-warning)
			       (sb-ext:compiler-note #'muffle-warning)
			       (sb-ext:code-deletion-note #'muffle-warning))
			    (eval form)))
	    (error (c)
	      (let ((text (format nil ">>> Lisp runtime error:~%    ~A~&"
				  c )))
		(report-error text)
		)))
	  ))
  (values))

(defparameter *sal-print-decimals* 3)
(defparameter *sal-print-list* t)

(defun sal-printer (x &key (stream *standard-output*) )
  (let ((*print-case* ':downcase))
    (cond ((and (consp x) *sal-print-list*)
	   (write-char #\{ stream)
	   (loop while x
	      do (sal-printer (car x) :stream stream) 
	      (if (cdr x) (write-char #\space stream))
	      (setq x (cdr x)))
	   (write-char #\} stream))
	  ((not x)
	   (write-string "#f" stream) )
	  ((eq x t)
	   (write-string "#t" stream))
	  ((floatp x)
	   (if (eq *sal-print-decimals* t) (princ x stream)
	       (princ (decimals x *sal-print-decimals*) stream)))
	  ((stringp x)
	   (write-string x stream))
	  (t
	   (write x :stream stream)))))

(defparameter *sal-printer* #'sal-printer)

(defun sal-message (string &rest args)
  (apply #'format *standard-output* string args)
  (terpri *standard-output*)
  (force-output *standard-output*)
  )

;;;
;;; Plot command. for now its just gnuplot, someday plotter available
;;; too.
;;;

(defun sal-plot (&rest args)
  (if (and *out* (typep *out* 'gnuplot-file))
      (apply (function gnuplot)
	     (file-output-filename *out*)
	     (append (event-stream-args *out*) args))
      (sal-message "No plotting stream open")))

(defun sal-define (type form)
  (let (name data)
    (cond ((member type '(:function :process))
	   (setq name (cadr form)
		 data (caddr form))) ; data=args
	  ((eql type ':variable)
	   (if (eql (car form) 'progn)
	       (setq name (mapcar #'cadr (cdr form)))
	       (setq name (list (cadr form))))))
    (sal-eval form t)
    (cond ((eql type ':variable)
	   (dolist (v name)
	     (format *standard-output* "~@(~A~): ~(~A~) = " type v)
	     (funcall *sal-printer* (symbol-value v))
	     (terpri *standard-output*)
	     (force-output *standard-output*)
	     )
	   )
	  ((member type '(:function :process))
	   (if (null data)
	       (sal-message "~@(~A~): ~(~A~)()"
			    type name)
	       (sal-message "~@(~A~): ~(~A~)(~(~A~)~{, ~(~A~)~})"
			    type name (car data) (cdr data)))))
    (values)))

(defun sal-print (&rest args)
  (map nil *sal-printer* args)
  (terpri *standard-output*)
  (force-output *standard-output*)
  (values))

(defun plus (&rest nums)
  (apply #'+ nums))

(defun minus (num &rest nums)
  (apply #'- num nums))

(defun times (&rest nums)
  (apply #'* nums))

(defun divide (num &rest nums)
  (apply #'/ num nums))

; dir "*.*
; chdir
; load "rts.sys"

(defun sal-chdir ( dir)
  (cd (expand-path-name dir))
  (sal-message "Directory: ~A" (pwd))
  (values))

;;; sigh, not all lisps support ~/ directory components.

(defun expand-path-name (path &optional absolute?)
  (let ((dir (pathname-directory path)))
    (flet ((curdir ()
	     (truename 
	      (make-pathname :directory
			     (pathname-directory
			      *default-pathname-defaults*)))))
      (cond ((null dir)
	     (if (equal path "~") 
		 (namestring (user-homedir-pathname))
		 (if absolute? 
		     (namestring (merge-pathnames path (curdir)))
		     (namestring path))))
	    ((eql (car dir) ':absolute)
	     (namestring path))
	    (t
	     (let* ((tok (second dir))
		    (len (length tok)))
	       (if (char= (elt tok 0) #\~)
		   (let ((uhd (pathname-directory (user-homedir-pathname))))
		     (if (= len 1)
			 (namestring
			  (make-pathname :directory (append uhd (cddr dir))
					 :defaults path))
			 (namestring
			  (make-pathname :directory
					 (append (butlast uhd)
						 (list (subseq tok 1))
						 (cddr dir))
					 :defaults path))))
		   (if absolute?
		       (namestring (merge-pathnames  path (curdir)))
		       (namestring path)))))))))

(defun sal-load (&rest args)
  (let ((files (list))
	(comp? nil)
	(verb? nil))
    comp? verb?
    (do ((tail args (cdr tail)))
	((or (null tail) (keywordp (car tail)))
	 (setq args tail)
	 (setq files (nreverse files)))
      (push (car tail) files))
    (setq verb? (getf args ':verbose))
    (setq comp? (getf args ':compile))
    (do ((tail files (cdr tail))
	 (type nil)
	 (file nil))
	((null tail) )
      (setq file (expand-path-name (car tail)))
      (setq type (pathname-type file))
      (sal-message "Loading: ~a" file)
      (cond ((not type)
	     (load file))
	    ((string= type "sal")
	     (load-sal-file file))
	    (t
	     (if comp?
		 (cload file :verbose verb?)
		 (load file :verbose verb?)))))
    (values)))

(defun sal-system (sys &rest pairs)
  (apply #'use-system sys pairs))

(defun load-sal-file (file)
  (with-open-file (f file :direction :input)
    (let ((input (make-array '(512) :element-type 'character
			     :fill-pointer 0 :adjustable t)))
      (loop with flag
	 for char = (read-char f nil ':eof)
	 until (or flag (eql char ':eof))
	 do
	   (when (char= char #\;)
	     (loop do (setq char (read-char f nil :eof))
		until (or (eql char :eof)
			  (char= char #\newline))))
	   (unless (eql char ':eof)
	     (vector-push-extend char input)))
      (sal input :pattern :statement-sequence))))

(defun sal-open (&rest args)
  (cond ((null args)
	 (if *out*
	     (sal-message "Opened: ~A" *out*)
	     (sal-message "No open output stream")))
	((equal args '(nil))
	 (when *out*
	   (let ((old *out*))
	     (when (typep *out* 'rt-stream) (close-io *out* ':force))
	     (setq *out* nil)
	     (sal-message "Closed: ~A" old))))
	(t
	 (setq *out* (apply #'init-io args))
	 (when (typep *out* 'rt-stream)
	   (open-io *out* t))
	 (sal-message "Opened: ~A" *out*))))

; (sal-open "test.mid")
; (sal-open)
; (sal-open nil)

(defun sal-sprout (obj &key at to)
  (if *out*
      (if (and (typep *out* <rt-stream>)
	       (eql (rts?) :running))
	  (sprout obj :at at :to to)
	  (let ((x (events obj *out* (or at 0))))
	    (sal-message "Wrote: ~A" x)))
      (sal-message "No output stream open")))

(defun sal-rts (mode)
  ;; mode 0 = off -1 = pause 1 = on,sec 1000 = on,msec
  (let ((stat (rts?)))
    (if (not stat)
	(sal-message "RTS system not loaded")
	(if (eql mode ':?)
	    (sal-message "RTS is ~(~A~)" stat)
	    (if (member mode '(-1 0 1 1000))
		(if (typep *out* 'rt-stream)
		    (cond ((and (eql stat ':stopped)
				(or (eql mode 1) (eql mode 1000)))
			   (rts *out* :time-format
				(if (= mode 1) :sec :msec)))
			  ((and (eql stat :running)
				(eql mode 0))
			   (rts-stop))
			  ((and (eql stat :running)
				(eql mode -1))
			   (rts-pause)
			   (sal-message "RTS paused"))
			  ((and (eql stat ':paused)
				(eql mode 1))
			   (rts-continue)
			   (sal-message "RTS continuing"))
			  (t
			   (sal-message "RTS status ~(~A~) unchanged"
					stat)))
		    (if (null *out*)
			(sal-message "No output port open")
			(sal-message "Output stream ~S does not support RTS"
				     *out*)))
		(sal-message "RTS mode ~S is not one of 0 -1 1 1000 :?"
			     mode))))))

(pushnew ':sal *features*)
