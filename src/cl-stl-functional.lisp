
(in-package :cl-stl)

(declaim (inline not1
				 not2
				 bind1st
				 bind2nd
				 ptr-fun1
				 ptr-fun2
				 mem-fun
				 mem-fun-ref
				 mem-fun1
				 mem-fun1-ref
   #-cl-stl-0x98 is-placeholder
   #-cl-stl-0x98 is-bind-expression
   #-cl-stl-0x98 target))

;-------------------------------------------------------------------------------
;
; functor basis
;
;-------------------------------------------------------------------------------
(defclass functor (clonable)
  ((closure	:type     cl:function
			:initform nil
			:initarg  :closure
			:accessor __functor-closure)))

(defclass unary-function  (functor) ())    ; deprecated in 0x11 or later
(defclass binary-function (functor) ())    ; deprecated in 0x11 or later

;-------------------------------------------------------------------------------
;
; default methods for functor
;
;-------------------------------------------------------------------------------
(defmethod functor-function ((func cl:function))
  func)

(defmethod functor-function ((func functor))
  (__functor-closure func))


(defmethod operator_clone ((func cl:function))
  func)

(defmethod operator_clone ((func functor))
  (make-instance (type-of func)
				 :closure (__functor-closure func)))


;-------------------------------------------------------------------------------
;
; 20.3.2, arithmetic operations:
;
;-------------------------------------------------------------------------------

;------------------------------------------------------------
; class plus
;------------------------------------------------------------
(defclass plus (#-cl-stl-0x98 functor
				#+cl-stl-0x98 binary-function) ())

(declare-constructor plus (0))

(labels ((__plus (arg1 arg2) (+ arg1 arg2)))
  (define-constructor plus ()
	(make-instance 'plus :closure #'__plus)))


;------------------------------------------------------------
; class minus
;------------------------------------------------------------
(defclass minus (#-cl-stl-0x98 functor
				 #+cl-stl-0x98 binary-function) ())

(declare-constructor minus (0))

(labels ((__minus (arg1 arg2) (- arg1 arg2)))
  (define-constructor minus ()
	(make-instance 'minus :closure #'__minus)))


;------------------------------------------------------------
; class multiplies
;------------------------------------------------------------
(defclass multiplies (#-cl-stl-0x98 functor
					  #+cl-stl-0x98 binary-function) ())

(declare-constructor multiplies (0))

(labels ((__multiplies (arg1 arg2) (* arg1 arg2)))
  (define-constructor multiplies ()
	(make-instance 'multiplies :closure #'__multiplies)))


;------------------------------------------------------------
; class divides
;------------------------------------------------------------
(defclass divides (#-cl-stl-0x98 functor
				   #+cl-stl-0x98 binary-function) ())

(declare-constructor divides (0))

(labels ((__divides (arg1 arg2) (/ arg1 arg2)))
  (define-constructor divides ()
	(make-instance 'divides :closure #'__divides)))


;------------------------------------------------------------
; class modulus
;------------------------------------------------------------
(defclass modulus (#-cl-stl-0x98 functor
				   #+cl-stl-0x98 binary-function) ())

(declare-constructor modulus (0))

(labels ((__modulus (arg1 arg2) (mod arg1 arg2)))
  (define-constructor modulus ()
	(make-instance 'modulus :closure #'__modulus)))


;------------------------------------------------------------
; class negate
;------------------------------------------------------------
(defclass negate (#-cl-stl-0x98 functor
				  #+cl-stl-0x98 unary-function) ())

(declare-constructor negate (0))

(labels ((__negate (arg) (* -1 arg)))
  (define-constructor negate ()
	(make-instance 'negate :closure #'__negate)))


;-------------------------------------------------------------------------------
;
; 20.3.3, comparisons:
;
;-------------------------------------------------------------------------------

;------------------------------------------------------------
; class equal-to
;------------------------------------------------------------
(defclass equal-to (#-cl-stl-0x98 functor
					#+cl-stl-0x98 binary-function)
  ((op  :initform nil
		:initarg  :operator
		:accessor equal-to-operator)))

(declare-constructor equal-to (0 1))

(labels ((__equal-to-ctor (op)
           (let* ((op  (clone op))
				  (fnc (functor-function op)))
			 (make-instance 'equal-to
							:operator op
							:closure (lambda (arg1 arg2)
									   (funcall fnc arg1 arg2))))))
  (define-constructor equal-to ()
	(__equal-to-ctor #'operator_==))
  #+cl-stl-extra
  (define-constructor equal-to (op)
	(__equal-to-ctor op))
  (defmethod operator_clone ((func equal-to))
	(__equal-to-ctor (equal-to-operator func))))


;------------------------------------------------------------
; class not-equal-to
;------------------------------------------------------------
(defclass not-equal-to (#-cl-stl-0x98 functor
						#+cl-stl-0x98 binary-function)
  ((op  :initform nil
		:initarg  :operator
		:accessor not-equal-to-operator)))

(declare-constructor not-equal-to (0 1))

(labels ((__not-equal-to-ctor (op)
           (let* ((op  (clone op))
				  (fnc (functor-function op)))
			 (make-instance 'not-equal-to
							:operator op
							:closure (lambda (arg1 arg2)
									   (funcall fnc arg1 arg2))))))
  (define-constructor not-equal-to ()
	(__not-equal-to-ctor #'operator_/=))
  #+cl-stl-extra
  (define-constructor not-equal-to (op)
	(__not-equal-to-ctor op))
  (defmethod operator_clone ((func not-equal-to))
	(__not-equal-to-ctor (not-equal-to-operator func))))


;------------------------------------------------------------
; class greater
;------------------------------------------------------------
(defclass greater (#-cl-stl-0x98 functor
				   #+cl-stl-0x98 binary-function)
  ((op  :initform nil
		:initarg  :operator
		:accessor greater-operator)))

(declare-constructor greater (0 1))

(labels ((__greater-ctor (op)
           (let* ((op  (clone op))
				  (fnc (functor-function op)))
			 (make-instance 'greater
							:operator op
							:closure (lambda (arg1 arg2)
									   (funcall fnc arg1 arg2))))))
  (define-constructor greater ()
	(__greater-ctor #'operator_>))
  #+cl-stl-extra
  (define-constructor greater (op)
	(__greater-ctor op))
  (defmethod operator_clone ((func greater))
	(__greater-ctor (greater-operator func))))


;------------------------------------------------------------
; class less
;------------------------------------------------------------
(defclass less (#-cl-stl-0x98 functor
				#+cl-stl-0x98 binary-function)
  ((op  :initform nil
		:initarg  :operator
		:accessor less-operator)))

(declare-constructor less (0 1))

(labels ((__less-ctor (op)
           (let* ((op  (clone op))
				  (fnc (functor-function op)))
			 (make-instance 'less
							:operator op
							:closure (lambda (arg1 arg2)
									   (funcall fnc arg1 arg2))))))
  (define-constructor less ()
	(__less-ctor #'operator_<))
  #+cl-stl-extra
  (define-constructor less (op)
	(__less-ctor op))
  (defmethod operator_clone ((func less))
	(__less-ctor (less-operator func))))


;------------------------------------------------------------
; class greater-equal
;------------------------------------------------------------
(defclass greater-equal (#-cl-stl-0x98 functor
						 #+cl-stl-0x98 binary-function)
  ((op  :initform nil
		:initarg  :operator
		:accessor greater-equal-operator)))

(declare-constructor greater-equal (0 1))

(labels ((__greater-equal-ctor (op)
           (let* ((op  (clone op))
				  (fnc (functor-function op)))
			 (make-instance 'greater-equal
							:operator op
							:closure (lambda (arg1 arg2)
									   (funcall fnc arg1 arg2))))))
  (define-constructor greater-equal ()
	(__greater-equal-ctor #'operator_>=))
  #+cl-stl-extra
  (define-constructor greater-equal (op)
	(__greater-equal-ctor op))
  (defmethod operator_clone ((func greater-equal))
	(__greater-equal-ctor (greater-equal-operator func))))


;------------------------------------------------------------
; class less-equal
;------------------------------------------------------------
(defclass less-equal (#-cl-stl-0x98 functor
					  #+cl-stl-0x98 binary-function)
  ((op  :initform nil
		:initarg  :operator
		:accessor less-equal-operator)))

(declare-constructor less-equal (0 1))

(labels ((__less-equal-ctor (op)
           (let* ((op  (clone op))
				  (fnc (functor-function op)))
			 (make-instance 'less-equal
							:operator op
							:closure (lambda (arg1 arg2)
									   (funcall fnc arg1 arg2))))))
  (define-constructor less-equal ()
	(__less-equal-ctor #'operator_<=))
  #+cl-stl-extra
  (define-constructor less-equal (op)
	(__less-equal-ctor op))
  (defmethod operator_clone ((func less-equal))
	(__less-equal-ctor (less-equal-operator func))))



;-------------------------------------------------------------------------------
;
; 20.3.4, logical operations:
;
;-------------------------------------------------------------------------------
;------------------------------------------------------------
; class logical-and
;------------------------------------------------------------
(defclass logical-and (#-cl-stl-0x98 functor
					   #+cl-stl-0x98 binary-function) ())

(declare-constructor logical-and (0))

(labels ((__logical-and (arg1 arg2)
		   (and arg1 arg2)))
  (define-constructor logical-and ()
	(make-instance 'logical-and :closure #'__logical-and)))


;------------------------------------------------------------
; class logical-or
;------------------------------------------------------------
(defclass logical-or (#-cl-stl-0x98 functor
					  #+cl-stl-0x98 binary-function) ())

(declare-constructor logical-or (0))

(labels ((__logical-or (arg1 arg2)
		   (or arg1 arg2)))
  (define-constructor logical-or ()
	(make-instance 'logical-or :closure #'__logical-or)))


;------------------------------------------------------------
; class logical-not
;------------------------------------------------------------
(defclass logical-not (#-cl-stl-0x98 functor
					   #+cl-stl-0x98 unary-function) ())

(declare-constructor logical-not (0))

(labels ((__logical-not (arg)
		   (not arg)))
  (define-constructor logical-not ()
	(make-instance 'logical-not :closure #'__logical-not)))



;-------------------------------------------------------------------------------
;
; 20.3.5, negators:
;
;-------------------------------------------------------------------------------

;------------------------------------------------------------
; class unary-negate & function not1
;------------------------------------------------------------
(defclass unary-negate (#-cl-stl-0x98 functor
						#+cl-stl-0x98 unary-function)
  ((op  :initform nil
		:initarg  :operator
		:accessor unary-negate-operator)))

(declare-constructor unary-negate (1))

(define-constructor unary-negate (op)
  (let* ((op  (clone op))
		 (fnc (functor-function op)))
	(make-instance 'unary-negate
				   :operator op
				   :closure (lambda (arg)
							  (not (funcall fnc arg))))))

(defun not1 (op)
  (let* ((op  (clone op))
		 (fnc (functor-function op)))
	(make-instance 'unary-negate
				   :operator op
				   :closure (lambda (arg)
							  (not (funcall fnc arg))))))

(defmethod operator_clone ((func unary-negate))
  (let* ((op (clone (unary-negate-operator func)))
		 (fnc (functor-function op)))
	(make-instance 'unary-negate
				   :operator op
				   :closure (lambda (arg)
							  (not (funcall fnc arg))))))



;------------------------------------------------------------
; class binary-negate & function not2
;------------------------------------------------------------
(defclass binary-negate (#-cl-stl-0x98 functor
						 #+cl-stl-0x98 binary-function)
  ((op  :initform nil
		:initarg  :operator
		:accessor binary-negate-operator)))

(declare-constructor binary-negate (1))

(define-constructor binary-negate (op)
  (let* ((op  (clone op))
		 (fnc (functor-function op)))
	(make-instance 'binary-negate
				   :operator op
				   :closure (lambda (arg1 arg2)
							  (not (funcall fnc arg1 arg2))))))

(defun not2 (op)
  (let* ((op  (clone op))
		 (fnc (functor-function op)))
	(make-instance 'binary-negate
				   :operator op
				   :closure (lambda (arg1 arg2)
							  (not (funcall fnc arg1 arg2))))))

(defmethod operator_clone ((func binary-negate))
  (let* ((op (clone (binary-negate-operator func)))
		 (fnc (functor-function op)))
	(make-instance 'binary-negate
				   :operator op
				   :closure (lambda (arg1 arg2)
							  (not (funcall fnc arg1 arg2))))))




;-------------------------------------------------------------------------------
;
; 20.3.6, binders:
;
;-------------------------------------------------------------------------------

;------------------------------------------------------------
; class binder1st & function bind1st
;------------------------------------------------------------
(defclass binder1st (#-cl-stl-0x98 functor
					 #+cl-stl-0x98 unary-function)
  ((op	:initform nil
		:initarg  :operator
		:accessor binder1st-operator)
   (arg :initform nil
		:initarg  :arg
		:accessor binder1st-arg)))

(declare-constructor binder1st (2))

(define-constructor binder1st (op arg1)
  #+cl-stl-warn-deprecated
  (progn
	#-cl-stl-0x98 (warn "binder1st is deprecated."))
  (let* ((op  (clone op))
		 (fnc (functor-function op)))
	(make-instance 'binder1st
				   :operator op
				   :arg      arg1
				   :closure (lambda (arg2)
							  (funcall fnc arg1 arg2)))))

(defun bind1st (functor arg)
  #+cl-stl-warn-deprecated
  (progn
	#-cl-stl-0x98 (warn "bind1st is deprecated."))
  (let* ((op  (clone functor))
		 (fnc (functor-function op)))
	(make-instance 'binder1st
				   :operator op
				   :arg      arg
				   :closure (lambda (arg2)
							  (funcall fnc arg arg2)))))

(defmethod operator_clone ((func binder1st))
  (let* ((op   (clone (binder1st-operator func)))
		 (arg1 (binder1st-arg func))
		 (fnc  (functor-function op)))
	(make-instance 'binder1st
				   :operator op
				   :arg      arg1
				   :closure (lambda (arg2)
							  (funcall fnc arg1 arg2)))))



;------------------------------------------------------------
; class binder2nd & function bind2nd
;------------------------------------------------------------
(defclass binder2nd (#-cl-stl-0x98 functor
					 #+cl-stl-0x98 unary-function)
  ((op	:initform nil
		:initarg  :operator
		:accessor binder2nd-operator)
   (arg :initform nil
		:initarg  :arg
		:accessor binder2nd-arg)))

(declare-constructor binder2nd (2))

(define-constructor binder2nd (op arg2)
  #+cl-stl-warn-deprecated
  (progn
	#-cl-stl-0x98 (warn "binder2nd is deprecated."))
  (let* ((op  (clone op))
		 (fnc (functor-function op)))
	(make-instance 'binder2nd
				   :operator op
				   :arg      arg2
				   :closure (lambda (arg1)
							  (funcall fnc arg1 arg2)))))

(defun bind2nd (functor arg)
  #+cl-stl-warn-deprecated
  (progn
	#-cl-stl-0x98 (warn "bind2nd is deprecated."))
  (let* ((op  (clone functor))
		 (fnc (functor-function op)))
	(make-instance 'binder2nd
				   :operator op
				   :arg      arg
				   :closure (lambda (arg1)
							  (funcall fnc arg1 arg)))))

(defmethod operator_clone ((func binder2nd))
  (let* ((op   (clone (binder2nd-operator func)))
		 (arg2 (binder2nd-arg func))
		 (fnc  (functor-function op)))
	(make-instance 'binder2nd
				   :operator op
				   :arg      arg2
				   :closure (lambda (arg1)
							  (funcall fnc arg1 arg2)))))



;-------------------------------------------------------------------------------
;
; 20.3.7, adaptors:
;
;-------------------------------------------------------------------------------

;------------------------------------------------------------
; class pointer-to-unary-function & function ptr-fun1
;                                     ( NONSENSE in CL-STL )
;------------------------------------------------------------
(defclass pointer-to-unary-function (#-cl-stl-0x98 functor
									 #+cl-stl-0x98 unary-function)
  ((op	:type     cl:function
		:initform nil
		:initarg  :operator
		:accessor pointer-to-unary-function-operator)))

(declare-constructor pointer-to-unary-function (1))

(define-constructor pointer-to-unary-function (func)
  #+cl-stl-warn-deprecated
  (progn
	#-cl-stl-0x98 (warn "pointer-to-unary-function is deprecated."))
  (make-instance 'pointer-to-unary-function
				 :operator func
				 :closure (lambda (arg)
							(funcall func arg))))

(defun ptr-fun1 (func)
  #+cl-stl-warn-deprecated
  (progn
	#-cl-stl-0x98 (warn "ptr-fun1 is deprecated."))
  (make-instance 'pointer-to-unary-function
				 :operator func
				 :closure (lambda (arg)
							(funcall func arg))))

(defmethod operator_clone ((func pointer-to-unary-function))
  (let ((func (pointer-to-unary-function-operator func)))
	(make-instance 'pointer-to-unary-function
				   :operator func
				   :closure (lambda (arg)
							  (funcall func arg)))))



;------------------------------------------------------------
; class pointer-to-binary-function & function ptr-fun2
;                                     ( NONSENSE in CL-STL )
;------------------------------------------------------------
(defclass pointer-to-binary-function (#-cl-stl-0x98 functor
									  #+cl-stl-0x98 binary-function)
  ((op	:type     cl:function
		:initform nil
		:initarg  :operator
		:accessor pointer-to-binary-function-operator)))

(declare-constructor pointer-to-binary-function (1))

(define-constructor pointer-to-binary-function (func)
  #+cl-stl-warn-deprecated
  (progn
	#-cl-stl-0x98 (warn "pointer-to-binary-function is deprecated."))
  (make-instance 'pointer-to-binary-function
				 :operator func
				 :closure (lambda (arg1 arg2)
							(funcall func arg1 arg2))))

(defun ptr-fun2 (func)
  #+cl-stl-warn-deprecated
  (progn
	#-cl-stl-0x98 (warn "ptr-fun2 is deprecated."))
  (make-instance 'pointer-to-binary-function
				 :operator func
				 :closure (lambda (arg1 arg2)
							(funcall func arg1 arg2))))

(defmethod operator_clone ((func pointer-to-binary-function))
  (let ((func (pointer-to-binary-function-operator func)))
	(make-instance 'pointer-to-binary-function
				   :operator func
				   :closure (lambda (arg1 arg2)
							  (funcall func arg1 arg2)))))




;-------------------------------------------------------------------------------
;
; 20.3.8, adaptors:
;
;-------------------------------------------------------------------------------

;------------------------------------------------------------
; class mem-fun-t & function mem-fun etc.
;                                      ( NONSENSE in CL-STL )
;------------------------------------------------------------
(defclass mem-fun-t (#-cl-stl-0x98 functor
					 #+cl-stl-0x98 unary-function)
  ((op	:type     cl:function
		:initform nil
		:initarg  :operator
		:accessor mem-fun-t-operator)))

(declare-constructor mem-fun-t (1))

(define-constructor mem-fun-t (func)
  #+cl-stl-warn-deprecated
  (progn
	#-cl-stl-0x98 (warn "mem-fun-t is deprecated."))
  (make-instance 'mem-fun-t
				 :operator func
				 :closure (lambda (obj)
							(funcall func obj))))

(defun mem-fun (func)
  #+cl-stl-warn-deprecated
  (progn
	#-cl-stl-0x98 (warn "mem-fun is deprecated."))
  (make-instance 'mem-fun-t
				 :operator func
				 :closure (lambda (obj)
							(funcall func obj))))

(defun mem-fun-ref (func)
  #+cl-stl-warn-deprecated
  (progn
	#-cl-stl-0x98 (warn "mem-fun-ref is deprecated."))
  (make-instance 'mem-fun-t
				 :operator func
				 :closure (lambda (obj)
							(funcall func obj))))

(defmethod operator_clone ((func mem-fun-t))
  (let ((func (mem-fun-t-operator func)))
	(make-instance 'mem-fun-t
				   :operator func
				   :closure (lambda (obj)
							  (funcall func obj)))))




;------------------------------------------------------------
; class mem-fun1-t & function mem-fun1 etc.
;                                      ( NONSENSE in CL-STL )
;------------------------------------------------------------
(defclass mem-fun1-t (#-cl-stl-0x98 functor
					  #+cl-stl-0x98 unary-function)
  ((op	:type     cl:function
		:initform nil
		:initarg  :operator
		:accessor mem-fun1-t-operator)))

(declare-constructor mem-fun1-t (1))

(define-constructor mem-fun1-t (func)
  #+cl-stl-warn-deprecated
  (progn
	#-cl-stl-0x98 (warn "mem-fun1-t is deprecated."))
  (make-instance 'mem-fun1-t
				 :operator func
				 :closure (lambda (obj arg)
							(funcall func obj arg))))

(defun mem-fun1 (func)
  #+cl-stl-warn-deprecated
  (progn
	#-cl-stl-0x98 (warn "mem-fun1 is deprecated."))
  (make-instance 'mem-fun1-t
				 :operator func
				 :closure (lambda (obj arg)
							(funcall func obj arg))))

(defun mem-fun1-ref (func)
  #+cl-stl-warn-deprecated
  (progn
	#-cl-stl-0x98 (warn "mem-fun1-ref is deprecated."))
  (make-instance 'mem-fun1-t
				 :operator func
				 :closure (lambda (obj arg)
							(funcall func obj arg))))

(defmethod operator_clone ((func mem-fun1-t))
  (let ((func (mem-fun1-t-operator func)))
	(make-instance 'mem-fun1-t
				   :operator func
				   :closure (lambda (obj arg)
							  (funcall func obj arg)))))



;------------------------------------------------------------
; class mem-fun-ref-t;
;------------------------------------------------------------
; --> use mem-fun-t instead.

;------------------------------------------------------------
; class mem-fun1-ref-t;
;------------------------------------------------------------
; --> use mem-fun1-t instead.




;-------------------------------------------------------------------------------
;
; 0x11
;
;-------------------------------------------------------------------------------

#-cl-stl-0x98
(defun is-placeholder (arg)
  (if (not (keywordp arg))
	  0
	  (handler-case (values (parse-integer (symbol-name arg)))
		(error () 0))))
			 
#-cl-stl-0x98
(defun is-bind-expression (arg)
  (eq (type-of arg) '__binded-expr))
			 

;------------------------------------------------------------
; class __binded-expr
;------------------------------------------------------------
#-cl-stl-0x98
(defclass __binded-expr (functor)
  ((bindee	:initarg  :bindee
			:accessor __binded-expr-bindee)
   (params	:type     cl:vector
			:initarg  :params
			:accessor __binded-expr-params)
   (cloner	:type     cl:function
			:initarg  :cloner
			:accessor __binded-expr-cloner)))

#-cl-stl-0x98
(defmethod operator_clone ((func __binded-expr))
  (funcall (__binded-expr-cloner func) func))



;------------------------------------------------------------
; bind macro
;------------------------------------------------------------
#-cl-stl-0x98
(defmacro bind (func &rest args)
  (let ((max-arg 0)
		(arg-hash (make-hash-table)))
	(labels ((get-argsym (idx)
			   (let ((is-new nil)
					 (ret (gethash idx arg-hash)))
				 (unless ret
				   (setf ret (gensym (format nil "ARG~A-" idx)))
				   (setf (gethash idx arg-hash) ret)
				   (setf is-new t)
				   (when (< max-arg idx)
					 (setf max-arg idx)))
				 (values ret is-new)))
			 (make-lambda-list (idx lambda-list ignore-list)
			   (if (< max-arg idx)
				   (values (nreverse lambda-list)
						   (nreverse ignore-list))
				   (multiple-value-bind (sym is-new) (get-argsym idx)
					 (cl:push sym lambda-list)
					 (when is-new
					   (cl:push sym ignore-list))
					 (make-lambda-list (1+ idx) lambda-list ignore-list))))
			 (imp (args acc1 acc2 prm-sym &optional (idx 0))
			   (if (null args)
				   (values (nreverse acc1)
						   (nreverse acc2))
				   (let* ((item (car args))
						  (ret  (is-placeholder item)))
					 (if (< 0 ret)
						 (cl:push (get-argsym ret) acc2)
						 (progn
						   (cl:push `(aref ,prm-sym ,idx) acc2)
						   (cl:push `(_= (aref ,prm-sym ,idx) ,(car args)) acc1)
						   (incf idx)))
					 (imp (cdr args) acc1 acc2 prm-sym idx)))))
	  (let ((g-imp    (gensym "IMP"))
			(g-fnc    (gensym "FNC"))
			(g-bindee (gensym "BINDEE"))
			(g-bd-fnc (gensym "BD-FNC"))
			(g-params (gensym "PARAMS"))
			(g-cloner (gensym "CLONER")))
		(multiple-value-bind (arg-lst prm-lst) (imp args nil nil g-params)
		  (multiple-value-bind (lambda-list ignore-list) (make-lambda-list 1 nil nil)
			`(labels ((,g-imp (,g-bindee ,g-params ,g-cloner)
						(declare (type cl:vector ,g-params))
						(let ((,g-bd-fnc (functor-function ,g-bindee)))
						  (declare (type cl:function ,g-bd-fnc))
						  (make-instance '__binded-expr
										 :bindee ,g-bindee :params ,g-params :cloner ,g-cloner
										 :closure (lambda (,@lambda-list)
													,(when ignore-list
														   `(declare (ignore ,@ignore-list)))
													(funcall ,g-bd-fnc ,@prm-lst))))))
			   (let* ((,g-bindee (clone ,func))
					  (,g-params (make-array ,(length arg-lst) :initial-element nil))
					  (,g-cloner (lambda (,g-fnc)
								   (,g-imp (clone (__binded-expr-bindee ,g-fnc))
										   (clone (__binded-expr-params ,g-fnc))
										   (__binded-expr-cloner ,g-fnc)))))
				 ,@arg-lst
				 (,g-imp ,g-bindee ,g-params ,g-cloner)))))))))


;------------------------------------------------------------
; class function
;------------------------------------------------------------
#-cl-stl-0x98
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass function (functor)
	((target :initform nil
			 :initarg  :target
			 :accessor __function-target))))

#-cl-stl-0x98
(declare-constructor function (0 1))

#-cl-stl-0x98
(labels ((__function-ctor (op)
		   (if (null op)
			   (make-instance 'function)
			   (let* ((target  (clone op))
					  (closure (functor-function target)))
				 (make-instance 'function
								:target  target
								:closure (lambda (&rest args)
										   (apply closure args)))))))
  (define-constructor function ()
	(make-instance 'function))
  (define-constructor function ((op (eql nil)))
	(make-instance 'function))
  (define-constructor function ((op cl:function))
	(__function-ctor op))
  (define-constructor function ((op functor))
	(__function-ctor op))
  (define-constructor function ((fn function))
	(__function-ctor (__function-target fn)))
  (defmethod operator_clone ((fn function))
	(__function-ctor (__function-target fn))))

#-cl-stl-0x98
(define-constructor function ((rm remove-reference))
  (let ((fnc (funcall (the cl:function (__rm-ref-closure rm)))))
	(__check-type-of-move-constructor fnc function)
	(let ((obj (make-instance 'function)))
	  (setf (__function-target obj) (__function-target fnc))
	  (setf (__functor-closure obj) (__functor-closure fnc))
	  (setf (__function-target fnc) nil)
	  (setf (__functor-closure fnc) nil)
	  obj)))

#-cl-stl-0x98
(defmethod functor-function ((func function))
  (let ((closure (__functor-closure func)))
	(unless closure
	  (error 'bad-function-call :what "empty function."))
	closure))

#-cl-stl-0x98
(defmethod operator_cast ((fn function) (typename (eql 'boolean)))
  (if (__function-target fn) t nil))
 
#-cl-stl-0x98
(locally (declare (optimize speed))
  (labels ((__assign (fnc op)
			 (declare (type function fnc))
			 (if (null op)
				 (progn
				   (setf (__function-target fnc) nil)
				   (setf (__functor-closure fnc) nil))
				 (let* ((target  (clone op))
						(closure (functor-function op)))
				   (setf (__function-target fnc) target)
				   (setf (__functor-closure fnc) (locally (declare (type cl:function closure))
												   (lambda (&rest args)
													 (apply closure args))))))))
	(declare (inline __assign))
	(defmethod operator_= ((lhs function) rhs)
	  (error 'type-mismatch :what (format nil "Can't convert ~A to stl:function." rhs))
	  lhs)
	(defmethod operator_= ((lhs function) (rhs function))
	  (__assign lhs (__function-target rhs))
	  lhs)
	(defmethod operator_= ((lhs function) (rhs (eql nil)))
	  (setf (__function-target lhs) nil)
	  (setf (__functor-closure lhs) nil)
	  lhs)
	(defmethod-overload assign ((this function) (fnc function))
	  (__assign this (__function-target fnc))
	  nil)
	(defmethod-overload assign ((this function) (fnc (eql nil)))
	  (setf (__function-target this) nil)
	  (setf (__functor-closure this) nil)
	  nil)))

#-cl-stl-0x98
(defmethod operator_move ((lhs function) (rhs function))
  (unless (eq lhs rhs)
	(setf (__function-target lhs) (__function-target rhs))
	(setf (__functor-closure lhs) (__functor-closure rhs))
	(setf (__function-target rhs) nil)
	(setf (__functor-closure rhs) nil))
  (values lhs rhs))

#-cl-stl-0x98
(defmethod-overload swap ((fn1 function) (fn2 function))
  (let ((op (__function-target fn1))
		(fn (__functor-closure fn1)))
	(setf (__function-target fn1) (__function-target fn2))
	(setf (__functor-closure fn1) (__functor-closure fn2))
	(setf (__function-target fn2) op)
	(setf (__functor-closure fn2) fn))
  (values fn1 fn2))

#-cl-stl-0x98
(defun target (fn)
  (__function-target fn))


#-cl-stl-0x98
(locally (declare (optimize speed))
  (labels ((__empty (fn)
			 (declare (type function fn))
			 (if (__function-target fn) nil t)))
	(declare (inline __empty))
	(defmethod operator_== ((fn function) (rhs (eql nil))) (__empty fn))
	(defmethod operator_== ((lhs (eql nil)) (fn function)) (__empty fn))
	(defmethod operator_/= ((fn function) (rhs (eql nil))) (not (__empty fn)))
	(defmethod operator_/= ((lhs (eql nil)) (fn function)) (not (__empty fn)))))



;------------------------------------------------------------
; class bit-and
;------------------------------------------------------------
#-cl-stl-0x98
(defclass bit-and (functor) ())

#-cl-stl-0x98
(declare-constructor bit-and (0))

#-cl-stl-0x98
(labels ((__bit-and (arg1 arg2) (logand arg1 arg2)))
  (define-constructor bit-and ()
	(make-instance 'bit-and :closure #'__bit-and)))


;------------------------------------------------------------
; class bit-or
;------------------------------------------------------------
#-cl-stl-0x98
(defclass bit-or (functor) ())

#-cl-stl-0x98
(declare-constructor bit-or (0))

#-cl-stl-0x98
(labels ((__bit-or (arg1 arg2) (logior arg1 arg2)))
  (define-constructor bit-or ()
	(make-instance 'bit-or :closure #'__bit-or)))


;------------------------------------------------------------
; class bit-xor
;------------------------------------------------------------
#-cl-stl-0x98
(defclass bit-xor (functor) ())

#-cl-stl-0x98
(declare-constructor bit-xor (0))

#-cl-stl-0x98
(labels ((__bit-xor (arg1 arg2) (logxor arg1 arg2)))
  (define-constructor bit-xor ()
	(make-instance 'bit-xor :closure #'__bit-xor)))


;;------------------------------------------------------------
;; class mem-fn
;;------------------------------------------------------------
#-cl-stl-0x98
(defclass mem-fn (functor)
  ((method :type     cl:function
		   :initform nil
		   :initarg  :method
		   :accessor mem-fn-method)))

#-cl-stl-0x98
(declare-constructor mem-fn (1))

#-cl-stl-0x98
(labels ((__mem-fn-ctor (method)
		   (make-instance 'mem-fn
						  :method  method
						  :closure (lambda (&rest args)
									 (apply method args)))))
  (define-constructor mem-fn (method)
	(__mem-fn-ctor method))
  (defmethod operator_clone ((func mem-fn))
	(__mem-fn-ctor (mem-fn-method func))))

