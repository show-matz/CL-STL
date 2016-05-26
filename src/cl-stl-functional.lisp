
(in-package :cl-stl)

(declaim (inline not1
				 not2
				 bind1st
				 bind2nd
				 ptr_fun1
				 ptr_fun2
				 mem_fun
				 mem_fun_ref
				 mem_fun1
				 mem_fun1_ref
   #-cl-stl-0x98 is_placeholder
   #-cl-stl-0x98 is_bind_expression
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

(defclass unary_function  (functor) ())    ; deprecated in 0x11 or later
(defclass binary_function (functor) ())    ; deprecated in 0x11 or later

;-------------------------------------------------------------------------------
;
; default methods for functor
;
;-------------------------------------------------------------------------------
(labels ((empty-fnc (&rest args)
		   (declare (ignore args))
		   (error 'bad_function_call :what "empty function.")))
  (defmethod initialize-instance :after ((fnctr functor) &key)
	(let ((closure (__functor-closure fnctr)))
	  (if (null closure)
		  (setf (__functor-closure fnctr) #'empty-fnc)
		  (closer-mop:set-funcallable-instance-function fnctr closure))))
  (defmethod (setf __functor-closure) :after (closure (fnctr functor))
	  (if (null closure)
		  (setf (__functor-closure fnctr) #'empty-fnc)
		  (closer-mop:set-funcallable-instance-function fnctr closure))))

(defmethod functor_function ((func cl:function))
  func)

(defmethod functor_function ((func functor))
  (__functor-closure func))


(defmethod operator_clone ((func cl:function))
  func)

(defmethod operator_clone ((func functor))
  (make-instance (type-of func)
				 :closure (__functor-closure func)))

(defmacro define-functor (name (&rest superclasses) (&rest slot-specifiers) &rest class-options)
  `(defclass ,name (,@superclasses closer-mop:funcallable-standard-object)
	 ,slot-specifiers
	 (:metaclass closer-mop:funcallable-standard-class)
	 ,@class-options))


;-------------------------------------------------------------------------------
;
; 20.3.2, arithmetic operations:
;
;-------------------------------------------------------------------------------

;------------------------------------------------------------
; class plus
;------------------------------------------------------------
(define-functor plus (#-cl-stl-0x98 functor
					  #+cl-stl-0x98 binary_function) ())

(declare-constructor plus (0))

(labels ((__plus (arg1 arg2) (+ arg1 arg2)))
  (define-constructor plus ()
	(make-instance 'plus :closure #'__plus)))


;------------------------------------------------------------
; class minus
;------------------------------------------------------------
(define-functor minus (#-cl-stl-0x98 functor
					   #+cl-stl-0x98 binary_function) ())

(declare-constructor minus (0))

(labels ((__minus (arg1 arg2) (- arg1 arg2)))
  (define-constructor minus ()
	(make-instance 'minus :closure #'__minus)))


;------------------------------------------------------------
; class multiplies
;------------------------------------------------------------
(define-functor multiplies (#-cl-stl-0x98 functor
							#+cl-stl-0x98 binary_function) ())

(declare-constructor multiplies (0))

(labels ((__multiplies (arg1 arg2) (* arg1 arg2)))
  (define-constructor multiplies ()
	(make-instance 'multiplies :closure #'__multiplies)))


;------------------------------------------------------------
; class divides
;------------------------------------------------------------
(define-functor divides (#-cl-stl-0x98 functor
						 #+cl-stl-0x98 binary_function) ())

(declare-constructor divides (0))

(labels ((__divides (arg1 arg2) (/ arg1 arg2)))
  (define-constructor divides ()
	(make-instance 'divides :closure #'__divides)))


;------------------------------------------------------------
; class modulus
;------------------------------------------------------------
(define-functor modulus (#-cl-stl-0x98 functor
						 #+cl-stl-0x98 binary_function) ())

(declare-constructor modulus (0))

(labels ((__modulus (arg1 arg2) (mod arg1 arg2)))
  (define-constructor modulus ()
	(make-instance 'modulus :closure #'__modulus)))


;------------------------------------------------------------
; class negate
;------------------------------------------------------------
(define-functor negate (#-cl-stl-0x98 functor
						#+cl-stl-0x98 unary_function) ())

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
; class equal_to
;------------------------------------------------------------
(define-functor equal_to (#-cl-stl-0x98 functor
						  #+cl-stl-0x98 binary_function)
  ((op  :initform nil
		:initarg  :operator
		:accessor equal_to-operator)))

(declare-constructor equal_to (0 1))

(labels ((__equal_to-ctor (op)
           (let* ((op  (clone op))
				  (fnc (functor_function op)))
			 (make-instance 'equal_to
							:operator op
							:closure (lambda (arg1 arg2)
									   (funcall fnc arg1 arg2))))))
  (define-constructor equal_to ()
	(__equal_to-ctor #'operator_==))
  #-cl-stl-noextra
  (define-constructor equal_to (op)
	(__equal_to-ctor op))
  (defmethod operator_clone ((func equal_to))
	(__equal_to-ctor (equal_to-operator func))))


;------------------------------------------------------------
; class not_equal_to
;------------------------------------------------------------
(define-functor not_equal_to (#-cl-stl-0x98 functor
							  #+cl-stl-0x98 binary_function)
  ((op  :initform nil
		:initarg  :operator
		:accessor not_equal_to-operator)))

(declare-constructor not_equal_to (0 1))

(labels ((__not_equal_to-ctor (op)
           (let* ((op  (clone op))
				  (fnc (functor_function op)))
			 (make-instance 'not_equal_to
							:operator op
							:closure (lambda (arg1 arg2)
									   (funcall fnc arg1 arg2))))))
  (define-constructor not_equal_to ()
	(__not_equal_to-ctor #'operator_/=))
  #-cl-stl-noextra
  (define-constructor not_equal_to (op)
	(__not_equal_to-ctor op))
  (defmethod operator_clone ((func not_equal_to))
	(__not_equal_to-ctor (not_equal_to-operator func))))


;------------------------------------------------------------
; class greater
;------------------------------------------------------------
(define-functor greater (#-cl-stl-0x98 functor
						 #+cl-stl-0x98 binary_function)
  ((op  :initform nil
		:initarg  :operator
		:accessor greater-operator)))

(declare-constructor greater (0 1))

(labels ((__greater-ctor (op)
           (let* ((op  (clone op))
				  (fnc (functor_function op)))
			 (make-instance 'greater
							:operator op
							:closure (lambda (arg1 arg2)
									   (funcall fnc arg1 arg2))))))
  (define-constructor greater ()
	(__greater-ctor #'operator_>))
  #-cl-stl-noextra
  (define-constructor greater (op)
	(__greater-ctor op))
  (defmethod operator_clone ((func greater))
	(__greater-ctor (greater-operator func))))


;------------------------------------------------------------
; class less
;------------------------------------------------------------
(define-functor less (#-cl-stl-0x98 functor
					  #+cl-stl-0x98 binary_function)
  ((op  :initform nil
		:initarg  :operator
		:accessor less-operator)))

(declare-constructor less (0 1))

(labels ((__less-ctor (op)
           (let* ((op  (clone op))
				  (fnc (functor_function op)))
			 (make-instance 'less
							:operator op
							:closure (lambda (arg1 arg2)
									   (funcall fnc arg1 arg2))))))
  (define-constructor less ()
	(__less-ctor #'operator_<))
  #-cl-stl-noextra
  (define-constructor less (op)
	(__less-ctor op))
  (defmethod operator_clone ((func less))
	(__less-ctor (less-operator func))))


;------------------------------------------------------------
; class greater_equal
;------------------------------------------------------------
(define-functor greater_equal (#-cl-stl-0x98 functor
							   #+cl-stl-0x98 binary_function)
  ((op  :initform nil
		:initarg  :operator
		:accessor greater_equal-operator)))

(declare-constructor greater_equal (0 1))

(labels ((__greater_equal-ctor (op)
           (let* ((op  (clone op))
				  (fnc (functor_function op)))
			 (make-instance 'greater_equal
							:operator op
							:closure (lambda (arg1 arg2)
									   (funcall fnc arg1 arg2))))))
  (define-constructor greater_equal ()
	(__greater_equal-ctor #'operator_>=))
  #-cl-stl-noextra
  (define-constructor greater_equal (op)
	(__greater_equal-ctor op))
  (defmethod operator_clone ((func greater_equal))
	(__greater_equal-ctor (greater_equal-operator func))))


;------------------------------------------------------------
; class less_equal
;------------------------------------------------------------
(define-functor less_equal (#-cl-stl-0x98 functor
							#+cl-stl-0x98 binary_function)
  ((op  :initform nil
		:initarg  :operator
		:accessor less_equal-operator)))

(declare-constructor less_equal (0 1))

(labels ((__less_equal-ctor (op)
           (let* ((op  (clone op))
				  (fnc (functor_function op)))
			 (make-instance 'less_equal
							:operator op
							:closure (lambda (arg1 arg2)
									   (funcall fnc arg1 arg2))))))
  (define-constructor less_equal ()
	(__less_equal-ctor #'operator_<=))
  #-cl-stl-noextra
  (define-constructor less_equal (op)
	(__less_equal-ctor op))
  (defmethod operator_clone ((func less_equal))
	(__less_equal-ctor (less_equal-operator func))))



;-------------------------------------------------------------------------------
;
; 20.3.4, logical operations:
;
;-------------------------------------------------------------------------------
;------------------------------------------------------------
; class logical_and
;------------------------------------------------------------
(define-functor logical_and (#-cl-stl-0x98 functor
							 #+cl-stl-0x98 binary_function) ())

(declare-constructor logical_and (0))

(labels ((__logical_and (arg1 arg2)
		   (and arg1 arg2)))
  (define-constructor logical_and ()
	(make-instance 'logical_and :closure #'__logical_and)))


;------------------------------------------------------------
; class logical_or
;------------------------------------------------------------
(define-functor logical_or (#-cl-stl-0x98 functor
							#+cl-stl-0x98 binary_function) ())

(declare-constructor logical_or (0))

(labels ((__logical_or (arg1 arg2)
		   (or arg1 arg2)))
  (define-constructor logical_or ()
	(make-instance 'logical_or :closure #'__logical_or)))


;------------------------------------------------------------
; class logical_not
;------------------------------------------------------------
(define-functor logical_not (#-cl-stl-0x98 functor
							 #+cl-stl-0x98 unary_function) ())

(declare-constructor logical_not (0))

(labels ((__logical_not (arg)
		   (not arg)))
  (define-constructor logical_not ()
	(make-instance 'logical_not :closure #'__logical_not)))



;-------------------------------------------------------------------------------
;
; 20.3.5, negators:
;
;-------------------------------------------------------------------------------

;------------------------------------------------------------
; class unary_negate & function not1
;------------------------------------------------------------
(define-functor unary_negate (#-cl-stl-0x98 functor
							  #+cl-stl-0x98 unary_function)
  ((op  :initform nil
		:initarg  :operator
		:accessor unary_negate-operator)))

(declare-constructor unary_negate (1))

(define-constructor unary_negate (op)
  (let* ((op  (clone op))
		 (fnc (functor_function op)))
	(make-instance 'unary_negate
				   :operator op
				   :closure (lambda (arg)
							  (not (funcall fnc arg))))))

(defun not1 (op)
  (let* ((op  (clone op))
		 (fnc (functor_function op)))
	(make-instance 'unary_negate
				   :operator op
				   :closure (lambda (arg)
							  (not (funcall fnc arg))))))

(defmethod operator_clone ((func unary_negate))
  (let* ((op (clone (unary_negate-operator func)))
		 (fnc (functor_function op)))
	(make-instance 'unary_negate
				   :operator op
				   :closure (lambda (arg)
							  (not (funcall fnc arg))))))



;------------------------------------------------------------
; class binary_negate & function not2
;------------------------------------------------------------
(define-functor binary_negate (#-cl-stl-0x98 functor
							   #+cl-stl-0x98 binary_function)
  ((op  :initform nil
		:initarg  :operator
		:accessor binary_negate-operator)))

(declare-constructor binary_negate (1))

(define-constructor binary_negate (op)
  (let* ((op  (clone op))
		 (fnc (functor_function op)))
	(make-instance 'binary_negate
				   :operator op
				   :closure (lambda (arg1 arg2)
							  (not (funcall fnc arg1 arg2))))))

(defun not2 (op)
  (let* ((op  (clone op))
		 (fnc (functor_function op)))
	(make-instance 'binary_negate
				   :operator op
				   :closure (lambda (arg1 arg2)
							  (not (funcall fnc arg1 arg2))))))

(defmethod operator_clone ((func binary_negate))
  (let* ((op (clone (binary_negate-operator func)))
		 (fnc (functor_function op)))
	(make-instance 'binary_negate
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
(define-functor binder1st (#-cl-stl-0x98 functor
						   #+cl-stl-0x98 unary_function)
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
		 (fnc (functor_function op)))
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
		 (fnc (functor_function op)))
	(make-instance 'binder1st
				   :operator op
				   :arg      arg
				   :closure (lambda (arg2)
							  (funcall fnc arg arg2)))))

(defmethod operator_clone ((func binder1st))
  (let* ((op   (clone (binder1st-operator func)))
		 (arg1 (binder1st-arg func))
		 (fnc  (functor_function op)))
	(make-instance 'binder1st
				   :operator op
				   :arg      arg1
				   :closure (lambda (arg2)
							  (funcall fnc arg1 arg2)))))



;------------------------------------------------------------
; class binder2nd & function bind2nd
;------------------------------------------------------------
(define-functor binder2nd (#-cl-stl-0x98 functor
						   #+cl-stl-0x98 unary_function)
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
		 (fnc (functor_function op)))
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
		 (fnc (functor_function op)))
	(make-instance 'binder2nd
				   :operator op
				   :arg      arg
				   :closure (lambda (arg1)
							  (funcall fnc arg1 arg)))))

(defmethod operator_clone ((func binder2nd))
  (let* ((op   (clone (binder2nd-operator func)))
		 (arg2 (binder2nd-arg func))
		 (fnc  (functor_function op)))
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
; class pointer_to_unary_function & function ptr_fun1
;                                     ( NONSENSE in CL-STL )
;------------------------------------------------------------
(define-functor pointer_to_unary_function (#-cl-stl-0x98 functor
										   #+cl-stl-0x98 unary_function)
  ((op	:type     cl:function
		:initform nil
		:initarg  :operator
		:accessor pointer_to_unary_function-operator)))

(declare-constructor pointer_to_unary_function (1))

(define-constructor pointer_to_unary_function (func)
  #+cl-stl-warn-deprecated
  (progn
	#-cl-stl-0x98 (warn "pointer_to_unary_function is deprecated."))
  (make-instance 'pointer_to_unary_function
				 :operator func
				 :closure (lambda (arg)
							(funcall func arg))))

(defun ptr_fun1 (func)
  #+cl-stl-warn-deprecated
  (progn
	#-cl-stl-0x98 (warn "ptr_fun1 is deprecated."))
  (make-instance 'pointer_to_unary_function
				 :operator func
				 :closure (lambda (arg)
							(funcall func arg))))

(defmethod operator_clone ((func pointer_to_unary_function))
  (let ((func (pointer_to_unary_function-operator func)))
	(make-instance 'pointer_to_unary_function
				   :operator func
				   :closure (lambda (arg)
							  (funcall func arg)))))



;------------------------------------------------------------
; class pointer_to_binary_function & function ptr_fun2
;                                     ( NONSENSE in CL-STL )
;------------------------------------------------------------
(define-functor pointer_to_binary_function (#-cl-stl-0x98 functor
											#+cl-stl-0x98 binary_function)
  ((op	:type     cl:function
		:initform nil
		:initarg  :operator
		:accessor pointer_to_binary_function-operator)))

(declare-constructor pointer_to_binary_function (1))

(define-constructor pointer_to_binary_function (func)
  #+cl-stl-warn-deprecated
  (progn
	#-cl-stl-0x98 (warn "pointer_to_binary_function is deprecated."))
  (make-instance 'pointer_to_binary_function
				 :operator func
				 :closure (lambda (arg1 arg2)
							(funcall func arg1 arg2))))

(defun ptr_fun2 (func)
  #+cl-stl-warn-deprecated
  (progn
	#-cl-stl-0x98 (warn "ptr_fun2 is deprecated."))
  (make-instance 'pointer_to_binary_function
				 :operator func
				 :closure (lambda (arg1 arg2)
							(funcall func arg1 arg2))))

(defmethod operator_clone ((func pointer_to_binary_function))
  (let ((func (pointer_to_binary_function-operator func)))
	(make-instance 'pointer_to_binary_function
				   :operator func
				   :closure (lambda (arg1 arg2)
							  (funcall func arg1 arg2)))))




;-------------------------------------------------------------------------------
;
; 20.3.8, adaptors:
;
;-------------------------------------------------------------------------------

;------------------------------------------------------------
; class mem_fun_t & function mem_fun etc.
;                                      ( NONSENSE in CL-STL )
;------------------------------------------------------------
(define-functor mem_fun_t (#-cl-stl-0x98 functor
						   #+cl-stl-0x98 unary_function)
  ((op	:type     cl:function
		:initform nil
		:initarg  :operator
		:accessor mem_fun_t-operator)))

(declare-constructor mem_fun_t (1))

(define-constructor mem_fun_t (func)
  #+cl-stl-warn-deprecated
  (progn
	#-cl-stl-0x98 (warn "mem_fun_t is deprecated."))
  (make-instance 'mem_fun_t
				 :operator func
				 :closure (lambda (obj)
							(funcall func obj))))

(defun mem_fun (func)
  #+cl-stl-warn-deprecated
  (progn
	#-cl-stl-0x98 (warn "mem_fun is deprecated."))
  (make-instance 'mem_fun_t
				 :operator func
				 :closure (lambda (obj)
							(funcall func obj))))

(defun mem_fun_ref (func)
  #+cl-stl-warn-deprecated
  (progn
	#-cl-stl-0x98 (warn "mem_fun_ref is deprecated."))
  (make-instance 'mem_fun_t
				 :operator func
				 :closure (lambda (obj)
							(funcall func obj))))

(defmethod operator_clone ((func mem_fun_t))
  (let ((func (mem_fun_t-operator func)))
	(make-instance 'mem_fun_t
				   :operator func
				   :closure (lambda (obj)
							  (funcall func obj)))))




;------------------------------------------------------------
; class mem_fun1_t & function mem_fun1 etc.
;                                      ( NONSENSE in CL-STL )
;------------------------------------------------------------
(define-functor mem_fun1_t (#-cl-stl-0x98 functor
							#+cl-stl-0x98 unary_function)
  ((op	:type     cl:function
		:initform nil
		:initarg  :operator
		:accessor mem_fun1_t-operator)))

(declare-constructor mem_fun1_t (1))

(define-constructor mem_fun1_t (func)
  #+cl-stl-warn-deprecated
  (progn
	#-cl-stl-0x98 (warn "mem_fun1_t is deprecated."))
  (make-instance 'mem_fun1_t
				 :operator func
				 :closure (lambda (obj arg)
							(funcall func obj arg))))

(defun mem_fun1 (func)
  #+cl-stl-warn-deprecated
  (progn
	#-cl-stl-0x98 (warn "mem_fun1 is deprecated."))
  (make-instance 'mem_fun1_t
				 :operator func
				 :closure (lambda (obj arg)
							(funcall func obj arg))))

(defun mem_fun1_ref (func)
  #+cl-stl-warn-deprecated
  (progn
	#-cl-stl-0x98 (warn "mem_fun1_ref is deprecated."))
  (make-instance 'mem_fun1_t
				 :operator func
				 :closure (lambda (obj arg)
							(funcall func obj arg))))

(defmethod operator_clone ((func mem_fun1_t))
  (let ((func (mem_fun1_t-operator func)))
	(make-instance 'mem_fun1_t
				   :operator func
				   :closure (lambda (obj arg)
							  (funcall func obj arg)))))



;------------------------------------------------------------
; class mem_fun_ref-t;
;------------------------------------------------------------
; --> use mem_fun_t instead.

;------------------------------------------------------------
; class mem_fun1_ref-t;
;------------------------------------------------------------
; --> use mem_fun1_t instead.




;-------------------------------------------------------------------------------
;
; 0x11
;
;-------------------------------------------------------------------------------

#-cl-stl-0x98
(defun is_placeholder (arg)
  (if (not (keywordp arg))
	  0
	  (handler-case (values (parse-integer (symbol-name arg)))
		(error () 0))))
			 
#-cl-stl-0x98
(defun is_bind_expression (arg)
  (eq (type-of arg) '__binded-expr))
			 

;------------------------------------------------------------
; class __binded-expr
;------------------------------------------------------------
#-cl-stl-0x98
(define-functor __binded-expr (functor)
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
						  (ret  (is_placeholder item)))
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
						(let ((,g-bd-fnc (functor_function ,g-bindee)))
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
  (define-functor function (functor)
	((target :initform nil
			 :initarg  :target
			 :accessor __function-target))))

#-cl-stl-0x98
(declare-constructor function (0 1))

#-cl-stl-0x98
(labels ((__function-ctor (op)
		   (if (null op)
			   (make-instance 'function :target nil :closure nil)
			   (let* ((target  (clone op)))
				 (make-instance 'function
								:target target
								:closure (functor_function target))))))
  (define-constructor function ()
	(__function-ctor nil))
  (define-constructor function ((op (eql nil)))
	(__function-ctor nil))
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
  (let ((fnctr (funcall (the cl:function (__rm-ref-closure rm)))))
	(__check-type-of-move-constructor fnctr function)
	(let ((target  (__function-target fnctr))
		  (closure (__functor-closure fnctr)))
	  (setf (__function-target fnctr) nil)
	  (setf (__functor-closure fnctr) nil)
	  (make-instance 'function :target  target :closure closure))))

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
						(closure (functor_function op)))
				   (setf (__function-target fnc) target)
				   (setf (__functor-closure fnc) closure)))))
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
; class bit_and
;------------------------------------------------------------
#-cl-stl-0x98
(define-functor bit_and (functor) ())

#-cl-stl-0x98
(declare-constructor bit_and (0))

#-cl-stl-0x98
(labels ((__bit_and (arg1 arg2) (logand arg1 arg2)))
  (define-constructor bit_and ()
	(make-instance 'bit_and :closure #'__bit_and)))


;------------------------------------------------------------
; class bit_or
;------------------------------------------------------------
#-cl-stl-0x98
(define-functor bit_or (functor) ())

#-cl-stl-0x98
(declare-constructor bit_or (0))

#-cl-stl-0x98
(labels ((__bit_or (arg1 arg2) (logior arg1 arg2)))
  (define-constructor bit_or ()
	(make-instance 'bit_or :closure #'__bit_or)))


;------------------------------------------------------------
; class bit_xor
;------------------------------------------------------------
#-cl-stl-0x98
(define-functor bit_xor (functor) ())

#-cl-stl-0x98
(declare-constructor bit_xor (0))

#-cl-stl-0x98
(labels ((__bit_xor (arg1 arg2) (logxor arg1 arg2)))
  (define-constructor bit_xor ()
	(make-instance 'bit_xor :closure #'__bit_xor)))


;;------------------------------------------------------------
;; class mem_fn
;;------------------------------------------------------------
#-cl-stl-0x98
(define-functor mem_fn (functor)
  ((method :type     cl:function
		   :initform nil
		   :initarg  :method
		   :accessor mem_fn-method)))

#-cl-stl-0x98
(declare-constructor mem_fn (1))

#-cl-stl-0x98
(labels ((__mem_fn-ctor (method)
		   (make-instance 'mem_fn
						  :method  method
						  :closure (lambda (&rest args)
									 (apply method args)))))
  (define-constructor mem_fn (method)
	(__mem_fn-ctor method))
  (defmethod operator_clone ((func mem_fn))
	(__mem_fn-ctor (mem_fn-method func))))

