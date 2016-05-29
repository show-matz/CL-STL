(in-package :cl-stl)

;;------------------------------------------------------------------------------
;;
;; class difinition
;;
;;------------------------------------------------------------------------------
(defclass stack (clonable)
  ((container :type     :pushable_back_container
			  :initform (new stl:deque)
			  :initarg  :container
			  :accessor __stck-container)))

;;------------------------------------------------------------------------------
;;
;; internal utilities
;;
;;------------------------------------------------------------------------------
(defmacro check-underlying-container-of-stack (cont)
  (check-type cont symbol)
  `(unless (typep ,cont 'pushable_back_container)
	 (error 'type-mismatch :what "Underlying container of stack must be pushable_back_container.")))


;;------------------------------------------------------------------------------
;;
;; constructors
;;
;;------------------------------------------------------------------------------
(declare-constructor stack (0 1))

; empty
(define-constructor stack ()
  (make-instance 'stack :container (new stl:deque)))

; initialize
; MEMO : container arg is copied.
(define-constructor stack ((arg pushable_back_container))
  (make-instance 'stack :container (clone arg)))

; copy constructor
(define-constructor stack ((arg stack))
  (let ((cont (__stck-container arg)))
	(make-instance 'stack :container (clone cont))))

; move constructor & move init constructor
#-cl-stl-0x98
(define-constructor stack ((arg remove-reference))
  (let ((cont (funcall (the cl:function (opr::__rm-ref-closure arg)))))
	(if (eq (type-of cont) 'stack)
		(let* ((src-cont (__stck-container cont))
			   (new-cont (dynamic-new (type-of src-cont))))
		  (swap new-cont src-cont)
		  (make-instance 'stack :container new-cont))
		(progn
		  (check-underlying-container-of-stack cont)
		  (let ((new-cont (dynamic-new (type-of cont))))
			(swap new-cont cont)
			(make-instance 'stack :container new-cont))))))

;; take internal container type
;; example : (new stl:stack foo:cont)
(define-constructor stack ((arg symbol))
  (let ((cont (dynamic-new arg)))
	(check-underlying-container-of-stack cont)
	(make-instance 'stack :container cont)))

(defmethod operator_clone ((arg stack))
  (make-instance 'stack
				 :container (clone (__stck-container arg))))


;;------------------------------------------------------------------------------
;;
;; methods
;;
;;------------------------------------------------------------------------------

;-----------------------------------------------------
; capacity
;-----------------------------------------------------
(defmethod empty ((cont stack))
  (zerop (size cont)))

(defmethod size ((cont stack))
  (size (__stck-container cont)))

;-----------------------------------------------------
; element access
;-----------------------------------------------------
(defmethod top ((cont stack))
  (back (__stck-container cont)))

(defmethod (setf top) (val (cont stack))
  (_= (back (__stck-container cont)) val))

;-----------------------------------------------------
; modifiers
;-----------------------------------------------------
(defmethod push ((cont stack) val)
  (push_back (__stck-container cont) val)
  nil)

(defmethod pop ((cont stack))
  (pop_back (__stck-container cont))
  nil)


#-cl-stl-0x98    ; emplace
(defmethod-overload emplace ((container stack) new-val)
  (__emplace_back-2 (__stck-container container) new-val)
  nil)

#-cl-stl-0x98
(defmethod-overload swap ((cont1 stack) (cont2 stack))
  (swap (__stck-container cont1) (__stck-container cont2))
  (values cont1 cont2))


;-----------------------------------------------------
; compare
;-----------------------------------------------------
(locally (declare (optimize speed))
  (labels ((__container-equal (cont1 cont2)
			 (if (/= (the fixnum (size cont1))
					 (the fixnum (size cont2)))
				 nil
				 (if (zerop (the fixnum (size cont1)))
					 t
					 (let ((cont1 (__stck-container cont1))
						   (cont2 (__stck-container cont2)))
					   (with-operators
						   (for (((itr1     (begin cont1))
								  (itr2     (begin cont2))
								  (itr2-end (end   cont2))) (_/= itr2 itr2-end)
															(progn ++itr1 ++itr2) :returns t)
							 (unless (_== *itr1 *itr2)
							   (return-from __container-equal nil)))))))))

  (defmethod operator_== ((cont1 stack) (cont2 stack))
	(__container-equal cont1 cont2))

  (defmethod operator_/= ((cont1 stack) (cont2 stack))
	(not (__container-equal cont1 cont2)))))




(locally (declare (optimize speed))
  (labels ((__container-compare (cont1 cont2)
			 (with-operators
				 (for (((itr1     (begin cont1))
						(itr1-end (end   cont1))
						(itr2     (begin cont2))
						(itr2-end (end   cont2))) t (progn ++itr1 ++itr2))
				   (let ((end1 (_== itr1 itr1-end))
						 (end2 (_== itr2 itr2-end)))
					 (if (and end1 end2)
						 (return-from __container-compare  0)
						 (if end1
							 (return-from __container-compare -1)
							 (if end2
								 (return-from __container-compare  1)
								 (let ((val1 *itr1)
									   (val2 *itr2))
								   (if (_< val1 val2)
									   (return-from __container-compare -1)
									   (if (_< val2 val1)
										   (return-from __container-compare  1))))))))))))

  (defmethod operator_< ((cont1 stack) (cont2 stack))
	(< (__container-compare (__stck-container cont1) (__stck-container cont2)) 0))

  (defmethod operator_<= ((cont1 stack) (cont2 stack))
	(<= (__container-compare (__stck-container cont1) (__stck-container cont2)) 0))

  (defmethod operator_> ((cont1 stack) (cont2 stack))
	(< 0 (__container-compare (__stck-container cont1) (__stck-container cont2))))

  (defmethod operator_>= ((cont1 stack) (cont2 stack))
	(<= 0 (__container-compare (__stck-container cont1) (__stck-container cont2))))))
		  
