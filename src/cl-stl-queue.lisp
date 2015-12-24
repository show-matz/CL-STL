(in-package :cl-stl)

;;------------------------------------------------------------------------------
;;
;; class difinition
;;
;;------------------------------------------------------------------------------
(defclass queue (clonable)
  ((container :type     :pushable-front-container
			  :initform (new stl:deque)
			  :initarg  :container
			  :accessor __que-container)))


;;------------------------------------------------------------------------------
;;
;; internal utilities
;;
;;------------------------------------------------------------------------------
(defmacro check-underlying-container-of-queue (cont)
  (check-type cont symbol)
  `(unless (and (typep ,cont 'pushable-back-container)
				(typep ,cont 'pushable-front-container))
	 (error 'type-mismatch :what "Underlying container of queue must be pushable-back-container and be pushable-front-container.")))


;;------------------------------------------------------------------------------
;;
;; constructors
;;
;;------------------------------------------------------------------------------
(declare-constructor queue (0 1))

; empty
(define-constructor queue ()
  (make-instance 'queue :container (new stl:deque)))

; initialize
; MEMO : container arg is copied.
(define-constructor queue ((arg pushable-front-container))
  (check-type arg pushable-back-container)
  (make-instance 'queue :container (clone arg)))

; copy constructor
(define-constructor queue ((arg queue))
  (let ((cont (__que-container arg)))
	(make-instance 'queue :container (clone cont))))

; move constructor & move init constructor
#-cl-stl-0x98
(define-constructor queue ((arg remove-reference))
  (let ((cont (funcall (the cl:function (__rm-ref-closure arg)))))
	(if (eq (type-of cont) 'queue)
		(let* ((src-cont (__que-container cont))
			   (new-cont (dynamic-new (type-of src-cont))))
		  (swap new-cont src-cont)
		  (make-instance 'queue :container new-cont))
		(progn
		  (check-underlying-container-of-queue cont)
		  (let ((new-cont (dynamic-new (type-of cont))))
			(swap new-cont cont)
			(make-instance 'queue :container new-cont))))))

;; take internal container type
; example : (new stl:queue foo:cont)
(define-constructor queue ((arg symbol))
  (let ((cont (dynamic-new arg)))
	(check-underlying-container-of-queue cont)
	(make-instance 'queue :container cont)))

(defmethod operator_clone ((arg queue))
  (make-instance 'queue
				 :container (clone (__que-container arg))))


;;------------------------------------------------------------------------------
;;
;; methods
;;
;;------------------------------------------------------------------------------

;-----------------------------------------------------
; capacity
;-----------------------------------------------------
(defmethod empty ((cont queue))
  (zerop (size cont)))

(defmethod size ((cont queue))
  (size (__que-container cont)))

;-----------------------------------------------------
; element access
;-----------------------------------------------------
(defmethod front ((cont queue))
  (front (__que-container cont)))

(defmethod (setf front) (val (cont queue))
  (_= (front (__que-container cont)) val))

(defmethod back ((cont queue))
  (back (__que-container cont)))

(defmethod (setf back) (val (cont queue))
  (_= (back (__que-container cont)) val))

;-----------------------------------------------------
; modifiers
;-----------------------------------------------------
(defmethod push ((cont queue) val)
  (push-back (__que-container cont) val)
  nil)

(defmethod pop ((cont queue))
  (pop-front (__que-container cont))
  nil)


#-cl-stl-0x98    ; emplace
(defmethod-overload emplace ((container queue) new-val)
  (__emplace-back-2 (__que-container container) new-val)
  nil)

#-cl-stl-0x98
(defmethod-overload swap ((cont1 queue) (cont2 queue))
  (swap (__que-container cont1) (__que-container cont2))
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
					 (let ((cont1 (__que-container cont1))
						   (cont2 (__que-container cont2)))
					   (with-operators
						   (for (((itr1 (begin cont1))
								  (itr2 (begin cont2))
								  (end2 (end   cont2))) (_/= itr2 end2) (progn ++itr1 ++itr2) :returns t)
							 (unless (_== *itr1 *itr2)
							   (return-from __container-equal nil)))))))))

  (defmethod operator_== ((cont1 queue) (cont2 queue))
	(__container-equal cont1 cont2))

  (defmethod operator_/= ((cont1 queue) (cont2 queue))
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

	(defmethod operator_< ((cont1 queue) (cont2 queue))
	  (< (__container-compare (__que-container cont1) (__que-container cont2)) 0))

	(defmethod operator_<= ((cont1 queue) (cont2 queue))
	  (<= (__container-compare (__que-container cont1) (__que-container cont2)) 0))

	(defmethod operator_> ((cont1 queue) (cont2 queue))
	  (< 0 (__container-compare (__que-container cont1) (__que-container cont2))))

	(defmethod operator_>= ((cont1 queue) (cont2 queue))
	  (<= 0 (__container-compare (__que-container cont1) (__que-container cont2))))))


