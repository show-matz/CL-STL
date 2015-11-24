
(in-package :cl-stl)

(declaim (inline make-pair
				 first
				 second))

;;------------------------------------------------------------------------------
;;
;; class definition
;;
;;------------------------------------------------------------------------------
(defclass pair (clonable)
  ((first  :initform nil
		   :initarg  :first
		   :accessor __pair-first)
   (second :initform nil
		   :initarg  :second
		   :accessor __pair-second)))


;;------------------------------------------------------------------------------
;;
;; utilities
;;
;;------------------------------------------------------------------------------
(defun make-pair (&optional (first nil) (second nil))
  (make-instance 'pair :first first :second second))


;;------------------------------------------------------------------------------
;; constructor
;;------------------------------------------------------------------------------
(declare-constructor pair (0 1 2))

;; default constructor
(define-constructor pair ()
  (make-instance 'pair :first nil :second nil))

;; copy constructor
(define-constructor pair ((arg pair))
  (make-instance 'pair :first  (__pair-first  arg)
					   :second (__pair-second arg)))

;; normal constructor
(define-constructor pair (first second)
  (make-instance 'pair :first first :second second))

;; move constructor
#-cl-stl-0x98
(define-constructor pair ((rm-ref remove-reference))
  (let ((pr (funcall (__rm-ref-closure rm-ref))))
	(__check-type-of-move-constructor pr pair)
	(prog1 (make-instance 'pair :first  (__pair-first  pr)
								:second (__pair-second pr))
	  (setf (__pair-first  pr) nil)
	  (setf (__pair-second pr) nil))))

;; move initialization
#-cl-stl-0x98
(define-constructor pair ((rm-ref1 remove-reference)
						  (rm-ref2 remove-reference))
  (let ((v1 (funcall (__rm-ref-closure rm-ref1)))
		(v2 (funcall (__rm-ref-closure rm-ref2))))
	(prog1 (make-instance 'pair :first v1 :second v2)
	  (funcall (__rm-ref-closure rm-ref1) nil)
	  (funcall (__rm-ref-closure rm-ref2) nil))))

(defmethod operator_clone ((obj pair))
  (make-instance 'pair :first  (__pair-first  obj)
					   :second (__pair-second obj)))


;-----------------------------------------------------
; assignment
;-----------------------------------------------------
(defmethod operator_= ((lhs (eql nil)) (rhs pair))
  (make-instance 'pair :first  (__pair-first  rhs)
					   :second (__pair-second rhs)))

(defmethod operator_= ((lhs pair) (rhs pair))
  (_= (__pair-first  lhs) (__pair-first  rhs))
  (_= (__pair-second lhs) (__pair-second rhs))
  lhs)

#-cl-stl-0x98
(defmethod operator_move ((lhs pair) (rhs pair))
  (unless (eq lhs rhs)
	(multiple-value-bind (a b) (operator_move (__pair-first  lhs)
											  (__pair-first  rhs))
	  (setf (__pair-first lhs) a)
	  (setf (__pair-first rhs) b))
	(multiple-value-bind (a b) (operator_move (__pair-second lhs)
											  (__pair-second rhs))
	  (setf (__pair-second lhs) a)
	  (setf (__pair-second rhs) b)))
  (values lhs rhs))


;-----------------------------------------------------
; element access
;-----------------------------------------------------
(defun first (pr)
  (__pair-first pr))

(defun (setf first) (new-val pr)
  (setf (__pair-first pr) new-val))

(defun second (pr)
  (__pair-second pr))

(defun (setf second) (new-val pr)
  (setf (__pair-second pr) new-val))

#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod-overload get ((idx integer) (obj pair))
	(case idx
	  (0 (__pair-first  obj))
	  (1 (__pair-second obj))
	  (t (error 'out-of-range :what "Index specified to get is out of range."))))
  (defmethod __tie-get ((idx integer) (obj pair))
	(case idx
	  (0 (__pair-first  obj))
	  (1 (__pair-second obj))
	  (t (error 'out-of-range :what "Index specified to get is out of range.")))))
	

#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod-overload (setf get) (new-val (idx integer) (obj pair))
	(case idx
	  (0 (setf (__pair-first  obj) new-val))
	  (1 (setf (__pair-second obj) new-val))
	  (t (error 'out-of-range :what "Index specified to get is out of range.")))
	new-val)
  #+cl-stl-extra
  (defmethod (setf __tie-get) (new-val (idx integer) (obj pair))
	(case idx
	  (0 (setf (__pair-first  obj) new-val))
	  (1 (setf (__pair-second obj) new-val))
	  (t (error 'out-of-range :what "Index specified to get is out of range.")))
	new-val))

;-----------------------------------------------------
; modifiers
;-----------------------------------------------------
#-cl-stl-0x98
(defmethod-overload swap ((p1 pair) (p2 pair))
  (unless (eq p1 p2)
	(swap (__pair-first  p1) (__pair-first  p2))
	(swap (__pair-second p1) (__pair-second p2)))
  (values p1 p2))


;-----------------------------------------------------
;; operators
;-----------------------------------------------------
(defmethod operator_== ((a pair) (b pair))
  (and (_== (__pair-first  a) (__pair-first  b))
	   (_== (__pair-second a) (__pair-second b))))

(defmethod operator_/= ((a pair) (b pair))
  (or (_/= (__pair-first  a) (__pair-first  b))
	  (_/= (__pair-second a) (__pair-second b))))

(labels ((pair< (a b)
		   (if (_< (__pair-first a) (__pair-first b))
			   t
			   (if (_< (__pair-first b) (__pair-first a))
				   nil
				   (if (_< (__pair-second a) (__pair-second b))
					   t
					   nil)))))
  (defmethod operator_<  ((a pair) (b pair)) (pair< a b))
  (defmethod operator_<= ((a pair) (b pair)) (not (pair< b a)))
  (defmethod operator_>  ((a pair) (b pair)) (pair< b a))
  (defmethod operator_>= ((a pair) (b pair)) (not (pair< a b))))


