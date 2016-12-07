(in-package :cl-stl)

(declaim (inline make_pair))

;;------------------------------------------------------------------------------
;;
;; class definition
;;
;;------------------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass pair (clonable)
	((first  :initform nil
			 :initarg  :first
			 :accessor first)
	 (second :initform nil
			 :initarg  :second
			 :accessor second))))


;;------------------------------------------------------------------------------
;;
;; utilities
;;
;;------------------------------------------------------------------------------
(defun make_pair (&optional (first nil) (second nil))
  (let ((1st nil)
		(2nd nil))
	(_= 1st  first)
	(_= 2nd second)
	(make-instance 'pair :first 1st :second 2nd)))
	

;;------------------------------------------------------------------------------
;; constructor
;;------------------------------------------------------------------------------
(declare-constructor pair (0 1 2))

;; default constructor
(define-constructor pair ()
  (make-instance 'pair :first nil :second nil))

;; copy constructor
(define-constructor pair ((arg pair))
  (make_pair (stl:first arg) (stl:second arg)))

;; normal constructor
(define-constructor pair (first second)
  (make_pair first second))

;; move constructor
#-cl-stl-0x98
(define-constructor pair ((rm& remove-reference))
  (with-reference (rm)
	(let ((pr rm))
	  (__check-type-of-move-constructor pr pair)
	  (prog1 (make-instance 'pair :first  (stl:first  pr)
								  :second (stl:second pr))
		(setf (stl:first  pr) nil)
		(setf (stl:second pr) nil)))))

(defmethod operator_clone ((obj pair))
  (make_pair (stl:first obj) (stl:second obj)))


;-----------------------------------------------------
; assignment
;-----------------------------------------------------
(defmethod operator_= ((lhs (eql nil)) (rhs pair))
  (make_pair (stl:first rhs) (stl:second rhs)))

(defmethod operator_= ((lhs pair) (rhs pair))
  (_= (stl:first  lhs) (stl:first  rhs))
  (_= (stl:second lhs) (stl:second rhs))
  lhs)

#-cl-stl-0x98
(defmethod operator_move ((lhs pair) (rhs pair))
  (unless (eq lhs rhs)
	(multiple-value-bind (a b) (operator_move (stl:first  lhs)
											  (stl:first  rhs))
	  (setf (stl:first lhs) a)
	  (setf (stl:first rhs) b))
	(multiple-value-bind (a b) (operator_move (stl:second lhs)
											  (stl:second rhs))
	  (setf (stl:second lhs) a)
	  (setf (stl:second rhs) b)))
  (values lhs rhs))


;-----------------------------------------------------
; element access
;-----------------------------------------------------
#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod-overload get ((idx integer) (obj pair))
	(case idx
	  (0 (stl:first  obj))
	  (1 (stl:second obj))
	  (t (error 'out_of_range :what "Index specified to get is out of range."))))
  (defmethod __tie-get ((idx integer) (obj pair))
	(case idx
	  (0 (stl:first  obj))
	  (1 (stl:second obj))
	  (t (error 'out_of_range :what "Index specified to get is out of range.")))))
	

#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod-overload (setf get) (new-val (idx integer) (obj pair))
	(case idx
	  (0 (setf (stl:first  obj) new-val))
	  (1 (setf (stl:second obj) new-val))
	  (t (error 'out_of_range :what "Index specified to get is out of range.")))
	new-val)
  #-cl-stl-noextra
  (defmethod (setf __tie-get) (new-val (idx integer) (obj pair))
	(case idx
	  (0 (setf (stl:first  obj) new-val))
	  (1 (setf (stl:second obj) new-val))
	  (t (error 'out_of_range :what "Index specified to get is out of range.")))
	new-val))

;-----------------------------------------------------
; modifiers
;-----------------------------------------------------
(defmethod-overload swap ((p1 pair) (p2 pair))
  (unless (eq p1 p2)
	(multiple-value-bind (v1 v2) (opr::__swap-2 (stl:first p1) (stl:first p2))
	  (setf (stl:first p1) v1)
	  (setf (stl:first p2) v2))
	(multiple-value-bind (v1 v2) (opr::__swap-2 (stl:second p1) (stl:second p2))
	  (setf (stl:second p1) v1)
	  (setf (stl:second p2) v2)))
  (values p1 p2))


;-----------------------------------------------------
;; operators
;-----------------------------------------------------
(defmethod operator_== ((a pair) (b pair))
  (and (_== (stl:first  a) (stl:first  b))
	   (_== (stl:second a) (stl:second b))))

(defmethod operator_/= ((a pair) (b pair))
  (or (_/= (stl:first  a) (stl:first  b))
	  (_/= (stl:second a) (stl:second b))))

(labels ((pair< (a b)
		   (if (_< (stl:first a) (stl:first b))
			   t
			   (if (_< (stl:first b) (stl:first a))
				   nil
				   (if (_< (stl:second a) (stl:second b))
					   t
					   nil)))))
  (defmethod operator_<  ((a pair) (b pair)) (pair< a b))
  (defmethod operator_<= ((a pair) (b pair)) (not (pair< b a)))
  (defmethod operator_>  ((a pair) (b pair)) (pair< b a))
  (defmethod operator_>= ((a pair) (b pair)) (not (pair< a b))))


