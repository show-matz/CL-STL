(in-package :cl-stl)

(declaim (inline make_pair
				 first
				 second
				 (setf first)
				 (setf second)))

;;------------------------------------------------------------------------------
;;
;; class definition
;;
;;------------------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass pair (clonable)
	((items  :type     cl:simple-vector
			 :initform (make-array 2 :initial-element nil)
			 :initarg  :items
			 :accessor __inner-array))))


;;------------------------------------------------------------------------------
;;
;; utilities
;;
;;------------------------------------------------------------------------------
(locally (declare (optimize speed))
  (defun make_pair (first second)
	(let ((arr (make-array 2 :initial-element nil)))
	  (declare (type cl:simple-vector arr))
	  (_= (svref arr 0)  first)
	  (_= (svref arr 1) second)
	  (make-instance 'pair :items arr))))
	

;;------------------------------------------------------------------------------
;;
;; constructor
;;
;;------------------------------------------------------------------------------
(declare-constructor pair (0 1 2))

;; default constructor
(define-constructor pair ()
  (make-instance 'pair))

;; copy constructor
(locally (declare (optimize speed))
  (define-constructor pair ((arg pair))
	(let ((arr (__inner-array arg)))
	  (declare (type cl:simple-vector arr))
	  (make_pair (svref arr 0) (svref arr 1)))))

;; normal constructor
(define-constructor pair (first second)
  (make_pair first second))

;; move constructor
#-cl-stl-0x98
(locally (declare (optimize speed))
  (define-constructor pair ((rm& remove-reference))
	(with-reference (rm)
	  (let ((pr rm))
		(__check-type-of-move-constructor pr pair)
		(let ((arr1 (__inner-array pr))
			  (arr2 (make-array 2 :initial-element nil)))
		  (declare (type cl:simple-vector arr1 arr2))
		  (rotatef (svref arr1 0) (svref arr2 0))
		  (rotatef (svref arr1 1) (svref arr2 1))
		  (make-instance 'pair :items arr2))))))

(locally (declare (optimize speed))
  (defmethod operator_clone ((obj pair))
	(let ((arr (__inner-array obj)))
	  (declare (type cl:simple-vector arr))
	  (make_pair (svref arr 0) (svref arr 1)))))


;;------------------------------------------------------------------------------
;;
;; accessor
;;
;;------------------------------------------------------------------------------
(locally (declare (optimize speed))
  
  (defun first (pr)
	(svref (the cl:simple-vector (__inner-array pr)) 0))
  
  (defun (setf first) (new-val pr)
	(setf (svref (the cl:simple-vector (__inner-array pr)) 0) new-val))

  (defun second (pr)
	(svref (the cl:simple-vector (__inner-array pr)) 1))

  (defun (setf second) (new-val pr)
	(setf (svref (the cl:simple-vector (__inner-array pr)) 1) new-val)))


;;------------------------------------------------------------------------------
;;
;; assignment
;;
;;------------------------------------------------------------------------------
(locally (declare (optimize speed))
  (defmethod operator_= ((lhs pair) (rhs pair))
	(let ((arr1 (__inner-array lhs))
		  (arr2 (__inner-array rhs)))
	  (declare (type cl:simple-vector arr1 arr2))
	  (_= (svref arr1 0) (svref arr2 0))
	  (_= (svref arr1 1) (svref arr2 1)))
	lhs))

#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod operator_move ((lhs pair) (rhs pair))
	(unless (eq lhs rhs)
	  (let ((arr1 (__inner-array lhs))
			(arr2 (__inner-array rhs)))
		(declare (type cl:simple-vector arr1 arr2))
		(multiple-value-bind (a b) (operator_move (svref arr1 0)
												  (svref arr2 0))
		  (setf (svref arr1 0) a)
		  (setf (svref arr2 0) b))
		(multiple-value-bind (a b) (operator_move (svref arr1 1)
												  (svref arr2 1))
		  (setf (svref arr1 1) a)
		  (setf (svref arr2 1) b))))
	(values lhs rhs)))


;;------------------------------------------------------------------------------
;;
;; modifiers
;;
;;------------------------------------------------------------------------------
(locally (declare (optimize speed))
  (defmethod-overload swap ((p1 pair) (p2 pair))
	(unless (eq p1 p2)
	  (let ((arr1 (__inner-array p1))
			(arr2 (__inner-array p2)))
		(declare (type cl:simple-vector arr1 arr2))
		(multiple-value-bind (v1 v2) (opr::__swap-2 (svref arr1 0) (svref arr2 0))
		  (setf (svref arr1 0) v1)
		  (setf (svref arr2 0) v2))
		(multiple-value-bind (v1 v2) (opr::__swap-2 (svref arr1 1) (svref arr2 1))
		  (setf (svref arr1 1) v1)
		  (setf (svref arr2 1) v2))))
	(values p1 p2)))


;;------------------------------------------------------------------------------
;;
;; operators
;;
;;------------------------------------------------------------------------------
(locally (declare (optimize speed))
  (defmethod operator_== ((a pair) (b pair))
	(let ((arr1 (__inner-array a))
		  (arr2 (__inner-array b)))
	  (declare (type cl:simple-vector arr1 arr2))
	  (and (_== (svref arr1 0) (svref arr2 0))
		   (_== (svref arr1 1) (svref arr2 1))))))

(locally (declare (optimize speed))
  (defmethod operator_/= ((a pair) (b pair))
	(let ((arr1 (__inner-array a))
		  (arr2 (__inner-array b)))
	  (declare (type cl:simple-vector arr1 arr2))
	  (or (_/= (svref arr1 0) (svref arr2 0))
		  (_/= (svref arr1 1) (svref arr2 1))))))

(locally (declare (optimize speed))
  (labels ((pair< (a b)
			 (let ((arr1 (__inner-array a))
				   (arr2 (__inner-array b)))
			   (declare (type cl:simple-vector arr1 arr2))
			   (if (_< (svref arr1 0) (svref arr2 0))
				   t
				   (if (_< (svref arr2 0) (svref arr1 0))
					   nil
					   (if (_< (svref arr1 1) (svref arr2 1))
						   t
						   nil))))))
	(defmethod operator_<  ((a pair) (b pair)) (pair< a b))
	(defmethod operator_<= ((a pair) (b pair)) (not (pair< b a)))
	(defmethod operator_>  ((a pair) (b pair)) (pair< b a))
	(defmethod operator_>= ((a pair) (b pair)) (not (pair< a b)))))


;;------------------------------------------------------------------------------
;;
;; 
;;
;;------------------------------------------------------------------------------
(defmethod print-object ((pr pair) stream)
  (print-unreadable-object (pr stream :type t)
	(format stream "~A" (__inner-array pr))))

