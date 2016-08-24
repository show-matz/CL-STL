(in-package :cl-stl)


;;--------------------------------------------------------------------
;;
;; class difinition
;;
;;--------------------------------------------------------------------
#-(and cl-stl-noextra cl-stl-0x98)
(defclass cons_const_iterator (forward_iterator)
  ((node  :type     :cons
		  :initform nil
		  :initarg  :node
		  :accessor __cons-itr-cons)))

#-(and cl-stl-noextra cl-stl-0x98)
(defclass cons_iterator (cons_const_iterator) ())


;;--------------------------------------------------------------------
;;
;; internal utilities
;;
;;--------------------------------------------------------------------
#-(and cl-stl-noextra cl-stl-0x98)
(locally (declare (optimize speed))
  (defun __conslist-count-nodes (node1 node2)
	(let ((cnt 0))
	  (declare (type fixnum cnt))
	  (for (nil (not (eq node1 node2)) (incf cnt) :returns cnt)
		(setf node1 (cdr node1))))))

#-(and cl-stl-noextra cl-stl-0x98)
(locally (declare (optimize speed))
  (defun __conslist-equal (lst1 lst2)
	(if (and (null lst1) (null lst2))
		t
		(if (or (null lst1) (null lst2))
			nil
			(if (eq lst1 lst2)
				t
				(if (not (_== (car lst1) (car lst2)))
					nil
					(__conslist-equal (cdr lst1) (cdr lst2))))))))

#-(and cl-stl-noextra cl-stl-0x98)
(locally (declare (optimize speed))
  (defun __conslist-compare (lst1 lst2)
	(if (and (null lst1) (null lst2))
		0
		(if (null lst1)
			-1
			(if (null lst2)
				1
				(if (eq lst1 lst2)
					0
					(let ((v1 (car lst1))
						  (v2 (car lst2)))
					  (if (_< v1 v2)
						  -1
						  (if (_< v2 v1)
							  1
							  (__conslist-compare (cdr lst1) (cdr lst2)))))))))))


;;------------------------------------------------------------------------------
;;
;; methods for cons_const_iterator
;;
;;------------------------------------------------------------------------------
#-(and cl-stl-noextra cl-stl-0x98)
(defmethod operator_= ((itr1 cons_const_iterator) (itr2 cons_const_iterator))
  (__error-when-const-removing-assign itr1 cons_iterator
									  itr2 cons_const_iterator)
  (setf (__cons-itr-cons itr1) (__cons-itr-cons itr2))
  itr1)

#-(and cl-stl-noextra cl-stl-0x98)
(defmethod operator_clone ((itr cons_const_iterator))
  (make-instance 'cons_const_iterator :node (__cons-itr-cons itr)))

#-(and cl-stl-noextra cl-stl-0x98)
(defmethod operator_== ((itr1 cons_const_iterator) (itr2 cons_const_iterator))
  (eq (__cons-itr-cons itr1) (__cons-itr-cons itr2)))

#-(and cl-stl-noextra cl-stl-0x98)
(defmethod operator_/= ((itr1 cons_const_iterator) (itr2 cons_const_iterator))
  (not (eq (__cons-itr-cons itr1) (__cons-itr-cons itr2))))

#-(and cl-stl-noextra cl-stl-0x98)
(defmethod operator_* ((itr cons_const_iterator))
  (car (__cons-itr-cons itr)))

#-(and cl-stl-noextra cl-stl-0x98)
(defmethod (setf operator_*) (new-val (itr cons_const_iterator))
  (error 'setf-to-const :what "setf to (_* cons_const_iterator)."))

#-(and cl-stl-noextra cl-stl-0x98)
(defmethod operator_++ ((itr cons_const_iterator))
  (setf (__cons-itr-cons itr) (cdr (__cons-itr-cons itr)))
  itr)

#-(and cl-stl-noextra cl-stl-0x98)
(locally (declare (optimize speed))
  (defmethod advance ((itr cons_const_iterator) (n integer))
	(declare (type fixnum n))
	(unless (>= n 0)
	  (error 'undefined-behavior :what "advance : Negative value for forward_iterator."))
	(let ((i 0)
		  (node (__cons-itr-cons itr)))
	  (declare (type fixnum i))
	  (for (nil (< i n) (incf i))
		(setf node (cdr node)))
	  (setf (__cons-itr-cons itr) node))
	nil))

#-(and cl-stl-noextra cl-stl-0x98)
(defmethod distance ((itr1 cons_const_iterator) (itr2 cons_const_iterator))
  (__conslist-count-nodes (__cons-itr-cons itr1) (__cons-itr-cons itr2)))


;;------------------------------------------------------------------------------
;;
;; methods for cons_iterator
;;
;;------------------------------------------------------------------------------
#-(and cl-stl-noextra cl-stl-0x98)
(defmethod operator_clone ((itr cons_iterator))
  (make-instance 'cons_iterator :node (__cons-itr-cons itr)))

#-(and cl-stl-noextra cl-stl-0x98)
(defmethod operator_cast ((itr cons_iterator)
						  (typename (eql 'cons_const_iterator)))
  (__check-exact-type-of-cast itr 'cons_iterator
								  'cons_const_iterator)
  (make-instance 'cons_iterator :node (__cons-itr-cons itr)))

#-(and cl-stl-noextra cl-stl-0x98)
(defmethod (setf operator_*) (new-val (itr cons_iterator))
  (setf (car (__cons-itr-cons itr)) new-val)
  new-val)



;-----------------------------------------------------
; begin, end, size ... etc.
;-----------------------------------------------------
#-cl-stl-noextra ; cl:list's begin, end & for.
(progn

  #-cl-stl-0x98 (defmethod data ((lst cl:list)) lst)
  (defmethod size ((lst cl:list)) (length lst))

  (defmethod begin  ((cont cl:list)) (make-instance 'cons_iterator :node cont))
  (defmethod end    ((cont cl:list)) (make-instance 'cons_iterator :node nil))

  #-cl-stl-0x98 (defmethod cbegin ((cont cl:list)) (make-instance 'cons_const_iterator :node cont))
  #-cl-stl-0x98 (defmethod cend   ((cont cl:list)) (make-instance 'cons_const_iterator :node nil)))


;-----------------------------------------------------
; compare
;-----------------------------------------------------
#-cl-stl-noextra
(locally (declare (optimize speed))
  (defmethod operator_== ((lst1 cl:list) (lst2 cl:list))       (__conslist-equal   lst1 lst2))
  (defmethod operator_/= ((lst1 cl:list) (lst2 cl:list)) (not  (__conslist-equal   lst1 lst2)))
  (defmethod operator_<  ((lst1 cl:list) (lst2 cl:list)) (<    (__conslist-compare lst1 lst2) 0))
  (defmethod operator_<= ((lst1 cl:list) (lst2 cl:list)) (<=   (__conslist-compare lst1 lst2) 0))
  (defmethod operator_>  ((lst1 cl:list) (lst2 cl:list)) (<  0 (__conslist-compare lst1 lst2)))
  (defmethod operator_>= ((lst1 cl:list) (lst2 cl:list)) (<= 0 (__conslist-compare lst1 lst2))))


;-----------------------------------------------------
; enumeration
;-----------------------------------------------------
#-cl-stl-noextra
(progn
  #-cl-stl-0x98
  (locally (declare (optimize speed))
	(defmethod-overload for ((cont cl:list) func)
	  ;;MEMO : func is always lambda function ( see stl:for ).
	  (declare (type cl:list     cont))
	  (declare (type cl:function func))
	  (dolist (v cont)
		(funcall func v)))))

