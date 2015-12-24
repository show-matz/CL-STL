(in-package :cl-stl)


;;--------------------------------------------------------------------
;;
;; class difinition
;;
;;--------------------------------------------------------------------
#+(or cl-stl-extra (not cl-stl-0x98))
(defclass cons-const-iterator (forward-iterator)
  ((node  :type     :cons
		  :initform nil
		  :initarg  :node
		  :accessor __cons-itr-cons)))

#+(or cl-stl-extra (not cl-stl-0x98))
(defclass cons-iterator (cons-const-iterator) ())


;;--------------------------------------------------------------------
;;
;; internal utilities
;;
;;--------------------------------------------------------------------
#+(or cl-stl-extra (not cl-stl-0x98))
(locally (declare (optimize speed))
  (defun __conslist-count-nodes (node1 node2)
	(let ((cnt 0))
	  (declare (type fixnum cnt))
	  (for (nil (not (eq node1 node2)) (incf cnt) :returns cnt)
		(setf node1 (cdr node1))))))

#+(or cl-stl-extra (not cl-stl-0x98))
(locally (declare (optimize speed))
  (defun __conslist-clone (lst)
	(labels ((imp (src top last)
			   (if (null src)
				   top
				   (let ((node (cons nil nil)))
					 (_= (car node) (car src))
					 (setf (cdr last) node)
					 (imp (cdr src) top node)))))
	  (if (null lst)
		  nil
		  (let ((top (cons nil nil)))
			(_= (car top) (car lst))
			(imp (cdr lst) top top))))))

#+(or cl-stl-extra (not cl-stl-0x98))
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

#+(or cl-stl-extra (not cl-stl-0x98))
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
;; methods for cons-const-iterator
;;
;;------------------------------------------------------------------------------
#+(or cl-stl-extra (not cl-stl-0x98))
(defmethod operator_= ((itr1 cons-const-iterator) (itr2 cons-const-iterator))
  (__error-when-const-removing-assign itr1 cons-iterator
									  itr2 cons-const-iterator)
  (setf (__cons-itr-cons itr1) (__cons-itr-cons itr2))
  itr1)

#+(or cl-stl-extra (not cl-stl-0x98))
(defmethod operator_clone ((itr cons-const-iterator))
  (make-instance 'cons-const-iterator :node (__cons-itr-cons itr)))

#+(or cl-stl-extra (not cl-stl-0x98))
(defmethod operator_== ((itr1 cons-const-iterator) (itr2 cons-const-iterator))
  (eq (__cons-itr-cons itr1) (__cons-itr-cons itr2)))

#+(or cl-stl-extra (not cl-stl-0x98))
(defmethod operator_/= ((itr1 cons-const-iterator) (itr2 cons-const-iterator))
  (not (eq (__cons-itr-cons itr1) (__cons-itr-cons itr2))))

#+(or cl-stl-extra (not cl-stl-0x98))
(defmethod operator_* ((itr cons-const-iterator))
  (car (__cons-itr-cons itr)))

#+(or cl-stl-extra (not cl-stl-0x98))
(defmethod (setf operator_*) (new-val (itr cons-const-iterator))
  (error 'setf-to-const :what "setf to (_* cons-const-iterator)."))

#+(or cl-stl-extra (not cl-stl-0x98))
(defmethod operator_++ ((itr cons-const-iterator))
  (setf (__cons-itr-cons itr) (cdr (__cons-itr-cons itr)))
  itr)

#+(or cl-stl-extra (not cl-stl-0x98))
(locally (declare (optimize speed))
  (defmethod advance ((itr cons-const-iterator) (n integer))
	(declare (type fixnum n))
	(unless (>= n 0)
	  (error 'undefined-behavior :what "advance : Negative value for forward-iterator."))
	(let ((i 0)
		  (node (__cons-itr-cons itr)))
	  (declare (type fixnum i))
	  (for (nil (< i n) (incf i))
		(setf node (cdr node)))
	  (setf (__cons-itr-cons itr) node))
	nil))

#+(or cl-stl-extra (not cl-stl-0x98))
(defmethod distance ((itr1 cons-const-iterator) (itr2 cons-const-iterator))
  (__conslist-count-nodes (__cons-itr-cons itr1) (__cons-itr-cons itr2)))


;;------------------------------------------------------------------------------
;;
;; methods for cons-iterator
;;
;;------------------------------------------------------------------------------
#+(or cl-stl-extra (not cl-stl-0x98))
(defmethod operator_clone ((itr cons-iterator))
  (make-instance 'cons-iterator :node (__cons-itr-cons itr)))

#+(or cl-stl-extra (not cl-stl-0x98))
(defmethod operator_cast ((itr cons-iterator)
						  (typename (eql 'cons-const-iterator)))
  (__check-exact-type-of-cast itr 'cons-iterator
								  'cons-const-iterator)
  (make-instance 'cons-iterator :node (__cons-itr-cons itr)))

#+(or cl-stl-extra (not cl-stl-0x98))
(defmethod (setf operator_*) (new-val (itr cons-iterator))
  (setf (car (__cons-itr-cons itr)) new-val)
  new-val)



;-----------------------------------------------------
; begin, end, size ... etc.
;-----------------------------------------------------
#+cl-stl-extra ; cl:list's begin, end & for.
(progn

  #-cl-stl-0x98 (defmethod data ((lst cl:list)) lst)
  (defmethod size ((lst cl:list)) (length lst))

  (defmethod begin  ((cont cl:list)) (make-instance 'cons-iterator :node cont))
  (defmethod end    ((cont cl:list)) (make-instance 'cons-iterator :node nil))

  #-cl-stl-0x98 (defmethod cbegin ((cont cl:list)) (make-instance 'cons-const-iterator :node cont))
  #-cl-stl-0x98 (defmethod cend   ((cont cl:list)) (make-instance 'cons-const-iterator :node nil))

  (defmethod operator_clone ((obj cl:list)) (__conslist-clone obj)))


;-----------------------------------------------------
; compare
;-----------------------------------------------------
#+cl-stl-extra
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
#+cl-stl-extra
(progn
  #-cl-stl-0x98
  (locally (declare (optimize speed))
	(defmethod-overload for ((cont cl:list) func)
	  ;;MEMO : func is always lambda function ( see stl:for ).
	  (declare (type cl:list     cont))
	  (declare (type cl:function func))
	  (dolist (v cont)
		(funcall func v)))))

