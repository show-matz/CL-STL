(in-package :cl-stl)

;;--------------------------------------------------------------------
;;
;; class difinition
;;
;;--------------------------------------------------------------------
#-cl-stl-0x98
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass stl:array (randomaccess_container)
	((buffer :type     :simple-vector
			 :initform nil
			 :initarg  :buffer
			 :accessor __inner-array))))

#-cl-stl-0x98
(defclass array_const_iterator (const-vector-pointer randomaccess_iterator) ())
#-cl-stl-0x98
(defclass array_iterator (vector-pointer array_const_iterator) ())
#-cl-stl-0x98
(defclass array_const_reverse_iterator (const-reverse-vector-pointer randomaccess_iterator) ())
#-cl-stl-0x98
(defclass array_reverse_iterator (reverse-vector-pointer array_const_reverse_iterator) ())

;;--------------------------------------------------------------------
;;
;; internal utilities
;;
;;--------------------------------------------------------------------
#-cl-stl-0x98
(defmacro __array-error-when-empty (buf-sym op)
  (check-type buf-sym symbol)
  `(when (zerop (length ,buf-sym))
	 (error 'undefined-behavior :what ,(format nil "~A for empty array." op))))

#-cl-stl-0x98
(defmacro __array-check-index (buf-sym idx-sym)
  (check-type buf-sym symbol)
  (check-type idx-sym symbol)
  (let ((g-size (gensym "SIZE")))
	`(let ((,g-size (if (null ,buf-sym)
						0
						(length ,buf-sym))))
	   (when (or (< ,idx-sym 0) (<= ,g-size ,idx-sym))
		 (error 'out_of_range :what ,(format nil "index ~A is out of range." idx-sym))))))

#-cl-stl-0x98
(defun __create-array (size &optional (initial-element nil))
  (make-instance 'stl:array
				 :buffer (if (zerop size)
							 (make-array 0)
							 (make-array size :initial-element initial-element))))



;;------------------------------------------------------------------------------
;;
;; constructors
;;
;;------------------------------------------------------------------------------
#-cl-stl-0x98
(declare-constructor array (1 2))

; default constructor
#-cl-stl-0x98
(define-constructor array ((arg integer))
  (__create-array arg))

; copy constructor
#-cl-stl-0x98
(define-constructor array ((arg1 integer) (arg2 stl:array))
  (let* ((src-buf (__inner-array arg2))
		 (size (length src-buf)))
	(if (/= arg1 size)
		(error 'type-mismatch :what (format nil "Can't convert array<~A> to array<~A>." size arg1))
		(if (zerop size)
			(__create-array arg1)
			(let ((index 0)
				  (dst-buf (make-array arg1 :initial-element nil)))
			  (locally (declare (optimize speed))
				(declare (type fixnum size index))
				(declare (type simple-vector src-buf dst-buf))
				(do ()
					((= index size) nil)
				  (_= (svref dst-buf index) (svref src-buf index))
				  (incf index)))
			  (make-instance 'stl:array :buffer dst-buf))))))

; constructor with initializer list
#-cl-stl-0x98
(locally (declare (optimize speed))
  (define-constructor array ((arg1 integer) (arg2 initializer_list))
	(declare (type fixnum arg1))
	(if (< arg1 (the fixnum (size arg2)))
		(error 'type-mismatch :what (format nil "Too many initializer for array<~A>." arg1))
		(let* ((obj (__create-array arg1))
			   (cnt arg1)
			   (dst (__inner-array obj))
			   (src (__initlist-data arg2)))
		  (declare (type fixnum cnt))
		  (declare (type simple-vector dst src) )
		  (when (< (length src) cnt)
			(setf cnt (length src)))
		  (do ((idx 0 (1+ idx)))
			  ((<= cnt idx) obj)
			(declare (type fixnum idx))
			(_= (svref dst idx) (svref src idx)))))))


; copy constructor
#-cl-stl-0x98
(defmethod operator_clone ((container stl:array))
  (let ((src-buf (__inner-array container)))
	(if (null src-buf)
		(make-instance 'stl:array)
		(let* ((size    (length src-buf))
			   (index   0)
			   (dst-buf (make-array size :initial-element nil)))
		  (locally (declare (optimize speed))
			(declare (type fixnum size index))
			(declare (type simple-vector src-buf dst-buf))
			(do ()
				((= index size) nil)
			  (_= (svref dst-buf index) (svref src-buf index))
			  (incf index)))
		  (make-instance 'stl:array :buffer dst-buf)))))

;;------------------------------------------------------------------------------
;;
;; methods
;;
;;------------------------------------------------------------------------------

;-----------------------------------------------------
; assignment
;-----------------------------------------------------
#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod operator_= ((cont1 stl:array) (cont2 stl:array))
	(if (eq cont1 cont2)
		cont1
		(let* ((buf1 (__inner-array cont1))
			   (buf2 (__inner-array cont2))
			   (cnt1 (if (null buf1) 0 (length buf1)))
			   (cnt2 (if (null buf2) 0 (length buf2))))
		  (declare (type simple-vector buf1 buf2))
		  (declare (type fixnum cnt1 cnt2))
		  (if (/= cnt1 cnt2)
			  (error 'type-mismatch :what "Type mismatch in assign of array.")
			  (do ((idx 0 (1+ idx)))
				  ((= idx cnt1) cont1)
				(declare (type fixnum idx))
				(_= (svref buf1 idx) (svref buf2 idx))))))))

#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod operator_= ((cont stl:array) (il initializer_list))
	(let* ((dst (__inner-array cont))
		   (dst-cnt (length dst))
		   (src (__initlist-data il))
		   (cnt (length src)))
	  (declare (type fixnum dst-cnt cnt))
	  (declare (type simple-vector dst src))
	  (if (< dst-cnt cnt)
		  (error 'type-mismatch :what (format nil "Too many initializer for array<~A>." dst-cnt))
		  (let ((idx 0))
			(declare (type fixnum idx))
			(do ()
				((= idx cnt) nil)
			  (_= (svref dst idx) (svref src idx))
			  (incf idx))
			(do ()
				((= idx dst-cnt) cont)
			  (setf (svref dst idx) nil)
			  (incf idx)))))))

#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod operator_move ((cont1 stl:array) (cont2 stl:array))
	(if (eq cont1 cont2)
		(values cont1 cont2)
		(if (/= (the fixnum (size cont1))
				(the fixnum (size cont2)))
			(error 'type-mismatch :what "Type mismatch in move of array.")
			(let* ((tmp (__inner-array cont1))
				   (cnt (length tmp)))
			  (declare (type simple-vector tmp))
			  (declare (type fixnum cnt))
			  (setf (__inner-array cont1) (__inner-array cont2))
			  (setf (__inner-array cont2) tmp)
			  (do ((idx 0 (incf idx)))
				  ((= cnt idx) (values cont1 cont2))
				(setf (svref tmp idx) nil)))))))



;-----------------------------------------------------
; iterators
;-----------------------------------------------------
#-cl-stl-0x98
(defmethod begin ((cont stl:array))
  (make-instance 'array_iterator :buffer (__inner-array cont) :index 0))

#-cl-stl-0x98
(defmethod end ((cont stl:array))
  (let ((buf (__inner-array cont)))
	(make-instance 'array_iterator :buffer buf
								   :index (length buf))))

#-cl-stl-0x98
(defmethod rbegin ((cont stl:array))
  (let ((buf (__inner-array cont)))
	(make-instance 'array_reverse_iterator
				   :buffer buf :index (1- (length buf)))))

#-cl-stl-0x98
(defmethod rend ((cont stl:array))
  (let ((buf (__inner-array cont)))
	(make-instance 'array_reverse_iterator :buffer buf :index -1)))

#-cl-stl-0x98
(defmethod cbegin ((cont stl:array))
  (make-instance 'array_const_iterator
				 :buffer (__inner-array cont) :index 0))

#-cl-stl-0x98
(defmethod cend ((cont stl:array))
  (let ((buf (__inner-array cont)))
	(make-instance 'array_const_iterator
				   :buffer buf :index (length buf))))

#-cl-stl-0x98
(defmethod crbegin ((cont stl:array))
  (let ((buf (__inner-array cont)))
	(make-instance 'array_const_reverse_iterator
				   :buffer buf :index (1- (length buf)))))

#-cl-stl-0x98
(defmethod crend ((cont stl:array))
  (let ((buf (__inner-array cont)))
	(make-instance 'array_const_reverse_iterator
				   :buffer buf :index -1)))

;-----------------------------------------------------
; capacity
;-----------------------------------------------------
#-cl-stl-0x98
(defmethod empty ((cont stl:array))
  (zerop (length (__inner-array cont))))

#-cl-stl-0x98
(defmethod size ((cont stl:array))
  (length (__inner-array cont)))

#-cl-stl-0x98
(defmethod max_size ((cont stl:array))
  most-positive-fixnum)

;-----------------------------------------------------
; element access
;-----------------------------------------------------
#-cl-stl-0x98
(defmethod front ((cont stl:array))
  (let ((buf (__inner-array cont)))
	(__array-error-when-empty buf "front")
	(svref buf 0)))

#-cl-stl-0x98
(defmethod (setf front) (val (cont stl:array))
  (let ((buf (__inner-array cont)))
	(__array-error-when-empty buf "front")
	(_= (svref buf 0) val)))

#-cl-stl-0x98
(defmethod back ((cont stl:array))
  (let ((buf (__inner-array cont)))
	(__array-error-when-empty buf "back")
	(svref buf (1- (length buf)))))

#-cl-stl-0x98
(defmethod (setf back) (val (cont stl:array))
  (let ((buf (__inner-array cont)))
	(__array-error-when-empty buf "back")
	(_= (svref buf (1- (length buf))) val)))

#-cl-stl-0x98
(defmethod at ((cont stl:array) (idx integer))
  (let ((buf (__inner-array cont)))
	(__array-check-index buf idx)
	(svref buf idx)))

#-cl-stl-0x98
(defmethod (setf at) (val (cont stl:array) (idx integer))
  (let ((buf (__inner-array cont)))
	(__array-check-index buf idx)
	(_= (svref buf idx) val)))

#-cl-stl-0x98
(defmethod operator_[] ((cont stl:array) (idx integer))
  (svref (__inner-array cont) idx))

#-cl-stl-0x98
(defmethod (setf operator_[]) (val (cont stl:array) (idx integer))
  (_= (svref (__inner-array cont) idx) val))

#-cl-stl-0x98
(defmethod operator_& ((cont stl:array) (idx integer))
  (let* ((buf (__inner-array cont))
		 (cnt (length buf)))
	(if (zerop cnt)
		(error 'undefined-behavior :what "operator_& for empty array.")
		(if (or (< idx 0) (< cnt idx))
			(error 'out_of_range :what (format nil "index ~A is out of range." idx))
			(make-instance 'vector-pointer :buffer buf :index idx)))))
  
#-cl-stl-0x98
(defmethod operator_const& ((cont stl:array) (idx integer))
  (let* ((buf (__inner-array cont))
		 (cnt (length buf)))
	(if (zerop cnt)
		(error 'undefined-behavior :what "operator_& for empty array.")
		(if (or (< idx 0) (< cnt idx))
			(error 'out_of_range :what (format nil "index ~A is out of range." idx))
			(make-instance 'const-vector-pointer :buffer buf :index idx)))))

#-cl-stl-0x98
(defmethod data ((container stl:array))
  (__inner-array container))

;-----------------------------------------------------
; modifiers
;-----------------------------------------------------
#-cl-stl-0x98
(defmethod-overload swap ((cont1 stl:array) (cont2 stl:array))
  (let ((buf1 (__inner-array cont1))
		(buf2 (__inner-array cont2)))
	(unless (= (length buf1) (length buf2))
	  (error 'type-mismatch :what "Type mismatch in swap of array."))
	(setf (__inner-array cont1) buf2)
	(setf (__inner-array cont2) buf1))
  (values cont1 cont2))

;-----------------------------------------------------
; specific operations
;-----------------------------------------------------
#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod-overload fill ((container stl:array) value)
	(let* ((buf (__inner-array container))
		   (idx 0)
		   (cnt (length buf)))
	  (declare (type fixnum idx cnt))
	  (declare (type simple-vector buf))
	  (do ()
		  ((= idx cnt) nil)
		(_= (svref buf idx) value)
		(incf idx)))))

;-----------------------------------------------------
; compare
;-----------------------------------------------------
#-cl-stl-0x98
(locally (declare (optimize speed))
  (labels ((__container-equal (cont1 cont2)
			 (if (eq cont1 cont2)
				 t
				 (let* ((buf1 (__inner-array cont1))
						(buf2 (__inner-array cont2))
						(cnt1 (if (null buf1) 0 (length buf1)))
						(cnt2 (if (null buf2) 0 (length buf2))))
				   (declare (type simple-vector buf1 buf2))
				   (declare (type fixnum cnt1 cnt2))
				   (if (/= cnt1 cnt2)
					   (error 'type-mismatch :what "Type mismatch in compare of array.")
					   (do ((idx 0))
						   ((= idx cnt1) t)
						 (declare (type fixnum idx))
						 (unless (_== (svref buf1 idx) (svref buf2 idx))
						   (return-from __container-equal nil))
						 (incf idx)))))))

	(defmethod operator_== ((cont1 stl:array) (cont2 stl:array))
	  (__container-equal cont1 cont2))

	(defmethod operator_/= ((cont1 stl:array) (cont2 stl:array))
	  (not (__container-equal cont1 cont2)))))



#-cl-stl-0x98
(locally (declare (optimize speed))
  (labels ((__container-compare (cont1 cont2)
			 (if (eq cont1 cont2)
				 0
				 (let* ((buf1 (__inner-array cont1))
						(buf2 (__inner-array cont2))
						(cnt1 (if (null buf1) 0 (length buf1)))
						(cnt2 (if (null buf2) 0 (length buf2))))
				   (declare (type simple-vector buf1 buf2))
				   (declare (type fixnum cnt1 cnt2))
				   (if (/= cnt1 cnt2)
					   (error 'type-mismatch :what "Type mismatch in compare of array.")
					   (do ((idx 0 (incf idx)))
						   ((= idx cnt1) 0)
						 (declare (type fixnum idx))
						 (if (_< (svref buf1 idx) (svref buf2 idx))
							 (return-from __container-compare -1)
							 (when (_< (svref buf2 idx) (svref buf1 idx))
							   (return-from __container-compare 1)))))))))

	(defmethod operator_< ((cont1 stl:array) (cont2 stl:array))
	  (< (__container-compare cont1 cont2) 0))

	(defmethod operator_<= ((cont1 stl:array) (cont2 stl:array))
	  (<= (__container-compare cont1 cont2) 0))

	(defmethod operator_> ((cont1 stl:array) (cont2 stl:array))
	  (< 0 (__container-compare cont1 cont2)))

	(defmethod operator_>= ((cont1 stl:array) (cont2 stl:array))
	  (<= 0 (__container-compare cont1 cont2)))))


;-----------------------------------------------------
; enumeration
;-----------------------------------------------------
#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod-overload for ((cont stl:array) func)
	;MEMO : func is always lambda function ( see stl:for ). 
	(declare (type cl:function func))
	(let ((buf (__inner-array cont)))
	  (when buf
		(locally (declare (type simple-vector buf))
		  (do ((idx 0 (incf idx))
			   (cnt (length buf)))
			  ((= idx cnt) nil)
			(declare (type fixnum idx cnt))
			(funcall func (svref buf idx))))))))


;;------------------------------------------------------------------------------
;;
;; methods for array_const_iterator
;;
;;------------------------------------------------------------------------------
#-cl-stl-0x98
(defmethod operator_= ((itr1 array_const_iterator) (itr2 array_const_iterator))
  (__error-when-const-removing-assign itr1 array_iterator
									  itr2 array_const_iterator)
  (setf (opr::vec-ptr-buffer itr1) (opr::vec-ptr-buffer itr2))
  (setf (opr::vec-ptr-index  itr1) (opr::vec-ptr-index  itr2))
  itr1)

#-cl-stl-0x98
(defmethod operator_clone ((itr array_const_iterator))
  (make-instance 'array_const_iterator
				 :buffer (opr::vec-ptr-buffer itr)
				 :index  (opr::vec-ptr-index  itr)))

#-cl-stl-0x98
(defmethod operator_+ ((itr array_const_iterator) (n integer))
  (make-instance 'array_const_iterator
				 :buffer (opr::vec-ptr-buffer itr)
				 :index  (+ n (opr::vec-ptr-index itr))))

#-cl-stl-0x98
(defmethod operator_- ((itr array_const_iterator) (n integer))
  (make-instance 'array_const_iterator
				 :buffer (opr::vec-ptr-buffer itr)
				 :index  (- (opr::vec-ptr-index itr) n)))

;; creating reverse iterator.
#-cl-stl-0x98
(define-constructor reverse_iterator ((itr array_const_iterator))
  (make-instance 'array_const_reverse_iterator
				 :buffer (opr::vec-ptr-buffer itr)
				 :index  (1- (opr::vec-ptr-index  itr))))



;;------------------------------------------------------------------------------
;;
;; methods for array_iterator
;;
;;------------------------------------------------------------------------------
#-cl-stl-0x98
(defmethod operator_clone ((itr array_iterator))
  (make-instance 'array_iterator
				 :buffer (opr::vec-ptr-buffer itr)
				 :index  (opr::vec-ptr-index  itr)))

#-cl-stl-0x98
(defmethod operator_cast ((itr array_iterator)
						  (typename (eql 'array_const_iterator)))
  (__check-exact-type-of-cast itr 'array_iterator 'array_const_iterator)
  (make-instance 'array_const_iterator
				 :buffer (opr::vec-ptr-buffer itr)
				 :index  (opr::vec-ptr-index  itr)))

#-cl-stl-0x98
(defmethod operator_+ ((itr array_iterator) (n integer))
  (make-instance 'array_iterator
				 :buffer (opr::vec-ptr-buffer itr)
				 :index  (+ n (opr::vec-ptr-index itr))))

#-cl-stl-0x98
(defmethod operator_- ((itr array_iterator) (n integer))
  (make-instance 'array_iterator
				 :buffer (opr::vec-ptr-buffer itr)
				 :index  (- (opr::vec-ptr-index itr) n)))

;; creating reverse iterator.
#-cl-stl-0x98
(define-constructor reverse_iterator ((itr array_iterator))
  (make-instance 'array_reverse_iterator
				 :buffer (opr::vec-ptr-buffer itr)
				 :index  (1- (opr::vec-ptr-index  itr))))


;;------------------------------------------------------------------------------
;;
;; methods for array_const_reverse_iterator
;;
;;------------------------------------------------------------------------------
#-cl-stl-0x98
(defmethod operator_= ((itr1 array_const_reverse_iterator)
					  (itr2 array_const_reverse_iterator))
  (__error-when-const-removing-assign itr1 array_reverse_iterator
									  itr2 array_const_reverse_iterator)
  (setf (opr::rev-vec-ptr-buffer itr1) (opr::rev-vec-ptr-buffer itr2))
  (setf (opr::rev-vec-ptr-index  itr1) (opr::rev-vec-ptr-index  itr2))
  itr1)

#-cl-stl-0x98
(defmethod operator_clone ((itr array_const_reverse_iterator))
  (make-instance 'array_const_reverse_iterator
				 :buffer (opr::rev-vec-ptr-buffer itr)
				 :index  (opr::rev-vec-ptr-index  itr)))

#-cl-stl-0x98
(defmethod operator_+ ((itr array_const_reverse_iterator) (n integer))
  (make-instance 'array_const_reverse_iterator
				 :buffer (opr::rev-vec-ptr-buffer itr)
				 :index  (- (opr::rev-vec-ptr-index itr) n)))

#-cl-stl-0x98
(defmethod operator_- ((itr array_const_reverse_iterator) (n integer))
  (make-instance 'array_const_reverse_iterator
				 :buffer (opr::rev-vec-ptr-buffer itr)
				 :index  (+ (opr::rev-vec-ptr-index itr) n)))

#-cl-stl-0x98
(defmethod base ((rev-itr array_const_reverse_iterator))
  (make-instance 'array_const_iterator
				 :buffer (opr::rev-vec-ptr-buffer rev-itr)
				 :index  (1+ (opr::rev-vec-ptr-index rev-itr))))

;; creating reverse iterator.
#-cl-stl-0x98
(define-constructor reverse_iterator ((itr array_const_reverse_iterator))
  (make-instance 'array_const_iterator
				 :buffer (opr::rev-vec-ptr-buffer itr)
				 :index  (1+ (opr::rev-vec-ptr-index  itr))))


;;------------------------------------------------------------------------------
;;
;; methods for array_reverse_iterator
;;
;;------------------------------------------------------------------------------
#-cl-stl-0x98
(defmethod operator_clone ((itr array_reverse_iterator))
  (make-instance 'array_reverse_iterator
				 :buffer (opr::rev-vec-ptr-buffer itr)
				 :index  (opr::rev-vec-ptr-index  itr)))

#-cl-stl-0x98
(defmethod operator_cast ((itr array_reverse_iterator)
						  (typename (eql 'array_const_reverse_iterator)))
  (__check-exact-type-of-cast itr 'array_reverse_iterator
								  'array_const_reverse_iterator)
  (make-instance 'array_const_reverse_iterator
				 :buffer (opr::rev-vec-ptr-buffer itr)
				 :index  (opr::rev-vec-ptr-index  itr)))

#-cl-stl-0x98
(defmethod operator_+ ((itr array_reverse_iterator) (n integer))
  (make-instance 'array_reverse_iterator
				 :buffer (opr::rev-vec-ptr-buffer itr)
				 :index  (+ n (opr::rev-vec-ptr-index itr))))

#-cl-stl-0x98
(defmethod operator_- ((itr array_reverse_iterator) (n integer))
  (make-instance 'array_reverse_iterator
				 :buffer (opr::rev-vec-ptr-buffer itr)
				 :index  (- (opr::rev-vec-ptr-index itr) n)))

#-cl-stl-0x98
(defmethod base ((rev-itr array_reverse_iterator))
  (make-instance 'array_iterator
				 :buffer (opr::rev-vec-ptr-buffer rev-itr)
				 :index  (1+ (opr::rev-vec-ptr-index rev-itr))))

;; creating reverse iterator.
#-cl-stl-0x98
(define-constructor reverse_iterator ((itr array_reverse_iterator))
  (make-instance 'array_iterator
				 :buffer (opr::rev-vec-ptr-buffer itr)
				 :index  (1+ (opr::rev-vec-ptr-index  itr))))





;;------------------------------------------------------------------------------
;;
;; debug methods for stl:array
;;
;;------------------------------------------------------------------------------
#+cl-stl-debug
(progn
  #-cl-stl-0x98
  (defmethod dump ((container stl:array) &optional (stream t) (print-item-fnc nil))
	(format stream "begin dump ---------------------~%")
	(let ((buf (__inner-array container)))
	  (when buf
		(setf print-item-fnc (if print-item-fnc
								 (functor_function (clone print-item-fnc))
								 (lambda (s x) (format s "~A" x))))
		(do ((idx 0)
			 (cnt (size container)))
			((= idx cnt) nil)
		  (format stream "~A : " idx)
		  (funcall print-item-fnc stream (svref buf idx))
		  (format stream "~%")
		  (incf idx))))
	(format stream "end dump -----------------------~%")
	nil))

#+cl-stl-debug
(progn
  #-cl-stl-0x98
  (defmethod check_integrity ((container stl:array) &optional (stream t))
	(declare (ignorable container stream))
	;; intentionally do nothing...
	t))
