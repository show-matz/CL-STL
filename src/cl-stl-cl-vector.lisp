
(in-package :cl-stl)

(defmacro __vector-pointer-check-index (buf-sym idx-sym)
  (check-type buf-sym symbol)
  (check-type idx-sym symbol)
  `(unless (and (<= 0 ,idx-sym)
				(<    ,idx-sym (if (null ,buf-sym)
								   0
								   (length ,buf-sym))))
	 (error 'out-of-range :what ,(format nil "index ~A is out of range." idx-sym))))

(defmacro __pointer-check-iterator-range (itr1 itr2)
  (declare (ignorable itr1 itr2))
  #-cl-stl-debug nil
  #+cl-stl-debug (check-type itr1 symbol)
  #+cl-stl-debug (check-type itr2 symbol)
  #+cl-stl-debug
  `(unless (and (eq (opr::vec-ptr-buffer ,itr1) (opr::vec-ptr-buffer ,itr2))
				(<= (opr::vec-ptr-index  ,itr1) (opr::vec-ptr-index  ,itr2)))
	 (error 'undefined-behavior :what ,(format nil "[~A ~A) isn't legal sequence." itr1 itr2))))



;;------------------------------------------------------------------------------
;;
;; implementation of const-vector-pointer
;;
;;------------------------------------------------------------------------------
(defmethod advance ((itr const-vector-pointer) (n integer))
  (incf (opr::vec-ptr-index itr) n)
  nil)

(defmethod distance ((itr1 const-vector-pointer) (itr2 const-vector-pointer))
  (- (opr::vec-ptr-index itr2) (opr::vec-ptr-index itr1)))

;; creating reverse iterator.
(define-constructor reverse-iterator ((itr const-vector-pointer))
  (make-instance 'const-reverse-vector-pointer
				 :buffer (opr::vec-ptr-buffer itr)
				 :index  (1- (opr::vec-ptr-index  itr))))


;;------------------------------------------------------------------------------
;;
;; implementation of vector-pointer
;;
;;------------------------------------------------------------------------------
;; creating reverse iterator.
(define-constructor reverse-iterator ((itr vector-pointer))
  (make-instance 'reverse-vector-pointer
				 :buffer (opr::vec-ptr-buffer itr)
				 :index  (1- (opr::vec-ptr-index  itr))))


;;------------------------------------------------------------------------------
;;
;; implementation for const-reverse-vector-pointer
;;
;;------------------------------------------------------------------------------
(defmethod advance ((itr const-reverse-vector-pointer) (n integer))
  (decf (opr::rev-vec-ptr-index itr) n)
  nil)

(defmethod distance ((itr1 const-reverse-vector-pointer)
					 (itr2 const-reverse-vector-pointer))
  (* -1 (- (opr::rev-vec-ptr-index itr2) (opr::rev-vec-ptr-index itr1))))

(defmethod base ((rev-itr const-reverse-vector-pointer))
  (make-instance 'const-vector-pointer
				 :buffer (opr::rev-vec-ptr-buffer rev-itr)
				 :index  (1+ (opr::rev-vec-ptr-index rev-itr))))

;; creating reverse iterator.
(define-constructor reverse-iterator ((itr const-reverse-vector-pointer))
  (make-instance 'const-vector-pointer
				 :buffer (opr::rev-vec-ptr-buffer itr)
				 :index  (1+ (opr::rev-vec-ptr-index  itr))))


;;------------------------------------------------------------------------------
;;
;; implementation for reverse-vector-pointer
;;
;;------------------------------------------------------------------------------
(defmethod base ((rev-itr reverse-vector-pointer))
  (make-instance 'vector-pointer
				 :buffer (opr::rev-vec-ptr-buffer rev-itr)
				 :index  (1+ (opr::rev-vec-ptr-index rev-itr))))

;; creating reverse iterator.
(define-constructor reverse-iterator ((itr reverse-vector-pointer))
  (make-instance 'vector-pointer
				 :buffer (opr::rev-vec-ptr-buffer itr)
				 :index  (1+ (opr::rev-vec-ptr-index  itr))))



#-cl-stl-0x98 ; cl:vector's begin, end & for.
(progn

  #+cl-stl-extra (defmethod data ((arr cl:vector)) arr)
  #+cl-stl-extra (defmethod size ((arr cl:vector)) (length arr))

  (defmethod begin ((arr cl:vector)) (_& arr 0))
  (defmethod end   ((arr cl:vector)) (_& arr (length arr)))

  #+cl-stl-extra (defmethod cbegin ((arr cl:vector)) (const_& arr 0))
  #+cl-stl-extra (defmethod cend   ((arr cl:vector)) (const_& arr (length arr)))

  #+cl-stl-extra
  (defmethod rbegin ((arr cl:vector))
	(make-instance 'reverse-vector-pointer :buffer arr :index (1- (length arr))))

  #+cl-stl-extra
  (defmethod rend ((arr cl:vector))
	(make-instance 'reverse-vector-pointer :buffer arr :index -1))

  #+cl-stl-extra
  (defmethod crbegin ((arr cl:vector))
	(make-instance 'const-reverse-vector-pointer :buffer arr :index (1- (length arr))))

  #+cl-stl-extra
  (defmethod crend   ((arr cl:vector))
	(make-instance 'const-reverse-vector-pointer :buffer arr :index -1))

  (locally (declare (optimize speed))
	(defmethod-overload for ((cont cl:vector) func)
	  (declare (type cl:vector cont))
	  ;;MEMO : func is always lambda function ( see stl:for ).
	  (let ((idx 0)
			(cnt (length cont)))
		(declare (type fixnum idx cnt))
		(for (nil (< idx cnt) (incf idx))
		  (funcall func (aref cont idx))))))

  (locally (declare (optimize speed))
	(defmethod operator_clone ((obj cl:vector))
	  (declare (type cl:vector obj))
	  (let* ((cnt (length obj))
			 (ret (make-array cnt :initial-element nil)))
		(declare (type fixnum cnt))
		(declare (type cl:vector ret))
		(let ((idx 0))
		  (declare (type fixnum idx))
		  (for (nil (< idx cnt) (incf idx) :returns ret)
			(_= (aref ret idx) (aref obj idx))))))))
