(in-package :cl-stl)


#-cl-stl-0x98
(locally (declare (optimize speed))
  (defun __make_tuple-from-list (lst)
	(declare (type cl::list lst))
	(let* ((cnt (length lst))
		   (arr (make-array cnt :initial-element nil)))
	  (declare (type fixnum cnt))
	  (declare (type simple-vector arr))
	  (let ((i 0))
		(declare (type fixnum i))
		(for (nil (< i cnt) (incf i) :returns (make-instance 'tuple :items arr))
		  (_= (aref arr i) (car lst))
		  (setf lst (cdr lst)))))))

#-cl-stl-0x98
(defmacro __make_tuple-from-args ((&rest args) &key (is-move nil))
  (let* ((cnt   (length args))
		 (g-arr (gensym "ARR"))
		 (assigns (do ((acc nil)
					   (idx 0 (1+ idx)))
					  ((null args) (nreverse acc))
					(if (not is-move)
						(cl:push `(_= (aref ,g-arr ,idx) ,(car args)) acc)
						(progn
						  (cl:push `(setf (aref ,g-arr ,idx) ,(car args)) acc)
						  (cl:push `(setf ,(car args) nil)                 acc)))
					(setf args (cdr args)))))
	`(locally (declare (optimize speed))
	   (let ((,g-arr (make-array ,cnt :initial-element nil)))
		 (declare (type simple-vector ,g-arr))
		 ,@assigns
		 (make-instance 'tuple :items ,g-arr)))))

;;------------------------------------------------------------------------------
;;
;; tuple
;;
;;------------------------------------------------------------------------------
#-cl-stl-0x98
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass tuple (clonable)
	((items :type     cl:vector
			:initform nil
			:initarg  :items
			:accessor __tuple-items))))


#-cl-stl-0x98
(defun make_tuple (&rest args)
  (__make_tuple-from-list args))

#-cl-stl-0x98
(define-compiler-macro make_tuple (&rest args)
  (let ((g-arr (gensym "ARR")))
	`(locally (declare (optimize speed))
	   (let ((,g-arr (make-array ,(length args) :initial-element nil)))
		 (declare (type cl:vector ,g-arr))
		 ,@(let ((idx 0))
			(mapcar (lambda (arg)
					  (prog1 `(_= (aref ,g-arr ,idx) ,arg)
						(incf idx))) args))
		 (make-instance 'tuple :items ,g-arr)))))


;;----------------------------------------------------------
;; constructors
;;----------------------------------------------------------
#-cl-stl-0x98
(declare-constructor tuple (0 1 0+))

;;default constructor
#-cl-stl-0x98
(define-constructor tuple ()
  (make-instance 'tuple :items (make-array 0)))

;;copy constructor
#-cl-stl-0x98
(define-constructor tuple ((arg tuple))
  (clone arg))

;;conversion from pair
#-cl-stl-0x98
(define-constructor tuple ((arg pair))
  (__make_tuple-from-args ((first arg) (second arg))))

;; move constructor & move from pair
#-cl-stl-0x98
(define-constructor tuple ((arg& remove-reference))
  (with-reference (arg)
	(let ((cont arg))
	  (typecase cont
		(stl:pair
		 (__make_tuple-from-args ((first cont) (second cont)) :is-move t))
		(stl:tuple
		 (let ((arr (__tuple-items cont)))
		   (declare (type simple-vector arr))
		   (prog1 (make-instance 'tuple :items arr)
			 (setf (__tuple-items cont)
				   (make-array (length arr) :initial-element nil)))))
		(t
		 (error 'type-mismatch :what "Can't convert to tuple."))))))
  
;;initialzation
#-cl-stl-0x98
(progn
  (define-constructor tuple (arg)
	(__make_tuple-from-args (arg)))

  (define-constructor tuple (&rest args)
	(__make_tuple-from-list args)))


#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod operator_clone ((obj tuple))
	(let* ((src (__tuple-items obj))
		   (cnt (length src))
		   (dst (make-array cnt :initial-element nil)))
	  (declare (type fixnum cnt))
	  (declare (type simple-vector src dst))
	  (let ((idx 0))
		(declare (type fixnum idx))
		(for (nil (< idx cnt) (incf idx) :returns (make-instance 'tuple :items dst))
		  (_= (aref dst idx) (aref src idx)))))))


;;------------------------------------------------------------------------------
;;
;; methods
;;
;;------------------------------------------------------------------------------
#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod-overload swap ((tpl1 tuple) (tpl2 tuple))
	(let ((arr1 (__tuple-items tpl1))
		  (arr2 (__tuple-items tpl2)))
	  (declare (type simple-vector arr1 arr2))
	  (let ((len1 (length arr1))
			(len2 (length arr2)))
		(declare (type fixnum len1 len2))
		(unless (= len1 len2)
		  (error 'type-mismatch :what "Type mismatch in swap of tuple."))
		(setf (__tuple-items tpl1) arr2)
		(setf (__tuple-items tpl2) arr1))
	  (values tpl1 tpl2))))


#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod operator_= ((tpl1 tuple) (tpl2 tuple))
	(let ((arr1 (__tuple-items tpl1))
		  (arr2 (__tuple-items tpl2)))
	  (declare (type simple-vector arr1 arr2))
	  (let ((len1 (length arr1))
			(len2 (length arr2)))
		(declare (type fixnum len1 len2))
		(when (/= len1 len2)
		  (error 'type-mismatch :what "Type mismatch in assignment of tuple."))
		(let ((idx 0))
		  (declare (type fixnum idx))
		  (for (nil (< idx len1) (incf idx) :returns tpl1)
			(_= (aref arr1 idx) (aref arr2 idx))))))))

;; convertion from pair
#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod operator_= ((tpl tuple) (pr pair))
	(let ((arr (__tuple-items tpl)))
	  (declare (type simple-vector arr))
	  (unless (= 2 (length arr))
		(error 'type-mismatch :what "Type mismatch in assignment of tuple."))
	  (_= (aref arr 0) (stl:first  pr))
	  (_= (aref arr 1) (stl:second pr))
	  tpl)))


#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod operator_move ((tpl1 tuple) (tpl2 tuple))
	(if (eq tpl1 tpl2)
		(values tpl1 tpl2)
		(let ((arr1 (__tuple-items tpl1))
			  (arr2 (__tuple-items tpl2)))
		  (declare (type simple-vector arr1 arr2))
		  (let ((len1 (length arr1))
				(len2 (length arr2)))
			(declare (type fixnum len1 len2))
			(when (/= len1 len2)
			  (error 'type-mismatch :what "Type mismatch in move of tuple."))
			(setf (__tuple-items tpl1) arr2)
			(setf (__tuple-items tpl2) arr1)
			(let ((idx 0))
			  (declare (type fixnum idx))
			  (for (nil (< idx len1) (incf idx) :returns (values tpl1 tpl2))
				(setf (aref arr1 idx) nil))))))))
	  
;; convertion from pair
#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod operator_move ((tpl tuple) (pr pair))
	(let ((arr (__tuple-items tpl)))
	  (declare (type simple-vector arr))
	  (unless (= 2 (length arr))
		(error 'type-mismatch :what "Type mismatch in move pair to tuple."))
	  (multiple-value-bind (a b)
		  (operator_move (aref arr 0) (stl:first pr))
		(setf (aref arr 0)   a)
		(setf (stl:first pr) b))
	  (multiple-value-bind (a b)
		  (operator_move (aref arr 1) (stl:second pr))
		(setf (aref arr 1)    a)
		(setf (stl:second pr) b)))
	(values tpl pr)))


#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod size ((tpl tuple))
	(length (the simple-vector (__tuple-items tpl)))))

#-cl-stl-0x98
(locally (declare (optimize speed))
  (labels ((__get-imp (idx obj)
			 (declare (type fixnum idx))
			 (declare (type tuple  obj))
			 (let ((arr (__tuple-items obj)))
			   (declare (type simple-vector arr))
			   (unless (and (<= 0 idx) (< idx (length arr)))
				 (error 'out_of_range :what "Index specified to get is out of range."))
			   (aref arr idx))))
	(declare (inline __get-imp))
	(defmethod-overload get ((idx integer) (obj tuple))
	  (__get-imp idx obj))
	(defmethod __tie-get ((idx integer) (obj tuple))
	  (__get-imp idx obj))))

#-cl-stl-0x98
(locally (declare (optimize speed))
  (labels ((__setf-get-imp (new-val idx obj)
			 (declare (type fixnum idx))
			 (declare (type tuple  obj))
			 (let ((arr (__tuple-items obj)))
			   (declare (type simple-vector arr))
			   (unless (and (<= 0 idx) (< idx (length arr)))
				 (error 'out_of_range :what "Index specified to get is out of range."))
			   (_= (aref arr idx) new-val)
			   new-val)))
	(declare (inline __setf-get-imp))
	(defmethod-overload (setf get) (new-val (idx integer) (obj tuple))
	  (__setf-get-imp new-val idx obj))
	#-cl-stl-noextra
	(defmethod (setf __tie-get) (new-val (idx integer) (obj tuple))
	  (__setf-get-imp new-val idx obj))))

; concatenate tuples...
#-cl-stl-0x98
(locally (declare (optimize speed))
  ;; MEMO : zero argument count is OK...
  (defun tuple_cat (&rest args)
	(labels ((count-imp (lst acc)
			   (declare (type cl::list lst))
			   (declare (type fixnum   acc))
			   (if (null lst)
				   acc
				   (count-imp (cdr lst)
							  (+ acc (the fixnum (etypecase (car lst)
												   (stl:pair  2)
												   (stl:tuple (size (car lst)))
												   (stl:array (size (car lst)))))))))
			 (copy-imp (lst arr idx)
			   (declare (type cl:list       lst))
			   (declare (type simple-vector arr))
			   (declare (type fixnum        idx))
			   (if (null lst)
				   (make-instance 'tuple :items arr)
				   (let ((obj (car lst)))
					 (typecase obj
					   (cons
						(_= (aref arr idx) (car obj)) (incf idx)
						(_= (aref arr idx) (cdr obj)) (incf idx))
					   (tuple
						(for (v (__tuple-items obj))
						  (_= (aref arr idx) v)
						  (incf idx)))
					   (stl:array
						(for (v obj)
						  (_= (aref arr idx) v)
						  (incf idx))))
					 (copy-imp (cdr lst) arr idx)))))
	  (copy-imp args (make-array (count-imp args 0) :initial-element nil) 0))))


; :ignore keyword can use.
#-cl-stl-0x98
(defmacro tie ((&rest vars) tpl)
  (let ((g-tpl (gensym "TPL")))
	(labels ((imp (idx lst acc)
			   (if (null lst)
				   (nreverse acc)
				   (progn
					 (unless (eq (car lst) :ignore)
					   (cl:push `(_= ,(car lst) (__tie-get ,idx ,g-tpl)) acc))
					 (imp (1+ idx) (cdr lst) acc)))))
	  `(let ((,g-tpl ,tpl))
		 (declare (ignorable ,g-tpl))
		 ,@(imp 0 vars nil)
		 nil))))

;; :ignore keyword can use.
#-cl-stl-noextra
(progn
  #-cl-stl-0x98
  (defmacro with_tie ((&rest vars) tpl &body body)
	(let ((g-tpl (gensym "TPL")))
	  (labels ((imp (lst idx acc)
				 (if (null lst)
					 (nreverse acc)
					 (progn
					   (unless (eq (car lst) :ignore)
						 (cl:push `(,(car lst) (__tie-get ,idx ,g-tpl)) acc))
					   (imp (cdr lst) (1+ idx) acc)))))
		`(let ((,g-tpl ,tpl))
		   (symbol-macrolet
			   (,@(imp vars 0 nil))
			 ,@body))))))


#-cl-stl-0x98
(locally (declare (optimize speed))
  (labels ((tuple-equal (arr1 arr2)
			 (declare (type simple-vector arr1 arr2))
			 (let ((cnt (length arr1)))
			   (declare (type fixnum cnt))
			   (unless (= cnt (length arr2))
				 (error 'type-mismatch :what "Type mismatch in compare of tuple."))
			   (let ((idx 0))
				 (declare (type fixnum idx))
				 (for (nil (< idx cnt) (incf idx) :returns t)
				   (unless (_== (aref arr1 idx) (aref arr2 idx))
					 (return-from tuple-equal nil)))))))

	(defmethod operator_== ((tpl1 tuple) (tpl2 tuple))
	  (if (eq tpl1 tpl2)
		  t
		  (tuple-equal (__tuple-items tpl1) (__tuple-items tpl2))))

	(defmethod operator_/= ((tpl1 tuple) (tpl2 tuple))
	  (if (eq tpl1 tpl2)
		  nil
		  (not (tuple-equal (__tuple-items tpl1) (__tuple-items tpl2)))))))



#-cl-stl-0x98
(locally (declare (optimize speed))
  (labels ((tuple-compare (arr1 arr2) ; returns -1, 0, 1
			 (declare (type simple-vector arr1 arr2))
			 (let ((cnt (length arr1)))
			   (declare (type fixnum cnt))
			   (unless (= cnt (length arr2))
				 (error 'type-mismatch :what "Type mismatch in compare of tuple."))
			   (let ((idx 0))
				 (declare (type fixnum idx))
				 (for (nil (< idx cnt) (incf idx) :returns 0)
				   (let ((val1 (aref arr1 idx))
						 (val2 (aref arr2 idx)))
					 (when (_< val1 val2) (return-from tuple-compare -1))
					 (when (_< val2 val1) (return-from tuple-compare 1))))))))

	(defmethod operator_< ((tpl1 tuple) (tpl2 tuple))
	  (if (eq tpl1 tpl2)
		  nil
		  (< (tuple-compare (__tuple-items tpl1) (__tuple-items tpl2)) 0)))

	(defmethod operator_<= ((tpl1 tuple) (tpl2 tuple))
	  (if (eq tpl1 tpl2)
		  t
		  (<= (tuple-compare (__tuple-items tpl1) (__tuple-items tpl2)) 0)))

	(defmethod operator_> ((tpl1 tuple) (tpl2 tuple))
	  (if (eq tpl1 tpl2)
		  nil
		  (< 0 (tuple-compare (__tuple-items tpl1) (__tuple-items tpl2)))))

	(defmethod operator_>= ((tpl1 tuple) (tpl2 tuple))
	  (if (eq tpl1 tpl2)
		  t
		  (<= 0 (tuple-compare (__tuple-items tpl1) (__tuple-items tpl2)))))))



;;------------------------------------------------------------------------------
;;
;; debug methods for tuple
;;
;;------------------------------------------------------------------------------

#+cl-stl-debug
(progn
  #-cl-stl-0x98
  (defmethod dump ((tpl tuple) &optional (stream t) (print-item-fnc nil))
	(setf print-item-fnc (if print-item-fnc
							 (functor_function (clone print-item-fnc))
							 (lambda (s x) (format s "~A" x))))
	(format stream "begin dump ---------------------~%")
	(let ((arr (__tuple-items tpl)))
	  (declare (type simple-vector arr))
	  (do ((cnt (length arr))
		   (idx 0 (1+ idx)))
		  ((= idx cnt) nil)
		(format stream "~A : " idx)
		(funcall print-item-fnc stream (aref arr idx))
		(format stream "~%")))
	(format stream "end dump -----------------------~%")
	nil))


