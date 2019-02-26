
(in-package :cl-stl)

;;-------------------------------------------------------------------- 
(defun __numeric_limits-imp (type min-or-max) 
  (ecase min-or-max 
	(:max (ecase type 
			(:fixnum       most-positive-fixnum) 
			(:long-float   most-positive-long-float) 
			(:short-float  most-positive-short-float) 
			(:double-float most-positive-double-float) 
			(:single-float most-positive-single-float))) 
	(:min (ecase type 
			(:fixnum       most-negative-fixnum) 
			(:long-float   most-negative-long-float) 
			(:short-float  most-negative-short-float) 
			(:double-float most-negative-double-float) 
			(:single-float most-negative-single-float))))) 


(defmacro numeric_limits (type min-or-max) 
  (if (and (keywordp type) 
		   (keywordp min-or-max)) 
	  (__numeric_limits-imp type min-or-max) 
	  `(__numeric_limits-imp ,type ,min-or-max))) 


;;------------------------------------------------------------------------------
;;
;; exported functions
;;
;;------------------------------------------------------------------------------

;; 26.4.1 Accumulate
(locally (declare (optimize speed))

  ;;PTN; accumulate : 0 -   i
  (labels ((__accumulate-imp-0 (first last init plus-bf)
			 (declare (type cl:function plus-bf))
			 (with-operators
				 (let ((acc nil))
				   (_= acc init)
				   (if (_== first last)
					   acc
					   (for (((itr @~first)) (_/= itr last) ++itr :returns acc)
							(_= acc (funcall plus-bf acc *itr))))))))

	(defmethod-overload accumulate ((first input_iterator) (last input_iterator) init)
	  (__accumulate-imp-0 first last init #'+))

	(defmethod-overload accumulate ((first input_iterator) (last input_iterator) init binary-op)
	  (__accumulate-imp-0 first last init (functor_function (clone binary-op)))))

  ;;PTN; accumulate : 1 -  cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__accumulate-imp-1 (cons1 cons2 init plus-bf)
			 (declare (type cl:function plus-bf))
			 (let ((acc nil))
			   (_= acc init)
			   (for (nil (not (eq cons1 cons2)) (setf cons1 (cdr cons1)) :returns acc)
				 (_= acc (funcall plus-bf acc (car cons1)))))))

	(defmethod-overload accumulate ((first cons_const_iterator) (last cons_const_iterator) init)
	  ;;(format t "specialized accumulate for cons_const_iterator is invoked.~%")
	  (__accumulate-imp-1 (__cons-itr-cons first)
						  (__cons-itr-cons  last) init #'+))

	(defmethod-overload accumulate ((first cons_const_iterator) (last cons_const_iterator) init binary-op)
	  ;;(format t "specialized accumulate for cons_const_iterator is invoked.~%")
	  (__accumulate-imp-1 (__cons-itr-cons first)
						  (__cons-itr-cons  last) init (functor_function (clone binary-op)))))

  ;;PTN; accumulate : 2 -  cvp
  (labels ((__accumulate-imp-2 (idx1 idx2 buffer init plus-bf)
			 (declare (type fixnum idx1 idx2))
			 (declare (type cl:vector buffer))
			 (declare (type cl:function plus-bf))
			 (let ((acc nil))
			   (_= acc init)
			   (for (nil (< idx1 idx2) (incf idx1) :returns acc)
				 (_= acc (funcall plus-bf acc (aref buffer idx1)))))))

	(defmethod-overload accumulate ((first const-vector-pointer) (last  const-vector-pointer) init)
	  ;;(format t "specialized accumulate for const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__accumulate-imp-2 (opr::vec-ptr-index  first)
						  (opr::vec-ptr-index  last)
						  (opr::vec-ptr-buffer first) init #'+))

	(defmethod-overload accumulate ((first const-vector-pointer) (last  const-vector-pointer) init binary-op)
	  ;;(format t "specialized accumulate for const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__accumulate-imp-2 (opr::vec-ptr-index  first)
						  (opr::vec-ptr-index  last)
						  (opr::vec-ptr-buffer first)
						  init (functor_function (clone binary-op))))))


;;--------------------------------------------------------------------
;; 26.4.2 Inner product
(locally (declare (optimize speed))

  ;;PTN; inner_product : 0 -   i  x  i 
  (labels ((__inner_product-imp-0 (first1 last1 first2 init plus-bf mult-bf)
			 (declare (type cl:function plus-bf mult-bf))
			 (with-operators
				 (let ((acc nil))
				   (_= acc init)
				   (if (_== first1 last1)
					   acc
					   (for (((itr1 @~first1) (itr2 @~first2)) (_/= itr1 last1) (progn ++itr1 ++itr2) :returns acc)
						 (_= acc (funcall plus-bf acc
										  (funcall mult-bf *itr1 *itr2)))))))))

	(defmethod-overload inner_product ((first1 input_iterator)
									   (last1  input_iterator) (first2 input_iterator) init)
	  (__inner_product-imp-0 first1 last1 first2 init #'+ #'*))

	(defmethod-overload inner_product ((first1 input_iterator) (last1  input_iterator)
									   (first2 input_iterator) init binary-op1 binary-op2)
	  (__inner_product-imp-0 first1 last1 first2 init (functor_function (clone binary-op1))
							 (functor_function (clone binary-op2)))))

  
  ;;PTN; inner_product : 1 -  cci x  i 
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__inner_product-imp-1 (cons1 end1 first2 init plus-bf mult-bf)
			 (declare (type cl:function plus-bf mult-bf))
			 (let ((acc nil))
			   (_= acc init)
			   (with-operators
				   (if (eq cons1 end1)
					   acc
					   (for (((itr2 @~first2)) (not (eq cons1 end1)) (progn (setf cons1 (cdr cons1)) ++itr2) :returns acc)
						 (_= acc (funcall plus-bf acc (funcall mult-bf (car cons1) *itr2)))))))))

	(defmethod-overload inner_product ((first1 cons_const_iterator)
									   (last1  cons_const_iterator) (first2 input_iterator) init)
	  ;;(format t "specialized inner_product for cons_const_iterator & input_iterator is invoked.~%")
	  (__inner_product-imp-1 (__cons-itr-cons first1)
							 (__cons-itr-cons  last1) first2 init #'+ #'*))

	(defmethod-overload inner_product ((first1 cons_const_iterator)
									   (last1  cons_const_iterator)
									   (first2 input_iterator) init binary-op1 binary-op2)
	  ;;(format t "specialized inner_product for cons_const_iterator & input_iterator is invoked.~%")
	  (__inner_product-imp-1 (__cons-itr-cons first1)
							 (__cons-itr-cons  last1) first2 init
							 (functor_function (clone binary-op1))
							 (functor_function (clone binary-op2)))))
			 

  ;;PTN; inner_product : 2 -  cvp x  i 
  (labels ((__inner_product-imp-2 (idx1 last1 buffer1 first2 init plus-bf mult-bf)
			 (declare (type fixnum idx1 last1))
			 (declare (type cl:vector buffer1))
			 (declare (type cl:function plus-bf mult-bf))
			 (let ((acc nil))
			   (_= acc init)
			   (with-operators
				   (if (= idx1 last1)
					   acc
					   (for (((itr2 @~first2)) (< idx1 last1) (progn (incf idx1) ++itr2) :returns acc)
						 (_= acc (funcall plus-bf acc
										  (funcall mult-bf (aref buffer1 idx1) *itr2)))))))))

	(defmethod-overload inner_product ((first1 const-vector-pointer)
									   (last1  const-vector-pointer) (first2 input_iterator) init)
	  ;;(format t "specialized inner_product for const-vector-pointer & input_iterator is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__inner_product-imp-2 (opr::vec-ptr-index  first1)
							 (opr::vec-ptr-index  last1)
							 (opr::vec-ptr-buffer first1) first2 init #'+ #'*))

	(defmethod-overload inner_product ((first1 const-vector-pointer)
									   (last1  const-vector-pointer)
									   (first2 input_iterator) init binary-op1 binary-op2)
	  ;;(format t "specialized inner_product for const-vector-pointer & input_iterator is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__inner_product-imp-2 (opr::vec-ptr-index  first1)
							 (opr::vec-ptr-index  last1)
							 (opr::vec-ptr-buffer first1)
							 first2 init
							 (functor_function (clone binary-op1))
							 (functor_function (clone binary-op2)))))

  ;;PTN; inner_product : 3 -   i  x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__inner_product-imp-3 (first1 last1 cons2 init plus-bf mult-bf)
			 (declare (type cl:function plus-bf mult-bf))
			 (let ((acc nil))
			   (_= acc init)
			   (if (_== first1 last1)
				   acc
				   (with-operators
					   (for (((itr1 @~first1)) (_/= itr1 last1)
											   (progn ++itr1 (setf cons2 (cdr cons2))) :returns acc)
						 (_= acc (funcall plus-bf acc
										  (funcall mult-bf *itr1 (car cons2))))))))))

	(defmethod-overload inner_product ((first1 input_iterator)
									   (last1  input_iterator)
									   (first2 cons_const_iterator) init)
	  ;;(format t "specialized inner_product for input_iterator & cons_const_iterator is invoked.~%")
	  (__inner_product-imp-3 first1 last1 (__cons-itr-cons first2) init #'+ #'*))

	(defmethod-overload inner_product ((first1 input_iterator)
									   (last1  input_iterator)
									   (first2 cons_const_iterator) init binary-op1 binary-op2)
	  ;;(format t "specialized inner_product for input_iterator & cons_const_iterator is invoked.~%")
	  (__inner_product-imp-3 first1 last1
							 (__cons-itr-cons first2) init
							 (functor_function (clone binary-op1))
							 (functor_function (clone binary-op2)))))

  ;;PTN; inner_product : 4 -  cci x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__inner_product-imp-4 (cons1 end1 cons2 init plus-bf mult-bf)
			 (declare (type cl:function plus-bf mult-bf))
			 (let ((acc nil))
			   (_= acc init)
			   (if (eq cons1 end1)
				   acc
				   (for (nil (not (eq cons1 end1)) (progn (setf cons1 (cdr cons1))
														  (setf cons2 (cdr cons2))) :returns acc)
					 (_= acc (funcall plus-bf acc (funcall mult-bf (car cons1) (car cons2)))))))))

	(defmethod-overload inner_product ((first1 cons_const_iterator)
									   (last1  cons_const_iterator)
									   (first2 cons_const_iterator) init)
	  ;;(format t "specialized inner_product for cons_const_iterator & cons_const_iterator is invoked.~%")
	  (__inner_product-imp-4 (__cons-itr-cons first1)
							 (__cons-itr-cons  last1)
							 (__cons-itr-cons first2) init #'+ #'*))

	(defmethod-overload inner_product ((first1 cons_const_iterator)
									   (last1  cons_const_iterator)
									   (first2 cons_const_iterator) init binary-op1 binary-op2)
	  ;;(format t "specialized inner_product for cons_const_iterator & cons_const_iterator is invoked.~%")
	  (__inner_product-imp-4 (__cons-itr-cons first1)
							 (__cons-itr-cons  last1)
							 (__cons-itr-cons first2)
							 init
							 (functor_function (clone binary-op1))
							 (functor_function (clone binary-op2)))))

  ;;PTN; inner_product : 5 -  cvp x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__inner_product-imp-5 (idx1 last1 buffer1 cons2 init plus-bf mult-bf)
			 (declare (type fixnum idx1 last1))
			 (declare (type cl:vector buffer1))
			 (declare (type cl:function plus-bf mult-bf))
			 (let ((acc nil))
			   (_= acc init)
			   (if (= idx1 last1)
				   acc
				   (for (nil (< idx1 last1) (progn (incf idx1)
												   (setf cons2 (cdr cons2))) :returns acc)
					 (_= acc (funcall plus-bf acc (funcall mult-bf
														   (aref buffer1 idx1) (car cons2)))))))))

	(defmethod-overload inner_product ((first1 const-vector-pointer)
									   (last1  const-vector-pointer)
									   (first2 cons_const_iterator) init)
	  ;;(format t "specialized inner_product for const-vector-pointer & cons_const_iterator is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__inner_product-imp-5 (opr::vec-ptr-index  first1)
							 (opr::vec-ptr-index   last1)
							 (opr::vec-ptr-buffer first1)
							 (__cons-itr-cons     first2) init #'+ #'*))

	(defmethod-overload inner_product ((first1 const-vector-pointer)
									   (last1  const-vector-pointer)
									   (first2 cons_const_iterator) init binary-op1 binary-op2)
	  ;;(format t "specialized inner_product for const-vector-pointer & cons_const_iterator is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__inner_product-imp-5 (opr::vec-ptr-index  first1)
							 (opr::vec-ptr-index   last1)
							 (opr::vec-ptr-buffer first1)
							 (__cons-itr-cons     first2)
							 init
							 (functor_function (clone binary-op1))
							 (functor_function (clone binary-op2)))))

  ;;PTN; inner_product : 6 -   i  x cvp
  (labels ((__inner_product-imp-6 (first1 last1 idx2 buffer2 init plus-bf mult-bf)
			 (declare (type fixnum idx2))
			 (declare (type cl:vector buffer2))
			 (declare (type cl:function plus-bf mult-bf))
			 (let ((acc nil))
			   (_= acc init)
			   (if (_== first1 last1)
				   acc
				   (with-operators
					   (for (((itr1 @~first1)) (_/= itr1 last1) (progn ++itr1 (incf idx2)) :returns acc)
						 (_= acc (funcall plus-bf acc
										  (funcall mult-bf *itr1 (aref buffer2 idx2))))))))))

	(defmethod-overload inner_product ((first1 input_iterator)
									   (last1  input_iterator)
									   (first2 const-vector-pointer) init)
	  ;;(format t "specialized inner_product for input_iterator & const-vector-pointer is invoked.~%")
	  (__inner_product-imp-6 first1 last1
							 (opr::vec-ptr-index  first2)
							 (opr::vec-ptr-buffer first2) init #'+ #'*))

	(defmethod-overload inner_product ((first1 input_iterator)
									   (last1  input_iterator)
									   (first2 const-vector-pointer) init binary-op1 binary-op2)
	  ;;(format t "specialized inner_product for input_iterator & const-vector-pointer is invoked.~%")
	  (__inner_product-imp-6 first1 last1
							 (opr::vec-ptr-index  first2)
							 (opr::vec-ptr-buffer first2)
							 init
							 (functor_function (clone binary-op1))
							 (functor_function (clone binary-op2)))))

  ;;PTN; inner_product : 7 -  cci x cvp
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__inner_product-imp-7 (cons1 end1 idx2 buffer2 init plus-bf mult-bf)
			 (declare (type fixnum idx2))
			 (declare (type cl:vector buffer2))
			 (declare (type cl:function plus-bf mult-bf))
			 (let ((acc nil))
			   (_= acc init)
			   (if (eq cons1 end1)
				   acc
				   (for (nil (not (eq cons1 end1)) (progn (setf cons1 (cdr cons1)) (incf idx2)) :returns acc)
					 (_= acc (funcall plus-bf acc (funcall mult-bf (car cons1) (aref buffer2 idx2)))))))))

	(defmethod-overload inner_product ((first1 cons_const_iterator)
									   (last1  cons_const_iterator)
									   (first2 const-vector-pointer) init)
	  ;;(format t "specialized inner_product for cons_const_iterator & const-vector-pointer is invoked.~%")
	  (__inner_product-imp-7 (__cons-itr-cons     first1)
							 (__cons-itr-cons      last1)
							 (opr::vec-ptr-index  first2)
							 (opr::vec-ptr-buffer first2) init #'+ #'*))

	(defmethod-overload inner_product ((first1 cons_const_iterator)
									   (last1  cons_const_iterator)
									   (first2 const-vector-pointer) init binary-op1 binary-op2)
	  ;;(format t "specialized inner_product for cons_const_iterator & const-vector-pointer is invoked.~%")
	  (__inner_product-imp-7 (__cons-itr-cons     first1)
							 (__cons-itr-cons      last1)
							 (opr::vec-ptr-index  first2)
							 (opr::vec-ptr-buffer first2)
							 init
							 (functor_function (clone binary-op1))
							 (functor_function (clone binary-op2)))))

  ;;PTN; inner_product : 8 -  cvp x cvp
  (labels ((__inner_product-imp-8 (idx1 last1 buffer1 idx2 buffer2 init plus-bf mult-bf)
			 (declare (type fixnum idx1 last1 idx2))
			 (declare (type cl:vector buffer1 buffer2))
			 (declare (type cl:function plus-bf mult-bf))
			 (let ((acc nil))
			   (_= acc init)
			   (if (= idx1 last1)
				   acc
				   (for (nil (< idx1 last1) (progn (incf idx1) (incf idx2)) :returns acc)
					 (_= acc (funcall plus-bf acc (funcall mult-bf
														   (aref buffer1 idx1)
														   (aref buffer2 idx2)))))))))

	(defmethod-overload inner_product ((first1 const-vector-pointer)
									   (last1  const-vector-pointer)
									   (first2 const-vector-pointer) init)
	  ;;(format t "specialized inner_product for const-vector-pointer & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__inner_product-imp-8 (opr::vec-ptr-index  first1)
							 (opr::vec-ptr-index  last1)
							 (opr::vec-ptr-buffer first1)
							 (opr::vec-ptr-index  first2)
							 (opr::vec-ptr-buffer first2) init #'+ #'*))

	(defmethod-overload inner_product ((first1 const-vector-pointer)
									   (last1  const-vector-pointer)
									   (first2 const-vector-pointer) init binary-op1 binary-op2)
	  ;;(format t "specialized inner_product for const-vector-pointer & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__inner_product-imp-8 (opr::vec-ptr-index  first1)
							 (opr::vec-ptr-index  last1)
							 (opr::vec-ptr-buffer first1)
							 (opr::vec-ptr-index  first2)
							 (opr::vec-ptr-buffer first2)
							 init
							 (functor_function (clone binary-op1))
							 (functor_function (clone binary-op2))))))


;;--------------------------------------------------------------------
;; 26.4.3 Partial sum
(locally (declare (optimize speed))

  ;;PTN; partial_sum : 0 -   i  x  o 
  (labels ((__partial_sum-imp-0 (first last result plus)
			 (declare (type cl:function plus))
			 (with-operators
				 (if (_== first last)
					 result
					 (let ((acc nil))
					   (_= acc *first)
					   (_= *result acc)
					   ++result
					   (for (((itr (next first))) (_/= itr last) (progn ++result ++itr) :returns result)
						 (_= acc (funcall plus acc *itr))
						 (_= *result acc)))))))

	(defmethod-overload partial_sum ((first input_iterator)
									 (last  input_iterator) (result output_iterator))
	  (__partial_sum-imp-0 first last (clone result) #'+))

	(defmethod-overload partial_sum ((first input_iterator)
									 (last  input_iterator) (result output_iterator) binary-op)
	  (__partial_sum-imp-0 first last (clone result) (functor_function (clone binary-op)))))


  ;;PTN; partial_sum : 1 -  cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__partial_sum-imp-1 (cons1 cons2 result plus)
			 (declare (type cl:function plus))
			 (with-operators
				 (if (eq cons1 cons2)
					 result
					 (let ((acc nil))
					   (_= acc (car cons1))
					   (_= *result acc)
					   ++result
					   (setf cons1 (cdr cons1))
					   (for (nil (not (eq cons1 cons2)) (progn ++result (setf cons1 (cdr cons1))) :returns result)
						 (_= acc (funcall plus acc (car cons1)))
						 (_= *result acc)))))))

	(defmethod-overload partial_sum ((first cons_const_iterator)
									 (last  cons_const_iterator) (result output_iterator))
	  ;;(format t "specialized partial_sum for cons_const_iterator & output_iterator is invoked.~%")
	  (__partial_sum-imp-1 (__cons-itr-cons first)
						   (__cons-itr-cons  last) (clone result) #'+))

	(defmethod-overload partial_sum ((first cons_const_iterator)
									 (last  cons_const_iterator) (result output_iterator) binary-op)
	  ;;(format t "specialized partial_sum for cons_const_iterator & output_iterator is invoked.~%")
	  (__partial_sum-imp-1 (__cons-itr-cons first)
						   (__cons-itr-cons  last) (clone result) (functor_function (clone binary-op)))))


  ;;PTN; partial_sum : 2 -  cvp x  o 
  (labels ((__partial_sum-imp-2 (idx1 idx2 buffer oitr plus-bf)
			 (declare (type fixnum idx1 idx2))
			 (declare (type cl:vector buffer))
			 (declare (type cl:function plus-bf))
			 (if (= idx1 idx2)
				 oitr
				 (with-operators
					 (let ((acc nil))
					   (_= acc (aref buffer idx1))
					   (_= *oitr acc)
					   ++oitr
					   (incf idx1)
					   (for (nil (< idx1 idx2) (progn ++oitr (incf idx1)) :returns oitr)
						 (_= acc (funcall plus-bf acc (aref buffer idx1)))
						 (_= *oitr acc)))))))

	(defmethod-overload partial_sum ((first  const-vector-pointer)
									 (last   const-vector-pointer) (result output_iterator))
	  ;;(format t "specialized partial_sum for const-vector-pointer & output_iterator is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__partial_sum-imp-2 (opr::vec-ptr-index  first)
						   (opr::vec-ptr-index  last)
						   (opr::vec-ptr-buffer first) (clone result) #'+))

	(defmethod-overload partial_sum ((first  const-vector-pointer)
									 (last   const-vector-pointer) (result output_iterator) binary-op)
	  ;;(format t "specialized partial_sum for const-vector-pointer & output_iterator is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__partial_sum-imp-2 (opr::vec-ptr-index  first)
						   (opr::vec-ptr-index  last)
						   (opr::vec-ptr-buffer first)
						   (clone result) (functor_function (clone binary-op)))))

  ;;PTN; partial_sum : 3 -   i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__partial_sum-imp-3 (first last out-cons plus-bf)
			 (declare (type cl:function plus-bf))
			 (with-operators
				 (if (_== first last)
					 out-cons
					 (let ((acc nil))
					   (_= acc *first)
					   (_= (car out-cons) acc)
					   (setf out-cons (cdr out-cons))
					   (for (((itr (next first))) (_/= itr last) (progn (setf out-cons (cdr out-cons)) ++itr) :returns out-cons)
						 (_= acc (funcall plus-bf acc *itr))
						 (_= (car out-cons) acc)))))))

	(defmethod-overload partial_sum ((first input_iterator)
									 (last  input_iterator) (result cons_iterator))
	  ;;(format t "specialized partial_sum for input_iterator & cons_iterator is invoked.~%")
	  (__algo-make-cns-iterator result
								 (__partial_sum-imp-3 first last (__cons-itr-cons result) #'+)))

	(defmethod-overload partial_sum ((first input_iterator)
									 (last  input_iterator) (result cons_iterator) binary-op)
	  ;;(format t "specialized partial_sum for input_iterator & cons_iterator is invoked.~%")
	  (__algo-make-cns-iterator result
								 (__partial_sum-imp-3 first last
													  (__cons-itr-cons  result)
													  (functor_function (clone binary-op))))))


  ;;PTN; partial_sum : 4 -  cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__partial_sum-imp-4 (cons1 cons2 out-cons plus-bf)
			 (declare (type cl:function plus-bf))
			 (if (eq cons1 cons2)
				 out-cons
				 (let ((acc nil))
				   (_= acc (car cons1))
				   (_= (car out-cons) acc)
				   (setf out-cons (cdr out-cons))
				   (setf cons1 (cdr cons1))
				   (for (nil (not (eq cons1 cons2)) (progn (setf cons1    (cdr cons1))
														   (setf out-cons (cdr out-cons))) :returns out-cons)
					 (_= acc (funcall plus-bf acc (car cons1)))
					 (_= (car out-cons) acc))))))

	(defmethod-overload partial_sum ((first cons_const_iterator)
									 (last  cons_const_iterator) (result cons_iterator))
	  ;;(format t "specialized partial_sum for cons_const_iterator & cons_iterator is invoked.~%")
	  (__algo-make-cns-iterator result
								 (__partial_sum-imp-4 (__cons-itr-cons  first)
													  (__cons-itr-cons   last)
													  (__cons-itr-cons result) #'+)))

	(defmethod-overload partial_sum ((first cons_const_iterator)
									 (last  cons_const_iterator) (result cons_iterator) binary-op)
	  ;;(format t "specialized partial_sum for cons_const_iterator & cons_iterator is invoked.~%")
	  (__algo-make-cns-iterator result
								 (__partial_sum-imp-4 (__cons-itr-cons  first)
													  (__cons-itr-cons   last)
													  (__cons-itr-cons result) (functor_function (clone binary-op))))))


  ;;PTN; partial_sum : 5 -  cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__partial_sum-imp-5 (idx1 idx2 src-buf out-cons plus-bf)
			 (declare (type fixnum idx1 idx2))
			 (declare (type cl:vector src-buf))
			 (declare (type cl:function plus-bf))
			 (if (= idx1 idx2)
				 out-cons
				 (let ((acc nil))
				   (_= acc (aref src-buf idx1))
				   (_= (car out-cons) acc)
				   (setf out-cons (cdr out-cons))
				   (incf idx1)
				   (for (nil (< idx1 idx2) (progn (incf idx1)
												  (setf out-cons (cdr out-cons))) :returns out-cons)
					 (_= acc (funcall plus-bf acc (aref src-buf idx1)))
					 (_= (car out-cons) acc))))))

	(defmethod-overload partial_sum ((first const-vector-pointer)
									 (last  const-vector-pointer) (result cons_iterator))
	  ;;(format t "specialized partial_sum for const-vector-pointer & cons_iterator is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__algo-make-cns-iterator result
								 (__partial_sum-imp-5 (opr::vec-ptr-index  first)
													  (opr::vec-ptr-index   last)
													  (opr::vec-ptr-buffer first)
													  (__cons-itr-cons    result) #'+)))

	(defmethod-overload partial_sum ((first const-vector-pointer)
									 (last  const-vector-pointer) (result cons_iterator) binary-op)
	  ;;(format t "specialized partial_sum for const-vector-pointer & cons_iterator is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__algo-make-cns-iterator result
								 (__partial_sum-imp-5 (opr::vec-ptr-index  first)
													  (opr::vec-ptr-index   last)
													  (opr::vec-ptr-buffer first)
													  (__cons-itr-cons    result)
													  (functor_function (clone binary-op))))))

  ;;PTN; partial_sum : 6 -   i  x  vp
  (labels ((__partial_sum-imp-6 (first last out-idx out-buf plus-bf)
			 (declare (type fixnum out-idx))
			 (declare (type cl:vector out-buf))
			 (declare (type cl:function plus-bf))
			 (with-operators
				 (if (_== first last)
					 out-idx
					 (let ((acc nil))
					   (_= acc *first)
					   (_= (aref out-buf out-idx) acc)
					   (incf out-idx)
					   (for (((itr (next first))) (_/= itr last) (progn (incf out-idx) ++itr) :returns out-idx)
						 (_= acc (funcall plus-bf acc *itr))
						 (_= (aref out-buf out-idx) acc)))))))

	(defmethod-overload partial_sum ((first  input_iterator)
									 (last   input_iterator) (result vector-pointer))
	  ;;(format t "specialized partial_sum for input_iterator & vector-pointer is invoked.~%")
	  (__algo-make-vct-iterator result
								 (__partial_sum-imp-6 first last
													  (opr::vec-ptr-index  result)
													  (opr::vec-ptr-buffer result) #'+)))

	(defmethod-overload partial_sum ((first  input_iterator)
									 (last   input_iterator) (result vector-pointer) binary-op)
	  ;;(format t "specialized partial_sum for input_iterator & vector-pointer is invoked.~%")
	  (__algo-make-vct-iterator result
								 (__partial_sum-imp-6 first last
													  (opr::vec-ptr-index  result)
													  (opr::vec-ptr-buffer result)
													  (functor_function (clone binary-op))))))


  ;;PTN; partial_sum : 7 -  cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__partial_sum-imp-7 (cons1 cons2 out-idx out-buf plus-bf)
			 (declare (type fixnum out-idx))
			 (declare (type cl:vector out-buf))
			 (declare (type cl:function plus-bf))
			 (if (eq cons1 cons2)
				 out-idx
				 (let ((acc nil))
				   (_= acc (car cons1))
				   (_= (aref out-buf out-idx) acc)
				   (incf out-idx)
				   (setf cons1 (cdr cons1))
				   (for (nil (not (eq cons1 cons2)) (progn (incf out-idx) (setf cons1 (cdr cons1))) :returns out-idx)
					 (_= acc (funcall plus-bf acc (car cons1)))
					 (_= (aref out-buf out-idx) acc))))))

	(defmethod-overload partial_sum ((first cons_const_iterator)
									 (last  cons_const_iterator) (result vector-pointer))
	  ;;(format t "specialized partial_sum for cons_const_iterator & vector-pointer is invoked.~%")
	  (__algo-make-vct-iterator result
								 (__partial_sum-imp-7 (__cons-itr-cons      first)
													  (__cons-itr-cons       last)
													  (opr::vec-ptr-index  result)
													  (opr::vec-ptr-buffer result) #'+)))

	(defmethod-overload partial_sum ((first cons_const_iterator)
									 (last  cons_const_iterator) (result vector-pointer) binary-op)
	  ;;(format t "specialized partial_sum for cons_const_iterator & vector-pointer is invoked.~%")
	  (__algo-make-vct-iterator result
								 (__partial_sum-imp-7 (__cons-itr-cons      first)
													  (__cons-itr-cons       last)
													  (opr::vec-ptr-index  result)
													  (opr::vec-ptr-buffer result) (functor_function (clone binary-op))))))


  ;;PTN; partial_sum : 8 -  cvp x  vp
  (labels ((__partial_sum-imp-8 (idx1 idx2 src-buf out-idx out-buf plus-bf)
			 (declare (type fixnum idx1 idx2 out-idx))
			 (declare (type cl:vector src-buf out-buf))
			 (declare (type cl:function plus-bf))
			 (if (= idx1 idx2)
				 out-idx
				 (let ((acc nil))
				   (_= acc (aref src-buf idx1))
				   (_= (aref out-buf out-idx) acc)
				   (incf out-idx)
				   (incf idx1)
				   (for (nil (< idx1 idx2) (progn (incf out-idx) (incf idx1)) :returns out-idx)
					 (_= acc (funcall plus-bf acc (aref src-buf idx1)))
					 (_= (aref out-buf out-idx) acc))))))

	(defmethod-overload partial_sum ((first const-vector-pointer)
									 (last  const-vector-pointer) (result vector-pointer))
	  ;;(format t "specialized partial_sum for const-vector-pointer & vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__algo-make-vct-iterator result
								 (__partial_sum-imp-8 (opr::vec-ptr-index  first)
													  (opr::vec-ptr-index  last)
													  (opr::vec-ptr-buffer first)
													  (opr::vec-ptr-index  result)
													  (opr::vec-ptr-buffer result) #'+)))

	(defmethod-overload partial_sum ((first const-vector-pointer)
									 (last  const-vector-pointer) (result vector-pointer) binary-op)
	  ;;(format t "specialized partial_sum for const-vector-pointer & vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__algo-make-vct-iterator result
								 (__partial_sum-imp-8 (opr::vec-ptr-index  first)
													  (opr::vec-ptr-index  last)
													  (opr::vec-ptr-buffer first)
													  (opr::vec-ptr-index  result)
													  (opr::vec-ptr-buffer result)
													  (functor_function (clone binary-op)))))))


;;--------------------------------------------------------------------
;; 26.4.4 Adjacent difference
(locally (declare (optimize speed))

  ;;PTN; adjacent_difference : 0 -   i  x  o 
  (labels ((__adjacent_difference-imp-0 (first last result minus)
			 (declare (type cl:function minus))
			 (with-operators
				 (let ((dest @~result))
				   (if (_== first last)
					   dest
					   (let ((prev nil))
						 (_= prev *first)
						 (for (((itr (next first))) (_/= itr last) (progn ++itr ++dest) :returns dest)
						   (let ((cur *itr))
							 (_= *dest (funcall minus cur prev))
							 (_= prev cur)))))))))

	(defmethod-overload adjacent_difference ((first input_iterator)
											 (last  input_iterator) (result output_iterator))
	  (__adjacent_difference-imp-0 first last result #'-))

	(defmethod-overload adjacent_difference ((first input_iterator)
											 (last  input_iterator) (result output_iterator) binary-op)
	  (__adjacent_difference-imp-0 first last result (functor_function (clone binary-op)))))


  ;;PTN; adjacent_difference : 1 -  cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__adjacent_difference-imp-1 (cons1 cons2 oitr minus-bf)
			 (declare (type cl:function minus-bf))
			 (if (eq cons1 cons2)
				 oitr
				 (let ((prev nil))
				   (_= prev (car cons1))
				   (setf cons1 (cdr cons1))
				   (with-operators
					   (for (nil (not (eq cons1 cons2)) (progn ++oitr (setf cons1 (cdr cons1))) :returns oitr)
						 (_= *oitr (funcall minus-bf (car cons1) prev))
						 (_= prev (car cons1))))))))

	(defmethod-overload adjacent_difference ((first cons_const_iterator)
											 (last  cons_const_iterator) (result output_iterator))
	  ;;(format t "specialized adjacent_difference for cons_const_iterator & output_iterator is invoked.~%")
	  (__adjacent_difference-imp-1 (__cons-itr-cons first)
								   (__cons-itr-cons  last) (clone result) #'-))

	(defmethod-overload adjacent_difference ((first  cons_const_iterator)
											 (last   cons_const_iterator) (result output_iterator) binary-op)
	  ;;(format t "specialized adjacent_difference for cons_const_iterator & output_iterator is invoked.~%")
	  (__adjacent_difference-imp-1 (__cons-itr-cons first)
								   (__cons-itr-cons  last)
								   (clone result) (functor_function (clone binary-op)))))


  ;;PTN; adjacent_difference : 2 -  cvp x  o 
  (labels ((__adjacent_difference-imp-2 (idx1 idx2 buffer oitr minus-bf)
			 (declare (type fixnum idx1 idx2))
			 (declare (type cl:vector buffer))
			 (declare (type cl:function minus-bf))
			 (if (= idx1 idx2)
				 oitr
				 (let ((prev nil))
				   (_= prev (aref buffer idx1))
				   (incf idx1)
				   (with-operators
					   (for (nil (< idx1 idx2) (progn ++oitr (incf idx1)) :returns oitr)
						 (_= *oitr (funcall minus-bf (aref buffer idx1) prev))
						 (_= prev (aref buffer idx1))))))))

	(defmethod-overload adjacent_difference ((first const-vector-pointer)
											 (last  const-vector-pointer) (result output_iterator))
	  ;;(format t "specialized adjacent_difference for const-vector-pointer & output_iterator is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__adjacent_difference-imp-2 (opr::vec-ptr-index  first)
								   (opr::vec-ptr-index  last)
								   (opr::vec-ptr-buffer first) (clone result) #'-))

	(defmethod-overload adjacent_difference ((first  const-vector-pointer)
											 (last   const-vector-pointer) (result output_iterator) binary-op)
	  ;;(format t "specialized adjacent_difference for const-vector-pointer & output_iterator is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__adjacent_difference-imp-2 (opr::vec-ptr-index  first)
								   (opr::vec-ptr-index  last)
								   (opr::vec-ptr-buffer first)
								   (clone result) (functor_function (clone binary-op)))))


  ;;PTN; adjacent_difference : 3 -   i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__adjacent_difference-imp-3 (first last out-cons minus-bf)
			 (declare (type cl:function minus-bf))
			 (with-operators
				 (if (_== first last)
					 out-cons
					 (let ((prev nil))
					   (_= prev *first)
					   (for (((itr (next first))) (_/= itr last) (progn ++itr
																		(setf out-cons (cdr out-cons))) :returns out-cons)
						 (let ((cur *itr))
							 (_= (car out-cons) (funcall minus-bf cur prev))
							 (_= prev cur))))))))

	(defmethod-overload adjacent_difference ((first input_iterator)
											 (last  input_iterator) (result cons_iterator))
	  ;;(format t "specialized adjacent_difference for input_iterator & cons_iterator is invoked.~%")
	  (__algo-make-cns-iterator result
								 (__adjacent_difference-imp-3 first last
															  (__cons-itr-cons result) #'-)))

	(defmethod-overload adjacent_difference ((first input_iterator)
											 (last  input_iterator) (result cons_iterator) binary-op)
	  ;;(format t "specialized adjacent_difference for input_iterator & cons_iterator is invoked.~%")
	  (__algo-make-cns-iterator result
								 (__adjacent_difference-imp-3 first last
															  (__cons-itr-cons result)
															  (functor_function (clone binary-op))))))


  ;;PTN; adjacent_difference : 4 -  cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__adjacent_difference-imp-4 (cons1 cons2 out-cons minus-bf)
			 (declare (type cl:function minus-bf))
			 (if (eq cons1 cons2)
				 out-cons
				 (let ((prev nil))
				   (_= prev (car cons1))
				   (setf cons1 (cdr cons1))
				   (for (nil (not (eq cons1 cons2)) (progn (setf cons1    (cdr cons1))
														   (setf out-cons (cdr out-cons))) :returns out-cons)
					   (_= (car out-cons) (funcall minus-bf (car cons1) prev))
					   (_= prev (car cons1)))))))

	(defmethod-overload adjacent_difference ((first cons_const_iterator)
											 (last  cons_const_iterator) (result cons_iterator))
	  ;;(format t "specialized adjacent_difference for cons_const_iterator & cons_iterator is invoked.~%")
	  (__algo-make-cns-iterator result
								 (__adjacent_difference-imp-4 (__cons-itr-cons  first)
															  (__cons-itr-cons   last)
															  (__cons-itr-cons result) #'-)))

	(defmethod-overload adjacent_difference ((first cons_const_iterator)
											 (last  cons_const_iterator) (result cons_iterator) binary-op)
	  ;;(format t "specialized adjacent_difference for cons_const_iterator & cons_iterator is invoked.~%")
	  (__algo-make-cns-iterator result
								 (__adjacent_difference-imp-4 (__cons-itr-cons  first)
															  (__cons-itr-cons   last)
															  (__cons-itr-cons result)
															  (functor_function (clone binary-op))))))


  ;;PTN; adjacent_difference : 5 -  cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__adjacent_difference-imp-5 (idx1 idx2 src-buf out-cons minus-bf)
			 (declare (type fixnum idx1 idx2))
			 (declare (type cl:vector src-buf))
			 (declare (type cl:function minus-bf))
			 (if (= idx1 idx2)
				 out-cons
				 (let ((prev nil))
				   (_= prev (aref src-buf idx1))
				   (incf idx1)
				   (for (nil (< idx1 idx2) (progn (incf idx1)
												  (setf out-cons (cdr out-cons))) :returns out-cons)
					 (_= (car out-cons) (funcall minus-bf (aref src-buf idx1) prev))
					 (_= prev (aref src-buf idx1)))))))

	(defmethod-overload adjacent_difference ((first const-vector-pointer)
											 (last  const-vector-pointer) (result cons_iterator))
	  ;;(format t "specialized adjacent_difference for const-vector-pointer & cons_iterator is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__algo-make-cns-iterator result
								 (__adjacent_difference-imp-5 (opr::vec-ptr-index  first)
															  (opr::vec-ptr-index   last)
															  (opr::vec-ptr-buffer first)
															  (__cons-itr-cons    result) #'-)))

	(defmethod-overload adjacent_difference ((first const-vector-pointer)
											 (last  const-vector-pointer) (result cons_iterator) binary-op)
	  ;;(format t "specialized adjacent_difference for const-vector-pointer & cons_iterator is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__algo-make-cns-iterator result
								 (__adjacent_difference-imp-5 (opr::vec-ptr-index  first)
															  (opr::vec-ptr-index   last)
															  (opr::vec-ptr-buffer first)
															  (__cons-itr-cons    result)
															  (functor_function (clone binary-op))))))


  ;;PTN; adjacent_difference : 6 -   i  x  vp
  (labels ((__adjacent_difference-imp-6 (first last out-idx out-buf minus-bf)
			 (declare (type fixnum out-idx))
			 (declare (type cl:vector out-buf))
			 (declare (type cl:function minus-bf))
			 (with-operators
				 (if (_== first last)
					 out-idx
					 (let ((prev nil))
					   (_= prev *first)
					   (for (((itr (next first))) (_/= itr last) (progn ++itr (incf out-idx)) :returns out-idx)
						 (let ((cur *itr))
							 (_= (aref out-buf out-idx) (funcall minus-bf cur prev))
							 (_= prev cur))))))))

	(defmethod-overload adjacent_difference ((first input_iterator)
											 (last  input_iterator) (result vector-pointer))
	  ;;(format t "specialized adjacent_difference for input_iterator & vector-pointer is invoked.~%")
	  (__algo-make-vct-iterator result
								 (__adjacent_difference-imp-6 first last
															  (opr::vec-ptr-index  result)
															  (opr::vec-ptr-buffer result) #'-)))

	(defmethod-overload adjacent_difference ((first input_iterator)
											 (last  input_iterator) (result vector-pointer) binary-op)
	  ;;(format t "specialized adjacent_difference for input_iterator & vector-pointer is invoked.~%")
	  (__algo-make-vct-iterator result
								 (__adjacent_difference-imp-6 first last
															  (opr::vec-ptr-index  result)
															  (opr::vec-ptr-buffer result)
															  (functor_function (clone binary-op))))))

  ;;PTN; adjacent_difference : 7 -  cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__adjacent_difference-imp-7 (cons1 cons2 out-idx out-buf minus-bf)
			 (declare (type fixnum out-idx))
			 (declare (type cl:vector out-buf))
			 (declare (type cl:function minus-bf))
			 (if (eq cons1 cons2)
				 out-idx
				 (let ((prev nil))
				   (_= prev (car cons1))
				   (setf cons1 (cdr cons1))
				   (for (nil (not (eq cons1 cons2)) (progn (incf out-idx)
														   (setf cons1 (cdr cons1))) :returns out-idx)
					   (_= (aref out-buf out-idx) (funcall minus-bf (car cons1) prev))
					   (_= prev (car cons1)))))))

	(defmethod-overload adjacent_difference ((first cons_const_iterator)
											 (last  cons_const_iterator) (result vector-pointer))
	  ;;(format t "specialized adjacent_difference for cons_const_iterator & vector-pointer is invoked.~%")
	  (__algo-make-vct-iterator result
								 (__adjacent_difference-imp-7 (__cons-itr-cons      first)
															  (__cons-itr-cons       last)
															  (opr::vec-ptr-index  result)
															  (opr::vec-ptr-buffer result) #'-)))

	(defmethod-overload adjacent_difference ((first cons_const_iterator)
											 (last  cons_const_iterator) (result vector-pointer) binary-op)
	  ;;(format t "specialized adjacent_difference for cons_const_iterator & vector-pointer is invoked.~%")
	  (__algo-make-vct-iterator result
								 (__adjacent_difference-imp-7 (__cons-itr-cons      first)
															  (__cons-itr-cons       last)
															  (opr::vec-ptr-index  result)
															  (opr::vec-ptr-buffer result)
															  (functor_function (clone binary-op))))))


  ;;PTN; adjacent_difference : 8 -  cvp x  vp
  (labels ((__adjacent_difference-imp-8 (idx1 idx2 src-buf out-idx out-buf minus-bf)
			 (declare (type fixnum idx1 idx2 out-idx))
			 (declare (type cl:vector src-buf out-buf))
			 (declare (type cl:function minus-bf))
			 (if (= idx1 idx2)
				 out-idx
				 (let ((prev nil))
				   (_= prev (aref src-buf idx1))
				   (incf idx1)
				   (for (nil (< idx1 idx2) (progn (incf idx1) (incf out-idx)) :returns out-idx)
					 (_= (aref out-buf out-idx) (funcall minus-bf (aref src-buf idx1) prev))
					 (_= prev (aref src-buf idx1)))))))

	(defmethod-overload adjacent_difference ((first  const-vector-pointer)
											 (last   const-vector-pointer) (result vector-pointer))
	  ;;(format t "specialized adjacent_difference for const-vector-pointer & vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__algo-make-vct-iterator result
								 (__adjacent_difference-imp-8 (opr::vec-ptr-index  first)
															  (opr::vec-ptr-index  last)
															  (opr::vec-ptr-buffer first)
															  (opr::vec-ptr-index  result)
															  (opr::vec-ptr-buffer result) #'-)))

	(defmethod-overload adjacent_difference ((first  const-vector-pointer)
											 (last   const-vector-pointer) (result vector-pointer) binary-op)
	  ;;(format t "specialized adjacent_difference for const-vector-pointer & vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__algo-make-vct-iterator result
								 (__adjacent_difference-imp-8 (opr::vec-ptr-index  first)
															  (opr::vec-ptr-index  last)
															  (opr::vec-ptr-buffer first)
															  (opr::vec-ptr-index  result)
															  (opr::vec-ptr-buffer result)
															  (functor_function (clone binary-op)))))))


;;--------------------------------------------------------------------
;; ??.?.? iota
;; first     : forward_iterator
;; last      : forward_iterator
;; init      : value
;; unary-op  : unary_function ( added by CL-STL. default : #'1+ )
;; returns   : nil.
#-cl-stl-0x98
(locally (declare (optimize speed))

  ;;PTN; iota : 0 -   f
  (labels ((__iota-imp-0 (first last init incr)
			 (declare (type cl:function incr))
			 (if (_== first last)
				 nil
				 (let ((acc nil))
				   (_= acc init)
				   (with-operators
					   (for (((itr @~first)) (_/= itr last) ++itr)
						 (_= *itr acc)
						 (_= acc (funcall incr acc))))))))

	(defmethod-overload iota ((first forward_iterator) (last forward_iterator) init)
	  (__iota-imp-0 first last init #'1+))

	#-cl-stl-noextra
	(defmethod-overload iota ((first forward_iterator) (last forward_iterator) init unary-op)
	  (__iota-imp-0 first last init (functor_function (clone unary-op)))))


  ;;PTN; iota : 1 -   ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__iota-imp-1 (cons1 cons2 init incr)
			 (declare (type cl:function incr))
			 (let ((cur nil))
			   (_= cur init)
			   (for (nil (not (eq cons1 cons2)) (setf cons1 (cdr cons1)))
				 (_= (car cons1) cur)
				 (_= cur (funcall incr cur))))))

	(defmethod-overload iota ((first cons_iterator) (last cons_iterator) init)
	  ;;(format t "specialized iota for cons_iterator is invoked.~%")
	  (__iota-imp-1 (__cons-itr-cons first)
					(__cons-itr-cons  last) init #'1+))
	#-cl-stl-noextra
	(defmethod-overload iota ((first cons_iterator) (last cons_iterator) init unary-op)
	  ;;(format t "specialized iota for cons_iterator is invoked.~%")
	  (__iota-imp-1 (__cons-itr-cons first)
					(__cons-itr-cons  last) init (functor_function (clone unary-op)))))


  ;;PTN; iota : 2 -   vp
  (labels ((__iota-imp-2 (idx1 idx2 buffer init incr)
			 (declare (type fixnum idx1 idx2))
			 (declare (type cl:vector buffer))
			 (declare (type cl:function incr))
			 (let ((cur nil))
			   (_= cur init)
			   (for (nil (< idx1 idx2) (incf idx1))
				 (_= (aref buffer idx1) cur)
				 (_= cur (funcall incr cur))))))

	(defmethod-overload iota ((first vector-pointer) (last vector-pointer) init)
	  ;;(format t "specialized iota for vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__iota-imp-2 (opr::vec-ptr-index  first)
					(opr::vec-ptr-index   last)
					(opr::vec-ptr-buffer first) init #'1+))
	#-cl-stl-noextra
	(defmethod-overload iota ((first vector-pointer) (last vector-pointer) init unary-op)
	  ;;(format t "specialized iota for vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__iota-imp-2 (opr::vec-ptr-index  first)
					(opr::vec-ptr-index   last)
					(opr::vec-ptr-buffer first) init (functor_function (clone unary-op))))))


;;--------------------------------------------------------------------
;; ??.?.? reduce
;; first     : input_iterator
;; last      : input_iterator
;; init      : initial value ( 0 by default )
;; binary-op : binary function ( #'+ by default )
;; returns   : reduced value.
#-(or cl-stl-0x98 cl-stl-0x11 cl-stl-0x14)
(locally (declare (optimize speed))

  ;;PTN; reduce : 0 -   i
  (labels ((__reduce-imp-0 (first last init plus-bf)
			 (declare (type cl:function plus-bf))
			 (with-operators
				 (let ((acc nil))
				   (_= acc init)
				   (if (_== first last)
					   acc
					   (for (((itr @~first)) (_/= itr last) ++itr :returns acc)
							(_= acc (funcall plus-bf acc *itr))))))))

	(defmethod-overload reduce ((first input_iterator) (last input_iterator))
	  (__reduce-imp-0 first last 0 #'+))

	(defmethod-overload reduce ((first input_iterator) (last input_iterator) init)
	  (__reduce-imp-0 first last init #'+))

	(defmethod-overload reduce ((first input_iterator) (last input_iterator) init binary-op)
	  (__reduce-imp-0 first last init (functor_function (clone binary-op)))))

  ;;PTN; reduce : 1 -  cci
  (labels ((__reduce-imp-1 (cons1 cons2 init plus-bf)
			 (declare (type cl:function plus-bf))
			 (let ((acc nil))
			   (_= acc init)
			   (for (nil (not (eq cons1 cons2)) (setf cons1 (cdr cons1)) :returns acc)
				 (_= acc (funcall plus-bf acc (car cons1)))))))

	(defmethod-overload reduce ((first cons_const_iterator) (last cons_const_iterator))
	  ;;(format t "specialized reduce for cons_const_iterator is invoked.~%")
	  (__reduce-imp-1 (__cons-itr-cons first)
					  (__cons-itr-cons  last) 0 #'+))

	(defmethod-overload reduce ((first cons_const_iterator) (last cons_const_iterator) init)
	  ;;(format t "specialized reduce for cons_const_iterator is invoked.~%")
	  (__reduce-imp-1 (__cons-itr-cons first)
					  (__cons-itr-cons  last) init #'+))

	(defmethod-overload reduce ((first cons_const_iterator) (last cons_const_iterator) init binary-op)
	  ;;(format t "specialized reduce for cons_const_iterator is invoked.~%")
	  (__reduce-imp-1 (__cons-itr-cons first)
					  (__cons-itr-cons  last) init (functor_function (clone binary-op)))))

  ;;PTN; reduce : 2 -  cvp
  (labels ((__reduce-imp-2 (idx1 idx2 buffer init plus-bf)
			 (declare (type fixnum idx1 idx2))
			 (declare (type cl:vector buffer))
			 (declare (type cl:function plus-bf))
			 (let ((acc nil))
			   (_= acc init)
			   (for (nil (< idx1 idx2) (incf idx1) :returns acc)
				 (_= acc (funcall plus-bf acc (aref buffer idx1)))))))

	(defmethod-overload reduce ((first const-vector-pointer) (last  const-vector-pointer))
	  ;;(format t "specialized reduce for const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__reduce-imp-2 (opr::vec-ptr-index  first)
					  (opr::vec-ptr-index   last)
					  (opr::vec-ptr-buffer first) 0 #'+))

	(defmethod-overload reduce ((first const-vector-pointer) (last  const-vector-pointer) init)
	  ;;(format t "specialized reduce for const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__reduce-imp-2 (opr::vec-ptr-index  first)
					  (opr::vec-ptr-index   last)
					  (opr::vec-ptr-buffer first) init #'+))

	(defmethod-overload reduce ((first const-vector-pointer) (last  const-vector-pointer) init binary-op)
	  ;;(format t "specialized reduce for const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__reduce-imp-2 (opr::vec-ptr-index  first)
					  (opr::vec-ptr-index  last)
					  (opr::vec-ptr-buffer first)
					  init (functor_function (clone binary-op))))))

