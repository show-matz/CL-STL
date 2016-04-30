(in-package :cl-stl)


;;--------------------------------------------------------------------------------
;;
;; class initializer_list
;;
;;--------------------------------------------------------------------------------
#-cl-stl-0x98
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass initializer_list ()
	((data :type    cl:vector
		   :initarg :data
		   :reader  __initlist-data))))


;;--------------------------------------------------------------------------------
;;
;; method for initializer_list
;;
;;--------------------------------------------------------------------------------
#-cl-stl-0x98
(locally (declare (optimize speed))

  (defmethod size ((obj initializer_list))
	(length (the simple-vector (__initlist-data obj))))

  (defmethod begin ((obj initializer_list))
	(const_& (__initlist-data obj) 0))

  (defmethod end ((obj initializer_list))
	(let ((arr (__initlist-data obj)))
	  (declare (type simple-vector arr))
	  (const_& arr (length arr))))

  (defmethod-overload for ((cont initializer_list) func)
	;;MEMO : func is always lambda function ( see stl:for ). 
	(declare (type cl:function func))
	(let ((arr (__initlist-data cont)))
	  (declare (type simple-vector arr))
	  (let ((idx 0)
			(cnt (length arr)))
		(declare (type fixnum idx cnt))
		(for (nil (< idx cnt) (incf idx))
		  (funcall func (aref arr idx)))))))


;;--------------------------------------------------------------------------------
;;
;; read macro for initializer_list  #{...} 
;;
;;--------------------------------------------------------------------------------
#-cl-stl-0x98
(onlisp/defdelim #\{ #\} (&rest items)
  (labels ((__inits (sym lst idx acc)
			 (if (null lst)
				 (nreverse acc)
				 (__inits sym (cdr lst) (1+ idx)
						  (cl:push `(setf (aref ,sym ,idx) ,(car lst)) acc)))))
	(let ((arr (gensym "ARR")))
	  `(locally (declare (optimize speed))
		 (let ((,arr (make-array ,(length items))))
		   (declare (type cl:vector ,arr))
		   ,@(__inits arr items 0 nil)
		   (make-instance 'initializer_list :data ,arr))))))



