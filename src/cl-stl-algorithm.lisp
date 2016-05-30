(in-package :cl-stl)



;;------------------------------------------------------------------------------
;;
;; __tmpitr
;;
;;------------------------------------------------------------------------------
(defclass __tmpitr (output_iterator)
  ((buffer   :type     :vector
			 :initform (new stl:vector)
			 :accessor tmpitr-buffer)
   (begin    :type     :vector_iterator
			 :initform nil
			 :initarg  :begin
			 :accessor tmpitr-begin)
   (cur      :type     :vector_iterator
			 :initform nil
			 :initarg  :cur
			 :accessor tmpitr-cur)
   (len      :type     :fixnum
			 :initform 0
			 :initarg  :maxlen
			 :accessor tmpitr-maxlen)))
   
;(defmethod operator_= ((itr1 __tmpitr) (itr2 __tmpitr))
;  (setf (slot-value itr1 'm-container) (slot-value itr2 'm-container)))

(defmethod operator_clone ((itr __tmpitr))
  itr)

(defmethod (setf operator_*) (new-val (itr __tmpitr))
  (_= (_* (tmpitr-cur itr)) new-val)
  (setf (tmpitr-cur itr)
		(operator_++ (tmpitr-cur itr)))
  new-val)

(defmethod operator_++ ((itr __tmpitr))
  itr)

(defmethod begin ((container __tmpitr))
  (clone (tmpitr-begin container)))

(defmethod end ((container __tmpitr))
  (clone (tmpitr-cur container)))

(defun new-tmpitr ()
  (make-instance '__tmpitr))

(defun tmpitr-init (tmpitr)
  (setf (tmpitr-cur tmpitr) (clone (tmpitr-begin tmpitr)))
  tmpitr)

(defun tmpitr-set-buf-len (tmpitr new-len)
  (resize (tmpitr-buffer tmpitr) new-len)
  (setf (tmpitr-begin  tmpitr) (begin (tmpitr-buffer tmpitr)))
  (setf (tmpitr-cur    tmpitr) (begin (tmpitr-buffer tmpitr)))
  (setf (tmpitr-maxlen tmpitr) new-len))



;;------------------------------------------------------------------------------
;;
;; internal constants
;;
;;------------------------------------------------------------------------------
(defconstant +CHUNK-SIZE+  7)
(defconstant +SORT-MAX+   16)


;;------------------------------------------------------------------------------
;;
;; internal functions
;;
;;------------------------------------------------------------------------------
(locally (declare (optimize speed))
  ;;ToDo : change __median to macro ? ... or inline ?
  (defun __median (v1 v2 v3 less-bf)
	(declare (type cl:function less-bf))
	(if (funcall less-bf v1 v2)
		(cond ((funcall less-bf v2 v3) v2)
			  ((funcall less-bf v1 v3) v3)
			  (t                       v1))
		(cond ((funcall less-bf v1 v3) v1)
			  ((funcall less-bf v2 v3) v3)
			  (t                       v2)))))


;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  (defun __copy-imp-0 (first last result)
	(with-operators
		(let ((dest @~result))
		  (if (_== first last)
			  dest
			  (for (((itr @~first)) (_/= itr last) (progn ++itr ++dest) :returns dest)
				(_= *dest *itr))))))
	
  (defun __copy-imp-1 (cons1 cons2 oitr)
	(declare (type cl:list cons1 cons2))
	(with-operators
		(for (nil (not (eq cons1 cons2)) (progn (setf cons1 (cdr cons1)) ++oitr) :returns oitr)
		  (_= *oitr (car cons1)))))

  (defun __copy-imp-2 (idx1 idx2 buffer oitr)
	(declare (type fixnum idx1 idx2))
	(declare (type cl:vector buffer))
	(with-operators
		(for (nil (< idx1 idx2) (progn (incf idx1) ++oitr) :returns oitr)
		  (_= *oitr (aref buffer idx1)))))

  (defun __copy-imp-3 (first last out-cons)
	(declare (type cl:list out-cons))
	(with-operators
		(for (((itr @~first)) (_/= itr last) (progn ++itr (setf out-cons (cdr out-cons))) :returns out-cons)
		(_= (car out-cons) *itr))))

  (defun __copy-imp-4 (cons1 cons2 out-cons)
	(declare (type cl:list cons1 cons2 out-cons))
	(for (nil (not (eq cons1 cons2)) (progn (setf cons1 (cdr cons1))
											(setf out-cons (cdr out-cons))) :returns out-cons)
	  (_= (car out-cons) (car cons1))))

  (defun __copy-imp-5 (idx1 idx2 src-buf out-cons)
	(declare (type fixnum idx1 idx2))
	(declare (type cl:vector src-buf))
	(declare (type cl:list out-cons))
	(for (nil (< idx1 idx2) (progn (incf idx1)
								   (setf out-cons (cdr out-cons))) :returns out-cons)
	  (_= (car out-cons) (aref src-buf idx1))))

  (defun __copy-imp-6 (first last out-idx out-buf)
	(declare (type fixnum out-idx))
	(declare (type cl:vector out-buf))
	(with-operators
	  (for (((itr @~first)) (_/= itr last) (progn ++itr (incf out-idx)) :returns out-idx)
		(_= (aref out-buf out-idx) *itr))))

  (defun __copy-imp-7 (cons1 cons2 out-idx out-buf)
	(declare (type cl:list cons1 cons2))
	(declare (type fixnum out-idx))
	(declare (type cl:vector out-buf))
	(for (nil (not (eq cons1 cons2)) (progn (incf out-idx)
											(setf cons1 (cdr cons1))) :returns out-idx)
	  (_= (aref out-buf out-idx) (car cons1))))

  (defun __copy-imp-8 (idx1 idx2 src-buf out-idx out-buf)
	(declare (type fixnum idx1 idx2 out-idx))
	(declare (type cl:vector src-buf out-buf))
	(for (nil (< idx1 idx2) (progn (incf idx1) (incf out-idx)) :returns out-idx)
		(_= (aref out-buf out-idx) (aref src-buf idx1)))))

;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  ;;IMP; copy_backward : 0 -  b  x  b 
  (defun __copy_backward-imp-0 (first last result)
	(with-operators
		(let ((result @~result))
		  (if (_== first last)
			  result
			  (for (((itr @~last)) (_/= first itr) nil :returns result)
				--itr
				--result
				(_= *result *itr))))))

  ;;IMP; copy_backward : 1 - cvp x  b 
  (defun __copy_backward-imp-1 (idx1 idx2 src-buf out-itr)
	(declare (type fixnum idx1 idx2))
	(declare (type cl:vector src-buf))
	(with-operators
		(for (nil (< idx1 idx2) nil :returns out-itr)
		  (decf idx2)
		  --out-itr
		  (_= *out-itr (aref src-buf idx2)))))

  ;;IMP; copy_backward : 2 -  b  x  vp
  (defun __copy_backward-imp-2 (first last out-idx out-buf)
	(declare (type fixnum out-idx))
	(declare (type cl:vector out-buf))
	(with-operators
		(for (((itr @~last)) (_/= first itr) nil :returns out-idx)
		  --itr
		  (decf out-idx)
		  (_= (aref out-buf out-idx) *itr))))

  ;;IMP; copy_backward : 3 - cvp x  vp
  (defun __copy_backward-imp-3 (idx1 idx2 src-buf out-idx out-buf)
	(declare (type fixnum idx1 idx2 out-idx))
	(declare (type cl:vector src-buf out-buf))
	(for (nil (< idx1 idx2) nil :returns out-idx)
	  (decf idx2)
	  (decf out-idx)
	  (_= (aref out-buf out-idx) (aref src-buf idx2)))))


;;------------------------------------------------------------------------------
#-cl-stl-0x98
(locally (declare (optimize speed))

  (defun __move-imp-0 (first last result)
	(with-operators
		(let ((dest @~result))
		  (if (_== first last)
			  dest
			  (for (((itr @~first)) (_/= itr last) (progn ++itr ++dest) :returns dest)
				(multiple-value-bind (a b) (operator_move *dest *itr)
				  (setf *itr  b)
				  (setf *dest a)))))))

  (defun __move-imp-1 (cons1 cons2 oitr)
	(declare (type cl:list cons1 cons2))
	(with-operators
		(for (nil (not (eq cons1 cons2)) (progn (setf cons1 (cdr cons1)) ++oitr) :returns oitr)
		  (multiple-value-bind (a b)
			  (operator_move *oitr (car cons1))
			(setf (car cons1) b)
			(setf *oitr       a)))))

  (defun __move-imp-2 (idx1 idx2 buffer oitr)
	(declare (type fixnum idx1 idx2))
	(declare (type cl:vector buffer))
	(with-operators
		(for (nil (< idx1 idx2) (progn (incf idx1) ++oitr) :returns oitr)
		  (multiple-value-bind (a b)
			  (operator_move *oitr (aref buffer idx1))
			(setf (aref buffer idx1) b)
			(setf *oitr              a)))))

  (defun __move-imp-3 (first last out)
	(declare (type cl:list out))
	(with-operators
		(for (((itr @~first)) (_/= itr last) (progn ++itr (setf out (cdr out))) :returns out)
		(multiple-value-bind (a b) (operator_move (car out) *itr)
		  (setf *itr      b)
		  (setf (car out) a)))))

  (defun __move-imp-4 (cons1 end1 out)
	(declare (type cl:list cons1 end1 out))
	(for (nil (not (eq cons1 end1)) (progn (setf cons1 (cdr cons1))
										   (setf out   (cdr out))) :returns out)
	  (multiple-value-bind (a b) (operator_move (car out) (car cons1))
		(setf (car cons1) b)
		(setf (car out)   a))))

  (defun __move-imp-5 (idx1 end1 buf1 out)
	(declare (type fixnum idx1 end1))
	(declare (type cl:vector buf1))
	(declare (type cl:list out))
	(for (nil (< idx1 end1) (progn (incf idx1)
								   (setf out (cdr out))) :returns out)
	  (multiple-value-bind (a b) (operator_move (car out) (aref buf1 idx1))
		  (setf (aref buf1 idx1) b)
		  (setf (car out)        a))))
  
  (defun __move-imp-6 (first last out-idx out-buf)
	(declare (type fixnum out-idx))
	(declare (type cl:vector out-buf))
	(with-operators
		(for (((itr @~first)) (_/= itr last) (progn ++itr (incf out-idx)) :returns out-idx)
		(multiple-value-bind (a b)
			(operator_move (aref out-buf out-idx) *itr)
		  (setf *itr                   b)
		  (setf (aref out-buf out-idx) a)))))

  (defun __move-imp-7 (cons1 end1 out-idx out-buf)
	(declare (type cl:list cons1 end1))
	(declare (type fixnum out-idx))
	(declare (type cl:vector out-buf))
	(for (nil (not (eq cons1 end1)) (progn (setf cons1 (cdr cons1)) (incf out-idx)) :returns out-idx)
	  (multiple-value-bind (a b)
		  (operator_move (aref out-buf out-idx) (car cons1))
		(setf (car cons1)            b)
		(setf (aref out-buf out-idx) a))))

  (defun __move-imp-8 (idx1 end1 buf1 out-idx out-buf)
	(declare (type fixnum idx1 end1 out-idx))
	(declare (type cl:vector buf1 out-buf))
	(let ((idx idx1))
	  (declare (type fixnum idx))
	  (for (nil (< idx end1) (progn (incf idx) (incf out-idx)) :returns out-idx)
		(multiple-value-bind (a b)
			(operator_move (aref out-buf out-idx) (aref buf1 idx))
		  (setf (aref buf1 idx)        b)
		  (setf (aref out-buf out-idx) a))))))


;;------------------------------------------------------------------------------
#-cl-stl-0x98    ; __move_backward-imp
(locally (declare (optimize speed))

  ;;IMP; move_backward : 0 -  b  x  b 
  (defun __move_backward-imp-0 (first last result)
	(with-operators
		(let ((result @~result))
		  (if (_== first last)
			  result
			  (for (((itr @~last)) (_/= first itr) nil :returns result)
				--itr
				--result
				(multiple-value-bind (a b) (operator_move *result *itr)
				  (setf *itr    b)
				  (setf *result a)))))))

  ;;IMP; move_backward : 1 - cvp x  b 
  (defun __move_backward-imp-1 (idx1 idx2 src-buf out-itr)
	(declare (type fixnum idx1 idx2))
	(declare (type cl:vector src-buf))
	(with-operators
		(for (nil (< idx1 idx2) nil :returns out-itr)
		  (decf idx2)
		  --out-itr
		  (multiple-value-bind (a b)
			  (operator_move *out-itr (aref src-buf idx2))
			(setf (aref src-buf idx2) b)
			(setf *out-itr            a)))))

  ;;IMP; move_backward : 2 -  b  x  vp
  (defun __move_backward-imp-2 (first last out-idx out-buf)
	(declare (type fixnum out-idx))
	(declare (type cl:vector out-buf))
	(with-operators
		(for (((itr @~last)) (_/= first itr) nil :returns out-idx)
		  --itr
		  (decf out-idx)
		  (multiple-value-bind (a b)
			  (operator_move (aref out-buf out-idx) *itr)
			(setf *itr                   b)
			(setf (aref out-buf out-idx) a)))))

  ;;IMP; move_backward : 3 - cvp x  vp
  (defun __move_backward-imp-3 (idx1 idx2 src-buf out-idx out-buf)
	(declare (type fixnum idx1 idx2 out-idx))
	(declare (type cl:vector src-buf out-buf))
	(for (nil (< idx1 idx2) nil :returns out-idx)
	  (decf idx2)
	  (decf out-idx)
	  (multiple-value-bind (a b)
		  (operator_move (aref out-buf out-idx) (aref src-buf idx2))
		(setf (aref src-buf idx2)    b)
		(setf (aref out-buf out-idx) a)))))


;;------------------------------------------------------------------------------
(locally (declare (optimize speed))
  (defun __swap_ranges-imp-0 (first1 last1 first2)
	(with-operators
		(if (_== first1 last1)
			@~first2
			(for (((itr1 @~first1)
				   (itr2 @~first2)) (_/= itr1 last1) (progn ++itr1 ++itr2) :returns itr2)
			  (swap *itr1 *itr2)))))

  (defun __swap_ranges-imp-1 (cons1 end1 first2)
	(declare (type cl:list cons1 end1))
	(with-operators
		(for (((itr2 @~first2)) (not (eq cons1 end1))
								(progn (setf cons1 (cdr cons1)) ++itr2) :returns itr2)
		  (swap (car cons1) *itr2))))

  (defun __swap_ranges-imp-2 (idx1 last1 buffer1 first2)
	(declare (type fixnum idx1 last1))
	(declare (type cl:vector buffer1))
	(with-operators
		(for (((itr2 @~first2)) (< idx1 last1) (progn (incf idx1) ++itr2) :returns itr2)
		  (swap (aref buffer1 idx1) *itr2))))

  (defun __swap_ranges-imp-3 (first1 last1 cons2)
	(declare (type cl:list cons2))
	(with-operators
		(for (((itr @~first1)) (_/= itr last1)
							   (progn ++itr (setf cons2 (cdr cons2))) :returns cons2)
		  (swap *itr (car cons2)))))
  
  (defun __swap_ranges-imp-4 (cons1 end1 cons2)
	(declare (type cl:list cons1 end1 cons2))
	(for (nil (not (eq cons1 end1)) (progn (setf cons1 (cdr cons1))
										   (setf cons2 (cdr cons2))) :returns cons2)
	  (swap (car cons1) (car cons2))))

  (defun __swap_ranges-imp-5 (idx1 last1 buf1 cons2)
	(declare (type fixnum idx1 last1))
	(declare (type cl:vector buf1))
	(declare (type cl:list cons2))
	(for (nil (< idx1 last1) (progn (incf idx1)
									(setf cons2 (cdr cons2))) :returns cons2)
	  (swap (aref buf1 idx1) (car cons2))))

  (defun __swap_ranges-imp-6 (first last idx2 buffer2)
	(declare (type fixnum idx2))
	(declare (type cl:vector buffer2))
	(with-operators
		(for (((itr @~first)) (_/= itr last) (progn ++itr (incf idx2)) :returns idx2)
		  (swap *itr (aref buffer2 idx2)))))

  (defun __swap_ranges-imp-7 (cons1 end1 idx2 buf2)
	(declare (type cl:list cons1 end1))
	(declare (type fixnum idx2))
	(declare (type cl:vector buf2))
	(for (nil (not (eq cons1 end1)) (progn (setf cons1 (cdr cons1)) (incf idx2)) :returns idx2)
	  (swap (car cons1) (aref buf2 idx2))))
  
  (defun __swap_ranges-imp-8 (idx1 last1 buf1 idx2 buf2)
	(declare (type fixnum idx1 last1 idx2))
	(declare (type cl:vector buf1 buf2))
	(for (nil (< idx1 last1) (progn (incf idx1) (incf idx2)) :returns idx2)
	  (swap (aref buf1 idx1) (aref buf2 idx2)))))


;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  (defun __rotate-imp-1 (first middle last)
	(declare (type cl:list first middle last))
	(if (or (eq first middle) (eq middle last))
		first
		(macrolet ((move-next (sym)
					 `(setf ,sym (cdr ,sym))))
		  (let ((first2 middle))
			(for (nil t nil)
			  (swap (car first) (car first2))
			  (move-next first)
			  (move-next first2)
			  (when (eq first middle)
				(setf middle first2))
			  (when (eq first2 last)
				(return)))
			(setf first2 middle)
			(for (((ret first)) (not (eq first2 last)) nil :returns ret)
			  (swap (car first) (car first2))
			  (move-next first)
			  (move-next first2)
			  (if (eq first middle)
				  (setf middle first2)
				  (when (eq first2 last)
					(setf first2 middle))))))))

  (defun __rotate-imp-2 (first middle last buffer)
	(declare (type fixnum first middle last))
	(declare (type cl:vector buffer))
	(if (or (= first middle) (= middle last))
		0
		(let ((ret  (- last  middle))
			  (len1 (- last   first))
			  (len2 (- middle first)))
		  (declare (type fixnum ret len1 len2))
		  (if (= len2 (- len1 len2))
			  (progn
				(__swap_ranges-imp-8 first middle buffer middle buffer)
				ret)
			  (let ((idx1 first))
				(declare (type fixnum idx1))
				(for (nil t nil)
				  (if (< len2 (- len1 len2))
					  (progn (when (= len2 1)
							   (let ((val nil))
								 (_= val (aref buffer idx1))
								 #+cl-stl-0x98 (__copy-imp-8 (the fixnum (1+ idx1))
															 (the fixnum (+ idx1 len1)) buffer idx1 buffer)
								 #-cl-stl-0x98 (__move-imp-8 (the fixnum (1+ idx1))
															 (the fixnum (+ idx1 len1)) buffer idx1 buffer)
								 (_= (aref buffer (the fixnum (+ idx1 (the fixnum (1- len1))))) val))
							   (return-from __rotate-imp-2 ret))
							 (let ((idx2 (+ idx1 len2)))
							   (declare (type fixnum idx2))
							   (let ((idx 0)
									 (cnt (- len1 len2)))
								 (declare (type fixnum idx cnt))
								 (for (nil (< idx cnt) (progn (incf idx) (incf idx1) (incf idx2)))
								   (swap (aref buffer idx1) (aref buffer idx2))))
							   (setf len1 (mod len1 len2)))
							 (when (zerop len1)
							   (return-from __rotate-imp-2 ret))
							 (rotatef len1 len2)
							 (setf len2 (- len1 len2)))

					  (progn (setf len2 (- len1 len2))
							 (when (= len2 1)
							   (let ((val nil))
								 (_= val (aref buffer (the fixnum (+ idx1 (the fixnum (1- len1))))))
								 #+cl-stl-0x98 (__copy_backward-imp-3 idx1
																	  (the fixnum (+ idx1 (the fixnum (1- len1))))
																	  buffer (the fixnum (+ idx1 len1)) buffer)
								 #-cl-stl-0x98 (__move_backward-imp-3 idx1
																	  (the fixnum (+ idx1 (the fixnum (1- len1))))
																	  buffer (the fixnum (+ idx1 len1)) buffer)
								 (_= (aref buffer idx1) val))
							   (return-from __rotate-imp-2 ret))
							 (let ((idx2 (+ idx1 len1)))
							   (declare (type fixnum idx2))
							   (setf idx1 idx2)
							   (decf idx1 len2)
							   (let ((idx 0)
									 (cnt (- len1 len2)))
								 (declare (type fixnum cnt idx))
								 (for (nil (< idx cnt) (incf idx))
								   (decf idx1)
								   (decf idx2)
								   (swap (aref buffer idx1) (aref buffer idx2))))
							   (setf len1 (mod len1 len2))
							   (when (zerop len1)
								 (return-from __rotate-imp-2 ret))
							   (rotatef len1 len2)))))))))))


;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  (defun __lower_bound-imp-0 (first last val comp)
	(declare (type cl:function comp))
	(with-operators
		(if (_== first last)
			@~last
			(for (((top @~first) (mid @~first) (last @~last)) t nil)
			  (let ((dist (the fixnum (distance top last))))
				(declare (type fixnum dist))
				(when (<= dist 0)
				  (return-from __lower_bound-imp-0 top))
				(_= mid top)
				(advance mid (ash dist -1))
				(if (funcall comp *mid val)
					(progn
					  (_= top mid)
					  ++top)
					(_= last mid)))))))
  
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __lower_bound-imp-1 (first last val comp)
	(declare (type cl:list first last))
	(declare (type cl:function comp))
	(if (eq first last)
		last
		(let ((top first)
			  (mid first))
		  (declare (type cl:list top mid))
		  (for (nil t nil)
			(let ((dist (the fixnum (__conslist-count-nodes top last))))
			  (declare (type fixnum dist))
			  (when (<= dist 0)
				(return-from __lower_bound-imp-1 top))
			  (setf mid top)
			  (dotimes (v (ash dist -1)) (setf mid (cdr mid)))
			  (if (funcall comp (car mid) val)
				  (setf top (cdr mid))
				  (setf last mid)))))))
  
  (defun __lower_bound-imp-2 (first last buf val less-bf)
	(declare (type fixnum first last))
	(declare (type cl:vector buf))
	(declare (type cl:function less-bf))
	(if (= first last)
		last
		(let ((middle first))
		  (declare (type fixnum middle))
		  (for (nil t nil)
			(let ((distance (the fixnum (- last first))))
			  (declare (type fixnum distance))
			  (when (<= distance 0)
				(return-from __lower_bound-imp-2 first))
			  (setf middle (+ first (ash distance -1)))
			  (if (funcall less-bf (aref buf middle) val)
				  (setf first (1+ middle))
				  (setf last middle))))))))

;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  (defun __upper_bound-imp-0 (first last val comp)
	(declare (type cl:function comp))
	(with-operators
		(if (_== first last)
			@~last
			(for (((top @~first) (mid @~first) (last @~last)) t nil)
			  (let ((dist (the fixnum (distance top last))))
				(declare (type fixnum dist))
				(when (<= dist 0)
				  (return-from __upper_bound-imp-0 top))
				(_= mid top)
				(advance mid (ash dist -1))
				(if (funcall comp val *mid)
					(_= last mid)
					(progn
					  (_= top mid)
					  ++top)))))))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __upper_bound-imp-1 (first last val comp)
	(declare (type cl:list first last))
	(declare (type cl:function comp))
	(if (eq first last)
		last
		(let ((top first)
			  (mid first))
		  (declare (type cl:list top mid))
		  (for (nil t nil)
			(let ((dist (the fixnum (__conslist-count-nodes top last))))
			  (declare (type fixnum dist))
			  (when (<= dist 0)
				(return-from __upper_bound-imp-1 top))
			  (setf mid top)
			  (dotimes (v (ash dist -1)) (setf mid (cdr mid)))
			  (if (funcall comp val (car mid))
				  (setf last mid)
				  (setf top (cdr mid))))))))
  
  (defun __upper_bound-imp-2 (top last buffer val less-bf)
	(declare (type fixnum top last))
	(declare (type cl:vector buffer))
	(declare (type cl:function less-bf))
	(if (= top last)
		last
		(let ((middle top))
		  (declare (type fixnum middle))
		  (for (nil t nil)
			(let ((distance (the fixnum (- last top))))
			  (declare (type fixnum distance))
			  (when (<= distance 0)
				(return-from __upper_bound-imp-2 top))
			  (setf middle (+ top (ash distance -1)))
			  (if (not (funcall less-bf val (aref buffer middle)))
				  (setf top (1+ middle))
				  (setf last middle))))))))


;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  ;;IMP; merge : 00 -   i  x  i  x  o 
  (defun __merge-imp-00 (first1 last1 first2 last2 result comp)
	(declare (type cl:function comp))
	(with-operators
		(for (((itr1 @~first1) (itr2 @~first2) (dest @~result)) t nil)
		  (when (_== itr1 last1)
			(for (nil (_/= itr2 last2) (progn ++itr2 ++dest))
			  (_= *dest *itr2))
			(return-from __merge-imp-00 dest))
		  (when (_== itr2 last2)
			(for (nil (_/= itr1 last1) (progn ++itr1 ++dest))
			  (_= *dest *itr1))
			(return-from __merge-imp-00 dest))
		  (let ((val1 *itr1)
				(val2 *itr2))
			(_= *dest (if (funcall comp val2 val1)
						  (progn ++itr2 val2)
						  (progn ++itr1 val1)))
			++dest))))

  ;;IMP; merge : 01 -   i  x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __merge-imp-01 (first1 last1 first2 last2 out less-bf)
	(declare (type cl:list out))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1) (itr2 @~first2)) t nil)
		  (when (_== itr1 last1)
			(for (nil (_/= itr2 last2) (progn ++itr2 (setf out (cdr out))))
			  (_= (car out) *itr2))
			(return-from __merge-imp-01 out))
		  (when (_== itr2 last2)
			(for (nil (_/= itr1 last1) (progn ++itr1 (setf out (cdr out))))
			  (_= (car out) *itr1))
			(return-from __merge-imp-01 out))
		  (let ((val1 *itr1)
				(val2 *itr2))
			(_= (car out) (if (funcall less-bf val2 val1)
							  (progn ++itr2 val2)
							  (progn ++itr1 val1)))
			(setf out (cdr out))))))
	
  ;;IMP; merge : 02 -   i  x  i  x  vp
  (defun __merge-imp-02 (first1 last1 first2 last2 out-idx out-buf less-bf)
	(declare (type fixnum out-idx))
	(declare (type cl:vector out-buf))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1) (itr2 @~first2)) t nil)
		  (when (_== itr1 last1)
			(for (nil (_/= itr2 last2) (progn ++itr2 (incf out-idx)))
			  (_= (aref out-buf out-idx) *itr2))
			(return-from __merge-imp-02 out-idx))
		  (when (_== itr2 last2)
			(for (nil (_/= itr1 last1) (progn ++itr1 (incf out-idx)))
			  (_= (aref out-buf out-idx) *itr1))
			(return-from __merge-imp-02 out-idx))
		  (let ((val1 *itr1)
				(val2 *itr2))
			(_= (aref out-buf out-idx) (if (funcall less-bf val2 val1)
										   (progn ++itr2 val2)
										   (progn ++itr1 val1)))
			(incf out-idx)))))

  ;;IMP; merge : 03 -   i  x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __merge-imp-03 (first1 last1 cons2 last2 oitr less-bf)
	(declare (type cl:list cons2 last2))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)) t nil)
		  (when (_== itr1 last1)
			(for (nil (not (eq cons2 last2)) (progn (setf cons2 (cdr cons2)) ++oitr))
			  (_= *oitr (car cons2)))
			(return-from __merge-imp-03 oitr))
		  (when (eq cons2 last2)
			(for (nil (_/= itr1 last1) (progn ++itr1 ++oitr))
			  (_= *oitr *itr1))
			(return-from __merge-imp-03 oitr))
		  (let ((val1 *itr1)
				(val2 (car cons2)))
			(_= *oitr (if (funcall less-bf val2 val1)
						  (progn (setf cons2 (cdr cons2)) val2)
						  (progn ++itr1                   val1)))
			++oitr))))

  ;;IMP; merge : 04 -   i  x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __merge-imp-04 (first1 last1 cons2 last2 out less-bf)
	(declare (type cl:list cons2 last2 out))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)) t nil)
		  (when (_== itr1 last1)
			(for (nil (not (eq cons2 last2)) (progn (setf cons2 (cdr cons2)) (setf out (cdr out))))
			  (_= (car out) (car cons2)))
			(return-from __merge-imp-04 out))
		  (when (eq cons2 last2)
			(for (nil (_/= itr1 last1) (progn ++itr1 (setf out (cdr out))))
			  (_= (car out) *itr1))
			(return-from __merge-imp-04 out))
		  (let ((val1 *itr1)
				(val2 (car cons2)))
			(_= (car out) (if (funcall less-bf val2 val1)
							  (progn (setf cons2 (cdr cons2)) val2)
							  (progn ++itr1                   val1)))
			(setf out (cdr out))))))

  ;;IMP; merge : 05 -   i  x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __merge-imp-05 (first1 last1 cons2 last2 out-idx out-buf less-bf)
	(declare (type cl:list cons2 last2))
	(declare (type fixnum out-idx))
	(declare (type cl:vector out-buf))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)) t nil)
		  (when (_== itr1 last1)
			(for (nil (not (eq cons2 last2)) (progn (setf cons2 (cdr cons2)) (incf out-idx)))
			  (_= (aref out-buf out-idx) (car cons2)))
			(return-from __merge-imp-05 out-idx))
		  (when (eq cons2 last2)
			(for (nil (_/= itr1 last1) (progn ++itr1 (incf out-idx)))
			  (_= (aref out-buf out-idx) *itr1))
			(return-from __merge-imp-05 out-idx))
		  (let ((val1 *itr1)
				(val2 (car cons2)))
			(_= (aref out-buf out-idx) (if (funcall less-bf val2 val1)
										   (progn (setf cons2 (cdr cons2)) val2)
										   (progn ++itr1      val1)))
			(incf out-idx)))))

  ;;IMP; merge : 06 -   i  x cvp x  o 
  (defun __merge-imp-06 (first1 last1 idx2 last2 buffer2 oitr less-bf)
	(declare (type fixnum idx2 last2))
	(declare (type cl:vector buffer2))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)) t nil)
		  (when (_== itr1 last1)
			(for (nil (< idx2 last2) (progn (incf idx2) ++oitr))
			  (_= *oitr (aref buffer2 idx2)))
			(return-from __merge-imp-06 oitr))
		  (when (= idx2 last2)
			(for (nil (_/= itr1 last1) (progn ++itr1 ++oitr))
			  (_= *oitr *itr1))
			(return-from __merge-imp-06 oitr))
		  (let ((val1 *itr1)
				(val2 (aref buffer2 idx2)))
			(_= *oitr (if (funcall less-bf val2 val1)
						  (progn (incf idx2) val2)
						  (progn ++itr1      val1)))
			++oitr))))

  ;;IMP; merge : 07 -   i  x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __merge-imp-07 (first1 last1 idx2 last2 buffer2 out less-bf)
	(declare (type fixnum idx2 last2))
	(declare (type cl:vector buffer2))
	(declare (type cl:list out))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)) t nil)
		  (when (_== itr1 last1)
			(for (nil (< idx2 last2) (progn (incf idx2) (setf out (cdr out))))
			  (_= (car out) (aref buffer2 idx2)))
			(return-from __merge-imp-07 out))
		  (when (= idx2 last2)
			(for (nil (_/= itr1 last1) (progn ++itr1 (setf out (cdr out))))
			  (_= (car out) *itr1))
			(return-from __merge-imp-07 out))
		  (let ((val1 *itr1)
				(val2 (aref buffer2 idx2)))
			(_= (car out) (if (funcall less-bf val2 val1)
							  (progn (incf idx2) val2)
							  (progn ++itr1      val1)))
			(setf out (cdr out))))))

  ;;IMP; merge : 08 -   i  x cvp x  vp
  (defun __merge-imp-08 (first1 last1 idx2 last2 buffer2 out-idx out-buf less-bf)
	(declare (type fixnum idx2 last2 out-idx))
	(declare (type cl:vector buffer2 out-buf))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)) t nil)
		  (when (_== itr1 last1)
			(for (nil (< idx2 last2) (progn (incf idx2) (incf out-idx)))
			  (_= (aref out-buf out-idx) (aref buffer2 idx2)))
			(return-from __merge-imp-08 out-idx))
		  (when (= idx2 last2)
			(for (nil (_/= itr1 last1) (progn ++itr1 (incf out-idx)))
			  (_= (aref out-buf out-idx) *itr1))
			(return-from __merge-imp-08 out-idx))
		  (let ((val1 *itr1)
				(val2 (aref buffer2 idx2)))
			(_= (aref out-buf out-idx) (if (funcall less-bf val2 val1)
										   (progn (incf idx2) val2)
										   (progn ++itr1      val1)))
			(incf out-idx)))))

  ;;IMP; merge : 09 -  cci x  i  x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __merge-imp-09 (cons1 last1 first2 last2 oitr less-bf)
	(declare (type cl:list cons1 last1))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr2 @~first2)) t nil)
		  (when (eq cons1 last1)
			(for (nil (_/= itr2 last2) (progn ++itr2 ++oitr))
			  (_= *oitr *itr2))
			(return-from __merge-imp-09 oitr))
		  (when (_== itr2 last2)
			(for (nil (not (eq cons1 last1)) (progn (setf cons1 (cdr cons1)) ++oitr))
			  (_= *oitr (car cons1)))
			(return-from __merge-imp-09 oitr))
		  (let ((val1 (car cons1))
				(val2 *itr2))
			(_= *oitr (if (funcall less-bf val2 val1)
						  (progn ++itr2      val2)
						  (progn (setf cons1 (cdr cons1)) val1)))
			++oitr))))

  ;;IMP; merge : 10 -  cci x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __merge-imp-10 (cons1 last1 first2 last2 out less-bf)
	(declare (type cl:list cons1 last1 out))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr2 @~first2)) t nil)
		  (when (eq cons1 last1)
			(for (nil (_/= itr2 last2) (progn ++itr2 (setf out (cdr out))))
			  (_= (car out) *itr2))
			(return-from __merge-imp-10 out))
		  (when (_== itr2 last2)
			(for (nil (not (eq cons1 last1)) (progn (setf cons1 (cdr cons1)) (setf out (cdr out))))
			  (_= (car out) (car cons1)))
			(return-from __merge-imp-10 out))
		  (let ((val1 (car cons1))
				(val2 *itr2))
			(_= (car out) (if (funcall less-bf val2 val1)
							  (progn ++itr2      val2)
							  (progn (setf cons1 (cdr cons1)) val1)))
			(setf out (cdr out))))))

  ;;IMP; merge : 11 -  cci x  i  x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __merge-imp-11 (cons1 last1 first2 last2 out-idx out-buf less-bf)
	(declare (type cl:list cons1 last1))
	(declare (type fixnum out-idx))
	(declare (type cl:vector out-buf))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr2 @~first2)) t nil)
		  (when (eq cons1 last1)
			(for (nil (_/= itr2 last2) (progn ++itr2 (incf out-idx)))
			  (_= (aref out-buf out-idx) *itr2))
			(return-from __merge-imp-11 out-idx))
		  (when (_== itr2 last2)
			(for (nil (not (eq cons1 last1)) (progn (setf cons1 (cdr cons1)) (incf out-idx)))
			  (_= (aref out-buf out-idx) (car cons1)))
			(return-from __merge-imp-11 out-idx))
		  (let ((val1 (car cons1))
				(val2 *itr2))
			(_= (aref out-buf out-idx) (if (funcall less-bf val2 val1)
										   (progn ++itr2      val2)
										   (progn (setf cons1 (cdr cons1)) val1)))
			(incf out-idx)))))
  
  ;;IMP; merge : 12 -  cci x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __merge-imp-12 (cons1 last1 cons2 last2 oitr less-bf)
	(declare (type cl:list cons1 last1 cons2 last2))
	(declare (type cl:function less-bf))
	(with-operators
		(for (nil t nil)
		  (when (eq cons1 last1)
			(for (nil (not (eq cons2 last2)) (progn (setf cons2 (cdr cons2)) ++oitr))
			  (_= *oitr (car cons2)))
			(return-from __merge-imp-12 oitr))
		  (when (eq cons2 last2)
			(for (nil (not (eq cons1 last1)) (progn (setf cons1 (cdr cons1)) ++oitr))
			  (_= *oitr (car cons1)))
			(return-from __merge-imp-12 oitr))
		  (let ((val1 (car cons1))
				(val2 (car cons2)))
			(_= *oitr (if (funcall less-bf val2 val1)
						  (progn (setf cons2 (cdr cons2)) val2)
						  (progn (setf cons1 (cdr cons1)) val1)))
			++oitr))))

  ;;IMP; merge : 13 -  cci x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __merge-imp-13 (cons1 last1 cons2 last2 out less-bf)
	(declare (type cl:list cons1 last1 cons2 last2 out))
	(declare (type cl:function less-bf))
	(for (nil t nil)
	  (when (eq cons1 last1)
		(for (nil (not (eq cons2 last2)) (progn (setf cons2 (cdr cons2))
												(setf   out (cdr   out))))
		  (_= (car out) (car cons2)))
		(return-from __merge-imp-13 out))
	  (when (eq cons2 last2)
		(for (nil (not (eq cons1 last1)) (progn (setf cons1 (cdr cons1)) (setf out (cdr out))))
		  (_= (car out) (car cons1)))
		(return-from __merge-imp-13 out))
	  (let ((val1 (car cons1))
			(val2 (car cons2)))
		(_= (car out) (if (funcall less-bf val2 val1)
						  (progn (setf cons2 (cdr cons2)) val2)
						  (progn (setf cons1 (cdr cons1)) val1)))
		(setf out (cdr out)))))

  ;;IMP; merge : 14 -  cci x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __merge-imp-14 (cons1 last1 cons2 last2 out-idx out-buf less-bf)
	(declare (type cl:list     cons1 last1 cons2 last2))
	(declare (type fixnum      out-idx))
	(declare (type cl:vector   out-buf))
	(declare (type cl:function less-bf))
	(for (nil t nil)
	  (when (eq cons1 last1)
		(for (nil (not (eq cons2 last2)) (progn (setf cons2 (cdr cons2)) (incf out-idx)))
		  (_= (aref out-buf out-idx) (car cons2)))
		(return-from __merge-imp-14 out-idx))
	  (when (eq cons2 last2)
		(for (nil (not (eq cons1 last1)) (progn (setf cons1 (cdr cons1)) (incf out-idx)))
		  (_= (aref out-buf out-idx) (car cons1)))
		(return-from __merge-imp-14 out-idx))
	  (let ((val1 (car cons1))
			(val2 (car cons2)))
		(_= (aref out-buf out-idx) (if (funcall less-bf val2 val1)
									   (progn (setf cons2 (cdr cons2)) val2)
									   (progn (setf cons1 (cdr cons1)) val1)))
		(incf out-idx))))
  
  ;;IMP; merge : 15 -  cci x cvp x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __merge-imp-15 (cons1 last1 idx2 last2 buffer2 oitr less-bf)
	(declare (type cl:list cons1 last1))
	(declare (type fixnum idx2 last2))
	(declare (type cl:vector buffer2))
	(declare (type cl:function less-bf))
	(with-operators
		(for (nil t nil)
		  (when (eq cons1 last1)
			(for (nil (< idx2 last2) (progn (incf idx2) ++oitr))
			  (_= *oitr (aref buffer2 idx2)))
			(return-from __merge-imp-15 oitr))
		  (when (= idx2 last2)
			(for (nil (not (eq cons1 last1)) (progn (setf cons1 (cdr cons1)) ++oitr))
			  (_= *oitr (car cons1)))
			(return-from __merge-imp-15 oitr))
		  (let ((val1 (car cons1))
				(val2 (aref buffer2 idx2)))
			(_= *oitr (if (funcall less-bf val2 val1)
						  (progn (incf idx2) val2)
						  (progn (setf cons1 (cdr cons1)) val1)))
			++oitr))))

  ;;IMP; merge : 16 -  cci x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __merge-imp-16 (cons1 last1 idx2 last2 buffer2 out less-bf)
	(declare (type cl:list cons1 last1 out))
	(declare (type fixnum idx2 last2))
	(declare (type cl:vector buffer2))
	(declare (type cl:function less-bf))
	(for (nil t nil)
	  (when (eq cons1 last1)
		(for (nil (< idx2 last2) (progn (incf idx2) (setf out (cdr out))))
		  (_= (car out) (aref buffer2 idx2)))
		(return-from __merge-imp-16 out))
	  (when (= idx2 last2)
		(for (nil (not (eq cons1 last1)) (progn (setf cons1 (cdr cons1)) (setf out (cdr out))))
		  (_= (car out) (car cons1)))
		(return-from __merge-imp-16 out))
	  (let ((val1 (car cons1))
			(val2 (aref buffer2 idx2)))
		(_= (car out) (if (funcall less-bf val2 val1)
						  (progn (incf idx2) val2)
						  (progn (setf cons1 (cdr cons1)) val1)))
		(setf out (cdr out)))))

  ;;IMP; merge : 17 -  cci x cvp x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __merge-imp-17 (cons1 last1 idx2 last2 buffer2 out-idx out-buf less-bf)
	(declare (type cl:list cons1 last1))
	(declare (type fixnum idx2 last2 out-idx))
	(declare (type cl:vector buffer2 out-buf))
	(declare (type cl:function less-bf))
	(for (nil t nil)
	  (when (eq cons1 last1)
		(for (nil (< idx2 last2) (progn (incf idx2) (incf out-idx)))
		  (_= (aref out-buf out-idx) (aref buffer2 idx2)))
		(return-from __merge-imp-17 out-idx))
	  (when (= idx2 last2)
		(for (nil (not (eq cons1 last1)) (progn (setf cons1 (cdr cons1)) (incf out-idx)))
		  (_= (aref out-buf out-idx) (car cons1)))
		(return-from __merge-imp-17 out-idx))
	  (let ((val1 (car cons1))
			(val2 (aref buffer2 idx2)))
		(_= (aref out-buf out-idx) (if (funcall less-bf val2 val1)
									   (progn (incf idx2) val2)
									   (progn (setf cons1 (cdr cons1)) val1)))
		(incf out-idx))))

  ;;IMP; merge : 18 -  cvp x  i  x  o 
  (defun __merge-imp-18 (idx1 last1 buffer1 first2 last2 oitr less-bf)
	(declare (type fixnum idx1 last1))
	(declare (type cl:vector buffer1))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr2 @~first2)) t nil)
		  (when (= idx1 last1)
			(for (nil (_/= itr2 last2) (progn ++itr2 ++oitr))
			  (_= *oitr *itr2))
			(return-from __merge-imp-18 oitr))
		  (when (_== itr2 last2)
			(for (nil (< idx1 last1) (progn (incf idx1) ++oitr))
			  (_= *oitr (aref buffer1 idx1)))
			(return-from __merge-imp-18 oitr))
		  (let ((val1 (aref buffer1 idx1))
				(val2 *itr2))
			(_= *oitr (if (funcall less-bf val2 val1)
						  (progn ++itr2      val2)
						  (progn (incf idx1) val1)))
			++oitr))))

  ;;IMP; merge : 19 -  cvp x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __merge-imp-19 (idx1 last1 buffer1 first2 last2 out less-bf)
	(declare (type fixnum idx1 last1))
	(declare (type cl:vector buffer1))
	(declare (type cl:list out))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr2 @~first2)) t nil)
		  (when (= idx1 last1)
			(for (nil (_/= itr2 last2) (progn ++itr2 (setf out (cdr out))))
			  (_= (car out) *itr2))
			(return-from __merge-imp-19 out))
		  (when (_== itr2 last2)
			(for (nil (< idx1 last1) (progn (incf idx1) (setf out (cdr out))))
			  (_= (car out) (aref buffer1 idx1)))
			(return-from __merge-imp-19 out))
		  (let ((val1 (aref buffer1 idx1))
				(val2 *itr2))
			(_= (car out) (if (funcall less-bf val2 val1)
							  (progn ++itr2      val2)
							  (progn (incf idx1) val1)))
			(setf out (cdr out))))))

  ;;IMP; merge : 20 -  cvp x  i  x  vp
  (defun __merge-imp-20 (idx1 last1 buffer1 first2 last2 out-idx out-buf less-bf)
	(declare (type fixnum idx1 last1 out-idx))
	(declare (type cl:vector buffer1 out-buf))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr2 @~first2)) t nil)
		  (when (= idx1 last1)
			(for (nil (_/= itr2 last2) (progn ++itr2 (incf out-idx)))
			  (_= (aref out-buf out-idx) *itr2))
			(return-from __merge-imp-20 out-idx))
		  (when (_== itr2 last2)
			(for (nil (< idx1 last1) (progn (incf idx1) (incf out-idx)))
			  (_= (aref out-buf out-idx) (aref buffer1 idx1)))
			(return-from __merge-imp-20 out-idx))
		  (let ((val1 (aref buffer1 idx1))
				(val2 *itr2))
			(_= (aref out-buf out-idx) (if (funcall less-bf val2 val1)
										   (progn ++itr2      val2)
										   (progn (incf idx1) val1)))
			(incf out-idx)))))
  
  ;;IMP; merge : 21 -  cvp x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __merge-imp-21 (idx1 last1 buffer1 cons2 last2 oitr less-bf)
	(declare (type fixnum idx1 last1))
	(declare (type cl:list cons2 last2))
	(declare (type cl:vector buffer1))
	(declare (type cl:function less-bf))
	(with-operators
		(for (nil t nil)
		  (when (= idx1 last1)
			(for (nil (not (eq cons2 last2)) (progn (setf cons2 (cdr cons2)) ++oitr))
			  (_= *oitr (car cons2)))
			(return-from __merge-imp-21 oitr))
		  (when (eq cons2 last2)
			(for (nil (< idx1 last1) (progn (incf idx1) ++oitr))
			  (_= *oitr (aref buffer1 idx1)))
			(return-from __merge-imp-21 oitr))
		  (let ((val1 (aref buffer1 idx1))
				(val2 (car cons2)))
			(_= *oitr (if (funcall less-bf val2 val1)
						  (progn (setf cons2 (cdr cons2)) val2)
						  (progn (incf idx1) val1)))
			++oitr))))

  ;;IMP; merge : 22 -  cvp x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __merge-imp-22 (idx1 last1 buffer1 cons2 last2 out less-bf)
	(declare (type fixnum idx1 last1))
	(declare (type cl:vector buffer1))
	(declare (type cl:list cons2 last2 out))
	(declare (type cl:function less-bf))
	(for (nil t nil)
	  (when (= idx1 last1)
		(for (nil (not (eq cons2 last2)) (progn (setf cons2 (cdr cons2))
												(setf   out (cdr   out))))
		  (_= (car out) (car cons2)))
		(return-from __merge-imp-22 out))
	  (when (eq cons2 last2)
		(for (nil (< idx1 last1) (progn (incf idx1) (setf out (cdr out))))
		  (_= (car out) (aref buffer1 idx1)))
		(return-from __merge-imp-22 out))
	  (let ((val1 (aref buffer1 idx1))
			(val2 (car cons2)))
		(_= (car out) (if (funcall less-bf val2 val1)
						  (progn (setf cons2 (cdr cons2)) val2)
						  (progn (incf idx1) val1)))
		(setf out (cdr out)))))

  ;;IMP; merge : 23 -  cvp x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __merge-imp-23 (idx1 last1 buffer1 cons2 last2 out-idx out-buf less-bf)
	(declare (type fixnum idx1 last1 out-idx))
	(declare (type cl:list cons2 last2))
	(declare (type cl:vector buffer1 out-buf))
	(declare (type cl:function less-bf))
	(for (nil t nil)
	  (when (= idx1 last1)
		(for (nil (not (eq cons2 last2)) (progn (setf cons2 (cdr cons2)) (incf out-idx)))
		  (_= (aref out-buf out-idx) (car cons2)))
		(return-from __merge-imp-23 out-idx))
	  (when (eq cons2 last2)
		(for (nil (< idx1 last1) (progn (incf idx1) (incf out-idx)))
		  (_= (aref out-buf out-idx) (aref buffer1 idx1)))
		(return-from __merge-imp-23 out-idx))
	  (let ((val1 (aref buffer1 idx1))
			(val2 (car cons2)))
		(_= (aref out-buf out-idx) (if (funcall less-bf val2 val1)
									   (progn (setf cons2 (cdr cons2)) val2)
									   (progn (incf idx1) val1)))
		(incf out-idx))))
  
  ;;IMP; merge : 24 -  cvp x cvp x  o 
  (defun __merge-imp-24 (idx1 last1 buffer1 idx2 last2 buffer2 oitr less-bf)
	(declare (type fixnum idx1 last1 idx2 last2))
	(declare (type cl:vector buffer1 buffer2))
	(declare (type cl:function less-bf))
	(with-operators
		(for (nil t nil)
		  (when (= idx1 last1)
			(for (nil (< idx2 last2) (progn (incf idx2) ++oitr))
			  (_= *oitr (aref buffer2 idx2)))
			(return-from __merge-imp-24 oitr))
		  (when (= idx2 last2)
			(for (nil (< idx1 last1) (progn (incf idx1) ++oitr))
			  (_= *oitr (aref buffer1 idx1)))
			(return-from __merge-imp-24 oitr))
		  (let ((val1 (aref buffer1 idx1))
				(val2 (aref buffer2 idx2)))
			(_= *oitr (if (funcall less-bf val2 val1)
						  (progn (incf idx2) val2)
						  (progn (incf idx1) val1)))
			++oitr))))

  ;;IMP; merge : 25 -  cvp x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __merge-imp-25 (idx1 last1 buffer1 idx2 last2 buffer2 out less-bf)
	(declare (type fixnum idx1 last1 idx2 last2))
	(declare (type cl:vector buffer1 buffer2))
	(declare (type cl:list out))
	(declare (type cl:function less-bf))
	(for (nil t nil)
	  (when (= idx1 last1)
		(for (nil (< idx2 last2) (progn (incf idx2) (setf out (cdr out))))
		  (_= (car out) (aref buffer2 idx2)))
		(return-from __merge-imp-25 out))
	  (when (= idx2 last2)
		(for (nil (< idx1 last1) (progn (incf idx1) (setf out (cdr out))))
		  (_= (car out) (aref buffer1 idx1)))
		(return-from __merge-imp-25 out))
	  (let ((val1 (aref buffer1 idx1))
			(val2 (aref buffer2 idx2)))
		(_= (car out) (if (funcall less-bf val2 val1)
						  (progn (incf idx2) val2)
						  (progn (incf idx1) val1)))
		(setf out (cdr out)))))

  ;;IMP; merge : 26 -  cvp x cvp x  vp
  (defun __merge-imp-26 (idx1 last1 buffer1 idx2 last2 buffer2 out-idx out-buf less-bf)
	(declare (type fixnum idx1 last1 idx2 last2 out-idx))
	(declare (type cl:vector buffer1 buffer2 out-buf))
	(declare (type cl:function less-bf))
	(for (nil t nil)
	  (when (= idx1 last1)
		(for (nil (< idx2 last2) (progn (incf idx2) (incf out-idx)))
		  (_= (aref out-buf out-idx) (aref buffer2 idx2)))
		(return-from __merge-imp-26 out-idx))
	  (when (= idx2 last2)
		(for (nil (< idx1 last1) (progn (incf idx1) (incf out-idx)))
		  (_= (aref out-buf out-idx) (aref buffer1 idx1)))
		(return-from __merge-imp-26 out-idx))
	  (let ((val1 (aref buffer1 idx1))
			(val2 (aref buffer2 idx2)))
		(_= (aref out-buf out-idx) (if (funcall less-bf val2 val1)
									   (progn (incf idx2) val2)
									   (progn (incf idx1) val1)))
		(incf out-idx)))))


;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  (defun __push_heap (rdm-itr idx1 idx2 val less-bf)
	(declare (type fixnum idx1 idx2))
	(declare (type cl:function less-bf))
	(let ((index (ash (1- idx1) -1)))
	  (declare (type fixnum index))
	  (with-operators
		  (for (nil (< idx2 idx1) nil)
			(unless (funcall less-bf rdm-itr[index] val)
			  (return nil))
			(_= rdm-itr[idx1] rdm-itr[index])
			(setf idx1 index)
			(setf index (ash (1- idx1) -1)))
		(_= rdm-itr[idx1] val))))

  ;;IMP; push_heap : 0 -  r
  (defun __push_heap-imp-0 (first last comp)
	(declare (type cl:function comp))
	(with-operators
		(let ((tmp nil))
		  (_= tmp last[-1])
		  (__push_heap first
					   (the fixnum (1- (the fixnum (_- last first))))
					   0 tmp comp)))
	nil)

  ;;IMP; push_heap : 1 -  vp
  (defun __push_heap-imp-1 (itr-buf itr-idx idx1 idx2 val less-bf)
	(declare (type fixnum itr-idx idx1 idx2))
	(declare (type cl:vector itr-buf))
	(declare (type cl:function less-bf))
	(let ((half (ash (1- idx1) -1)))
	  (declare (type fixnum half))
	  (for (nil (< idx2 idx1) nil)
		(unless (funcall less-bf (aref itr-buf (the fixnum (+ itr-idx half))) val)
		  (return nil))
		(_= (aref itr-buf (the fixnum (+ itr-idx idx1)))
			(aref itr-buf (the fixnum (+ itr-idx half))))
		(setf idx1 half)
		(setf half (ash (1- idx1) -1))))
	(_= (aref itr-buf (the fixnum (+ itr-idx idx1))) val)))


;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  (defun __adjust-heap-0 (rdm-itr half dist v less-bf)
	(declare (type fixnum half dist))
	(declare (type cl:function less-bf))
	(let ((org-hlf half)
		  (idx     (the fixnum (+ 2 (the fixnum (* 2 half))))))
	  (declare (type fixnum org-hlf idx))
	  (with-operators
		  (for (nil (< idx dist) nil)
			(when (funcall less-bf rdm-itr[idx]
						   (_[] rdm-itr (the fixnum (1- idx))))
			  (decf idx))
			(_= rdm-itr[half] rdm-itr[idx])
			(setf half idx)
			(setf idx (the fixnum (+ 2 (the fixnum (* idx 2))))))
		(when (= idx dist)
		  (_= rdm-itr[half] (_[] rdm-itr (the fixnum (1- idx))))
		  (setf half (the fixnum (1- idx))))
		(__push_heap rdm-itr half org-hlf v less-bf)))))


(locally (declare (optimize speed))
  (defun __adjust-heap-1 (itr-buf itr-idx half dist v less-bf)
	(declare (type fixnum itr-idx half dist))
	(declare (type cl:vector itr-buf))
	(declare (type cl:function less-bf))
	(let ((org-hlf half)
		  (idx     (the fixnum (+ 2 (the fixnum (* 2 half))))))
	  (declare (type fixnum org-hlf idx))
	  (for (nil (< idx dist) nil)
		(when (funcall less-bf (aref itr-buf (the fixnum (+ itr-idx idx)))
							   (aref itr-buf (the fixnum (+ itr-idx (the fixnum (1- idx))))))
		  (decf idx))
		(_= (aref itr-buf (the fixnum (+ itr-idx half)))
			(aref itr-buf (the fixnum (+ itr-idx  idx))))
		(setf half idx)
		(setf idx (the fixnum (+ 2 (the fixnum (* idx 2))))))
	  (when (= idx dist)
		(_= (aref itr-buf (the fixnum (+ itr-idx half)))
			(aref itr-buf (the fixnum (+ itr-idx (the fixnum (1- idx))))))
		(setf half (the fixnum (1- idx))))
	  (__push_heap-imp-1 itr-buf itr-idx half org-hlf v less-bf))))

;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  ;;IMP; pop_heap : 0 -  r
  (defun __pop_heap-imp-0 (rdm-itr1 rdm-itr2 rdm-itr3 v less-bf)
	(declare (type cl:function less-bf))
	(with-operators
		(_= *rdm-itr3 *rdm-itr1)
		(__adjust-heap-0 rdm-itr1 0 (the fixnum (_- rdm-itr2 rdm-itr1)) v less-bf)))

  ;;IMP; pop_heap : 1 -  vp
  (defun __pop_heap-imp-1 (itr-buf itr-idx1 itr-idx2 itr-idx3 v less-bf)
	(declare (type cl:vector itr-buf))
	(declare (type fixnum itr-idx1 itr-idx2 itr-idx3))
	(declare (type cl:function less-bf))
	(_= (aref itr-buf itr-idx3) (aref itr-buf itr-idx1))
	(__adjust-heap-1 itr-buf itr-idx1 0 (the fixnum (- itr-idx2 itr-idx1)) v less-bf)))


;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  ;;IMP; make_heap : 0 -  r
  (defun __make_heap-imp-0 (first last comp)
	(declare (type cl:function comp))
	(let ((dist (the fixnum (_- last first))))
	  (declare (type fixnum dist))
	  (when (< 1 dist)
		(with-operators
			(for (((tmp nil) (half (ash dist -1))) (< 0 half) nil)
			  (decf half)
			  (_= tmp first[half])
			  (__adjust-heap-0 first half dist tmp comp))))))

  ;;IMP; make_heap : 1 -  vp
  (defun __make_heap-imp-1 (idx1 idx2 buffer less-bf)
	(declare (type fixnum idx1 idx2))
	(declare (type cl:vector buffer))
	(declare (type cl:function less-bf))
	(let ((dist (- idx2 idx1)))
	  (declare (type fixnum dist))
	  (if (< dist 2)
		  nil
		  (let ((half (ash dist -1)))
			(declare (type fixnum half))
			(for (((tmp nil)) (< 0 half) nil)
			  (decf half)
			  (_= tmp (aref buffer (the fixnum (+ idx1 half))))
			  (__adjust-heap-1 buffer idx1 half dist tmp less-bf)))))))


;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  ;;IMP; sort_heap : 0 -  r
  (defun __sort_heap-imp-0 (first last comp)
	(declare (type cl:function comp))
	(with-operators
		(for (((itr @~last)) (< 1 (the fixnum (_- itr first))) --itr)
		  (__pop_heap-imp-0 first (_- itr 1) (_- itr 1) itr[-1] comp))))

  ;;IMP; sort_heap : 1 -  vp
  (defun __sort_heap-imp-1 (idx1 idx2 buffer less-bf)
	(declare (type fixnum idx1 idx2))
	(declare (type cl:vector buffer))
	(declare (type cl:function less-bf))
	(for (((tmp nil)) (< 1 (- idx2 idx1)) (decf idx2))
	  (_= tmp (aref buffer (the fixnum (1- idx2))))
	  (__pop_heap-imp-1 buffer idx1
						(the fixnum (1- idx2))
						(the fixnum (1- idx2)) tmp less-bf))))




;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  (defun __unguarded-insert-0 (bid-itr val less-fnc)
	(declare (type cl:function less-fnc))
	(with-operators
		(let ((itr (for (((itr1 @~bid-itr)
						  (itr2 (prev bid-itr))) (funcall less-fnc val *itr2)
												 (progn --itr1 --itr2) :returns itr1)
					 (_= *itr1 *itr2))))
		  (_= *itr val))))

  (defun __unguarded-insert-1 (idx buffer val less-bf)
	(declare (type fixnum idx))
	(declare (type cl:vector buffer))
	(declare (type cl:function less-bf))
	(let ((ret (let ((idx1 idx)
					 (idx2 (1- idx)))
				 (declare (type fixnum idx1 idx2))
				 (for (nil (funcall less-bf val (aref buffer idx2))
						   (progn (decf idx1) (decf idx2)) :returns idx1)
				   (_= (aref buffer idx1) (aref buffer idx2))))))
	  (declare (type fixnum ret))
	  (_= (aref buffer ret) val))))


;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  (defun __insertion-sort-0 (rdm-itr1 rdm-itr2 less-bf)
	(declare (type cl:function less-bf))
	(if (_== rdm-itr1 rdm-itr2)
		nil
		(with-operators
			(for (((tmp nil) (itr (next rdm-itr1))) (_/= itr rdm-itr2) ++itr)
			  (_= tmp *itr)
			  (if (not (funcall less-bf tmp *rdm-itr1))
				  (__unguarded-insert-0 itr tmp less-bf)
				  (progn
					(__copy_backward-imp-0 rdm-itr1 itr (_+ itr 1))
					(_= *rdm-itr1 tmp)))))))

  (defun __insertion-sort-1 (idx1 idx2 buffer less-bf)
	(declare (type fixnum idx1 idx2))
	(declare (type cl:vector buffer))
	(declare (type cl:function less-bf))
	(if (= idx1 idx2)
		nil
		(let ((idx (1+ idx1)))
		  (declare (type fixnum idx))
		  (for (((tmp nil)) (< idx idx2) (incf idx))
			(_= tmp (aref buffer idx))
			(if (not (funcall less-bf tmp (aref buffer idx1)))
				(__unguarded-insert-1 idx buffer tmp less-bf)
				(progn
				  (__copy_backward-imp-3 idx1 idx buffer (1+ idx) buffer)
				  (_= (aref buffer idx1) tmp))))))))


;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  (defun __unguarded-partition-0 (rdm-itr1 rdm-itr2 v less-bf)
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~rdm-itr1) (itr2 @~rdm-itr2)) t ++itr1)
		  (for (nil (funcall less-bf *itr1 v) nil)
			++itr1)
		  --itr2
		  (for (nil (funcall less-bf v *itr2) nil)
			--itr2)
		  (when (<= (the fixnum (_- itr2 itr1)) 0)
			(return-from __unguarded-partition-0 itr1))
		  (swap *itr1 *itr2))))

  (defun __unguarded-partition-1 (idx1 idx2 buffer val less-bf)
	(declare (type fixnum idx1 idx2))
	(declare (type cl:vector buffer))
	(declare (type cl:function less-bf))
	(for (nil t (incf idx1))
	  (for (nil (funcall less-bf (aref buffer idx1) val) nil)
		(incf idx1))
	  (decf idx2)
	  (for (nil (funcall less-bf val (aref buffer idx2)) nil)
		(decf idx2))
	  (when (<= (- idx2 idx1) 0)
		(return-from __unguarded-partition-1 idx1))
	  (swap (aref buffer idx1) (aref buffer idx2)))))



;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  ;; begin1, end1, begin2, end2, and oitr must be bidirectional_iterator
  (defun __merge-backward-0 (begin1 end1 begin2 end2 oitr less-bf)
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~end1) (itr2 @~end2) (oitr @~oitr)) t nil)
		  (when (_== begin1 itr1)
			(return-from __merge-backward-0 (__copy_backward-imp-0 begin2 itr2 oitr)))
		  (when (_== begin2 itr2)
			(return-from __merge-backward-0 (__copy_backward-imp-0 begin1 itr1 oitr)))
		  --oitr
		  --itr2
		  --itr1
		  (if (funcall less-bf *itr2 *itr1)
			  (progn (_= *oitr *itr1) --itr2)
			  (progn (_= *oitr *itr2) --itr1)))))

  (defun __merge-backward-1 (begin1 end1 buf1 begin2 end2 buf2 out-idx out-buf less-bf)
	(declare (type fixnum begin1 end1 begin2 end2 out-idx))
	(declare (type cl:vector buf1 buf2 out-buf))
	(declare (type cl:function less-bf))
	(let ((idx1 end1)
		  (idx2 end2))
	  (declare (type fixnum idx1 idx2))
	  (for (nil t nil)
		(when (= begin1 idx1)
		  (return-from __merge-backward-1 (__copy_backward-imp-3 begin2 idx2 buf2 out-idx out-buf)))
		(when (= begin2 idx2)
		  (return-from __merge-backward-1 (__copy_backward-imp-3 begin1 idx1 buf1 out-idx out-buf)))
		(decf out-idx)
		(if (funcall less-bf (aref buf2 (decf idx2))
							 (aref buf1 (decf idx1)))
			(progn (_= (aref out-buf out-idx) (aref buf1 idx1)) (decf idx2))
			(progn (_= (aref out-buf out-idx) (aref buf2 idx2)) (decf idx1)))))))


;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  ;; itr1, itr2, and itr3 must be bidirectional_iterator
  (defun __buffered-rotate-0 (itr1 itr2 itr3 d1 d2 tmpitr)
	(declare (type fixnum d1 d2))
	(if (and (<= d1 d2) (<= d1 (the fixnum (tmpitr-maxlen tmpitr))))
		(progn
		  ;; MEMO : intentionally use generic 'copy' & 'copy_backward' function.
		  (copy itr1 itr2 (tmpitr-init tmpitr))
		  (copy itr2 itr3 itr1)
		  (copy_backward (begin tmpitr) (end tmpitr) itr3))
		(if (<= d2 (the fixnum (tmpitr-maxlen tmpitr)))
			(progn
			  ;; MEMO : intentionally use generic 'copy' & 'copy_backward' function.
			  (copy itr2 itr3 (tmpitr-init tmpitr))
			  (copy_backward itr1 itr2 itr3)
			  (copy (begin tmpitr) (end tmpitr) itr1))
			(let ((itr nil))
			  ;; MEMO : intentyonally use generic 'rotate' function.
			  (rotate itr1 itr2 itr3)
			  (setf itr (clone itr1))
			  (advance itr d2)
			  itr))))
  
  (defun __buffered-rotate-1 (idx1 idx2 idx3 buffer d1 d2 tmpitr)
	(declare (type fixnum idx1 idx2 idx3 d1 d2))
	(declare (type cl:vector buffer))
	(if (and (<= d1 d2) (<= d1 (the fixnum (tmpitr-maxlen tmpitr))))
		(progn
		  (__copy-imp-2 idx1 idx2 buffer (tmpitr-init tmpitr))
		  (__copy-imp-8 idx2 idx3 buffer idx1 buffer)
		  ;; MEMO : depends on implementation of tmpitr...
		  (the fixnum 
			   (__copy_backward-imp-3 (opr::vec-ptr-index (tmpitr-begin tmpitr))
									  (opr::vec-ptr-index (tmpitr-cur   tmpitr))
									  (__vector-data (tmpitr-buffer tmpitr)) idx3 buffer)))
		(if (<= d2 (the fixnum (tmpitr-maxlen tmpitr)))
			(progn
			  (__copy-imp-2 idx2 idx3 buffer (tmpitr-init tmpitr))
			  (__copy_backward-imp-3 idx1 idx2 buffer idx3 buffer)
			  ;; MEMO : depends on implementation of tmpitr...
			  (the fixnum 
				   (__copy-imp-8 (opr::vec-ptr-index (tmpitr-begin tmpitr))
								 (opr::vec-ptr-index (tmpitr-cur   tmpitr))
								 (__vector-data (tmpitr-buffer tmpitr)) idx1 buffer)))
			(progn
			  (__rotate-imp-2 idx1 idx2 idx3 buffer)
			  (the fixnum (+ idx1 d2)))))))
			  
;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  ;; itr1, itr2, and itr3 must be bidirectional_iterator
  (defun __buffered-merge-0 (itr1 itr2 itr3 d1 d2 tmpitr less-bf)
	(declare (type fixnum d1 d2))
	(declare (type cl:function less-bf))
	(if (and (zerop d1) (zerop d2))
		nil
		(with-operators
			(if (= 2 (+ d1 d2))
				(progn
				  (when (funcall less-bf *itr2 *itr1)
					(swap *itr1 *itr2))
				  nil)
				(if (and (<= d1 d2) (<= d1 (the fixnum (tmpitr-maxlen tmpitr))))
					(progn
					  ;; MEMO : intentionally use generic 'copy' function.
					  (copy itr1 itr2 (tmpitr-init tmpitr))
					  ;; MEMO : intentionally use generic 'merge' function.
					  (merge (begin tmpitr) (end tmpitr) itr2 itr3 itr1 less-bf)
					  nil)
					(if (<= d2 (the fixnum (tmpitr-maxlen tmpitr)))
						(progn
						  ;; MEMO : intentionally use generic 'copy' function.
						  (copy itr2 itr3 (tmpitr-init tmpitr))
						  (__merge-backward-0 itr1 itr2 (begin tmpitr) (end tmpitr) itr3 less-bf)
						  nil)
						(let (itr1-n itr2-n itr3-n (d1n 0) (d2n 0))
						  (declare (type fixnum d1n d2n))
						  (if (< d2 d1)
							  (progn
								(setf d1n (ash d1 -1))
								(setf itr1-n (clone itr1))
								(advance itr1-n d1n)
								(setf itr3-n (__lower_bound-imp-0 itr2 itr3 *itr1-n less-bf))
								(setf d2n (the fixnum (distance itr2 itr3-n))))
							  (progn
								(setf d2n (ash d2 -1))
								(setf itr3-n (clone itr2))
								(advance itr3-n d2n)
								(setf itr1-n (__lower_bound-imp-0 itr1 itr2 *itr3-n less-bf))
								(setf d1n (the fixnum (distance itr1 itr1-n)))))
						  (setf itr2-n (__buffered-rotate-0 itr1-n itr2 itr3-n (the fixnum (- d1 d1n)) d2n tmpitr))
						  (__buffered-merge-0 itr1 itr1-n itr2-n d1n d2n tmpitr less-bf)
						  (__buffered-merge-0 itr2 itr3-n itr3 (- d1 d1n) (- d2 d2n) tmpitr less-bf))))))))

  (defun __buffered-merge-1 (idx1 idx2 idx3 buffer d1 d2 tmpitr less-bf)
	(declare (type fixnum idx1 idx2 idx3 d1 d2))
	(declare (type cl:vector buffer))
	(declare (type cl:function less-bf))
	(if (and (zerop d1) (zerop d2))
		nil
		(if (= 2 (+ d1 d2))
			(progn
			  (when (funcall less-bf (aref buffer idx2) (aref buffer idx1))
				(swap (aref buffer idx2) (aref buffer idx1)))
			  nil)
			(if (and (<= d1 d2) (<= d1 (the fixnum (tmpitr-maxlen tmpitr))))
				(progn
				  (__copy-imp-2 idx1 idx2 buffer (tmpitr-init tmpitr))
				  ;; MEMO : depends on implementation of tmpitr...
				  (__merge-imp-26 (opr::vec-ptr-index (tmpitr-begin tmpitr))
								  (opr::vec-ptr-index (tmpitr-cur   tmpitr))
								  (__vector-data (tmpitr-buffer tmpitr))
								  idx2 idx3 buffer idx1 buffer less-bf)
				  nil)
				(if (<= d2 (the fixnum (tmpitr-maxlen tmpitr)))
					(progn
					  (__copy-imp-2 idx2 idx3 buffer (tmpitr-init tmpitr))
					  ;; MEMO : depends on implementation of tmpitr...
					  (__merge-backward-1 idx1 idx2 buffer
										  (opr::vec-ptr-index (tmpitr-begin tmpitr))
										  (opr::vec-ptr-index (tmpitr-cur   tmpitr))
										  (__vector-data (tmpitr-buffer tmpitr)) idx3 buffer less-bf)
					  nil)
					(let ((idx1-n 0)
						  (idx2-n 0)
						  (idx3-n 0)
						  (d1n    0)
						  (d2n    0))
					  (declare (type fixnum idx1-n idx2-n idx3-n d1n d2n))
					  (if (< d2 d1)
						  (progn
							(setf d1n (ash d1 -1))
							(setf idx1-n (+ idx1 d1n))
							(setf idx3-n (__lower_bound-imp-2 idx2 idx3 buffer (aref buffer idx1-n) less-bf))
							(setf d2n (- idx3-n idx2)))
						  (progn
							(setf d2n (ash d2 -1))
							(setf idx3-n (+ idx2 d2n))
							(setf idx1-n (__lower_bound-imp-2 idx1 idx2 buffer (aref buffer idx3-n) less-bf))
							(setf d1n (- idx1-n idx1))))
					  (setf idx2-n (__buffered-rotate-1 idx1-n idx2 idx3-n buffer (the fixnum (- d1 d1n)) d2n tmpitr))
					  (__buffered-merge-1 idx1 idx1-n idx2-n buffer d1n d2n tmpitr less-bf)
					  (__buffered-merge-1 idx2 idx3-n idx3   buffer (- d1 d1n) (- d2 d2n) tmpitr less-bf))))))))


;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  (defun __chunked-merge-0 (bid-itr1 bid-itr2 out-itr d n less-bf)
	(declare (type fixnum d n))
	(declare (type cl:function less-bf))
	(let ((d2   (* d 2))
		  (itr1 (clone bid-itr1))
		  (itr2 (clone bid-itr1))
		  (itr_ (clone bid-itr1)))
	  (declare (type fixnum d2))
	  (for (nil (<= d2 n) nil)
		(_= itr1 itr_)
		(advance itr1 d)
		(_= itr2 itr1)
		(advance itr2 d)
		(setf out-itr (stl:merge itr_ itr1 itr1 itr2 out-itr less-bf))
		(_= itr_ itr2)
		(setf n (- n d2)))
	  (if (<= n d)
		  (stl:copy itr_ bid-itr2 out-itr)
		  (progn
			(_= itr1 itr_)
			(advance itr1 d)
			(stl:merge itr_ itr1 itr1 bid-itr2 out-itr less-bf)))))

  (defun __chunked-merge-1 (bid-idx1 bid-idx2 bid-buf oitr d n less-bf)
	(declare (type fixnum bid-idx1 bid-idx2 d n))
	(declare (type cl:vector bid-buf))
	(declare (type cl:function less-bf))
	(let ((d2   (* d 2))
		  (idx1 bid-idx1)
		  (idx2 bid-idx1)
		  (idx_ bid-idx1))
	  (declare (type fixnum d2 idx1 idx2 idx_))
	  (for (nil (<= d2 n) nil)
		(setf idx1 (+ idx_ d))
		(setf idx2 (+ idx1 d))
		(setf oitr (__merge-imp-24 idx_ idx1 bid-buf idx1 idx2 bid-buf oitr less-bf))
		(setf idx_ idx2)
		(setf n (- n d2)))
	  (if (<= n d)
		  (__copy-imp-2 idx_ bid-idx2 bid-buf oitr)
		  (progn
			(setf idx1 (+ idx_ d))
			(__merge-imp-24 idx_ idx1 bid-buf idx1 bid-idx2 bid-buf oitr less-bf))))))


;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  (defun __buffered-merge-sort-0 (itr1 itr2 n tmpitr less-bf)
	(declare (type fixnum n))
	(declare (type cl:function less-bf))
	(let ((middle1 (clone itr1))
		  (middle2 (clone itr1)))
	  (labels ((imp1 (idx)
				 (declare (type fixnum idx))
				 (for (nil (<= +CHUNK-SIZE+ idx) nil)
				   (_= middle2 middle1)
				   (advance middle2 +CHUNK-SIZE+)
				   (__insertion-sort-0 middle1 middle2 less-bf)
				   (_= middle1 middle2)
				   (setf idx (- idx +CHUNK-SIZE+))))
			   (imp2 (d itr)
				 (declare (type fixnum d))
				 (for (nil (< d n) nil)
				   (_= itr itr1)
				   (__chunked-merge-0 itr1 itr2 (tmpitr-init tmpitr) d n less-bf)
				   (setf d (* d 2))
				   (__chunked-merge-0 (begin tmpitr) (end tmpitr) itr d n less-bf)
				   (setf d (* d 2)))))
		(imp1 n)
		(__insertion-sort-0 middle1 itr2 less-bf)
		(imp2 +CHUNK-SIZE+ (clone itr1)))))
  
  (defun __buffered-merge-sort-1 (idx1 idx2 buffer n tmpitr less-bf)
	(declare (type fixnum idx1 idx2 n))
	(declare (type cl:vector buffer))
	(declare (type cl:function less-bf))
	(let ((middle1 idx1)
		  (middle2 idx1))
	  (declare (type fixnum middle1 middle2))
	  (labels ((imp1 (idx)
				 (declare (type fixnum idx))
				 (for (nil (<= +CHUNK-SIZE+ idx) nil)
				   (setf middle2 (+ middle1 +CHUNK-SIZE+))
				   (__insertion-sort-1 middle1 middle2 buffer less-bf)
				   (setf middle1 middle2)
				   (setf idx (- idx +CHUNK-SIZE+))))
			   (imp2 (d idx)
				 (declare (type fixnum d idx))
				 (for (((itr (_& buffer idx))) (< d n) nil)
				   (setf idx idx1)
				   (setf (opr::vec-ptr-index itr) idx1)
				   (__chunked-merge-1 idx1 idx2 buffer (tmpitr-init tmpitr) d n less-bf)
				   (setf d (* d 2))
				   ;; MEMO : depends on implementation of tmpitr...
				   (__chunked-merge-1 (opr::vec-ptr-index (tmpitr-begin tmpitr))
									  (opr::vec-ptr-index (tmpitr-cur   tmpitr))
									  (__vector-data (tmpitr-buffer tmpitr)) itr d n less-bf)
				   (setf d (* d 2)))))
		(imp1 n)
		(__insertion-sort-1 middle1 idx2 buffer less-bf)
		(imp2 +CHUNK-SIZE+ idx1)))))

;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  (defun __recursive-sort-0 (rdm-itr1 rdm-itr2 ideal less-bf)
	(declare (type fixnum ideal))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~rdm-itr1)
			   (itr2 @~rdm-itr2)) (< +SORT-MAX+ (the fixnum (_- itr2 itr1))) nil)
		  (if (zerop ideal)
			  (progn
				(__make_heap-imp-0 itr1 itr2 less-bf)
				(__sort_heap-imp-0 itr1 itr2 less-bf)
				(return-from __recursive-sort-0 nil))
			  (let* ((n   (ash (the fixnum (_- itr2 itr1)) -1))
					 (val (__median *itr1 itr1[n] itr2[-1] less-bf))
					 (itr (__unguarded-partition-0 itr1 itr2 val less-bf)))
				(declare (type fixnum n))
				(setf ideal (ash ideal -1))
				(__recursive-sort-0 itr1 itr ideal less-bf)
				(_= itr1 itr))))))

  (defun __recursive-sort-1 (idx1 idx2 buffer ideal less-bf)
	(declare (type fixnum idx1 idx2 ideal))
	(declare (type cl:vector buffer))
	(declare (type cl:function less-bf))
	(for (nil (< +SORT-MAX+ (- idx2 idx1)) nil)
	  (if (zerop ideal)
		  (progn
			(__make_heap-imp-1 idx1 idx2 buffer less-bf)
			(__sort_heap-imp-1 idx1 idx2 buffer less-bf)
			(return-from __recursive-sort-1 nil))
		  (let* ((n   (ash (- idx2 idx1) -1))
				 (val nil)
				 (idx 0))
			(declare (type fixnum n idx))
			(_= val (__median (aref buffer idx1)
							  (aref buffer (the fixnum (+ n idx1)))
							  (aref buffer (the fixnum (1- idx2))) less-bf))
			(setf idx (__unguarded-partition-1 idx1 idx2 buffer val less-bf))
			(setf ideal (ash ideal -1))
			(__recursive-sort-1 idx1 idx buffer ideal less-bf)
			(setf idx1 idx))))))


;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  (defun __recursive-stable_partition-0 (bid-itr1 bid-itr2 pred-uf n tmpitr)
	(declare (type fixnum n))
	(declare (type cl:function pred-uf))
	(labels ((imp (bid-itr1_ itr)
			   (with-operators
				   (for (nil (_/= bid-itr1_ bid-itr2) ++bid-itr1_)
					 (if (funcall pred-uf *bid-itr1_)
						 (progn (_= *itr    *bid-itr1_) ++itr)
						 (progn (_= *tmpitr *bid-itr1_) ++tmpitr))))))
	  (with-operators
		  (if (= n 1)
			  (if (funcall pred-uf *bid-itr1)
				  @~bid-itr2
				  @~bid-itr1)
			  (let* ((bid-itr1_ @~bid-itr1)
					 (itr       @~bid-itr1_))
				(if (<= n (the fixnum (tmpitr-maxlen tmpitr)))
					(progn
					  (tmpitr-init tmpitr)
					  (imp bid-itr1_ itr)
					  (stl:copy (begin tmpitr) (end tmpitr) itr)
					  itr)
					(let ((lp nil)
						  (rp nil)
						  (half (ash n -1)))
					  (declare (type fixnum half))
					  (advance itr half)
					  (setf lp (__recursive-stable_partition-0 bid-itr1_ itr pred-uf half       tmpitr))
					  (setf rp (__recursive-stable_partition-0 itr  bid-itr2 pred-uf (- n half) tmpitr))
					  (__buffered-rotate-0 lp itr rp
										   (the fixnum (distance lp itr))
										   (the fixnum (distance itr rp)) tmpitr))))))))

  (defun __recursive-stable_partition-1 (bid-idx1 bid-idx2 buffer pred-uf n tmpitr)
	(declare (type fixnum bid-idx1 bid-idx2 n))
	(declare (type cl:vector buffer))
	(declare (type cl:function pred-uf))
	(labels ((imp (bid-idx1_ idx)
			   (declare (type fixnum bid-idx1_ idx))
			   (with-operators
				   (for (nil (< bid-idx1_ bid-idx2) (incf bid-idx1_) :returns (values bid-idx1_ idx))
					 (if (funcall pred-uf (aref buffer bid-idx1_))
						 (progn
						   (_= (aref buffer idx) (aref buffer bid-idx1_))
						   (incf idx))
						 (progn
						   (_= *tmpitr (aref buffer bid-idx1_))
						   ++tmpitr))))))
	  (if (= n 1)
		  (if (funcall pred-uf (aref buffer bid-idx1))
			  bid-idx2
			  bid-idx1)
		  (let ((bid-idx1_ bid-idx1)
				(idx       bid-idx1))
			(declare (type fixnum bid-idx1_ idx))
			(if (<= n (the fixnum (tmpitr-maxlen tmpitr)))
				(progn
				  (tmpitr-init tmpitr)
				  (multiple-value-setq (bid-idx1_ idx) (imp bid-idx1_ idx))
				  ;; MEMO : depends on implementation of tmpitr...
				  (__copy-imp-8 (opr::vec-ptr-index (tmpitr-begin tmpitr))
								(opr::vec-ptr-index (tmpitr-cur   tmpitr))
								(__vector-data (tmpitr-buffer tmpitr)) idx buffer)
				  idx)
				(let ((lp   0)
					  (rp   0)
					  (half (ash n -1)))
				  (declare (type fixnum lp rp half))
				  (incf idx half)
				  (setf lp (__recursive-stable_partition-1 bid-idx1_ idx      buffer pred-uf half       tmpitr))
				  (setf rp (__recursive-stable_partition-1 idx       bid-idx2 buffer pred-uf (- n half) tmpitr))
				  (__buffered-rotate-1 lp idx rp buffer
									   (the fixnum (- idx lp))
									   (the fixnum (- rp idx)) tmpitr))))))))


;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  (defun __recursive-stable_sort-0 (itr1 itr2 n tmpitr less-bf)
	(declare (type fixnum n))
	(declare (type cl:function less-bf))
	(if (<= n +SORT-MAX+)
		(__insertion-sort-0 itr1 itr2 less-bf)
		(let ((n2  (ash n -1))
			  (itr (clone itr1)))
		  (declare (type fixnum n2))
		  (advance itr n2)
		  (if (<= n2 (the fixnum (tmpitr-maxlen tmpitr)))
			  (progn
				(__buffered-merge-sort-0 itr1 itr n2       tmpitr less-bf) 
				(__buffered-merge-sort-0 itr itr2 (- n n2) tmpitr less-bf))
			  (progn
				(__recursive-stable_sort-0 itr1 itr n2       tmpitr less-bf)
				(__recursive-stable_sort-0 itr itr2 (- n n2) tmpitr less-bf)))
		  (__buffered-merge-0 itr1 itr itr2 n2 (- n n2) tmpitr less-bf))))
  
  (defun __recursive-stable_sort-1 (idx1 idx2 buffer n tmpitr less-bf)
	(declare (type fixnum idx1 idx2 n))
	(declare (type cl:vector buffer))
	(declare (type cl:function less-bf))
	(if (<= n +SORT-MAX+)
		(__insertion-sort-1 idx1 idx2 buffer less-bf)
		(let ((n2  (ash n -1))
			  (idx idx1))
		  (declare (type fixnum n2 idx))
		  (incf idx n2)
		  (if (<= n2 (the fixnum (tmpitr-maxlen tmpitr)))
			  (progn
				(__buffered-merge-sort-1 idx1 idx  buffer n2       tmpitr less-bf)
				(__buffered-merge-sort-1 idx  idx2 buffer (- n n2) tmpitr less-bf))
			  (progn
				(__recursive-stable_sort-1 idx1 idx  buffer n2       tmpitr less-bf)
				(__recursive-stable_sort-1 idx  idx2 buffer (- n n2) tmpitr less-bf)))
		  (__buffered-merge-1 idx1 idx idx2 buffer n2 (- n n2) tmpitr less-bf)))))


;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  (defun __find_end-imp-0 (first1 last1 first2 last2 eql-bf)
	(declare (type cl:function eql-bf))
	(labels ((imp (itr1 itr2)
			   (with-operators
				   (for (nil (_/= itr2 last2) (progn ++itr1 ++itr2) :returns t)
					 (unless (funcall eql-bf *itr1 *itr2)
					   (return-from imp nil))))))
	  (with-operators
		  (let ((ret  @~last1)
				(len1 (the fixnum (distance first1 last1)))
				(len2 (the fixnum (distance first2 last2))))
			(declare (type fixnum len1 len2))
			(if (<= len2 0)
				ret
				(for (((found ret)
					   (itr1 @~first1)
					   (wk1  @~first1)
					   (wk2  @~first2)) (<= len2 len1) (progn ++itr1 (decf len1)) :returns found)
				  (when (imp (_= wk1 itr1) (_= wk2 first2))
					(_= found itr1))))))))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __find_end-imp-1 (cons1 end1 first2 last2 eql-bf)
	(declare (type cl:list cons1 end1))
	(declare (type cl:function eql-bf))
	(labels ((imp (cns itr2)
			   (declare (type cl:list cns))
			   (with-operators
				   (for (nil (_/= itr2 last2) (progn (setf cns (cdr cns)) ++itr2) :returns t)
					 (unless (funcall eql-bf (car cns) *itr2)
					   (return-from imp nil))))))
	  (with-operators
		  (let ((ret  end1)
				(len1 (the fixnum (__conslist-count-nodes cons1 end1)))
				(len2 (the fixnum (distance first2 last2))))
			(declare (type cl:list ret))
			(declare (type fixnum len1 len2))
			(if (<= len2 0)
				ret
				(let ((found ret)
					  (cns   cons1))
				  (declare (type cl:list found cns))
				  (for (((wk2 @~first2)) (<= len2 len1) (progn (setf cns (cdr cns)) (decf len1)) :returns found)
					(when (imp cns (_= wk2 first2))
					  (setf found cns)))))))))

  (defun __find_end-imp-2 (begin1 end1 buffer1 first2 last2 eql-bf)
	(declare (type fixnum begin1 end1))
	(declare (type cl:vector buffer1))
	(declare (type cl:function eql-bf))
	(labels ((imp (idx1 itr2)
			   (declare (type fixnum idx1))
			   (with-operators
				   (for (nil (_/= itr2 last2) (progn (incf idx1) ++itr2) :returns t)
					 (unless (funcall eql-bf (aref buffer1 idx1) *itr2)
					   (return-from imp nil))))))
	  (with-operators
		  (let ((ret  end1)
				(len1 (- end1 begin1))
				(len2 (the fixnum (distance first2 last2))))
			(declare (type fixnum len1 len2))
			(if (<= len2 0)
				ret
				(let ((found ret)
					  (idx1  begin1))
				  (declare (type fixnum found idx1))
				  (for (((wk2 @~first2)) (<= len2 len1) (progn (incf idx1) (decf len1)) :returns found)
					(when (imp idx1 (_= wk2 first2))
					  (setf found idx1)))))))))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __find_end-imp-3 (first1 last1 cons2 end2 eql-bf)
	(declare (type cl:list cons2 end2))
	(declare (type cl:function eql-bf))
	(labels ((imp (itr1)
			   (let ((cns cons2))
				 (declare (type cl:list cns))
				 (with-operators
					 (for (nil (not (eq cns end2)) (progn ++itr1 (setf cns (cdr cns))) :returns t)
					   (unless (funcall eql-bf *itr1 (car cns))
						 (return-from imp nil)))))))
	  (with-operators
		  (let ((ret  @~last1)
				(len1 (the fixnum (distance first1 last1)))
				(len2 (__conslist-count-nodes cons2 end2)))
			(declare (type fixnum len1 len2))
			(if (<= len2 0)
				ret
				(for (((found ret)
					   (itr1  @~first1)
					   (wk1   @~first1)) (<= len2 len1) (progn ++itr1 (decf len1)) :returns found)
				  (when (imp (_= wk1 itr1))
					(_= found itr1))))))))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __find_end-imp-4 (cons1 end1 cons2 end2 eql-bf)
	(declare (type cl:list cons1 end1 cons2 end2))
	(declare (type cl:function eql-bf))
	(labels ((imp (cns1)
			   (declare (type cl:list cns1))
			   (let ((cns2 cons2))
				 (declare (type cl:list cns2))
				 (for (nil (not (eq cns2 end2)) (progn (setf cns1 (cdr cns1))
													   (setf cns2 (cdr cns2))) :returns t)
				   (unless (funcall eql-bf (car cns1) (car cns2))
					 (return-from imp nil))))))
	  (let ((ret  end1)
			(len1 (__conslist-count-nodes cons1 end1))
			(len2 (__conslist-count-nodes cons2 end2)))
		(declare (type cl:list ret))
		(declare (type fixnum len1 len2))
		(if (<= len2 0)
			ret
			(let ((found ret)
				  (cns1  cons1))
			  (declare (type cl:list found cns1))
			  (for (nil (<= len2 len1) (progn (setf cns1 (cdr cns1)) (decf len1)) :returns found)
				(when (imp cns1)
				  (setf found cns1))))))))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __find_end-imp-5 (begin1 end1 buffer1 cons2 end2 eql-bf)
	(declare (type fixnum begin1 end1))
	(declare (type cl:vector buffer1))
	(declare (type cl:list cons2 end2))
	(declare (type cl:function eql-bf))
	(labels ((imp (idx1)
			   (declare (type fixnum idx1))
			   (let ((cns2 cons2))
				 (declare (type cl:list cns2))
				 (for (nil (not (eq cns2 end2)) (progn (incf idx1)
													   (setf cns2 (cdr cns2))) :returns t)
				   (unless (funcall eql-bf (aref buffer1 idx1) (car cns2))
					 (return-from imp nil))))))
	  (let ((ret  end1)
			(len1 (- end1 begin1))
			(len2 (__conslist-count-nodes cons2 end2)))
		(declare (type fixnum len1 len2))
		(if (<= len2 0)
			ret
			(let ((found ret)
				  (idx1  begin1))
			  (declare (type fixnum found idx1))
			  (for (nil (<= len2 len1) (progn (incf idx1) (decf len1)) :returns found)
				(when (imp idx1)
				  (setf found idx1))))))))
  
  (defun __find_end-imp-6 (first1 last1 begin2 end2 buffer2 eql-bf)
	(declare (type fixnum begin2 end2))
	(declare (type cl:vector buffer2))
	(declare (type cl:function eql-bf))
	(labels ((imp (itr1)
			   (let ((idx2 begin2))
				 (declare (type fixnum idx2))
				 (with-operators
					 (for (nil (< idx2 end2) (progn ++itr1 (incf idx2)) :returns t)
					   (unless (funcall eql-bf *itr1 (aref buffer2 idx2))
						 (return-from imp nil)))))))
	  (with-operators
		  (let ((ret  @~last1)
				(len1 (the fixnum (distance first1 last1)))
				(len2 (- end2 begin2)))
			(declare (type fixnum len1 len2))
			(if (<= len2 0)
				ret
				(for (((found ret)
					   (itr1  @~first1)
					   (wk1   @~first1)) (<= len2 len1) (progn ++itr1 (decf len1)) :returns found)
				  (when (imp (_= wk1 itr1))
					(_= found itr1))))))))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __find_end-imp-7 (cons1 end1 begin2 end2 buffer2 eql-bf)
	(declare (type cl:list cons1 end1))
	(declare (type fixnum begin2 end2))
	(declare (type cl:vector buffer2))
	(declare (type cl:function eql-bf))
	(labels ((imp (cns1)
			   (declare (type cl:list cns1))
			   (let ((idx2 begin2))
				 (declare (type fixnum idx2))
				 (for (nil (< idx2 end2) (progn (setf cns1 (cdr cns1)) (incf idx2)) :returns t)
				   (unless (funcall eql-bf (car cns1) (aref buffer2 idx2))
					 (return-from imp nil))))))
	  (let ((ret  end1)
			(len1 (__conslist-count-nodes cons1 end1))
			(len2 (- end2 begin2)))
		(declare (type cl:list ret))
		(declare (type fixnum len1 len2))
		(if (<= len2 0)
			ret
			(let ((found ret)
				  (cns1  cons1))
			  (declare (type cl:list found cns1))
			  (for (nil (<= len2 len1) (progn (setf cns1 (cdr cns1)) (decf len1)) :returns found)
				(when (imp cns1)
				  (setf found cns1))))))))
  
  (defun __find_end-imp-8 (begin1 end1 buffer1 begin2 end2 buffer2 eql-bf)
	(declare (type fixnum begin1 end1 begin2 end2))
	(declare (type cl:vector buffer1 buffer2))
	(declare (type cl:function eql-bf))
	(labels ((imp (idx1)
			   (declare (type fixnum idx1))
			   (let ((idx2 begin2))
				 (declare (type fixnum idx2))
				 (for (nil (< idx2 end2) (progn (incf idx1) (incf idx2)) :returns t)
				   (unless (funcall eql-bf (aref buffer1 idx1) (aref buffer2 idx2))
					 (return-from imp nil))))))
	  (let ((ret  end1)
			(len1 (- end1 begin1))
			(len2 (- end2 begin2)))
		(declare (type fixnum len1 len2))
		(if (<= len2 0)
			ret
			(let ((found ret)
				  (idx1  begin1))
			  (declare (type fixnum found idx1))
			  (for (nil (<= len2 len1) (progn (incf idx1) (decf len1)) :returns found)
				(when (imp idx1)
				  (setf found idx1)))))))))

  
  
;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  ;;PTN; adjacent_find : 0 -  f
  (defun __adjacent_find-imp-0 (first last eql-bf)
	(declare (type cl:function eql-bf))
	(with-operators
		(if (_== first last)
			@~first
			(for (((prv @~first)
				   (cur (next first))) (_/= cur last) (progn ++prv ++cur) :returns cur)
			  (when (funcall eql-bf *prv *cur)
				(return-from __adjacent_find-imp-0 prv))))))

  ;;PTN; adjacent_find : 1 - cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __adjacent_find-imp-1 (cons1 cons2 eql-bf)
	(declare (type cl:function eql-bf))
	(let ((chk (cdr cons1)))
	  (for (nil (not (eq chk cons2)) (progn (setf cons1 (cdr cons1))
											(setf   chk (cdr   chk))) :returns cons2)
		(when (funcall eql-bf (car cons1) (car chk))
		  (return-from __adjacent_find-imp-1 cons1)))))

  ;;PTN; adjacent_find : 2 - cvp
  (defun __adjacent_find-imp-2 (idx1 idx2 buffer eql-bf)
	(declare (type fixnum idx1 idx2))
	(declare (type cl:vector buffer))
	(declare (type cl:function eql-bf))
	(let ((chk (1+ idx1)))
	  (declare (type fixnum chk))
	  (for (nil (< chk idx2) (progn (incf idx1) (incf chk)) :returns idx2)
		(when (funcall eql-bf (aref buffer idx1) (aref buffer chk))
		  (return-from __adjacent_find-imp-2 idx1))))))



;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  (defun __binary_search-imp-0 (first last val comp)
	(declare (type cl:function comp))
	(if (_== first last)
		nil
		(let ((itr (__lower_bound-imp-0 first last val comp)))
		  (if (_== itr last)
			  nil
			  (with-operators
				  (if (funcall comp val *itr) nil t))))))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __binary_search-imp-1 (cons1 cons2 val comp)
	(declare (type cl:list cons1 cons2))
	(declare (type cl:function comp))
	(if (eq cons1 cons2)
		nil
		(let ((cns (__lower_bound-imp-1 cons1 cons2 val comp)))
		  (if (eq cns cons2)
			  nil
			  (if (funcall comp val (car cns))
				  nil
				  t)))))

  (defun __binary_search-imp-2 (idx1 idx2 buffer val comp)
	(declare (type fixnum idx1 idx2))
	(declare (type cl:vector buffer))
	(declare (type cl:function comp))
	(if (= idx1 idx2)
		nil
		(let ((idx (__lower_bound-imp-2 idx1 idx2 buffer val comp)))
		  (if (= idx idx2)
			  nil
			  (if (funcall comp val (aref buffer idx))
				  nil
				  t))))))

;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  (defun __count_if-imp-0 (itr1 itr2 pred)
	(declare (type cl:function pred))
	(let ((cnt  0))
	  (declare (type fixnum cnt))
	  (with-operators
		  (for (((itr @~itr1)) (_/= itr itr2) ++itr :returns cnt)
			(when (funcall pred *itr)
			  (incf cnt))))))

  (defun __count_if-imp-1 (cons1 cons2 pred)
	(declare (type cl:function pred))
	(let ((cnt 0))
	  (declare (type fixnum cnt))
	  (for (nil (not (eq cons1 cons2)) (setf cons1 (cdr cons1)) :returns cnt)
		(when (funcall pred (car cons1))
		  (incf cnt)))))
	
  (defun __count_if-imp-2 (idx1 idx2 buffer pred)
	(declare (type fixnum idx1 idx2))
	(declare (type cl:vector buffer))
	(declare (type cl:function pred))
	(let ((cnt 0))
	  (declare (type fixnum cnt))
	  (for (nil (< idx1 idx2) (incf idx1) :returns cnt)
		(when (funcall pred (aref buffer idx1))
		  (incf cnt))))))

;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  ;;IMP; find_if : 0 -  i
  (defun __find_if-imp-0 (first last pred)
	(declare (type cl:function pred))
	(with-operators
		(for (((itr @~first)) (_/= itr last) ++itr :returns itr)
		  (when (funcall pred *itr)
			(return-from __find_if-imp-0 itr)))))

  ;;IMP; find_if : 1 - cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __find_if-imp-1 (cons1 cons2 pred)
	(declare (type cl:function pred))
	(for (nil (not (eq cons1 cons2)) (setf cons1 (cdr cons1)) :returns cons2)
	  (when (funcall pred (car cons1))
		(return-from __find_if-imp-1 cons1))))

  ;;IMP; find_if : 2 - cvp
  (defun __find_if-imp-2 (idx1 idx2 buffer pred)
	(declare (type fixnum idx1 idx2))
	(declare (type cl:vector buffer))
	(declare (type cl:function pred))
	(for (nil (< idx1 idx2) (incf idx1) :returns idx2)
	  (when (funcall pred (aref buffer idx1))
		(return-from __find_if-imp-2 idx1)))))

;;------------------------------------------------------------------------------
#-cl-stl-0x98
(locally (declare (optimize speed))

  (defun __is_heap_until-imp-0 (itr len less-bf)
	(declare (type fixnum len))
	(declare (type cl:function less-bf))
	(let ((parent 0))
	  (declare (type fixnum parent))
	  (with-operators
		  (for (((child 1)) (< child len) (incf child) :returns len)
			(when (funcall less-bf itr[parent] itr[child])
			  (return-from __is_heap_until-imp-0 child))
			(when (zerop (mod child 2))
			  (incf parent))))))

  (defun __is_heap_until-imp-1 (idx len buffer less-bf)
	(declare (type fixnum idx len))
	(declare (type cl:vector buffer))
	(declare (type cl:function less-bf))
	(let ((parent 0)
		  (child  1))
	  (declare (type fixnum parent child))
	  (for (nil (< child len) (incf child) :returns len)
		(when (funcall less-bf (aref buffer (the fixnum (+ idx parent)))
							   (aref buffer (the fixnum (+ idx  child))))
		  (return-from __is_heap_until-imp-1 child))
		(when (zerop (mod child 2))
		  (incf parent))))))


;;------------------------------------------------------------------------------
#-cl-stl-0x98
(locally (declare (optimize speed))

  (defun __is_sorted_until-imp-0 (first last comp)
	(declare (type cl:function comp))
	(with-operators
		(let ((itr @~first))
		  (if (_== itr last)
			  itr
			  (for (((prv *first) (itr ++itr)) (_/= itr last) ++itr :returns itr)
				(let ((cur *itr))
				  (when (funcall comp cur prv)
					(return-from __is_sorted_until-imp-0 itr))
				  (setf prv cur)))))))

  (defun __is_sorted_until-imp-1 (cons1 cons2 less-bf)
	(declare (type cl:list     cons1 cons2))
	(declare (type cl:function less-bf))
	(if (eq cons1 cons2)
		cons1
		(progn
		  (setf cons1 (cdr cons1))
		  (for (((prv (car cons1))) (not (eq cons1 cons2)) (setf cons1 (cdr cons1)) :returns cons1)
			(let ((cur (car cons1)))
			  (when (funcall less-bf cur prv)
				(return-from __is_sorted_until-imp-1 cons1))
			  (setf prv cur))))))

  (defun __is_sorted_until-imp-2 (idx1 idx2 buffer less-bf)
	(declare (type fixnum idx1 idx2))
	(declare (type cl:vector buffer))
	(declare (type cl:function less-bf))
	(if (= idx1 idx2)
		idx1
		(progn
		  (incf idx1)
		  (for (((prv (aref buffer idx1))) (< idx1 idx2) (incf idx1) :returns idx1)
			(let ((cur (aref buffer idx1)))
			  (when (funcall less-bf cur prv)
				(return-from __is_sorted_until-imp-2 idx1))
			  (setf prv cur)))))))

;;------------------------------------------------------------------------------
#-cl-stl-0x98
(locally (declare (optimize speed))

  ;;IMP; minmax_element : 0 -   f
  (defun __minmax_element-imp-0 (first last comp)
	(declare (type cl:function comp))
	(with-operators
		(let ((next (next first)))
		  (when (or (_== first last)
					(_== next  last))
			(return-from __minmax_element-imp-0 (make_pair @~first @~first)))
		  (let ((min nil)
				(max nil)
				(itr @~first))
			(if (funcall comp *next *itr)
				(setf min @~next max @~itr)
				(setf min @~itr  max @~next))
			(_= itr next)
			++itr
			(for (nil (_/= itr last) ++itr)
			  (_= next itr)
			  (when (_== ++next last)
				(if (funcall comp *itr *min)
					(_= min itr)
					(if (not (funcall comp *itr *max))
						(_= max itr)))
				(return))
			  (if (funcall comp *next *itr)
				  (progn
					(when   (funcall comp *next *min) (_= min next))
					(unless (funcall comp *itr  *max) (_= max  itr)))
				  (progn
					(when   (funcall comp *itr  *min) (_= min itr))
					(unless (funcall comp *next *max) (_= max next))))
			  (_= itr next))
			(make_pair min max)))))

  ;;IMP; minmax_element : 1 -  cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __minmax_element-imp-1 (cons1 cons2 less-bf)
	(declare (type cl:list cons1 cons2))
	(declare (type cl:function less-bf))
	(when (or (eq cons1 cons2)
			  (eq (cdr cons1) cons2))
	  (return-from __minmax_element-imp-1 (values cons1 cons1)))
	(let ((min  nil)
		  (max  nil)
		  (cns  cons1)
		  (next (cdr cons1)))
	  (declare (type cl:list min max cns next))
	  (macrolet ((cmp (s1 s2)
				   `(funcall less-bf (car ,s1) (car ,s2))))
		(if (cmp next cns)
			(setf min next max cns)
			(setf min cns  max next))
		(setf cns (cdr next))
		(for (nil (not (eq cns cons2)) nil)
		  (setf next (cdr cns))
		  (when (eq next cons2)
			(if (cmp cns min)
				(setf min cns)
				(if (not (cmp cns max))
					(setf max cns)))
			(return))
		  (if (cmp next cns)
			  (progn
				(when   (cmp next min) (setf min next))
				(unless (cmp  cns max) (setf max  cns)))
			  (progn
				(when   (cmp  cns min) (setf min  cns))
				(unless (cmp next max) (setf max next))))
		  (setf cns (cdr next))))
	  (values min max)))

  ;;IMP; minmax_element : 2 -  cvp
  (defun __minmax_element-imp-2 (idx1 idx2 buffer less-bf)
	(declare (type fixnum idx1 idx2))
	(declare (type cl:vector buffer))
	(declare (type cl:function less-bf))
	(when (or (= idx1 idx2)
			  (= (1+ idx1) idx2))
	  (return-from __minmax_element-imp-2 (values idx1 idx1)))
	(let ((min  0)
		  (max  0)
		  (idx  idx1)
		  (next (1+ idx1)))
	  (declare (type fixnum min max next idx))
	  (macrolet ((cmp (s1 s2)
				   `(funcall less-bf (aref buffer ,s1)
							 (aref buffer ,s2))))
		(if (cmp next idx)
			(setf min next max idx)
			(setf min idx  max next))
		(setf idx (1+ next))
		(for (nil (< idx idx2) nil)
		  (setf next idx)
		  (when (= (incf next) idx2)
			(if (cmp idx min)
				(setf min idx)
				(if (not (cmp idx max))
					(setf max idx)))
			(return))
		  (if (cmp next idx)
			  (progn
				(when   (cmp next min) (setf min next))
				(unless (cmp  idx max) (setf max  idx)))
			  (progn
				(when   (cmp  idx min) (setf min  idx))
				(unless (cmp next max) (setf max next))))
		  (setf idx (1+ next))))
	  (values min max))))


;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  (defun __reverse-imp-0b (first last)
	(if (_== first last)
		nil
		(with-operators
			(for (((first @~first) (last @~last)) (_/= first last) ++first)
			  (when (_== first --last)
				(return nil))
			  (swap *first *last)))))

  (defun __reverse-imp-0r (first last)
	(if (_== first last)
		nil
		(with-operators
			(let ((first @~first)
				  (last  @~last))
			  --last
			  (for (nil (_< first last) (progn ++first --last))
				(swap *first *last))))))

  (defun __reverse-imp-1 (idx1 idx2 buffer)
	(declare (type fixnum idx1 idx2))
	(declare (type cl:vector buffer))
	(for (nil (< idx1 idx2) nil)
	  (decf idx2)
	  (unless (= idx1 idx2)
		(swap (aref buffer idx1) (aref buffer idx2))
		(incf idx1)))))


;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  (defun __partial_sort_copy-imp-0 (first last result-first result-last comp)
	(declare (type cl:function comp))
	(labels ((imp1 (src dst)
			   (with-operators
				   (for (nil (and (_/= src last) (_/= dst result-last)) (progn ++src ++dst))
					 (_= *dst *src))))
			 (imp2 (src dst)
			   (with-operators
				   (for (((tmp nil)) (_/= src last) ++src)
					 (when (funcall comp *src *result-first)
					   (_= tmp *src)
					   (__adjust-heap-0 result-first 0
										(the fixnum (_- dst result-first)) tmp comp))))))
	  (with-operators
		  (if (_== result-first result-last)
			  @~result-last
			  (let ((src @~first)
					(dst @~result-first))
				(imp1 src dst)
				(__make_heap-imp-0 result-first dst comp)
				(imp2 src dst)
				(__sort_heap-imp-0 result-first dst comp)
				dst)))))

  (defun __partial_sort_copy-imp-1 (cons1 end1 result-first result-last less-bf)
	(declare (type cl:list cons1 end1))
	(declare (type cl:function less-bf))
	(labels ((imp1 (src dst)
			   (declare (type cl:list src))
			   (with-operators
				   (for (nil (and (not (eq src end1))
								  (_/= dst result-last)) (progn (setf src (cdr src)) ++dst) :returns src)
					 (_= *dst (car src)))))
			 (imp2 (src dst)
			   (declare (type cl:list src))
			   (with-operators
				   (for (((tmp nil)) (not (eq src end1)) (setf src (cdr src)))
					 (when (funcall less-bf (car src) *result-first)
					   (_= tmp (car src))
					   (__adjust-heap-0 result-first 0 (the fixnum (_- dst result-first)) tmp less-bf))))))
	  (with-operators
		  (if (_== result-first result-last)
			  @~result-last
			  (let ((src cons1)
					(dst @~result-first))
				(setf src (imp1 src dst))
				(__make_heap-imp-0 result-first dst less-bf)
				(imp2 src dst)
				(__sort_heap-imp-0 result-first dst less-bf)
				dst)))))
	
	
  (defun __partial_sort_copy-imp-2 (in-idx1 in-idx2 in-buffer result-first result-last less-bf)
	(declare (type fixnum in-idx1 in-idx2))
	(declare (type cl:vector in-buffer))
	(declare (type cl:function less-bf))
	(labels ((imp1 (src dst)
			   (declare (type fixnum src))
			   (with-operators
				   (for (nil (and (< src in-idx2)
								  (_/= dst result-last)) (progn (incf src) ++dst) :returns src)
					 (_= *dst (aref in-buffer src)))))
			 (imp2 (src dst)
			   (declare (type fixnum src))
			   (with-operators
				   (for (((tmp nil)) (< src in-idx2) (incf src))
					 (when (funcall less-bf (aref in-buffer src) *result-first)
					   (_= tmp (aref in-buffer src))
					   (__adjust-heap-0 result-first 0 (the fixnum (_- dst result-first)) tmp less-bf))))))
	  (with-operators
		  (if (_== result-first result-last)
			  @~result-last
			  (let ((src in-idx1)
					(dst @~result-first))
				(setf src (imp1 src dst))
				(__make_heap-imp-0 result-first dst less-bf)
				(imp2 src dst)
				(__sort_heap-imp-0 result-first dst less-bf)
				dst)))))

  (defun __partial_sort_copy-imp-3 (first last rdm-idx1 rdm-idx2 rdm-buffer less-bf)
	(declare (type fixnum rdm-idx1 rdm-idx2))
	(declare (type cl:vector rdm-buffer))
	(declare (type cl:function less-bf))
	(labels ((imp1 (src dst)
			   (declare (type fixnum dst))
			   (with-operators
				   (for (nil (and (_/= src last)
								  (< dst rdm-idx2)) (progn ++src (incf dst)) :returns dst)
					 (_= (aref rdm-buffer dst) *src))))
			 (imp2 (src dst)
			   (declare (type fixnum dst))
			   (with-operators
				   (for (((tmp nil)) (_/= src last) ++src)
					 (when (funcall less-bf *src (aref rdm-buffer rdm-idx1))
					   (_= tmp *src)
					   (__adjust-heap-1 rdm-buffer rdm-idx1 0 (the fixnum (- dst rdm-idx1)) tmp less-bf))))))
	  (with-operators
		  (if (= rdm-idx1 rdm-idx2)
			  rdm-idx2
			  (let ((src @~first)
					(dst rdm-idx1))
				(setf dst (imp1 src dst))
				(__make_heap-imp-1 rdm-idx1 dst rdm-buffer less-bf)
				(imp2 src dst)
				(__sort_heap-imp-1 rdm-idx1 dst rdm-buffer less-bf)
				dst)))))

  (defun __partial_sort_copy-imp-4 (cons1 end1 rdm-idx1 rdm-idx2 rdm-buffer less-bf)
	(declare (type cl:list cons1 end1))
	(declare (type fixnum rdm-idx1 rdm-idx2))
	(declare (type cl:vector rdm-buffer))
	(declare (type cl:function less-bf))
	(labels ((imp1 (src dst)
			   (declare (type cl:list src))
			   (declare (type fixnum dst))
			   (for (nil (and (not (eq src end1))
							  (< dst rdm-idx2)) (progn (setf src (cdr src)) (incf dst))
							  :returns (values src dst))
				 (_= (aref rdm-buffer dst) (car src))))
			 (imp2 (src dst)
			   (declare (type cl:list src))
			   (declare (type fixnum dst))
			   (for (nil (not (eq src end1)) (setf src (cdr src)))
				 (when (funcall less-bf (car src) (aref rdm-buffer rdm-idx1))
				   (__adjust-heap-1 rdm-buffer rdm-idx1 0
									(the fixnum (- dst rdm-idx1)) (car src) less-bf)))))
	  (if (= rdm-idx1 rdm-idx2)
		  rdm-idx2
		  (let ((src cons1)
				(dst rdm-idx1))
			(multiple-value-setq (src dst) (imp1 src dst))
			(__make_heap-imp-1 rdm-idx1 dst rdm-buffer less-bf)
			(imp2 src dst)
			(__sort_heap-imp-1 rdm-idx1 dst rdm-buffer less-bf)
			dst))))

  (defun __partial_sort_copy-imp-5 (in-idx1 in-idx2 in-buffer rdm-idx1 rdm-idx2 rdm-buffer less-bf)
	(declare (type fixnum in-idx1 in-idx2 rdm-idx1 rdm-idx2))
	(declare (type cl:vector in-buffer rdm-buffer))
	(declare (type cl:function less-bf))
	(labels ((imp1 (src dst)
			   (declare (type fixnum src dst))
			   (for (nil (and (< src in-idx2)
							  (< dst rdm-idx2)) (progn (incf src) (incf dst))
							  :returns (values src dst))
				 (_= (aref rdm-buffer dst) (aref in-buffer src))))
			 (imp2 (src dst)
			   (declare (type fixnum src dst))
			   (for (nil (< src in-idx2) (incf src))
				 (when (funcall less-bf (aref in-buffer src) (aref rdm-buffer rdm-idx1))
				   (__adjust-heap-1 rdm-buffer rdm-idx1 0
									(the fixnum (- dst rdm-idx1)) (aref in-buffer src) less-bf)))))
	  (if (= rdm-idx1 rdm-idx2)
		  rdm-idx2
		  (let ((src in-idx1)
				(dst rdm-idx1))
			(multiple-value-setq (src dst) (imp1 src dst))
			(__make_heap-imp-1 rdm-idx1 dst rdm-buffer less-bf)
			(imp2 src dst)
			(__sort_heap-imp-1 rdm-idx1 dst rdm-buffer less-bf)
			dst)))))

  
;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  ;;IMP; is_permutation : 0 -  f  x  f 
  (labels ((imp0 (first1 last1 first2 last2 pred)
			 (declare (type cl:function pred))
			 (with-operators
				 (for (((scan @~first1)) (_/= scan last1) ++scan :returns t)
				   (labels ((pred2 (arg)
							  (funcall pred arg *scan)))
					 (unless (_/= scan (__find_if-imp-0 first1 scan #'pred2))
					   (let ((matches (__count_if-imp-0 first2 last2 #'pred2)))
						 (when (or (zerop matches) (/= matches (__count_if-imp-0 scan last1 #'pred2)))
						   (return-from imp0 nil)))))))))

	(defun __is_permutation-imp-0a (first1 last1 first2 pred)
	  (declare (type cl:function pred))
	  (with-operators
		  (let ((itr1 @~first1)
				(itr2 @~first2))
			(for (nil (and (_/= itr1 last1) (funcall pred *itr1 *itr2)) nil)
			  ++itr1 ++itr2)
			(if (_== itr1 last1)
				t
				(imp0 itr1 last1 itr2 (next itr2 (distance itr1 last1)) pred)))))

	#-(or cl-stl-0x98 cl-stl-0x11)
	(defun __is_permutation-imp-0b (first1 last1 first2 last2 pred) ; non randomaccess_iterator version.
	  (declare (type cl:function pred))
	  (with-operators
		  (let ((itr1 @~first1)
				(itr2 @~first2))
			(for (nil (and (_/= itr1 last1) (_/= itr2 last2) (funcall pred *itr1 *itr2)) nil)
			  ++itr1 ++itr2)
			(let ((d1 (distance itr1 last1))
				  (d2 (distance itr2 last2)))
			  (declare (type fixnum d1 d2))
			  (cond
				((=  d1 d2 0) t)
				((/= d1 d2)   nil)
				(t            (imp0 itr1 last1 itr2 last2 pred)))))))

	#-(or cl-stl-0x98 cl-stl-0x11)
	(defun __is_permutation-imp-0c (first1 last1 first2 last2 pred) ; randomaccess_iterator version.
	  (declare (type cl:function pred))
	  (if (/= (the fixnum (distance first1 last1))
			  (the fixnum (distance first2 last2)))
		  nil
		  (with-operators
			  (let ((first1 @~first1)
					(first2 @~first2))
				(for (nil (and (_/= first1 last1) (funcall pred *first1 *first2)) nil)
				  ++first1 ++first2)
				(if (_== first1 last1)
					t
					(imp0 first1 last1 first2 last2 pred)))))))

  ;;IMP; is_permutation : 1 - cci x  f
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((imp1 (cons1 last1 first2 last2 pred)
			 (declare (type cl:list cons1 last1))
			 (declare (type cl:function pred))
			 (let ((scan cons1))
			   (declare (type cl:list scan))
			   (for (nil (not (eq scan last1)) (setf scan (cdr scan)) :returns t)
				 (labels ((pred2 (arg)
							(funcall pred arg (car scan))))
				   (unless (not (eq scan (__find_if-imp-1 cons1 scan #'pred2)))
					   (let ((matches (__count_if-imp-0 first2 last2 #'pred2)))
						 (when (or (zerop matches)
								   (/= matches (__count_if-imp-1 scan last1 #'pred2)))
						   (return-from imp1 nil)))))))))

	(defun __is_permutation-imp-1a (cons1 last1 first2 pred)
	  (declare (type cl:list cons1 last1))
	  (declare (type cl:function pred))
	  (with-operators
		  (let ((itr2 @~first2))
			(for (nil (and (not (eq cons1 last1))
						   (funcall pred (car cons1) *itr2)) nil)
			  ++itr2
			  (setf cons1 (cdr cons1)))
			(if (eq cons1 last1)
				t
				(imp1 cons1 last1 itr2 (next itr2 (__conslist-count-nodes cons1 last1)) pred)))))

	#-(or cl-stl-0x98 cl-stl-0x11)
	(defun __is_permutation-imp-1b (cons1 last1 first2 last2 pred)
	  (declare (type cl:list cons1 last1))
	  (declare (type cl:function pred))
	  (if (/= (the fixnum (distance first2 last2))
			  (the fixnum (__conslist-count-nodes cons1 last1)))
		  nil
		  (with-operators
			  (let ((itr2 @~first2))
				(for (nil (and (not (eq cons1 last1))
							   (funcall pred (car cons1) *itr2)) nil)
				  ++itr2
				  (setf cons1 (cdr cons1)))
				(if (eq cons1 last1)
					t
					(imp1 cons1 last1 itr2 last2 pred)))))))

  ;;IMP; is_permutation : 2 - cvp x  f 
  (labels ((imp2 (idx1 last1 buf1 first2 last2 pred)
			 (declare (type fixnum idx1 last1))
			 (declare (type cl:vector buf1))
			 (declare (type cl:function pred))
			 (let ((scan idx1))
			   (declare (type fixnum scan))
			   (for (nil (< scan last1) (incf scan) :returns t)
				 (labels ((pred2 (arg)
							(funcall pred arg (aref buf1 scan))))
				   (unless (_/= scan (__find_if-imp-2 idx1 scan buf1 #'pred2))
					   (let ((matches (__count_if-imp-0 first2 last2 #'pred2)))
						 (when (or (zerop matches)
								   (/= matches (__count_if-imp-2 scan last1 buf1 #'pred2)))
						   (return-from imp2 nil)))))))))

	(defun __is_permutation-imp-2a (idx1 last1 buf1 first2 pred)
	  (declare (type fixnum idx1 last1))
	  (declare (type cl:vector buf1))
	  (declare (type cl:function pred))
	  (with-operators
		  (let ((itr2 @~first2))
			(for (nil (and (< idx1 last1)
						   (funcall pred (aref buf1 idx1) *itr2)) nil)
			  ++itr2
			  (incf idx1))
			(if (= idx1 last1)
				t
				(imp2 idx1 last1 buf1 itr2
					  (next itr2 (the fixnum (- last1 idx1))) pred)))))

	#-(or cl-stl-0x98 cl-stl-0x11)
	(defun __is_permutation-imp-2b (idx1 last1 buf1 first2 last2 pred)
	  (declare (type fixnum idx1 last1))
	  (declare (type cl:vector buf1))
	  (declare (type cl:function pred))
	  (if (/= (- last1 idx1) (the fixnum (distance first2 last2)))
		  nil
		  (with-operators
			  (let ((itr2 @~first2))
				(for (nil (and (< idx1 last1)
							   (funcall pred (aref buf1 idx1) *itr2)) nil)
				  ++itr2
				  (incf idx1))
				(if (= idx1 last1)
					t
					(imp2 idx1 last1 buf1 itr2 last2 pred)))))))

  ;;IMP; is_permutation : 3 -  f  x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((imp3 (first1 last1 cons2 last2 pred)
			 (declare (type cl:list cons2 last2))
			 (declare (type cl:function pred))
			 (with-operators
				 (for (((scan @~first1)) (_/= scan last1) ++scan :returns t)
				   (labels ((pred2 (arg)
							  (funcall pred arg *scan)))
					 (unless (_/= scan (__find_if-imp-0 first1 scan #'pred2))
					   (let ((matches (__count_if-imp-1 cons2 last2 #'pred2)))
						 (when (or (zerop matches)
								   (/= matches (__count_if-imp-0 scan last1 #'pred2)))
						   (return-from imp3 nil)))))))))

	(defun __is_permutation-imp-3a (first1 last1 cons2 pred)
	  (declare (type cl:list cons2))
	  (declare (type cl:function pred))
	  (with-operators
		  (let ((itr1 @~first1))
			(for (nil (and (_/= itr1 last1)
						   (funcall pred *itr1 (car cons2))) nil)
			  ++itr1
			  (setf cons2 (cdr cons2)))
			(if (_== itr1 last1)
				t
				(imp3 itr1 last1 cons2 (nthcdr (distance itr1 last1) cons2) pred)))))

	#-(or cl-stl-0x98 cl-stl-0x11)
	(defun __is_permutation-imp-3b (first1 last1 cons2 last2 pred)
	  (declare (type cl:list cons2 last2))
	  (declare (type cl:function pred))
	  (if (/= (the fixnum (distance first1 last1))
			  (the fixnum (__conslist-count-nodes cons2 last2)))
		  nil
		  (with-operators
			  (let ((itr1 @~first1))
				(for (nil (and (_/= itr1 last1)
							   (funcall pred *itr1 (car cons2))) nil)
				  ++itr1
				  (setf cons2 (cdr cons2)))
				(if (_== itr1 last1)
					t
					(imp3 itr1 last1 cons2 last2 pred)))))))

  ;;IMP; is_permutation : 4 - cci x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((imp4 (cons1 last1 cons2 last2 pred)
			 (declare (type cl:list cons1 last1 cons2 last2))
			 (declare (type cl:function pred))
			 (let ((scan cons1))
			   (declare (type cl:list scan))
			   (for (nil (not (eq scan last1)) (setf scan (cdr scan)) :returns t)
				 (labels ((pred2 (arg)
							(funcall pred arg (car scan))))
				   (unless (not (eq scan (__find_if-imp-1 cons1 scan #'pred2)))
					 (let ((matches (__count_if-imp-1 cons2 last2 #'pred2)))
					   (when (or (zerop matches)
								 (/= matches (__count_if-imp-1 scan last1 #'pred2)))
						 (return-from imp4 nil)))))))))

	(defun __is_permutation-imp-4a (cons1 last1 cons2 pred)
	  (declare (type cl:list cons1 last1 cons2))
	  (declare (type cl:function pred))
	  (for (nil (and (not (eq cons1 last1))
					 (funcall pred (car cons1) (car cons2))) nil)
		(setf cons1 (cdr cons1))
		(setf cons2 (cdr cons2)))
	  (if (eq cons1 last1)
		  t
		  (imp4 cons1 last1 cons2 (nthcdr (__conslist-count-nodes cons1 last1) cons2) pred)))

	#-(or cl-stl-0x98 cl-stl-0x11)
	(defun __is_permutation-imp-4b (cons1 last1 cons2 last2 pred)
	  (declare (type cl:list cons1 last1 cons2 last2))
	  (declare (type cl:function pred))
	  (if (/= (the fixnum (__conslist-count-nodes cons1 last1))
			  (the fixnum (__conslist-count-nodes cons2 last2)))
		  nil
		  (progn
			(for (nil (and (not (eq cons1 last1))
						   (funcall pred (car cons1) (car cons2))) nil)
			  (setf cons1 (cdr cons1))
			  (setf cons2 (cdr cons2)))
			(if (eq cons1 last1)
				t
				(imp4 cons1 last1 cons2 last2 pred))))))

  ;;IMP; is_permutation : 5 - cvp x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((imp5 (idx1 last1 buf1 cons2 last2 pred)
			 (declare (type fixnum idx1 last1))
			 (declare (type cl:vector buf1))
			 (declare (type cl:list cons2 last2))
			 (declare (type cl:function pred))
			 (let ((scan idx1))
			   (declare (type fixnum scan))
			   (for (nil (< scan last1) (incf scan) :returns t)
				 (labels ((pred2 (arg)
							(funcall pred arg (aref buf1 scan))))
				   (unless (/= scan (__find_if-imp-2 idx1 scan buf1 #'pred2))
					 (let ((matches (__count_if-imp-1 cons2 last2 #'pred2)))
					   (when (or (zerop matches)
								 (/= matches (__count_if-imp-2 scan last1 buf1 #'pred2)))
						 (return-from imp5 nil)))))))))

	(defun __is_permutation-imp-5a (idx1 last1 buf1 cons2 pred)
	  (declare (type fixnum idx1 last1))
	  (declare (type cl:vector buf1))
	  (declare (type cl:list cons2))
	  (declare (type cl:function pred))
	  (for (nil (and (< idx1 last1)
					 (funcall pred (aref buf1 idx1) (car cons2))) nil)
		(incf idx1)
		(setf cons2 (cdr cons2)))
	  (if (= idx1 last1)
		  t
		  (imp5 idx1 last1 buf1 cons2
				(nthcdr (the fixnum (- last1 idx1)) cons2) pred)))

	#-(or cl-stl-0x98 cl-stl-0x11)
	(defun __is_permutation-imp-5b (idx1 last1 buf1 cons2 last2 pred)
	  (declare (type fixnum idx1 last1))
	  (declare (type cl:vector buf1))
	  (declare (type cl:list cons2 last2))
	  (declare (type cl:function pred))
	  (if (/= (- last1 idx1) (the fixnum (__conslist-count-nodes cons2 last2)))
		  nil
		  (progn
			(for (nil (and (< idx1 last1)
						   (funcall pred (aref buf1 idx1) (car cons2))) nil)
			  (incf idx1)
			  (setf cons2 (cdr cons2)))
			(if (= idx1 last1)
				t
				(imp5 idx1 last1 buf1 cons2 last2 pred))))))

  ;;IMP; is_permutation : 6 -  f  x cvp
  (labels ((imp6 (first1 last1 idx2 last2 buf2 pred)
			 (declare (type fixnum idx2 last2))
			 (declare (type cl:vector buf2))
			 (declare (type cl:function pred))
			 (with-operators
				 (for (((scan @~first1)) (_/= scan last1) ++scan :returns t)
				   (labels ((pred2 (arg)
							  (funcall pred arg *scan)))
					 (unless (_/= scan (__find_if-imp-0 first1 scan #'pred2))
					   (let ((matches (__count_if-imp-2 idx2 last2 buf2 #'pred2)))
						 (when (or (zerop matches)
								   (/= matches (__count_if-imp-0 scan last1 #'pred2)))
						   (return-from imp6 nil)))))))))

	(defun __is_permutation-imp-6a (first1 last1 idx2 buf2 pred)
	  (declare (type fixnum idx2))
	  (declare (type cl:vector buf2))
	  (declare (type cl:function pred))
	  (with-operators
		  (let ((itr1 @~first1))
			(for (nil (and (_/= itr1 last1)
						   (funcall pred *itr1 (aref buf2 idx2))) nil)
			  ++itr1
			  (incf idx2))
			(if (_== itr1 last1)
				t
				(imp6 itr1 last1 idx2
					  (+ idx2 (the fixnum (distance itr1 last1))) buf2 pred)))))

	#-(or cl-stl-0x98 cl-stl-0x11)
	(defun __is_permutation-imp-6b (first1 last1 idx2 last2 buf2 pred)
	  (declare (type fixnum idx2 last2))
	  (declare (type cl:vector buf2))
	  (declare (type cl:function pred))
	  (if (/= (- last2 idx2) (the fixnum (distance first1 last1)))
		  nil
		  (with-operators
			  (let ((itr1 @~first1))
				(for (nil (and (_/= itr1 last1)
							   (funcall pred *itr1 (aref buf2 idx2))) nil)
				  ++itr1
				  (incf idx2))
				(if (_== itr1 last1)
					t
					(imp6 itr1 last1 idx2 last2 buf2 pred)))))))

  ;;IMP; is_permutation : 7 - cci x cvp
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((imp7 (cons1 last1 idx2 last2 buf2 pred)
			 (declare (type cl:list cons1 last1))
			 (declare (type fixnum idx2 last2))
			 (declare (type cl:vector buf2))
			 (declare (type cl:function pred))
			 (let ((scan cons1))
			   (declare (type cl:list scan))
			   (for (nil (not (eq scan last1)) (setf scan (cdr scan)) :returns t)
				 (labels ((pred2 (arg)
							(funcall pred arg (car scan))))
				   (unless (not (eq scan (__find_if-imp-1 cons1 scan #'pred2)))
					 (let ((matches (__count_if-imp-2 idx2 last2 buf2 #'pred2)))
					   (when (or (zerop matches)
								 (/= matches (__count_if-imp-1 scan last1 #'pred2)))
						 (return-from imp7 nil)))))))))

	(defun __is_permutation-imp-7a (cons1 last1 idx2 buf2 pred)
	  (declare (type cl:list cons1 last1))
	  (declare (type fixnum idx2))
	  (declare (type cl:vector buf2))
	  (declare (type cl:function pred))
	  (for (nil (and (not (eq cons1 last1))
					 (funcall pred (car cons1)
								   (aref buf2 idx2))) nil)
		(setf cons1 (cdr cons1))
		(incf idx2))
	  (if (eq cons1 last1)
		  t
		  (imp7 cons1 last1 idx2
				(+ idx2 (the fixnum (__conslist-count-nodes cons1 last1))) buf2 pred)))

	#-(or cl-stl-0x98 cl-stl-0x11)
	(defun __is_permutation-imp-7b (cons1 last1 idx2 last2 buf2 pred)
	  (declare (type cl:list cons1 last1))
	  (declare (type fixnum idx2 last2))
	  (declare (type cl:vector buf2))
	  (declare (type cl:function pred))
	  (if (/= (- last2 idx2) (the fixnum (__conslist-count-nodes cons1 last1)))
		  nil
		  (progn
			(for (nil (and (not (eq cons1 last1))
						   (funcall pred (car cons1)
										 (aref buf2 idx2))) nil)
			  (setf cons1 (cdr cons1))
			  (incf idx2))
			(if (eq cons1 last1)
				t
				(imp7 cons1 last1 idx2 last2 buf2 pred))))))

  ;;IMP; is_permutation : 8 - cvp x cvp
  (labels ((imp8 (idx1 last1 buf1 idx2 last2 buf2 pred)
			 (declare (type fixnum idx1 last1 idx2 last2))
			 (declare (type cl:vector buf1 buf2))
			 (declare (type cl:function pred))
			 (let ((scan idx1))
			   (declare (type fixnum scan))
			   (for (nil (< scan last1) (incf scan) :returns t)
				 (labels ((pred2 (arg)
							(funcall pred arg (aref buf1 scan))))
				   (unless (/= scan (__find_if-imp-2 idx1 scan buf1 #'pred2))
					 (let ((matches (__count_if-imp-2 idx2 last2 buf2 #'pred2)))
					   (when (or (zerop matches)
								 (/= matches (__count_if-imp-2 scan last1 buf1 #'pred2)))
						 (return-from imp8 nil)))))))))

	(defun __is_permutation-imp-8a (idx1 last1 buf1 idx2 buf2 pred)
	  (declare (type fixnum idx1 last1 idx2))
	  (declare (type cl:vector buf1 buf2))
	  (declare (type cl:function pred))
	  (for (nil (and (< idx1 last1)
					 (funcall pred (aref buf1 idx1)
								   (aref buf2 idx2))) nil)
		(incf idx1)
		(incf idx2))
	  (if (= idx1 last1)
		  t
		  (imp8 idx1 last1 buf1 idx2
				(+ idx2 (the fixnum (- last1 idx1))) buf2 pred)))

	#-(or cl-stl-0x98 cl-stl-0x11)
	(defun __is_permutation-imp-8b (idx1 last1 buf1 idx2 last2 buf2 pred)
	  (declare (type fixnum idx1 last1 idx2 last2))
	  (declare (type cl:vector buf1 buf2))
	  (declare (type cl:function pred))
	  (if (/= (- last1 idx1) (- last2 idx2))
		  nil
		  (progn
			(for (nil (and (< idx1 last1)
						   (funcall pred (aref buf1 idx1)
									(aref buf2 idx2))) nil)
			  (incf idx1)
			  (incf idx2))
			(if (= idx1 last1)
				t
				(imp8 idx1 last1 buf1 idx2 last2 buf2 pred)))))))


;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  ;;IMP; set_union : 00 -   i  x  i  x  o 
  (defun __set_union-imp-00 (first1 last1 first2 last2 result comp)
	(declare (type cl:function comp))
	(with-operators
		(for (((itr1 @~first1) (itr2 @~first2) (dest @~result)) t nil)
		  (let ((is-last1 (_== itr1 last1))
				(is-last2 (_== itr2 last2)))
			(cond
			  ((and is-last1 is-last2) (return-from __set_union-imp-00 dest))
			  (is-last1 (_= *dest *itr2) ++itr2 ++dest)
			  (is-last2 (_= *dest *itr1) ++itr1 ++dest)
			  (t (let ((v1 *itr1)
					   (v2 *itr2))
				   (cond
					 ((funcall comp v1 v2) (_= *dest    v1) ++itr1 ++dest)
					 ((funcall comp v2 v1) (_= *dest *itr2) ++itr2 ++dest)
					 (t (_= *dest *itr1) ++itr1 ++itr2 ++dest)))))))))

  ;;IMP; set_union : 01 -   i  x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_union-imp-01 (first1 last1 first2 last2 out less-bf)
	(declare (type cl:list out))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)
			   (itr2 @~first2)) t nil)
		  (let ((is-end1 (_== itr1 last1))
				(is-end2 (_== itr2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_union-imp-01 out))
				  (is-end1 (_= (car out) *itr2) ++itr2 (setf out (cdr out)))
				  (is-end2 (_= (car out) *itr1) ++itr1 (setf out (cdr out)))
				  (t (let ((val1 *itr1)
						   (val2 *itr2))
					   (cond ((funcall less-bf val1 val2) (_= (car out) val1) ++itr1        (setf out (cdr out)))
							 ((funcall less-bf val2 val1) (_= (car out) val2)        ++itr2 (setf out (cdr out)))
							 (t                           (_= (car out) val1) ++itr1 ++itr2 (setf out (cdr out)))))))))))

  ;;IMP; set_union : 02 -   i  x  i  x  vp
  (defun __set_union-imp-02 (first1 last1 first2 last2 out-idx out-buf less-bf)
	(declare (type fixnum out-idx))
	(declare (type cl:vector out-buf))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)
			   (itr2 @~first2)) t nil)
		  (let ((is-end1 (_== itr1 last1))
				(is-end2 (_== itr2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_union-imp-02 out-idx))
				  (is-end1 (_= (aref out-buf out-idx) *itr2) ++itr2 (incf out-idx))
				  (is-end2 (_= (aref out-buf out-idx) *itr1) ++itr1 (incf out-idx))
				  (t (let ((val1 *itr1)
						   (val2 *itr2))
					   (cond ((funcall less-bf val1 val2) (_= (aref out-buf out-idx) val1) ++itr1        (incf out-idx))
							 ((funcall less-bf val2 val1) (_= (aref out-buf out-idx) val2)        ++itr2 (incf out-idx))
							 (t                           (_= (aref out-buf out-idx) val1) ++itr1 ++itr2 (incf out-idx))))))))))

  ;;IMP; set_union : 03 -   i  x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_union-imp-03 (first1 last1 cons2 last2 oitr less-bf)
	(declare (type cl:list cons2 last2))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)) t nil)
		  (let ((is-end1 (_== itr1 last1))
				(is-end2 (eq cons2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_union-imp-03 oitr))
				  (is-end1 (_= *oitr (car cons2)) (setf cons2 (cdr cons2)) ++oitr)
				  (is-end2 (_= *oitr *itr1)       ++itr1                   ++oitr)
				  (t (let ((val1 *itr1)
						   (val2 (car cons2)))
					   (cond ((funcall less-bf val1 val2) (_= *oitr val1) ++itr1                          ++oitr)
							 ((funcall less-bf val2 val1) (_= *oitr val2)        (setf cons2 (cdr cons2)) ++oitr)
							 (t                           (_= *oitr val1) ++itr1 (setf cons2 (cdr cons2)) ++oitr)))))))))

  ;;IMP; set_union : 04 -   i  x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_union-imp-04 (first1 last1 cons2 last2 out less-bf)
	(declare (type cl:list cons2 last2 out))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)) t nil)
		  (let ((is-end1 (_== itr1 last1))
				(is-end2 (eq cons2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_union-imp-04 out))
				  (is-end1 (_= (car out) (car cons2)) (setf cons2 (cdr cons2)) (setf out (cdr out)))
				  (is-end2 (_= (car out) *itr1)       ++itr1                   (setf out (cdr out)))
				  (t (let ((val1 *itr1)
						   (val2 (car cons2)))
					   (cond ((funcall less-bf val1 val2) (_= (car out) val1) ++itr1                          (setf out (cdr out)))
							 ((funcall less-bf val2 val1) (_= (car out) val2)        (setf cons2 (cdr cons2)) (setf out (cdr out)))
							 (t                           (_= (car out) val1) ++itr1 (setf cons2 (cdr cons2)) (setf out (cdr out)))))))))))

  ;;IMP; set_union : 05 -   i  x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_union-imp-05 (first1 last1 cons2 last2 out-idx out-buf less-bf)
	(declare (type cl:list cons2 last2))
	(declare (type fixnum out-idx))
	(declare (type cl:vector out-buf))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)) t nil)
		  (let ((is-end1 (_== itr1 last1))
				(is-end2 (eq cons2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_union-imp-05 out-idx))
				  (is-end1 (_= (aref out-buf out-idx) (car cons2)) (setf cons2 (cdr cons2)) (incf out-idx))
				  (is-end2 (_= (aref out-buf out-idx) *itr1)       ++itr1                   (incf out-idx))
				  (t (let ((val1 *itr1)
						   (val2 (car cons2)))
					   (cond ((funcall less-bf val1 val2) (_= (aref out-buf out-idx) val1) ++itr1                          (incf out-idx))
							 ((funcall less-bf val2 val1) (_= (aref out-buf out-idx) val2)        (setf cons2 (cdr cons2)) (incf out-idx))
							 (t                           (_= (aref out-buf out-idx) val1) ++itr1 (setf cons2 (cdr cons2)) (incf out-idx))))))))))

  ;;IMP; set_union : 06 -   i  x cvp x  o 
  (defun __set_union-imp-06 (first1 last1 idx2 last2 buf2 oitr less-bf)
	(declare (type fixnum idx2 last2))
	(declare (type cl:vector buf2))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)) t nil)
		  (let ((is-end1 (_== itr1 last1))
				(is-end2 (= idx2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_union-imp-06 oitr))
				  (is-end1 (_= *oitr (aref buf2 idx2)) (incf idx2) ++oitr)
				  (is-end2 (_= *oitr *itr1)               ++itr1      ++oitr)
				  (t (let ((val1 *itr1)
						   (val2 (aref buf2 idx2)))
					   (cond ((funcall less-bf val1 val2) (_= *oitr val1) ++itr1             ++oitr)
							 ((funcall less-bf val2 val1) (_= *oitr val2)        (incf idx2) ++oitr)
							 (t                           (_= *oitr val1) ++itr1 (incf idx2) ++oitr)))))))))

  ;;IMP; set_union : 07 -   i  x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_union-imp-07 (first1 last1 idx2 last2 buf2 out less-bf)
	(declare (type fixnum idx2 last2))
	(declare (type cl:vector buf2))
	(declare (type cl:list out))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)) t nil)
		  (let ((is-end1 (_== itr1 last1))
				(is-end2 (= idx2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_union-imp-07 out))
				  (is-end1 (_= (car out) (aref buf2 idx2)) (incf idx2) (setf out (cdr out)))
				  (is-end2 (_= (car out) *itr1)                ++itr1      (setf out (cdr out)))
				  (t (let ((val1 *itr1)
						   (val2 (aref buf2 idx2)))
					   (cond ((funcall less-bf val1 val2) (_= (car out) val1) ++itr1             (setf out (cdr out)))
							 ((funcall less-bf val2 val1) (_= (car out) val2)        (incf idx2) (setf out (cdr out)))
							 (t                           (_= (car out) val1) ++itr1 (incf idx2) (setf out (cdr out)))))))))))

  ;;IMP; set_union : 08 -   i  x cvp x  vp
  (defun __set_union-imp-08 (first1 last1 idx2 last2 buf2 out-idx out-buf less-bf)
	(declare (type fixnum idx2 last2 out-idx))
	(declare (type cl:vector buf2 out-buf))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)) t nil)
		  (let ((is-end1 (_== itr1 last1))
				(is-end2 (= idx2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_union-imp-08 out-idx))
				  (is-end1 (_= (aref out-buf out-idx) (aref buf2 idx2)) (incf idx2) (incf out-idx))
				  (is-end2 (_= (aref out-buf out-idx) *itr1)                ++itr1      (incf out-idx))
				  (t (let ((val1 *itr1)
						   (val2 (aref buf2 idx2)))
					   (cond ((funcall less-bf val1 val2) (_= (aref out-buf out-idx) val1) ++itr1             (incf out-idx))
							 ((funcall less-bf val2 val1) (_= (aref out-buf out-idx) val2)        (incf idx2) (incf out-idx))
							 (t                           (_= (aref out-buf out-idx) val1) ++itr1 (incf idx2) (incf out-idx))))))))))

  ;;IMP; set_union : 09 -  cci x  i  x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_union-imp-09 (cons1 last1 first2 last2 oitr less-bf)
	(declare (type cl:list cons1 last1))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr2 @~first2)) t nil)
		  (let ((is-end1 (eq cons1 last1))
				(is-end2 (_== itr2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_union-imp-09 oitr))
				  (is-end1 (_= *oitr *itr2)       ++itr2                   ++oitr)
				  (is-end2 (_= *oitr (car cons1)) (setf cons1 (cdr cons1)) ++oitr)
				  (t (let ((val1 (car cons1))
						   (val2 *itr2))
					   (cond ((funcall less-bf val1 val2) (_= *oitr val1) (setf cons1 (cdr cons1))        ++oitr)
							 ((funcall less-bf val2 val1) (_= *oitr val2)                          ++itr2 ++oitr)
							 (t                           (_= *oitr val1) (setf cons1 (cdr cons1)) ++itr2 ++oitr)))))))))

  ;;IMP; set_union : 10 -  cci x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_union-imp-10 (cons1 last1 first2 last2 out less-bf)
	(declare (type cl:list cons1 last1 out))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr2 @~first2)) t nil)
		  (let ((is-end1 (eq cons1 last1))
				(is-end2 (_== itr2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_union-imp-10 out))
				  (is-end1 (_= (car out) *itr2)       ++itr2                   (setf out (cdr out)))
				  (is-end2 (_= (car out) (car cons1)) (setf cons1 (cdr cons1)) (setf out (cdr out)))
				  (t (let ((val1 (car cons1))
						   (val2 *itr2))
					   (cond ((funcall less-bf val1 val2) (_= (car out) val1) (setf cons1 (cdr cons1))        (setf out (cdr out)))
							 ((funcall less-bf val2 val1) (_= (car out) val2)                          ++itr2 (setf out (cdr out)))
							 (t                           (_= (car out) val1) (setf cons1 (cdr cons1)) ++itr2 (setf out (cdr out)))))))))))

  ;;IMP; set_union : 11 -  cci x  i  x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_union-imp-11 (cons1 last1 first2 last2 out-idx out-buf less-bf)
	(declare (type cl:list cons1 last1))
	(declare (type fixnum out-idx))
	(declare (type cl:vector out-buf))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr2 @~first2)) t nil)
		  (let ((is-end1 (eq cons1 last1))
				(is-end2 (_== itr2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_union-imp-11 out-idx))
				  (is-end1 (_= (aref out-buf out-idx) *itr2)       ++itr2                   (incf out-idx))
				  (is-end2 (_= (aref out-buf out-idx) (car cons1)) (setf cons1 (cdr cons1)) (incf out-idx))
				  (t (let ((val1 (car cons1))
						   (val2 *itr2))
					   (cond ((funcall less-bf val1 val2) (_= (aref out-buf out-idx) val1) (setf cons1 (cdr cons1))        (incf out-idx))
							 ((funcall less-bf val2 val1) (_= (aref out-buf out-idx) val2)                          ++itr2 (incf out-idx))
							 (t                           (_= (aref out-buf out-idx) val1) (setf cons1 (cdr cons1)) ++itr2 (incf out-idx))))))))))

  ;;IMP; set_union : 12 -  cci x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_union-imp-12 (cons1 last1 cons2 last2 oitr less-bf)
	(declare (type cl:list cons1 last1 cons2 last2))
	(declare (type cl:function less-bf))
	(for (nil t nil)
	  (with-operators
		  (let ((is-end1 (eq cons1 last1))
				(is-end2 (eq cons2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_union-imp-12 oitr))
				  (is-end1 (_= *oitr (car cons2)) (setf cons2 (cdr cons2)) ++oitr)
				  (is-end2 (_= *oitr (car cons1)) (setf cons1 (cdr cons1)) ++oitr)
				  (t (let ((val1 (car cons1))
						   (val2 (car cons2)))
					   (cond ((funcall less-bf val1 val2) (_= *oitr val1) (setf cons1 (cdr cons1))                          ++oitr)
							 ((funcall less-bf val2 val1) (_= *oitr val2)                          (setf cons2 (cdr cons2)) ++oitr)
							 (t                           (_= *oitr val1) (setf cons1 (cdr cons1)) (setf cons2 (cdr cons2)) ++oitr)))))))))

  ;;IMP; set_union : 13 -  cci x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_union-imp-13 (cons1 last1 cons2 last2 out less-bf)
	(declare (type cl:list cons1 last1 cons2 last2 out))
	(declare (type cl:function less-bf))
	(for (nil t nil)
	  (let ((is-end1 (eq cons1 last1))
			(is-end2 (eq cons2 last2)))
		(cond ((and is-end1 is-end2) (return-from __set_union-imp-13 out))
			  (is-end1 (_= (car out) (car cons2)) (setf cons2 (cdr cons2)) (setf out (cdr out)))
			  (is-end2 (_= (car out) (car cons1)) (setf cons1 (cdr cons1)) (setf out (cdr out)))
			  (t (let ((val1 (car cons1))
					   (val2 (car cons2)))
				   (cond ((funcall less-bf val1 val2) (_= (car out) val1) (setf cons1 (cdr cons1))                          (setf out (cdr out)))
						 ((funcall less-bf val2 val1) (_= (car out) val2)                          (setf cons2 (cdr cons2)) (setf out (cdr out)))
						 (t                           (_= (car out) val1) (setf cons1 (cdr cons1)) (setf cons2 (cdr cons2)) (setf out (cdr out))))))))))

  ;;IMP; set_union : 14 -  cci x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_union-imp-14 (cons1 last1 cons2 last2 out-idx out-buf less-bf)
	(declare (type cl:list cons1 last1 cons2 last2))
	(declare (type fixnum out-idx))
	(declare (type cl:vector out-buf))
	(declare (type cl:function less-bf))
	(for (nil t nil)
	  (let ((is-end1 (eq cons1 last1))
			(is-end2 (eq cons2 last2)))
		(cond ((and is-end1 is-end2) (return-from __set_union-imp-14 out-idx))
			  (is-end1 (_= (aref out-buf out-idx) (car cons2)) (setf cons2 (cdr cons2)) (incf out-idx))
			  (is-end2 (_= (aref out-buf out-idx) (car cons1)) (setf cons1 (cdr cons1)) (incf out-idx))
			  (t (let ((val1 (car cons1))
					   (val2 (car cons2)))
				   (cond ((funcall less-bf val1 val2) (_= (aref out-buf out-idx) val1) (setf cons1 (cdr cons1))                          (incf out-idx))
						 ((funcall less-bf val2 val1) (_= (aref out-buf out-idx) val2)                          (setf cons2 (cdr cons2)) (incf out-idx))
						 (t                           (_= (aref out-buf out-idx) val1) (setf cons1 (cdr cons1)) (setf cons2 (cdr cons2)) (incf out-idx)))))))))

  ;;IMP; set_union : 15 -  cci x cvp x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_union-imp-15 (cons1 last1 idx2 last2 buf2 oitr less-bf)
	(declare (type cl:list cons1 last1))
	(declare (type fixnum idx2 last2))
	(declare (type cl:vector buf2))
	(declare (type cl:function less-bf))
	(for (nil t nil)
	  (with-operators
		  (let ((is-end1 (eq cons1 last1))
				(is-end2 (= idx2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_union-imp-15 oitr))
				  (is-end1 (_= *oitr (aref buf2 idx2)) (incf idx2)              ++oitr)
				  (is-end2 (_= *oitr (car cons1))      (setf cons1 (cdr cons1)) ++oitr)
				  (t (let ((val1 (car cons1))
						   (val2 (aref buf2 idx2)))
					   (cond ((funcall less-bf val1 val2) (_= *oitr val1) (setf cons1 (cdr cons1))             ++oitr)
							 ((funcall less-bf val2 val1) (_= *oitr val2)                          (incf idx2) ++oitr)
							 (t                           (_= *oitr val1) (setf cons1 (cdr cons1)) (incf idx2) ++oitr)))))))))

  ;;IMP; set_union : 16 -  cci x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_union-imp-16 (cons1 last1 idx2 last2 buf2 out less-bf)
	(declare (type cl:list cons1 last1 out))
	(declare (type fixnum idx2 last2))
	(declare (type cl:vector buf2))
	(declare (type cl:list))
	(declare (type cl:function less-bf))
	(for (nil t nil)
	  (let ((is-end1 (eq cons1 last1))
			(is-end2 (= idx2 last2)))
		(cond ((and is-end1 is-end2) (return-from __set_union-imp-16 out))
			  (is-end1 (_= (car out) (aref buf2 idx2)) (incf idx2)              (setf out (cdr out)))
			  (is-end2 (_= (car out) (car cons1))      (setf cons1 (cdr cons1)) (setf out (cdr out)))
			  (t (let ((val1 (car cons1))
					   (val2 (aref buf2 idx2)))
				   (cond ((funcall less-bf val1 val2) (_= (car out) val1) (setf cons1 (cdr cons1))             (setf out (cdr out)))
						 ((funcall less-bf val2 val1) (_= (car out) val2)                          (incf idx2) (setf out (cdr out)))
						 (t                           (_= (car out) val1) (setf cons1 (cdr cons1)) (incf idx2) (setf out (cdr out))))))))))

  ;;IMP; set_union : 17 -  cci x cvp x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_union-imp-17 (cons1 last1 idx2 last2 buf2 out-idx out-buf less-bf)
	(declare (type cl:list cons1 last1))
	(declare (type fixnum idx2 last2 out-idx))
	(declare (type cl:vector buf2 out-buf))
	(declare (type cl:function less-bf))
	(for (nil t nil)
	  (let ((is-end1 (eq cons1 last1))
			(is-end2 (= idx2 last2)))
		(cond ((and is-end1 is-end2) (return-from __set_union-imp-17 out-idx))
			  (is-end1 (_= (aref out-buf out-idx) (aref buf2 idx2)) (incf idx2)              (incf out-idx))
			  (is-end2 (_= (aref out-buf out-idx) (car cons1))      (setf cons1 (cdr cons1)) (incf out-idx))
			  (t (let ((val1 (car cons1))
					   (val2 (aref buf2 idx2)))
				   (cond ((funcall less-bf val1 val2) (_= (aref out-buf out-idx) val1) (setf cons1 (cdr cons1))             (incf out-idx))
						 ((funcall less-bf val2 val1) (_= (aref out-buf out-idx) val2)                          (incf idx2) (incf out-idx))
						 (t                           (_= (aref out-buf out-idx) val1) (setf cons1 (cdr cons1)) (incf idx2) (incf out-idx)))))))))

  ;;IMP; set_union : 18 -  cvp x  i  x  o 
  (defun __set_union-imp-18 (idx1 last1 buf1 first2 last2 oitr less-bf)
	(declare (type fixnum idx1 last1))
	(declare (type cl:vector buf1))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr2 @~first2)) t nil)
		  (let ((is-end1 (= idx1 last1))
				(is-end2 (_== itr2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_union-imp-18 oitr))
				  (is-end1 (_= *oitr *itr2)            ++itr2      ++oitr)
				  (is-end2 (_= *oitr (aref buf1 idx1)) (incf idx1) ++oitr)
				  (t (let ((val1 (aref buf1 idx1))
						   (val2 *itr2))
					   (cond ((funcall less-bf val1 val2) (_= *oitr val1) (incf idx1)        ++oitr)
							 ((funcall less-bf val2 val1) (_= *oitr val2)             ++itr2 ++oitr)
							 (t                           (_= *oitr val1) (incf idx1) ++itr2 ++oitr)))))))))

  ;;IMP; set_union : 19 -  cvp x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_union-imp-19 (idx1 last1 buf1 first2 last2 out less-bf)
	(declare (type fixnum idx1 last1))
	(declare (type cl:vector buf1))
	(declare (type cl:list out))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr2 @~first2)) t nil)
		  (let ((is-end1 (= idx1 last1))
				(is-end2 (_== itr2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_union-imp-19 out))
				  (is-end1 (_= (car out) *itr2)            ++itr2      (setf out (cdr out)))
				  (is-end2 (_= (car out) (aref buf1 idx1)) (incf idx1) (setf out (cdr out)))
				  (t (let ((val1 (aref buf1 idx1))
						   (val2 *itr2))
					   (cond ((funcall less-bf val1 val2) (_= (car out) val1) (incf idx1)        (setf out (cdr out)))
							 ((funcall less-bf val2 val1) (_= (car out) val2)             ++itr2 (setf out (cdr out)))
							 (t                           (_= (car out) val1) (incf idx1) ++itr2 (setf out (cdr out)))))))))))

  ;;IMP; set_union : 20 -  cvp x  i  x  vp
  (defun __set_union-imp-20 (idx1 last1 buf1 first2 last2 out-idx out-buf less-bf)
	(declare (type fixnum idx1 last1 out-idx))
	(declare (type cl:vector buf1 out-buf))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr2 @~first2)) t nil)
		  (let ((is-end1 (= idx1 last1))
				(is-end2 (_== itr2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_union-imp-20 out-idx))
				  (is-end1 (_= (aref out-buf out-idx) *itr2)            ++itr2      (incf out-idx))
				  (is-end2 (_= (aref out-buf out-idx) (aref buf1 idx1)) (incf idx1) (incf out-idx))
				  (t (let ((val1 (aref buf1 idx1))
						   (val2 *itr2))
					   (cond ((funcall less-bf val1 val2) (_= (aref out-buf out-idx) val1) (incf idx1)        (incf out-idx))
							 ((funcall less-bf val2 val1) (_= (aref out-buf out-idx) val2)             ++itr2 (incf out-idx))
							 (t                           (_= (aref out-buf out-idx) val1) (incf idx1) ++itr2 (incf out-idx))))))))))

  ;;IMP; set_union : 21 -  cvp x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_union-imp-21 (idx1 last1 buf1 cons2 last2 oitr less-bf)
	(declare (type fixnum idx1 last1))
	(declare (type cl:vector buf1))
	(declare (type cl:list cons2 last2))
	(declare (type cl:function less-bf))
	(for (nil t nil)
	  (with-operators
		  (let ((is-end1 (= idx1 last1))
				(is-end2 (eq cons2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_union-imp-21 oitr))
				  (is-end1 (_= *oitr (car cons2))      (setf cons2 (cdr cons2)) ++oitr)
				  (is-end2 (_= *oitr (aref buf1 idx1)) (incf idx1)              ++oitr)
				  (t (let ((val1 (aref buf1 idx1))
						   (val2 (car cons2)))
					   (cond ((funcall less-bf val1 val2) (_= *oitr val1) (incf idx1)                          ++oitr)
							 ((funcall less-bf val2 val1) (_= *oitr val2)             (setf cons2 (cdr cons2)) ++oitr)
							 (t                           (_= *oitr val1) (incf idx1) (setf cons2 (cdr cons2)) ++oitr)))))))))

  ;;IMP; set_union : 22 -  cvp x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_union-imp-22 (idx1 last1 buf1 cons2 last2 out less-bf)
	(declare (type fixnum idx1 last1))
	(declare (type cl:vector buf1))
	(declare (type cl:list cons2 last2 out))
	(declare (type cl:function less-bf))
	(for (nil t nil)
	  (let ((is-end1 (= idx1 last1))
			(is-end2 (eq cons2 last2)))
		(cond ((and is-end1 is-end2) (return-from __set_union-imp-22 out))
			  (is-end1 (_= (car out) (car cons2))      (setf cons2 (cdr cons2)) (setf out (cdr out)))
			  (is-end2 (_= (car out) (aref buf1 idx1)) (incf idx1)              (setf out (cdr out)))
			  (t (let ((val1 (aref buf1 idx1))
					   (val2 (car cons2)))
				   (cond ((funcall less-bf val1 val2) (_= (car out) val1) (incf idx1)                          (setf out (cdr out)))
						 ((funcall less-bf val2 val1) (_= (car out) val2)             (setf cons2 (cdr cons2)) (setf out (cdr out)))
						 (t                           (_= (car out) val1) (incf idx1) (setf cons2 (cdr cons2)) (setf out (cdr out))))))))))

  ;;IMP; set_union : 23 -  cvp x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_union-imp-23 (idx1 last1 buf1 cons2 last2 out-idx out-buf less-bf)
	(declare (type fixnum idx1 last1 out-idx))
	(declare (type cl:vector buf1 out-buf))
	(declare (type cl:list cons2 last2))
	(declare (type cl:function less-bf))
	(for (nil t nil)
	  (let ((is-end1 (= idx1 last1))
			(is-end2 (eq cons2 last2)))
		(cond ((and is-end1 is-end2) (return-from __set_union-imp-23 out-idx))
			  (is-end1 (_= (aref out-buf out-idx) (car cons2))      (setf cons2 (cdr cons2)) (incf out-idx))
			  (is-end2 (_= (aref out-buf out-idx) (aref buf1 idx1)) (incf idx1)              (incf out-idx))
			  (t (let ((val1 (aref buf1 idx1))
					   (val2 (car cons2)))
				   (cond ((funcall less-bf val1 val2) (_= (aref out-buf out-idx) val1) (incf idx1)                          (incf out-idx))
						 ((funcall less-bf val2 val1) (_= (aref out-buf out-idx) val2)             (setf cons2 (cdr cons2)) (incf out-idx))
						 (t                           (_= (aref out-buf out-idx) val1) (incf idx1) (setf cons2 (cdr cons2)) (incf out-idx)))))))))

  ;;IMP; set_union : 24 -  cvp x cvp x  o 
  (defun __set_union-imp-24 (idx1 last1 buf1 idx2 last2 buf2 oitr less-bf)
	(declare (type fixnum idx1 last1 idx2 last2))
	(declare (type cl:vector buf1 buf2))
	(declare (type cl:function less-bf))
	(for (nil t nil)
	  (with-operators
		  (let ((is-end1 (= idx1 last1))
				(is-end2 (= idx2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_union-imp-24 oitr))
				  (is-end1 (_= *oitr (aref buf2 idx2)) (incf idx2) ++oitr)
				  (is-end2 (_= *oitr (aref buf1 idx1)) (incf idx1) ++oitr)
				  (t (let ((val1 (aref buf1 idx1))
						   (val2 (aref buf2 idx2)))
					   (cond ((funcall less-bf val1 val2) (_= *oitr val1) (incf idx1)             ++oitr)
							 ((funcall less-bf val2 val1) (_= *oitr val2)             (incf idx2) ++oitr)
							 (t                           (_= *oitr val1) (incf idx1) (incf idx2) ++oitr)))))))))

  ;;IMP; set_union : 25 -  cvp x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_union-imp-25 (idx1 last1 buf1 idx2 last2 buf2 out less-bf)
	(declare (type fixnum idx1 last1 idx2 last2))
	(declare (type cl:vector buf1 buf2))
	(declare (type cl:list out))
	(declare (type cl:function less-bf))
	(for (nil t nil)
	  (let ((is-end1 (= idx1 last1))
			(is-end2 (= idx2 last2)))
		(cond ((and is-end1 is-end2) (return-from __set_union-imp-25 out))
			  (is-end1 (_= (car out) (aref buf2 idx2)) (incf idx2) (setf out (cdr out)))
			  (is-end2 (_= (car out) (aref buf1 idx1)) (incf idx1) (setf out (cdr out)))
			  (t (let ((val1 (aref buf1 idx1))
					   (val2 (aref buf2 idx2)))
				   (cond ((funcall less-bf val1 val2) (_= (car out) val1) (incf idx1)             (setf out (cdr out)))
						 ((funcall less-bf val2 val1) (_= (car out) val2)             (incf idx2) (setf out (cdr out)))
						 (t                           (_= (car out) val1) (incf idx1) (incf idx2) (setf out (cdr out))))))))))

  ;;IMP; set_union : 26 -  cvp x cvp x  vp
  (defun __set_union-imp-26 (idx1 last1 buf1 idx2 last2 buf2 out-idx out-buf less-bf)
	(declare (type fixnum idx1 last1 idx2 last2 out-idx))
	(declare (type cl:vector buf1 buf2 out-buf))
	(declare (type cl:function less-bf))
	(for (nil t nil)
	  (let ((is-end1 (= idx1 last1))
			(is-end2 (= idx2 last2)))
		(cond ((and is-end1 is-end2) (return-from __set_union-imp-26 out-idx))
			  (is-end1 (_= (aref out-buf out-idx) (aref buf2 idx2)) (incf idx2) (incf out-idx))
			  (is-end2 (_= (aref out-buf out-idx) (aref buf1 idx1)) (incf idx1) (incf out-idx))
			  (t (let ((val1 (aref buf1 idx1))
					   (val2 (aref buf2 idx2)))
				   (cond ((funcall less-bf val1 val2) (_= (aref out-buf out-idx) val1) (incf idx1)             (incf out-idx))
						 ((funcall less-bf val2 val1) (_= (aref out-buf out-idx) val2)             (incf idx2) (incf out-idx))
						 (t                           (_= (aref out-buf out-idx) val1) (incf idx1) (incf idx2) (incf out-idx))))))))))

;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  ;;IMP; set_difference : 00 -   i  x  i  x  o 
  (defun __set_difference-imp-00 (first1 last1 first2 last2 result comp)
	(declare (type cl:function comp))
	(with-operators
		(for (((itr1 @~first1)
			   (itr2 @~first2)
			   (dest @~result)) (_/= itr1 last1) nil :returns dest)
		  (if (_== itr2 last2)
			  (progn (_= *dest *itr1) ++itr1 ++dest)
			  (let ((val1 *itr1)
					(val2 *itr2))
				(if (funcall comp val1 val2)
					(progn (_= *dest val1) ++itr1 ++dest)
					(if (funcall comp val2 val1)
						++itr2
						(progn ++itr1 ++itr2))))))))

  ;;IMP; set_difference : 01 -   i  x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_difference-imp-01 (first1 last1 first2 last2 out less-bf)
	(declare (type cl:list out))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)
			   (itr2 @~first2)) (_/= itr1 last1) nil :returns out)
		  (if (_== itr2 last2)
			  (progn
				(_= (car out) *itr1)
				++itr1
				(setf out (cdr out)))
			  (let ((val1 *itr1)
					(val2 *itr2))
				(if (funcall less-bf val1 val2)
					(progn
					  (_= (car out) val1)
					  ++itr1
					  (setf out (cdr out)))
					(if (funcall less-bf val2 val1)
						++itr2
						(progn ++itr1 ++itr2))))))))

  ;;IMP; set_difference : 02 -   i  x  i  x  vp
  (defun __set_difference-imp-02 (first1 last1 first2 last2 out-idx out-buf less-bf)
	(declare (type fixnum out-idx))
	(declare (type cl:vector out-buf))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)
			   (itr2 @~first2)) (_/= itr1 last1) nil :returns out-idx)
		  (if (_== itr2 last2)
			  (progn
				(_= (aref out-buf out-idx) *itr1)
				++itr1
				(incf out-idx))
			  (let ((val1 *itr1)
					(val2 *itr2))
				(if (funcall less-bf val1 val2)
					(progn
					  (_= (aref out-buf out-idx) val1)
					  ++itr1
					  (incf out-idx))
					(if (funcall less-bf val2 val1)
						++itr2
						(progn ++itr1 ++itr2))))))))

  ;;IMP; set_difference : 03 -   i  x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_difference-imp-03 (first1 last1 cons2 last2 oitr less-bf)
	(declare (type cl:list cons2 last2))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)) (_/= itr1 last1) nil :returns oitr)
		  (if (eq cons2 last2)
			  (progn (_= *oitr *itr1) ++itr1 ++oitr)
			  (let ((val1 *itr1)
					(val2 (car cons2)))
				(if (funcall less-bf val1 val2)
					(progn (_= *oitr val1) ++itr1 ++oitr)
					(if (funcall less-bf val2 val1)
						(setf cons2 (cdr cons2))
						(progn ++itr1 (setf cons2 (cdr cons2))))))))))

  ;;IMP; set_difference : 04 -   i  x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_difference-imp-04 (first1 last1 cons2 last2 out less-bf)
	(declare (type cl:list cons2 last2 out))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)) (_/= itr1 last1) nil :returns out)
		  (if (eq cons2 last2)
			  (progn
				(_= (car out) *itr1)
				++itr1
				(setf out (cdr out)))
			  (let ((val1 *itr1)
					(val2 (car cons2)))
				(if (funcall less-bf val1 val2)
					(progn
					  (_= (car out) val1)
					  ++itr1
					  (setf out (cdr out)))
					(if (funcall less-bf val2 val1)
						(setf cons2 (cdr cons2))
						(progn ++itr1 (setf cons2 (cdr cons2))))))))))

  ;;IMP; set_difference : 05 -   i  x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_difference-imp-05 (first1 last1 cons2 last2 out-idx out-buf less-bf)
	(declare (type cl:list cons2 last2))
	(declare (type fixnum out-idx))
	(declare (type cl:vector out-buf))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)) (_/= itr1 last1) nil :returns out-idx)
		  (if (eq cons2 last2)
			  (progn
				(_= (aref out-buf out-idx) *itr1)
				++itr1
				(incf out-idx))
			  (let ((val1 *itr1)
					(val2 (car cons2)))
				(if (funcall less-bf val1 val2)
					(progn
					  (_= (aref out-buf out-idx) val1)
					  ++itr1
					  (incf out-idx))
					(if (funcall less-bf val2 val1)
						(setf cons2 (cdr cons2))
						(progn ++itr1 (setf cons2 (cdr cons2))))))))))

  ;;IMP; set_difference : 06 -   i  x cvp x  o 
  (defun __set_difference-imp-06 (first1 last1 idx2 last2 buffer2 oitr less-bf)
	(declare (type fixnum idx2 last2))
	(declare (type cl:vector buffer2))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)) (_/= itr1 last1) nil :returns oitr)
		  (if (= idx2 last2)
			  (progn (_= *oitr *itr1) ++itr1 ++oitr)
			  (let ((val1 *itr1)
					(val2 (aref buffer2 idx2)))
				(if (funcall less-bf val1 val2)
					(progn (_= *oitr val1) ++itr1 ++oitr)
					(if (funcall less-bf val2 val1)
						(incf idx2)
						(progn ++itr1 (incf idx2)))))))))

  ;;IMP; set_difference : 07 -   i  x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_difference-imp-07 (first1 last1 idx2 last2 buf2 out less-bf)
	(declare (type fixnum idx2 last2))
	(declare (type cl:vector buf2))
	(declare (type cl:list out))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)) (_/= itr1 last1) nil :returns out)
		  (if (= idx2 last2)
			  (progn
				(_= (car out) *itr1)
				++itr1
				(setf out (cdr out)))
			  (let ((val1 *itr1)
					(val2 (aref buf2 idx2)))
				(if (funcall less-bf val1 val2)
					(progn
					  (_= (car out) val1)
					  ++itr1
					  (setf out (cdr out)))
					(if (funcall less-bf val2 val1)
						(incf idx2)
						(progn ++itr1 (incf idx2)))))))))

  ;;IMP; set_difference : 08 -   i  x cvp x  vp
  (defun __set_difference-imp-08 (first1 last1 idx2 last2 src-buf2 out-idx out-buf less-bf)
	(declare (type fixnum idx2 last2 out-idx))
	(declare (type cl:vector src-buf2 out-buf))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)) (_/= itr1 last1) nil :returns out-idx)
		  (if (= idx2 last2)
			  (progn
				(_= (aref out-buf out-idx) *itr1)
				++itr1
				(incf out-idx))
			  (let ((val1 *itr1)
					(val2 (aref src-buf2 idx2)))
				(if (funcall less-bf val1 val2)
					(progn
					  (_= (aref out-buf out-idx) val1)
					  ++itr1
					  (incf out-idx))
					(if (funcall less-bf val2 val1)
						(incf idx2)
						(progn ++itr1 (incf idx2)))))))))

  ;;IMP; set_difference : 09 -  cci x  i  x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_difference-imp-09 (cons1 last1 first2 last2 oitr less-bf)
	(declare (type cl:list cons1 last1))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr2 @~first2)) (not (eq cons1 last1)) nil :returns oitr)
		  (if (_== itr2 last2)
			  (progn
				(_= *oitr (car cons1))
				(setf cons1 (cdr cons1))
				++oitr)
			  (let ((val1 (car cons1))
					(val2 *itr2))
				(if (funcall less-bf val1 val2)
					(progn
					  (_= *oitr val1)
					  (setf cons1 (cdr cons1))
					  ++oitr)
					(if (funcall less-bf val2 val1)
						++itr2
						(progn (setf cons1 (cdr cons1)) ++itr2))))))))

  ;;IMP; set_difference : 10 -  cci x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_difference-imp-10 (cons1 last1 first2 last2 out less-bf)
	(declare (type cl:list cons1 last1 out))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr2 @~first2)) (not (eq cons1 last1)) nil :returns out)
		  (if (_== itr2 last2)
			  (progn
				(_= (car out) (car cons1))
				(setf cons1 (cdr cons1))
				(setf out (cdr out)))
			  (let ((val1 (car cons1))
					(val2 *itr2))
				(if (funcall less-bf val1 val2)
					(progn
					  (_= (car out) val1)
					  (setf cons1 (cdr cons1))
					  (setf out (cdr out)))
					(if (funcall less-bf val2 val1)
						++itr2
						(progn (setf cons1 (cdr cons1)) ++itr2))))))))

  ;;IMP; set_difference : 11 -  cci x  i  x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_difference-imp-11 (cons1 last1 first2 last2 out-idx out-buf less-bf)
	(declare (type cl:list cons1 last1))
	(declare (type fixnum out-idx))
	(declare (type cl:vector out-buf))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr2 @~first2)) (not (eq cons1 last1)) nil :returns out-idx)
		  (if (_== itr2 last2)
			  (progn
				(_= (aref out-buf out-idx) (car cons1))
				(setf cons1 (cdr cons1))
				(incf out-idx))
			  (let ((val1 (car cons1))
					(val2 *itr2))
				(if (funcall less-bf val1 val2)
					(progn
					  (_= (aref out-buf out-idx) val1)
					  (setf cons1 (cdr cons1))
					  (incf out-idx))
					(if (funcall less-bf val2 val1)
						++itr2
						(progn (setf cons1 (cdr cons1)) ++itr2))))))))

  ;;IMP; set_difference : 12 -  cci x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_difference-imp-12 (cons1 last1 cons2 last2 oitr less-bf)
	(declare (type cl:list cons1 last1 cons2 last2))
	(declare (type cl:function less-bf))
	(for (nil (not (eq cons1 last1)) nil :returns oitr)
	  (with-operators
		  (if (eq cons2 last2)
			  (progn
				(_= *oitr (car cons1))
				(setf cons1 (cdr cons1))
				++oitr)
			  (let ((val1 (car cons1))
					(val2 (car cons2)))
				(if (funcall less-bf val1 val2)
					(progn
					  (_= *oitr val1)
					  (setf cons1 (cdr cons1))
					  ++oitr)
					(if (funcall less-bf val2 val1)
						(setf cons2 (cdr cons2))
						(progn
						  (setf cons1 (cdr cons1))
						  (setf cons2 (cdr cons2))))))))))

  ;;IMP; set_difference : 13 -  cci x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_difference-imp-13 (cons1 last1 cons2 last2 out less-bf)
	(declare (type cl:list cons1 last1 cons2 last2 out))
	(declare (type cl:function less-bf))
	(for (nil (not (eq cons1 last1)) nil :returns out)
	  (if (eq cons2 last2)
		  (progn
			(_= (car out) (car cons1))
			(setf cons1 (cdr cons1))
			(setf out (cdr out)))
		  (let ((val1 (car cons1))
				(val2 (car cons2)))
			(if (funcall less-bf val1 val2)
				(progn
				  (_= (car out) val1)
				  (setf cons1 (cdr cons1))
				  (setf out   (cdr  out)))
				(if (funcall less-bf val2 val1)
					(setf cons2 (cdr cons2))
					(progn
					  (setf cons1 (cdr cons1))
					  (setf cons2 (cdr cons2)))))))))

  ;;IMP; set_difference : 14 -  cci x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_difference-imp-14 (cons1 last1 cons2 last2 out-idx out-buf less-bf)
	(declare (type cl:list cons1 last1 cons2 last2))
	(declare (type fixnum out-idx))
	(declare (type cl:vector out-buf))
	(declare (type cl:function less-bf))
	(for (nil (not (eq cons1 last1)) nil :returns out-idx)
	  (if (eq cons2 last2)
		  (progn
			(_= (aref out-buf out-idx) (car cons1))
			(setf cons1 (cdr cons1))
			(incf out-idx))
		  (let ((val1 (car cons1))
				(val2 (car cons2)))
			(if (funcall less-bf val1 val2)
				(progn
				  (_= (aref out-buf out-idx) val1)
				  (setf cons1 (cdr cons1))
				  (incf out-idx))
				(if (funcall less-bf val2 val1)
					(setf cons2 (cdr cons2))
					(progn
					  (setf cons1 (cdr cons1))
					  (setf cons2 (cdr cons2)))))))))

  ;;IMP; set_difference : 15 -  cci x cvp x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_difference-imp-15 (cons1 last1 idx2 last2 buf2 oitr less-bf)
	(declare (type cl:list cons1 last1))
	(declare (type fixnum idx2 last2))
	(declare (type cl:vector buf2))
	(declare (type cl:function less-bf))
	(for (nil (not (eq cons1 last1)) nil :returns oitr)
	  (with-operators
		  (if (= idx2 last2)
			  (progn
				(_= *oitr (car cons1))
				(setf cons1 (cdr cons1))
				++oitr)
			  (let ((val1 (car cons1))
					(val2 (aref buf2 idx2)))
				(if (funcall less-bf val1 val2)
					(progn
					  (_= *oitr val1)
					  (setf cons1 (cdr cons1))
					  ++oitr)
					(if (funcall less-bf val2 val1)
						(incf idx2)
						(progn
						  (setf cons1 (cdr cons1))
						  (incf idx2)))))))))

  ;;IMP; set_difference : 16 -  cci x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_difference-imp-16 (cons1 last1 idx2 last2 buf2 out less-bf)
	(declare (type cl:list cons1 last1 out))
	(declare (type fixnum idx2 last2))
	(declare (type cl:vector buf2))
	(declare (type cl:function less-bf))
	(for (nil (not (eq cons1 last1)) nil :returns out)
	  (if (= idx2 last2)
		  (progn
			(_= (car out) (car cons1))
			(setf cons1 (cdr cons1))
			(setf out   (cdr   out)))
		  (let ((val1 (car cons1))
				(val2 (aref buf2 idx2)))
			(if (funcall less-bf val1 val2)
				(progn
				  (_= (car out) val1)
				  (setf cons1 (cdr cons1))
				  (setf out (cdr out)))
				(if (funcall less-bf val2 val1)
					(incf idx2)
					(progn
					  (setf cons1 (cdr cons1))
					  (incf idx2))))))))

  ;;IMP; set_difference : 17 -  cci x cvp x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_difference-imp-17 (cons1 last1 idx2 last2 buf2 out-idx out-buf less-bf)
	(declare (type cl:list cons1 last1))
	(declare (type fixnum idx2 last2 out-idx))
	(declare (type cl:vector buf2 out-buf))
	(declare (type cl:function less-bf))
	(for (nil (not (eq cons1 last1)) nil :returns out-idx)
	  (if (= idx2 last2)
		  (progn
			(_= (aref out-buf out-idx) (car cons1))
			(setf cons1 (cdr cons1))
			(incf out-idx))
		  (let ((val1 (car cons1))
				(val2 (aref buf2 idx2)))
			(if (funcall less-bf val1 val2)
				(progn
				  (_= (aref out-buf out-idx) val1)
				  (setf cons1 (cdr cons1))
				  (incf out-idx))
				(if (funcall less-bf val2 val1)
					(incf idx2)
					(progn
					  (setf cons1 (cdr cons1))
					  (incf idx2))))))))

  ;;IMP; set_difference : 18 -  cvp x  i  x  o 
  (defun __set_difference-imp-18 (idx1 last1 buffer1 first2 last2 oitr less-bf)
	(declare (type fixnum idx1 last1))
	(declare (type cl:vector buffer1))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr2 @~first2)) (< idx1 last1) nil :returns oitr)
		  (if (_== itr2 last2)
			  (progn
				(_= *oitr (aref buffer1 idx1))
				(incf idx1)
				++oitr)
			  (let ((val1 (aref buffer1 idx1))
					(val2 *itr2))
				(if (funcall less-bf val1 val2)
					(progn
					  (_= *oitr val1)
					  (incf idx1)
					  ++oitr)
					(if (funcall less-bf val2 val1)
						++itr2
						(progn (incf idx1) ++itr2))))))))

  ;;IMP; set_difference : 19 -  cvp x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_difference-imp-19 (idx1 last1 buf1 first2 last2 out less-bf)
	(declare (type fixnum idx1 last1))
	(declare (type cl:vector buf1))
	(declare (type cl:list out))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr2 @~first2)) (< idx1 last1) nil :returns out)
		  (if (_== itr2 last2)
			  (progn
				(_= (car out) (aref buf1 idx1))
				(incf idx1)
				(setf out (cdr out)))
			  (let ((val1 (aref buf1 idx1))
					(val2 *itr2))
				(if (funcall less-bf val1 val2)
					(progn
					  (_= (car out) val1)
					  (incf idx1)
					  (setf out (cdr out)))
					(if (funcall less-bf val2 val1)
						++itr2
						(progn (incf idx1) ++itr2))))))))

  ;;IMP; set_difference : 20 -  cvp x  i  x  vp
  (defun __set_difference-imp-20 (idx1 last1 src-buf1 first2 last2 out-idx out-buf less-bf)
	(declare (type fixnum idx1 last1 out-idx))
	(declare (type cl:vector src-buf1 out-buf))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr2 @~first2)) (< idx1 last1) nil :returns out-idx)
		  (if (_== itr2 last2)
			  (progn
				(_= (aref out-buf out-idx) (aref src-buf1 idx1))
				(incf idx1)
				(incf out-idx))
			  (let ((val1 (aref src-buf1 idx1))
					(val2 *itr2))
				(if (funcall less-bf val1 val2)
					(progn
					  (_= (aref out-buf out-idx) val1)
					  (incf idx1)
					  (incf out-idx))
					(if (funcall less-bf val2 val1)
						++itr2
						(progn (incf idx1) ++itr2))))))))

  ;;IMP; set_difference : 21 -  cvp x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_difference-imp-21 (idx1 last1 buf1 cons2 last2 oitr less-bf)
	(declare (type fixnum idx1 last1))
	(declare (type cl:vector buf1))
	(declare (type cl:list cons2 last2))
	(declare (type cl:function less-bf))
	(for (nil (< idx1 last1) nil :returns oitr)
	  (with-operators
		  (if (eq cons2 last2)
			  (progn
				(_= *oitr (aref buf1 idx1))
				(incf idx1)
				++oitr)
			  (let ((val1 (aref buf1 idx1))
					(val2 (car cons2)))
				(if (funcall less-bf val1 val2)
					(progn
					  (_= *oitr val1)
					  (incf idx1)
					  ++oitr)
					(if (funcall less-bf val2 val1)
						(setf cons2 (cdr cons2))
						(progn
						  (incf idx1)
						  (setf cons2 (cdr cons2))))))))))

  ;;IMP; set_difference : 22 -  cvp x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_difference-imp-22 (idx1 last1 buf1 cons2 last2 out less-bf)
	(declare (type fixnum idx1 last1))
	(declare (type cl:vector buf1))
	(declare (type cl:list cons2 last2 out))
	(declare (type cl:function less-bf))
	(for (nil (< idx1 last1) nil :returns out)
	  (if (eq cons2 last2)
		  (progn
			(_= (car out) (aref buf1 idx1))
			(incf idx1)
			(setf out (cdr out)))
		  (let ((val1 (aref buf1 idx1))
				(val2 (car cons2)))
			(if (funcall less-bf val1 val2)
				(progn
				  (_= (car out) val1)
				  (incf idx1)
				  (setf out (cdr out)))
				(if (funcall less-bf val2 val1)
					(setf cons2 (cdr cons2))
					(progn
					  (incf idx1)
					  (setf cons2 (cdr cons2)))))))))

  ;;IMP; set_difference : 23 -  cvp x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_difference-imp-23 (idx1 last1 buf1 cons2 last2 out-idx out-buf less-bf)
	(declare (type fixnum idx1 last1 out-idx))
	(declare (type cl:list cons2 last2))
	(declare (type cl:vector buf1 out-buf))
	(declare (type cl:function less-bf))
	(for (nil (< idx1 last1) nil :returns out-idx)
	  (if (eq cons2 last2)
		  (progn
			(_= (aref out-buf out-idx) (aref buf1 idx1))
			(incf idx1)
			(incf out-idx))
		  (let ((val1 (aref buf1 idx1))
				(val2 (car cons2)))
			(if (funcall less-bf val1 val2)
				(progn
				  (_= (aref out-buf out-idx) val1)
				  (incf idx1)
				  (incf out-idx))
				(if (funcall less-bf val2 val1)
					(setf cons2 (cdr cons2))
					(progn
					  (incf idx1)
					  (setf cons2 (cdr cons2)))))))))

  ;;IMP; set_difference : 24 -  cvp x cvp x  o 
  (defun __set_difference-imp-24 (idx1 last1 buffer1 idx2 last2 buffer2 oitr less-bf)
	(declare (type fixnum idx1 last1 idx2 last2))
	(declare (type cl:vector buffer1 buffer2))
	(declare (type cl:function less-bf))
	(for (nil (< idx1 last1) nil :returns oitr)
	  (with-operators
		  (if (= idx2 last2)
			  (progn
				(_= *oitr (aref buffer1 idx1))
				(incf idx1)
				++oitr)
			  (let ((val1 (aref buffer1 idx1))
					(val2 (aref buffer2 idx2)))
				(if (funcall less-bf val1 val2)
					(progn
					  (_= *oitr val1)
					  (incf idx1)
					  ++oitr)
					(if (funcall less-bf val2 val1)
						(incf idx2)
						(progn
						  (incf idx1)
						  (incf idx2)))))))))

  ;;IMP; set_difference : 25 -  cvp x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_difference-imp-25 (idx1 last1 src-buf1 idx2 last2 src-buf2 out less-bf)
	(declare (type fixnum idx1 last1 idx2 last2))
	(declare (type cl:vector src-buf1 src-buf2))
	(declare (type cl:list out))
	(declare (type cl:function less-bf))
	(for (nil (< idx1 last1) nil :returns out)
	  (if (= idx2 last2)
		  (progn
			(_= (car out) (aref src-buf1 idx1))
			(incf idx1)
			(setf out (cdr out)))
		  (let ((val1 (aref src-buf1 idx1))
				(val2 (aref src-buf2 idx2)))
			(if (funcall less-bf val1 val2)
				(progn
				  (_= (car out) val1)
				  (incf idx1)
				  (setf out (cdr out)))
				(if (funcall less-bf val2 val1)
					(incf idx2)
					(progn
					  (incf idx1)
					  (incf idx2))))))))

  ;;IMP; set_difference : 26 -  cvp x cvp x  vp
  (defun __set_difference-imp-26 (idx1 last1 src-buf1 idx2 last2 src-buf2 out-idx out-buf less-bf)
	(declare (type fixnum idx1 last1 idx2 last2 out-idx))
	(declare (type cl:vector src-buf1 src-buf2 out-buf))
	(declare (type cl:function less-bf))
	(for (nil (< idx1 last1) nil :returns out-idx)
	  (if (= idx2 last2)
		  (progn
			(_= (aref out-buf out-idx) (aref src-buf1 idx1))
			(incf idx1)
			(incf out-idx))
		  (let ((val1 (aref src-buf1 idx1))
				(val2 (aref src-buf2 idx2)))
			(if (funcall less-bf val1 val2)
				(progn
				  (_= (aref out-buf out-idx) val1)
				  (incf idx1)
				  (incf out-idx))
				(if (funcall less-bf val2 val1)
					(incf idx2)
					(progn
					  (incf idx1)
					  (incf idx2)))))))))


;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  ;;IMP; set_intersection : 00 -   i  x  i  x  o 
  (defun __set_intersection-imp-00 (first1 last1 first2 last2 result comp)
	(declare (type cl:function comp))
	(with-operators
		(for (((itr1 @~first1)
			   (itr2 @~first2)
			   (dest @~result)) (and (_/= itr1 last1)
									 (_/= itr2 last2)) nil :returns dest)
		  (let ((val1 *itr1)
				(val2 *itr2))
			(if (funcall comp val1 val2)
				++itr1
				(if (funcall comp val2 val1)
					++itr2
					(progn (_= *dest val1) ++itr1 ++itr2 ++dest)))))))

  ;;IMP; set_intersection : 01 -   i  x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_intersection-imp-01 (first1 last1 first2 last2 out less-bf)
	(declare (type cl:list out))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)
			   (itr2 @~first2)) (and (_/= itr1 last1)
									 (_/= itr2 last2)) nil :returns out)
		  (let ((val1 *itr1)
				(val2 *itr2))
			(if (funcall less-bf val1 val2)
				++itr1
				(if (funcall less-bf val2 val1)
					++itr2
					(progn
					  (_= (car out) val1)
					  (setf out (cdr out))
					  ++itr1
					  ++itr2)))))))

  ;;IMP; set_intersection : 02 -   i  x  i  x  vp
  (defun __set_intersection-imp-02 (first1 last1 first2 last2 out-idx out-buf less-bf)
	(declare (type fixnum out-idx))
	(declare (type cl:vector out-buf))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)
			   (itr2 @~first2)) (and (_/= itr1 last1)
									 (_/= itr2 last2)) nil :returns out-idx)
		  (let ((val1 *itr1)
				(val2 *itr2))
			(if (funcall less-bf val1 val2)
				++itr1
				(if (funcall less-bf val2 val1)
					++itr2
					(progn
					  (_= (aref out-buf out-idx) val1)
					  (incf out-idx)
					  ++itr1
					  ++itr2)))))))

  ;;IMP; set_intersection : 03 -   i  x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_intersection-imp-03 (first1 last1 cons2 last2 oitr less-bf)
	(declare (type cl:list cons2 last2))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)) (and (_/= itr1 last1) (not (eq cons2 last2))) nil :returns oitr)
		  (let ((val1 *itr1)
				(val2 (car cons2)))
			(if (funcall less-bf val1 val2)
				++itr1
				(if (funcall less-bf val2 val1)
					(setf cons2 (cdr cons2))
					(progn
					  (_= *oitr val1)
					  ++oitr
					  ++itr1
					  (setf cons2 (cdr cons2)))))))))

  ;;IMP; set_intersection : 04 -   i  x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_intersection-imp-04 (first1 last1 cons2 last2 out less-bf)
	(declare (type cl:list cons2 last2 out))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)) (and (_/= itr1 last1) (not (eq cons2 last2))) nil :returns out)
		  (let ((val1 *itr1)
				(val2 (car cons2)))
			(if (funcall less-bf val1 val2)
				++itr1
				(if (funcall less-bf val2 val1)
					(setf cons2 (cdr cons2))
					(progn
					  (_= (car out) val1)
					  (setf out (cdr out))
					  ++itr1
					  (setf cons2 (cdr cons2)))))))))

  ;;IMP; set_intersection : 05 -   i  x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_intersection-imp-05 (first1 last1 cons2 last2 out-idx out-buf less-bf)
	(declare (type cl:list cons2 last2))
	(declare (type fixnum out-idx))
	(declare (type cl:vector out-buf))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)) (and (_/= itr1 last1) (not (eq cons2 last2))) nil :returns out-idx)
		  (let ((val1 *itr1)
				(val2 (car cons2)))
			(if (funcall less-bf val1 val2)
				++itr1
				(if (funcall less-bf val2 val1)
					(setf cons2 (cdr cons2))
					(progn
					  (_= (aref out-buf out-idx) val1)
					  (incf out-idx)
					  ++itr1
					  (setf cons2 (cdr cons2)))))))))

  ;;IMP; set_intersection : 06 -   i  x cvp x  o 
  (defun __set_intersection-imp-06 (first1 last1 idx2 last2 buffer2 oitr less-bf)
	(declare (type fixnum idx2 last2))
	(declare (type cl:vector buffer2))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)) (and (_/= itr1 last1) (< idx2 last2)) nil :returns oitr)
		  (let ((val1 *itr1)
				(val2 (aref buffer2 idx2)))
			(if (funcall less-bf val1 val2)
				++itr1
				(if (funcall less-bf val2 val1)
					(incf idx2)
					(progn
					  (_= *oitr val1)
					  ++oitr
					  ++itr1
					  (incf idx2))))))))

  ;;IMP; set_intersection : 07 -   i  x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_intersection-imp-07 (first1 last1 idx2 last2 buf2 out less-bf)
	(declare (type fixnum idx2 last2))
	(declare (type cl:vector buf2))
	(declare (type cl:list out))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)) (and (_/= itr1 last1) (< idx2 last2)) nil :returns out)
		  (let ((val1 *itr1)
				(val2 (aref buf2 idx2)))
			(if (funcall less-bf val1 val2)
				++itr1
				(if (funcall less-bf val2 val1)
					(incf idx2)
					(progn
					  (_= (car out) val1)
					  (setf out (cdr out))
					  ++itr1
					  (incf idx2))))))))

  ;;IMP; set_intersection : 08 -   i  x cvp x  vp
  (defun __set_intersection-imp-08 (first1 last1 idx2 last2 src-buf2 out-idx out-buf less-bf)
	(declare (type fixnum idx2 last2 out-idx))
	(declare (type cl:vector src-buf2 out-buf))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)) (and (_/= itr1 last1) (< idx2 last2)) nil :returns out-idx)
		  (let ((val1 *itr1)
				(val2 (aref src-buf2 idx2)))
			(if (funcall less-bf val1 val2)
				++itr1
				(if (funcall less-bf val2 val1)
					(incf idx2)
					(progn
					  (_= (aref out-buf out-idx) val1)
					  (incf out-idx)
					  ++itr1
					  (incf idx2))))))))

  ;;IMP; set_intersection : 09 -  cci x  i  x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_intersection-imp-09 (cons1 last1 first2 last2 oitr less-bf)
	(declare (type cl:list cons1 last1))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr2 @~first2)) (and (not (eq cons1 last1)) (_/= itr2 last2)) nil :returns oitr)
		  (let ((val1 (car cons1))
				(val2 *itr2))
			(if (funcall less-bf val1 val2)
				(setf cons1 (cdr cons1))
				(if (funcall less-bf val2 val1)
					++itr2
					(with-operators
						(_= *oitr val1)
					  ++oitr
					  (setf cons1 (cdr cons1))
					  ++itr2)))))))

  ;;IMP; set_intersection : 10 -  cci x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_intersection-imp-10 (cons1 last1 first2 last2 out less-bf)
	(declare (type cl:list cons1 last1 out))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr2 @~first2)) (and (not (eq cons1 last1)) (_/= itr2 last2)) nil :returns out)
		  (let ((val1 (car cons1))
				(val2 *itr2))
			(if (funcall less-bf val1 val2)
				(setf cons1 (cdr cons1))
				(if (funcall less-bf val2 val1)
					++itr2
					(progn
					  (_= (car out) val1)
					  (setf out (cdr out))
					  (setf cons1 (cdr cons1))
					  ++itr2)))))))

  ;;IMP; set_intersection : 11 -  cci x  i  x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_intersection-imp-11 (cons1 last1 first2 last2 out-idx out-buf less-bf)
	(declare (type cl:list cons1 last1))
	(declare (type fixnum out-idx))
	(declare (type cl:vector out-buf))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr2 @~first2)) (and (not (eq cons1 last1)) (_/= itr2 last2)) nil :returns out-idx)
		  (let ((val1 (car cons1))
				(val2 *itr2))
			(if (funcall less-bf val1 val2)
				(setf cons1 (cdr cons1))
				(if (funcall less-bf val2 val1)
					++itr2
					(progn
					  (_= (aref out-buf out-idx) val1)
					  (incf out-idx)
					  (setf cons1 (cdr cons1))
					  ++itr2)))))))

  ;;IMP; set_intersection : 12 -  cci x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_intersection-imp-12 (cons1 last1 cons2 last2 oitr less-bf)
	(declare (type cl:list cons1 last1 cons2 last2))
	(declare (type cl:function less-bf))
	(for (nil (and (not (eq cons1 last1)) (not (eq cons2 last2))) nil :returns oitr)
	  (let ((val1 (car cons1))
			(val2 (car cons2)))
		(if (funcall less-bf val1 val2)
			(setf cons1 (cdr cons1))
			(if (funcall less-bf val2 val1)
				(setf cons2 (cdr cons2))
				(with-operators
					(_= *oitr val1)
				  ++oitr
				  (setf cons1 (cdr cons1))
				  (setf cons2 (cdr cons2))))))))

  ;;IMP; set_intersection : 13 -  cci x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_intersection-imp-13 (cons1 last1 cons2 last2 out less-bf)
	(declare (type cl:list cons1 last1 cons2 last2 out))
	(declare (type cl:function less-bf))
	(for (nil (and (not (eq cons1 last1)) (not (eq cons2 last2))) nil :returns out)
	  (let ((val1 (car cons1))
			(val2 (car cons2)))
		(if (funcall less-bf val1 val2)
			(setf cons1 (cdr cons1))
			(if (funcall less-bf val2 val1)
				(setf cons2 (cdr cons2))
				(progn
				  (_= (car out) val1)
				  (setf out (cdr out))
				  (setf cons1 (cdr cons1))
				  (setf cons2 (cdr cons2))))))))

  ;;IMP; set_intersection : 14 -  cci x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_intersection-imp-14 (cons1 last1 cons2 last2 out-idx out-buf less-bf)
	(declare (type cl:list cons1 last1 cons2 last2))
	(declare (type fixnum out-idx))
	(declare (type cl:vector out-buf))
	(declare (type cl:function less-bf))
	(for (nil (and (not (eq cons1 last1)) (not (eq cons2 last2))) nil :returns out-idx)
	  (let ((val1 (car cons1))
			(val2 (car cons2)))
		(if (funcall less-bf val1 val2)
			(setf cons1 (cdr cons1))
			(if (funcall less-bf val2 val1)
				(setf cons2 (cdr cons2))
				(progn
				  (_= (aref out-buf out-idx) val1)
				  (incf out-idx)
				  (setf cons1 (cdr cons1))
				  (setf cons2 (cdr cons2))))))))

  ;;IMP; set_intersection : 15 -  cci x cvp x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_intersection-imp-15 (cons1 last1 idx2 last2 buffer2 oitr less-bf)
	(declare (type cl:list cons1 last1))
	(declare (type fixnum idx2 last2))
	(declare (type cl:vector buffer2))
	(declare (type cl:function less-bf))
	(for (nil (and (not (eq cons1 last1)) (< idx2 last2)) nil :returns oitr)
	  (let ((val1 (car cons1))
			(val2 (aref buffer2 idx2)))
		(if (funcall less-bf val1 val2)
			(setf cons1 (cdr cons1))
			(if (funcall less-bf val2 val1)
				(incf idx2)
				(with-operators
					(_= *oitr val1)
				  ++oitr
				  (setf cons1 (cdr cons1))
				  (incf idx2)))))))

  ;;IMP; set_intersection : 16 -  cci x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_intersection-imp-16 (cons1 last1 idx2 last2 buf2 out less-bf)
	(declare (type cl:list cons1 last1 out))
	(declare (type fixnum idx2 last2))
	(declare (type cl:vector buf2))
	(declare (type cl:function less-bf))
	(for (nil (and (not (eq cons1 last1)) (< idx2 last2)) nil :returns out)
	  (let ((val1 (car  cons1))
			(val2 (aref buf2 idx2)))
		(if (funcall less-bf val1 val2)
			(setf cons1 (cdr cons1))
			(if (funcall less-bf val2 val1)
				(incf idx2)
				(progn
				  (_= (car out) val1)
				  (setf out (cdr out))
				  (setf cons1 (cdr cons1))
				  (incf idx2)))))))

  ;;IMP; set_intersection : 17 -  cci x cvp x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_intersection-imp-17 (cons1 last1 idx2 last2 src-buf2 out-idx out-buf less-bf)
	(declare (type cl:list cons1 last1))
	(declare (type fixnum idx2 last2 out-idx))
	(declare (type cl:vector src-buf2 out-buf))
	(declare (type cl:function less-bf))
	(for (nil (and (not (eq cons1 last1)) (< idx2 last2)) nil :returns out-idx)
	  (let ((val1 (car cons1))
			(val2 (aref src-buf2 idx2)))
		(if (funcall less-bf val1 val2)
			(setf cons1 (cdr cons1))
			(if (funcall less-bf val2 val1)
				(incf idx2)
				(progn
				  (_= (aref out-buf out-idx) val1)
				  (incf out-idx)
				  (setf cons1 (cdr cons1))
				  (incf idx2)))))))

  ;;IMP; set_intersection : 18 -  cvp x  i  x  o 
  (defun __set_intersection-imp-18 (idx1 last1 buffer1 first2 last2 oitr less-bf)
	(declare (type fixnum idx1 last1))
	(declare (type cl:vector buffer1))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr2 @~first2)) (and (< idx1 last1) (_/= itr2 last2)) nil :returns oitr)
		  (let ((val1 (aref buffer1 idx1))
				(val2 *itr2))
			(if (funcall less-bf val1 val2)
				(incf idx1)
				(if (funcall less-bf val2 val1)
					++itr2
					(with-operators
						(_= *oitr val1)
					  ++oitr
					  (incf idx1)
					  ++itr2)))))))

  ;;IMP; set_intersection : 19 -  cvp x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_intersection-imp-19 (idx1 last1 buf1 first2 last2 out less-bf)
	(declare (type fixnum idx1 last1))
	(declare (type cl:vector buf1))
	(declare (type cl:list out))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr2 @~first2)) (and (< idx1 last1) (_/= itr2 last2)) nil :returns out)
		  (let ((val1 (aref buf1 idx1))
				(val2 *itr2))
			(if (funcall less-bf val1 val2)
				(incf idx1)
				(if (funcall less-bf val2 val1)
					++itr2
					(progn
					  (_= (car out) val1)
					  (setf out (cdr out))
					  (incf idx1)
					  ++itr2)))))))

  ;;IMP; set_intersection : 20 -  cvp x  i  x  vp
  (defun __set_intersection-imp-20 (idx1 last1 src-buf1 first2 last2 out-idx out-buf less-bf)
	(declare (type fixnum idx1 last1 out-idx))
	(declare (type cl:vector src-buf1 out-buf))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr2 @~first2)) (and (< idx1 last1) (_/= itr2 last2)) nil :returns out-idx)
		  (let ((val1 (aref src-buf1 idx1))
				(val2 *itr2))
			(if (funcall less-bf val1 val2)
				(incf idx1)
				(if (funcall less-bf val2 val1)
					++itr2
					(progn
					  (_= (aref out-buf out-idx) val1)
					  (incf out-idx)
					  (incf idx1)
					  ++itr2)))))))

  ;;IMP; set_intersection : 21 -  cvp x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_intersection-imp-21 (idx1 last1 buf1 cons2 last2 oitr less-bf)
	(declare (type fixnum idx1 last1))
	(declare (type cl:vector buf1))
	(declare (type cl:list cons2 last2))
	(declare (type cl:function less-bf))
	(for (nil (and (< idx1 last1) (not (eq cons2 last2))) nil :returns oitr)
	  (let ((val1 (aref buf1 idx1))
			(val2 (car cons2)))
		(if (funcall less-bf val1 val2)
			(incf idx1)
			(if (funcall less-bf val2 val1)
				(setf cons2 (cdr cons2))
				(with-operators
					(_= *oitr val1)
				  ++oitr
				  (incf idx1)
				  (setf cons2 (cdr cons2))))))))

  ;;IMP; set_intersection : 22 -  cvp x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_intersection-imp-22 (idx1 last1 buf1 cons2 last2 out less-bf)
	(declare (type fixnum idx1 last1))
	(declare (type cl:vector buf1))
	(declare (type cl:list cons2 last2 out))
	(declare (type cl:function less-bf))
	(for (nil (and (< idx1 last1) (not (eq cons2 last2))) nil :returns out)
	  (let ((val1 (aref buf1 idx1))
			(val2 (car cons2)))
		(if (funcall less-bf val1 val2)
			(incf idx1)
			(if (funcall less-bf val2 val1)
				(setf cons2 (cdr cons2))
				(progn
				  (_= (car out) val1)
				  (setf out (cdr out))
				  (incf idx1)
				  (setf cons2 (cdr cons2))))))))

  ;;IMP; set_intersection : 23 -  cvp x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_intersection-imp-23 (idx1 last1 buf1 cons2 last2 out-idx out-buf less-bf)
	(declare (type fixnum idx1 last1 out-idx))
	(declare (type cl:vector buf1 out-buf))
	(declare (type cl:list cons2 last2))
	(declare (type cl:function less-bf))
	(for (nil (and (< idx1 last1) (not (eq cons2 last2))) nil :returns out-idx)
	  (let ((val1 (aref buf1 idx1))
			(val2 (car cons2)))
		(if (funcall less-bf val1 val2)
			(incf idx1)
			(if (funcall less-bf val2 val1)
				(setf cons2 (cdr cons2))
				(progn
				  (_= (aref out-buf out-idx) val1)
				  (incf out-idx)
				  (incf idx1)
				  (setf cons2 (cdr cons2))))))))

  ;;IMP; set_intersection : 24 -  cvp x cvp x  o 
  (defun __set_intersection-imp-24 (idx1 last1 buffer1 idx2 last2 buffer2 oitr less-bf)
	(declare (type fixnum idx1 last1 idx2 last2))
	(declare (type cl:vector buffer1 buffer2))
	(declare (type cl:function less-bf))
	(for (nil (and (< idx1 last1) (< idx2 last2)) nil :returns oitr)
	  (let ((val1 (aref buffer1 idx1))
			(val2 (aref buffer2 idx2)))
		(if (funcall less-bf val1 val2)
			(incf idx1)
			(if (funcall less-bf val2 val1)
				(incf idx2)
				(with-operators
					(_= *oitr val1)
				  ++oitr
				  (incf idx1)
				  (incf idx2)))))))

  ;;IMP; set_intersection : 25 -  cvp x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_intersection-imp-25 (idx1 last1 buf1 idx2 last2 buf2 out less-bf)
	(declare (type fixnum idx1 last1 idx2 last2))
	(declare (type cl:vector buf1 buf2))
	(declare (type cl:list out))
	(declare (type cl:function less-bf))
	(for (nil (and (< idx1 last1) (< idx2 last2)) nil :returns out)
	  (let ((val1 (aref buf1 idx1))
			(val2 (aref buf2 idx2)))
		(if (funcall less-bf val1 val2)
			(incf idx1)
			(if (funcall less-bf val2 val1)
				(incf idx2)
				(progn
				  (_= (car out) val1)
				  (setf out (cdr out))
				  (incf idx1)
				  (incf idx2)))))))

  ;;IMP; set_intersection : 26 -  cvp x cvp x  vp
  (defun __set_intersection-imp-26 (idx1 last1 src-buf1 idx2 last2 src-buf2 out-idx out-buf less-bf)
	(declare (type fixnum idx1 last1 idx2 last2 out-idx))
	(declare (type cl:vector src-buf1 src-buf2 out-buf))
	(declare (type cl:function less-bf))
	(for (nil (and (< idx1 last1) (< idx2 last2)) nil :returns out-idx)
	  (let ((val1 (aref src-buf1 idx1))
			(val2 (aref src-buf2 idx2)))
		(if (funcall less-bf val1 val2)
			(incf idx1)
			(if (funcall less-bf val2 val1)
				(incf idx2)
				(progn
				  (_= (aref out-buf out-idx) val1)
				  (incf out-idx)
				  (incf idx1)
				  (incf idx2))))))))
  

;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  ;;IMP; set_symmetric_difference : 00 -   i  x  i  x  o 
  (defun __set_symmetric_difference-imp-00 (first1 last1 first2 last2 result comp)
	(declare (type cl:function comp))
	(with-operators
		(for (((itr1 @~first1) (itr2 @~first2) (dest @~result)) t nil)
		  (let ((is-last1 (_== itr1 last1))
				(is-last2 (_== itr2 last2)))
			(cond
			  ((and is-last1 is-last2) (return-from __set_symmetric_difference-imp-00 dest))
			  (is-last1 (_= *dest *itr2) ++itr2 ++dest)
			  (is-last2 (_= *dest *itr1) ++itr1 ++dest)
			  (t (let ((v1 *itr1)
					   (v2 *itr2))
				   (cond
					 ((funcall comp v1 v2) (_= *dest    v1) ++itr1 ++dest)
					 ((funcall comp v2 v1) (_= *dest *itr2) ++itr2 ++dest)
					 (t ++itr1 ++itr2)))))))))

  ;;IMP; set_symmetric_difference : 01 -   i  x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_symmetric_difference-imp-01 (first1 last1 first2 last2 out less-bf)
	(declare (type cl:list out))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)
			   (itr2 @~first2)) t nil)
		  (let ((is-end1 (_== itr1 last1))
				(is-end2 (_== itr2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_symmetric_difference-imp-01 out))
				  (is-end1 (_= (car out) *itr2) ++itr2 (setf out (cdr out)))
				  (is-end2 (_= (car out) *itr1) ++itr1 (setf out (cdr out)))
				  (t (let ((val1 *itr1)
						   (val2 *itr2))
					   (cond ((funcall less-bf val1 val2) (_= (car out) val1) ++itr1 (setf out (cdr out)))
							 ((funcall less-bf val2 val1) (_= (car out) val2) ++itr2 (setf out (cdr out)))
							 (t ++itr1 ++itr2)))))))))

  ;;IMP; set_symmetric_difference : 02 -   i  x  i  x  vp
  (defun __set_symmetric_difference-imp-02 (first1 last1 first2 last2 out-idx out-buf less-bf)
	(declare (type fixnum out-idx))
	(declare (type cl:vector out-buf))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)
			   (itr2 @~first2)) t nil)
		  (let ((is-end1 (_== itr1 last1))
				(is-end2 (_== itr2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_symmetric_difference-imp-02 out-idx))
				  (is-end1 (_= (aref out-buf out-idx) *itr2) ++itr2 (incf out-idx))
				  (is-end2 (_= (aref out-buf out-idx) *itr1) ++itr1 (incf out-idx))
				  (t (let ((val1 *itr1)
						   (val2 *itr2))
					   (cond ((funcall less-bf val1 val2) (_= (aref out-buf out-idx) val1) ++itr1 (incf out-idx))
							 ((funcall less-bf val2 val1) (_= (aref out-buf out-idx) val2) ++itr2 (incf out-idx))
							 (t ++itr1 ++itr2)))))))))

  ;;IMP; set_symmetric_difference : 03 -   i  x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_symmetric_difference-imp-03 (first1 last1 cons2 last2 oitr less-bf)
	(declare (type cl:list cons2 last2))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)) t nil)
		  (let ((is-end1 (_== itr1 last1))
				(is-end2 (eq cons2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_symmetric_difference-imp-03 oitr))
				  (is-end1 (_= *oitr (car cons2)) (setf cons2 (cdr cons2)) ++oitr)
				  (is-end2 (_= *oitr *itr1)       ++itr1                   ++oitr)
				  (t (let ((val1 *itr1)
						   (val2 (car cons2)))
					   (cond ((funcall less-bf val1 val2) (_= *oitr val1) ++itr1                   ++oitr)
							 ((funcall less-bf val2 val1) (_= *oitr val2) (setf cons2 (cdr cons2)) ++oitr)
							 (t ++itr1 (setf cons2 (cdr cons2)))))))))))

  ;;IMP; set_symmetric_difference : 04 -   i  x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_symmetric_difference-imp-04 (first1 last1 cons2 last2 out less-bf)
	(declare (type cl:list cons2 last2 out))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)) t nil)
		  (let ((is-end1 (_== itr1 last1))
				(is-end2 (eq cons2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_symmetric_difference-imp-04 out))
				  (is-end1 (_= (car out) (car cons2)) (setf cons2 (cdr cons2)) (setf out (cdr out)))
				  (is-end2 (_= (car out) *itr1)       ++itr1                   (setf out (cdr out)))
				  (t (let ((val1 *itr1)
						   (val2 (car cons2)))
					   (cond ((funcall less-bf val1 val2) (_= (car out) val1) ++itr1                   (setf out (cdr out)))
							 ((funcall less-bf val2 val1) (_= (car out) val2) (setf cons2 (cdr cons2)) (setf out (cdr out)))
							 (t ++itr1 (setf cons2 (cdr cons2)))))))))))

  ;;IMP; set_symmetric_difference : 05 -   i  x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_symmetric_difference-imp-05 (first1 last1 cons2 last2 out-idx out-buf less-bf)
	(declare (type cl:list cons2 last2))
	(declare (type fixnum out-idx))
	(declare (type cl:vector out-buf))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)) t nil)
		  (let ((is-end1 (_== itr1 last1))
				(is-end2 (eq cons2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_symmetric_difference-imp-05 out-idx))
				  (is-end1 (_= (aref out-buf out-idx) (car cons2)) (setf cons2 (cdr cons2)) (incf out-idx))
				  (is-end2 (_= (aref out-buf out-idx) *itr1)       ++itr1                   (incf out-idx))
				  (t (let ((val1 *itr1)
						   (val2 (car cons2)))
					   (cond ((funcall less-bf val1 val2) (_= (aref out-buf out-idx) val1) ++itr1                   (incf out-idx))
							 ((funcall less-bf val2 val1) (_= (aref out-buf out-idx) val2) (setf cons2 (cdr cons2)) (incf out-idx))
							 (t ++itr1 (setf cons2 (cdr cons2)))))))))))

  ;;IMP; set_symmetric_difference : 06 -   i  x cvp x  o 
  (defun __set_symmetric_difference-imp-06 (first1 last1 idx2 last2 buffer2 oitr less-bf)
	(declare (type fixnum idx2 last2))
	(declare (type cl:vector buffer2))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)) t nil)
		  (let ((is-end1 (_== itr1 last1))
				(is-end2 (= idx2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_symmetric_difference-imp-06 oitr))
				  (is-end1 (_= *oitr (aref buffer2 idx2)) (incf idx2) ++oitr)
				  (is-end2 (_= *oitr *itr1)               ++itr1      ++oitr)
				  (t (let ((val1 *itr1)
						   (val2 (aref buffer2 idx2)))
					   (cond ((funcall less-bf val1 val2) (_= *oitr val1) ++itr1      ++oitr)
							 ((funcall less-bf val2 val1) (_= *oitr val2) (incf idx2) ++oitr)
							 (t ++itr1 (incf idx2))))))))))

  ;;IMP; set_symmetric_difference : 07 -   i  x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_symmetric_difference-imp-07 (first1 last1 idx2 last2 src-buf2 out less-bf)
	(declare (type fixnum idx2 last2))
	(declare (type cl:vector src-buf2))
	(declare (type cl:list out))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)) t nil)
		  (let ((is-end1 (_== itr1 last1))
				(is-end2 (= idx2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_symmetric_difference-imp-07 out))
				  (is-end1 (_= (car out) (aref src-buf2 idx2)) (incf idx2) (setf out (cdr out)))
				  (is-end2 (_= (car out) *itr1)                ++itr1      (setf out (cdr out)))
				  (t (let ((val1 *itr1)
						   (val2 (aref src-buf2 idx2)))
					   (cond ((funcall less-bf val1 val2) (_= (car out) val1) ++itr1      (setf out (cdr out)))
							 ((funcall less-bf val2 val1) (_= (car out) val2) (incf idx2) (setf out (cdr out)))
							 (t ++itr1 (incf idx2))))))))))

  ;;IMP; set_symmetric_difference : 08 -   i  x cvp x  vp
  (defun __set_symmetric_difference-imp-08 (first1 last1 idx2 last2 src-buf2 out-idx out-buf less-bf)
	(declare (type fixnum idx2 last2 out-idx))
	(declare (type cl:vector src-buf2 out-buf))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr1 @~first1)) t nil)
		  (let ((is-end1 (_== itr1 last1))
				(is-end2 (= idx2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_symmetric_difference-imp-08 out-idx))
				  (is-end1 (_= (aref out-buf out-idx) (aref src-buf2 idx2)) (incf idx2) (incf out-idx))
				  (is-end2 (_= (aref out-buf out-idx) *itr1)                ++itr1      (incf out-idx))
				  (t (let ((val1 *itr1)
						   (val2 (aref src-buf2 idx2)))
					   (cond ((funcall less-bf val1 val2) (_= (aref out-buf out-idx) val1) ++itr1      (incf out-idx))
							 ((funcall less-bf val2 val1) (_= (aref out-buf out-idx) val2) (incf idx2) (incf out-idx))
							 (t ++itr1 (incf idx2))))))))))

  ;;IMP; set_symmetric_difference : 09 -  cci x  i  x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_symmetric_difference-imp-09 (cons1 last1 first2 last2 oitr less-bf)
	(declare (type cl:list cons1 last1))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr2 @~first2)) t nil)
		  (let ((is-end1 (eq cons1 last1))
				(is-end2 (_== itr2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_symmetric_difference-imp-09 oitr))
				  (is-end1 (_= *oitr *itr2)       ++itr2                   ++oitr)
				  (is-end2 (_= *oitr (car cons1)) (setf cons1 (cdr cons1)) ++oitr)
				  (t (let ((val1 (car cons1))
						   (val2 *itr2))
					   (cond ((funcall less-bf val1 val2) (_= *oitr val1) (setf cons1 (cdr cons1)) ++oitr)
							 ((funcall less-bf val2 val1) (_= *oitr val2) ++itr2                   ++oitr)
							 (t (setf cons1 (cdr cons1)) ++itr2)))))))))

  ;;IMP; set_symmetric_difference : 10 -  cci x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_symmetric_difference-imp-10 (cons1 last1 first2 last2 out less-bf)
	(declare (type cl:list cons1 last1 out))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr2 @~first2)) t nil)
		  (let ((is-end1 (eq cons1 last1))
				(is-end2 (_== itr2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_symmetric_difference-imp-10 out))
				  (is-end1 (_= (car out) *itr2)       ++itr2                   (setf out (cdr out)))
				  (is-end2 (_= (car out) (car cons1)) (setf cons1 (cdr cons1)) (setf out (cdr out)))
				  (t (let ((val1 (car cons1))
						   (val2 *itr2))
					   (cond ((funcall less-bf val1 val2) (_= (car out) val1) (setf cons1 (cdr cons1)) (setf out (cdr out)))
							 ((funcall less-bf val2 val1) (_= (car out) val2) ++itr2                   (setf out (cdr out)))
							 (t (setf cons1 (cdr cons1)) ++itr2)))))))))

  ;;IMP; set_symmetric_difference : 11 -  cci x  i  x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_symmetric_difference-imp-11 (cons1 last1 first2 last2 out-idx out-buf less-bf)
	(declare (type cl:list cons1 last1))
	(declare (type fixnum out-idx))
	(declare (type cl:vector out-buf))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr2 @~first2)) t nil)
		  (let ((is-end1 (eq cons1 last1))
				(is-end2 (_== itr2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_symmetric_difference-imp-11 out-idx))
				  (is-end1 (_= (aref out-buf out-idx) *itr2)       ++itr2                   (incf out-idx))
				  (is-end2 (_= (aref out-buf out-idx) (car cons1)) (setf cons1 (cdr cons1)) (incf out-idx))
				  (t (let ((val1 (car cons1))
						   (val2 *itr2))
					   (cond ((funcall less-bf val1 val2) (_= (aref out-buf out-idx) val1) (setf cons1 (cdr cons1)) (incf out-idx))
							 ((funcall less-bf val2 val1) (_= (aref out-buf out-idx) val2) ++itr2                   (incf out-idx))
							 (t (setf cons1 (cdr cons1)) ++itr2)))))))))

  ;;IMP; set_symmetric_difference : 12 -  cci x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_symmetric_difference-imp-12 (cons1 last1 cons2 last2 oitr less-bf)
	(declare (type cl:list cons1 last1 cons2 last2))
	(declare (type cl:function less-bf))
	(for (nil t nil)
	  (with-operators
		  (let ((is-end1 (eq cons1 last1))
				(is-end2 (eq cons2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_symmetric_difference-imp-12 oitr))
				  (is-end1 (_= *oitr (car cons2)) (setf cons2 (cdr cons2)) ++oitr)
				  (is-end2 (_= *oitr (car cons1)) (setf cons1 (cdr cons1)) ++oitr)
				  (t (let ((val1 (car cons1))
						   (val2 (car cons2)))
					   (cond ((funcall less-bf val1 val2) (_= *oitr val1) (setf cons1 (cdr cons1)) ++oitr)
							 ((funcall less-bf val2 val1) (_= *oitr val2) (setf cons2 (cdr cons2)) ++oitr)
							 (t (setf cons1 (cdr cons1)) (setf cons2 (cdr cons2)))))))))))

  ;;IMP; set_symmetric_difference : 13 -  cci x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_symmetric_difference-imp-13 (cons1 last1 cons2 last2 out less-bf)
	(declare (type cl:list cons1 last1 cons2 last2 out))
	(declare (type cl:function less-bf))
	(for (nil t nil)
	  (let ((is-end1 (eq cons1 last1))
			(is-end2 (eq cons2 last2)))
		(cond ((and is-end1 is-end2) (return-from __set_symmetric_difference-imp-13 out))
			  (is-end1 (_= (car out) (car cons2)) (setf cons2 (cdr cons2)) (setf out (cdr out)))
			  (is-end2 (_= (car out) (car cons1)) (setf cons1 (cdr cons1)) (setf out (cdr out)))
			  (t (let ((val1 (car cons1))
					   (val2 (car cons2)))
				   (cond ((funcall less-bf val1 val2) (_= (car out) val1) (setf cons1 (cdr cons1)) (setf out (cdr out)))
						 ((funcall less-bf val2 val1) (_= (car out) val2) (setf cons2 (cdr cons2)) (setf out (cdr out)))
						 (t (setf cons1 (cdr cons1)) (setf cons2 (cdr cons2))))))))))

  ;;IMP; set_symmetric_difference : 14 -  cci x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_symmetric_difference-imp-14 (cons1 last1 cons2 last2 out-idx out-buf less-bf)
	(declare (type cl:list cons1 last1 cons2 last2))
	(declare (type fixnum out-idx))
	(declare (type cl:vector out-buf))
	(declare (type cl:function less-bf))
	(for (nil t nil)
	  (let ((is-end1 (eq cons1 last1))
			(is-end2 (eq cons2 last2)))
		(cond ((and is-end1 is-end2) (return-from __set_symmetric_difference-imp-14 out-idx))
			  (is-end1 (_= (aref out-buf out-idx) (car cons2)) (setf cons2 (cdr cons2)) (incf out-idx))
			  (is-end2 (_= (aref out-buf out-idx) (car cons1)) (setf cons1 (cdr cons1)) (incf out-idx))
			  (t (let ((val1 (car cons1))
					   (val2 (car cons2)))
				   (cond ((funcall less-bf val1 val2) (_= (aref out-buf out-idx) val1) (setf cons1 (cdr cons1)) (incf out-idx))
						 ((funcall less-bf val2 val1) (_= (aref out-buf out-idx) val2) (setf cons2 (cdr cons2)) (incf out-idx))
						 (t (setf cons1 (cdr cons1)) (setf cons2 (cdr cons2))))))))))

  ;;IMP; set_symmetric_difference : 15 -  cci x cvp x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_symmetric_difference-imp-15 (cons1 last1 idx2 last2 buf2 oitr less-bf)
	(declare (type cl:list cons1 last1))
	(declare (type fixnum idx2 last2))
	(declare (type cl:vector buf2))
	(declare (type cl:function less-bf))
	(for (nil t nil)
	  (with-operators
		  (let ((is-end1 (eq cons1 last1))
				(is-end2 (= idx2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_symmetric_difference-imp-15 oitr))
				  (is-end1 (_= *oitr (aref buf2 idx2)) (incf idx2)              ++oitr)
				  (is-end2 (_= *oitr (car cons1))      (setf cons1 (cdr cons1)) ++oitr)
				  (t (let ((val1 (car cons1))
						   (val2 (aref buf2 idx2)))
					   (cond ((funcall less-bf val1 val2) (_= *oitr val1) (setf cons1 (cdr cons1)) ++oitr)
							 ((funcall less-bf val2 val1) (_= *oitr val2) (incf idx2)              ++oitr)
							 (t (setf cons1 (cdr cons1)) (incf idx2))))))))))

  ;;IMP; set_symmetric_difference : 16 -  cci x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_symmetric_difference-imp-16 (cons1 last1 idx2 last2 buf2 out less-bf)
	(declare (type cl:list cons1 last1 out))
	(declare (type fixnum idx2 last2))
	(declare (type cl:vector buf2))
	(declare (type cl:function less-bf))
	(for (nil t nil)
	  (let ((is-end1 (eq cons1 last1))
			(is-end2 (= idx2 last2)))
		(cond ((and is-end1 is-end2) (return-from __set_symmetric_difference-imp-16 out))
			  (is-end1 (_= (car out) (aref buf2 idx2)) (incf idx2)              (setf out (cdr out)))
			  (is-end2 (_= (car out) (car cons1))      (setf cons1 (cdr cons1)) (setf out (cdr out)))
			  (t (let ((val1 (car cons1))
					   (val2 (aref buf2 idx2)))
				   (cond ((funcall less-bf val1 val2) (_= (car out) val1) (setf cons1 (cdr cons1)) (setf out (cdr out)))
						 ((funcall less-bf val2 val1) (_= (car out) val2) (incf idx2)              (setf out (cdr out)))
						 (t (setf cons1 (cdr cons1)) (incf idx2)))))))))

  ;;IMP; set_symmetric_difference : 17 -  cci x cvp x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_symmetric_difference-imp-17 (cons1 last1 idx2 last2 buf2 out-idx out-buf less-bf)
	(declare (type cl:list cons1 last1))
	(declare (type fixnum idx2 last2 out-idx))
	(declare (type cl:vector buf2 out-buf))
	(declare (type cl:function less-bf))
	(for (nil t nil)
	  (let ((is-end1 (eq cons1 last1))
			(is-end2 (= idx2 last2)))
		(cond ((and is-end1 is-end2) (return-from __set_symmetric_difference-imp-17 out-idx))
			  (is-end1 (_= (aref out-buf out-idx) (aref buf2 idx2)) (incf idx2)              (incf out-idx))
			  (is-end2 (_= (aref out-buf out-idx) (car cons1))      (setf cons1 (cdr cons1)) (incf out-idx))
			  (t (let ((val1 (car cons1))
					   (val2 (aref buf2 idx2)))
				   (cond ((funcall less-bf val1 val2) (_= (aref out-buf out-idx) val1) (setf cons1 (cdr cons1)) (incf out-idx))
						 ((funcall less-bf val2 val1) (_= (aref out-buf out-idx) val2) (incf idx2)              (incf out-idx))
						 (t (setf cons1 (cdr cons1)) (incf idx2)))))))))

  ;;IMP; set_symmetric_difference : 18 -  cvp x  i  x  o 
  (defun __set_symmetric_difference-imp-18 (idx1 last1 buffer1 first2 last2 oitr less-bf)
	(declare (type fixnum idx1 last1))
	(declare (type cl:vector buffer1))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr2 @~first2)) t nil)
		  (let ((is-end1 (= idx1 last1))
				(is-end2 (_== itr2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_symmetric_difference-imp-18 oitr))
				  (is-end1 (_= *oitr *itr2)               ++itr2      ++oitr)
				  (is-end2 (_= *oitr (aref buffer1 idx1)) (incf idx1) ++oitr)
				  (t (let ((val1 (aref buffer1 idx1))
						   (val2 *itr2))
					   (cond ((funcall less-bf val1 val2) (_= *oitr val1) (incf idx1) ++oitr)
							 ((funcall less-bf val2 val1) (_= *oitr val2) ++itr2      ++oitr)
							 (t (incf idx1) ++itr2)))))))))

  ;;IMP; set_symmetric_difference : 19 -  cvp x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_symmetric_difference-imp-19 (idx1 last1 src-buf1 first2 last2 out less-bf)
	(declare (type fixnum idx1 last1))
	(declare (type cl:vector src-buf1))
	(declare (type cl:list out))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr2 @~first2)) t nil)
		  (let ((is-end1 (= idx1 last1))
				(is-end2 (_== itr2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_symmetric_difference-imp-19 out))
				  (is-end1 (_= (car out) *itr2)                ++itr2      (setf out (cdr out)))
				  (is-end2 (_= (car out) (aref src-buf1 idx1)) (incf idx1) (setf out (cdr out)))
				  (t (let ((val1 (aref src-buf1 idx1))
						   (val2 *itr2))
					   (cond ((funcall less-bf val1 val2) (_= (car out) val1) (incf idx1) (setf out (cdr out)))
							 ((funcall less-bf val2 val1) (_= (car out) val2) ++itr2      (setf out (cdr out)))
							 (t (incf idx1) ++itr2)))))))))

  ;;IMP; set_symmetric_difference : 20 -  cvp x  i  x  vp
  (defun __set_symmetric_difference-imp-20 (idx1 last1 src-buf1 first2 last2 out-idx out-buf less-bf)
	(declare (type fixnum idx1 last1 out-idx))
	(declare (type cl:vector src-buf1 out-buf))
	(declare (type cl:function less-bf))
	(with-operators
		(for (((itr2 @~first2)) t nil)
		  (let ((is-end1 (= idx1 last1))
				(is-end2 (_== itr2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_symmetric_difference-imp-20 out-idx))
				  (is-end1 (_= (aref out-buf out-idx) *itr2)                ++itr2      (incf out-idx))
				  (is-end2 (_= (aref out-buf out-idx) (aref src-buf1 idx1)) (incf idx1) (incf out-idx))
				  (t (let ((val1 (aref src-buf1 idx1))
						   (val2 *itr2))
					   (cond ((funcall less-bf val1 val2) (_= (aref out-buf out-idx) val1) (incf idx1) (incf out-idx))
							 ((funcall less-bf val2 val1) (_= (aref out-buf out-idx) val2) ++itr2      (incf out-idx))
							 (t (incf idx1) ++itr2)))))))))

  ;;IMP; set_symmetric_difference : 21 -  cvp x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_symmetric_difference-imp-21 (idx1 last1 buf1 cons2 last2 oitr less-bf)
	(declare (type fixnum idx1 last1))
	(declare (type cl:vector buf1))
	(declare (type cl:list cons2 last2))
	(declare (type cl:function less-bf))
	(for (nil t nil)
	  (with-operators
		  (let ((is-end1 (=  idx1  last1))
				(is-end2 (eq cons2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_symmetric_difference-imp-21 oitr))
				  (is-end1 (_= *oitr (car      cons2)) (setf cons2 (cdr cons2)) ++oitr)
				  (is-end2 (_= *oitr (aref buf1 idx1)) (incf idx1)              ++oitr)
				  (t (let ((val1 (aref buf1 idx1))
						   (val2 (car      cons2)))
					   (cond ((funcall less-bf val1 val2) (_= *oitr val1) (incf idx1)              ++oitr)
							 ((funcall less-bf val2 val1) (_= *oitr val2) (setf cons2 (cdr cons2)) ++oitr)
							 (t (incf idx1) (setf cons2 (cdr cons2)))))))))))

  ;;IMP; set_symmetric_difference : 22 -  cvp x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_symmetric_difference-imp-22 (idx1 last1 buf1 cons2 last2 out less-bf)
	(declare (type fixnum idx1 last1))
	(declare (type cl:vector buf1))
	(declare (type cl:list cons2 last2 out))
	(declare (type cl:function less-bf))
	(for (nil t nil)
	  (let ((is-end1 (=  idx1  last1))
			(is-end2 (eq cons2 last2)))
		(cond ((and is-end1 is-end2) (return-from __set_symmetric_difference-imp-22 out))
			  (is-end1 (_= (car out) (car      cons2)) (setf cons2 (cdr cons2)) (setf out (cdr out)))
			  (is-end2 (_= (car out) (aref buf1 idx1)) (incf idx1)              (setf out (cdr out)))
			  (t (let ((val1 (aref buf1 idx1))
					   (val2 (car      cons2)))
				   (cond ((funcall less-bf val1 val2) (_= (car out) val1) (incf idx1)              (setf out (cdr out)))
						 ((funcall less-bf val2 val1) (_= (car out) val2) (setf cons2 (cdr cons2)) (setf out (cdr out)))
						 (t (incf idx1) (setf cons2 (cdr cons2))))))))))

  ;;IMP; set_symmetric_difference : 23 -  cvp x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_symmetric_difference-imp-23 (idx1 last1 buf1 cons2 last2 out-idx out-buf less-bf)
	(declare (type fixnum idx1 last1 out-idx))
	(declare (type cl:vector buf1 out-buf))
	(declare (type cl:list cons2 last2))
	(declare (type cl:function less-bf))
	(for (nil t nil)
	  (let ((is-end1 (=  idx1  last1))
			(is-end2 (eq cons2 last2)))
		(cond ((and is-end1 is-end2) (return-from __set_symmetric_difference-imp-23 out-idx))
			  (is-end1 (_= (aref out-buf out-idx) (car cons2))      (setf cons2 (cdr cons2)) (incf out-idx))
			  (is-end2 (_= (aref out-buf out-idx) (aref buf1 idx1)) (incf idx1) (incf out-idx))
			  (t (let ((val1 (aref buf1 idx1))
					   (val2 (car cons2)))
				   (cond ((funcall less-bf val1 val2) (_= (aref out-buf out-idx) val1) (incf idx1)              (incf out-idx))
						 ((funcall less-bf val2 val1) (_= (aref out-buf out-idx) val2) (setf cons2 (cdr cons2)) (incf out-idx))
						 (t (incf idx1) (setf cons2 (cdr cons2))))))))))

  ;;IMP; set_symmetric_difference : 24 -  cvp x cvp x  o 
  (defun __set_symmetric_difference-imp-24 (idx1 last1 buffer1 idx2 last2 buffer2 oitr less-bf)
	(declare (type fixnum idx1 last1 idx2 last2))
	(declare (type cl:vector buffer1 buffer2))
	(declare (type cl:function less-bf))
	(for (nil t nil)
	  (with-operators
		  (let ((is-end1 (= idx1 last1))
				(is-end2 (= idx2 last2)))
			(cond ((and is-end1 is-end2) (return-from __set_symmetric_difference-imp-24 oitr))
				  (is-end1 (_= *oitr (aref buffer2 idx2)) (incf idx2) ++oitr)
				  (is-end2 (_= *oitr (aref buffer1 idx1)) (incf idx1) ++oitr)
				  (t (let ((val1 (aref buffer1 idx1))
						   (val2 (aref buffer2 idx2)))
					   (cond ((funcall less-bf val1 val2) (_= *oitr val1) (incf idx1) ++oitr)
							 ((funcall less-bf val2 val1) (_= *oitr val2) (incf idx2) ++oitr)
							 (t (incf idx1) (incf idx2))))))))))

  ;;IMP; set_symmetric_difference : 25 -  cvp x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defun __set_symmetric_difference-imp-25 (idx1 last1 src-buf1 idx2 last2 src-buf2 out less-bf)
	(declare (type fixnum idx1 last1 idx2 last2))
	(declare (type cl:vector src-buf1 src-buf2))
	(declare (type cl:list out))
	(declare (type cl:function less-bf))
	(for (nil t nil)
	  (let ((is-end1 (= idx1 last1))
			(is-end2 (= idx2 last2)))
		(cond ((and is-end1 is-end2) (return-from __set_symmetric_difference-imp-25 out))
			  (is-end1 (_= (car out) (aref src-buf2 idx2)) (incf idx2) (setf out (cdr out)))
			  (is-end2 (_= (car out) (aref src-buf1 idx1)) (incf idx1) (setf out (cdr out)))
			  (t (let ((val1 (aref src-buf1 idx1))
					   (val2 (aref src-buf2 idx2)))
				   (cond ((funcall less-bf val1 val2) (_= (car out) val1) (incf idx1) (setf out (cdr out)))
						 ((funcall less-bf val2 val1) (_= (car out) val2) (incf idx2) (setf out (cdr out)))
						 (t (incf idx1) (incf idx2)))))))))

  ;;IMP; set_symmetric_difference : 26 -  cvp x cvp x  vp
  (defun __set_symmetric_difference-imp-26 (idx1 last1 src-buf1 idx2 last2 src-buf2 out-idx out-buf less-bf)
	(declare (type fixnum idx1 last1 idx2 last2 out-idx))
	(declare (type cl:vector src-buf1 src-buf2 out-buf))
	(declare (type cl:function less-bf))
	(for (nil t nil)
	  (let ((is-end1 (= idx1 last1))
			(is-end2 (= idx2 last2)))
		(cond ((and is-end1 is-end2) (return-from __set_symmetric_difference-imp-26 out-idx))
			  (is-end1 (_= (aref out-buf out-idx) (aref src-buf2 idx2)) (incf idx2) (incf out-idx))
			  (is-end2 (_= (aref out-buf out-idx) (aref src-buf1 idx1)) (incf idx1) (incf out-idx))
			  (t (let ((val1 (aref src-buf1 idx1))
					   (val2 (aref src-buf2 idx2)))
				   (cond ((funcall less-bf val1 val2) (_= (aref out-buf out-idx) val1) (incf idx1) (incf out-idx))
						 ((funcall less-bf val2 val1) (_= (aref out-buf out-idx) val2) (incf idx2) (incf out-idx))
						 (t (incf idx1) (incf idx2))))))))))





;;------------------------------------------------------------------------------
;;
;; exported functions
;;
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; 25.1, non-modifying sequence operations:
;;------------------------------------------------------------------------------

;; first   : input_iterator
;; last    : input_iterator
;; pred    : unary_function
;; returns : boolean value
#-cl-stl-0x98    ; all_of
(locally (declare (optimize speed))

  ;;PTN; all_of : 0 -  i
  (defmethod all_of ((first input_iterator) (last input_iterator) pred)
	(if (_== first last)
		t
		(with-operators
			(let ((pred (functor_function @~pred)))
			  (declare (type cl:function pred))
			  (for (((itr @~first)) (_/= itr last) ++itr :returns t)
				(unless (funcall pred *itr)
				  (return-from all_of nil)))))))

  ;;PTN; all_of : 1 - cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod all_of ((first cons_const_iterator) (last cons_const_iterator) pred)
	;;(format t "specialized all_of for cons_const_iterator is invoked.~%")
	(let ((cons1 (__cons-itr-cons first))
		  (cons2 (__cons-itr-cons  last)))
	  (if (eq cons1 cons2)
		  t
		  (let ((pred (functor_function (clone pred))))
			(declare (type cl:function pred))
			(for (nil (not (eq cons1 cons2)) (setf cons1 (cdr cons1)) :returns t)
			  (unless (funcall pred (car cons1))
				(return-from all_of nil)))))))

  ;;PTN; all_of : 2 - cvp
  (defmethod all_of ((first const-vector-pointer) (last const-vector-pointer) pred)
	;;(format t "specialized all_of for const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index first))
		  (idx2 (opr::vec-ptr-index  last)))
	  (declare (type fixnum idx1 idx2))
	  (if (= idx1 idx2)
		  t
		  (let ((buffer (opr::vec-ptr-buffer first))
				(pred   (functor_function (clone pred))))
			(declare (type cl:vector buffer))
			(declare (type cl:function pred))
			(for (nil (< idx1 idx2) (incf idx1) :returns t)
			  (unless (funcall pred (aref buffer idx1))
				(return-from all_of nil))))))))



; first   : input_iterator
; last    : input_iterator
; pred    : unary_function
; returns : boolean value
#-cl-stl-0x98    ; any_of
(locally (declare (optimize speed))

  ;;PTN; any_of : 0 -  i
  (defmethod any_of ((first input_iterator) (last input_iterator) pred)
	(if (_== first last)
		nil
		(with-operators
			(let ((pred (functor_function @~pred)))
			  (declare (type cl:function pred))
			  (for (((itr @~first)) (_/= itr last) ++itr :returns nil)
				(when (funcall pred *itr)
				  (return-from any_of t)))))))

  ;;PTN; any_of : 1 - cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod any_of ((first cons_const_iterator) (last cons_const_iterator) pred)
	;;(format t "specialized any_of for cons_const_iterator is invoked.~%")
	(let ((cons1 (__cons-itr-cons first))
		  (cons2 (__cons-itr-cons  last)))
	  (if (eq cons1 cons2)
		  nil
		  (let ((pred (functor_function (clone pred))))
			(declare (type cl:function pred))
			(for (nil (not (eq cons1 cons2)) (setf cons1 (cdr cons1)) :returns nil)
			  (when (funcall pred (car cons1))
				(return-from any_of t)))))))

  ;;PTN; any_of : 2 - cvp
  (defmethod any_of ((first const-vector-pointer) (last const-vector-pointer) pred)
	;;(format t "specialized any_of for const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(if (_== first last)
		nil
		(let ((idx1   (opr::vec-ptr-index  first))
			  (idx2   (opr::vec-ptr-index  last))
			  (buffer (opr::vec-ptr-buffer first))
			  (pred   (functor_function (clone pred))))
		  (declare (type fixnum idx1 idx2))
		  (declare (type cl:vector buffer))
		  (declare (type cl:function pred))
		  (for (nil (< idx1 idx2) (incf idx1) :returns nil)
			(when (funcall pred (aref buffer idx1))
			  (return-from any_of t)))))))



; first   : input_iterator
; last    : input_iterator
; pred    : unary_function
; returns : boolean value
#-cl-stl-0x98    ; none_of
(locally (declare (optimize speed))

  ;;PTN; none_of : 0 -  i
  (defmethod none_of ((first input_iterator) (last input_iterator) pred)
	(if (_== first last)
		t
		(with-operators
			(let ((pred (functor_function @~pred)))
			  (declare (type cl:function pred))
			  (for (((itr @~first)) (_/= itr last) ++itr :returns t)
				(when (funcall pred *itr)
				  (return-from none_of nil)))))))

  ;;PTN; none_of : 1 - cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod none_of ((first cons_const_iterator) (last cons_const_iterator) pred)
	;;(format t "specialized none_of for cons_const_iterator is invoked.~%")
	(let ((cons1 (__cons-itr-cons first))
		  (cons2 (__cons-itr-cons  last)))
	  (if (eq cons1 cons2)
		  t
		  (let ((pred (functor_function (clone pred))))
			(declare (type cl:function pred))
			(for (nil (not (eq cons1 cons2)) (setf cons1 (cdr cons1)) :returns t)
			  (when (funcall pred (car cons1))
				(return-from none_of nil)))))))

  ;;PTN; none_of : 2 - cvp
  (defmethod none_of ((first const-vector-pointer) (last const-vector-pointer) pred)
	;;(format t "specialized none_of for const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index first))
		  (idx2 (opr::vec-ptr-index  last)))
	  (declare (type fixnum idx1 idx2))
	  (if (= idx1 idx2)
		  t
		  (let ((buffer (opr::vec-ptr-buffer first))
				(pred   (functor_function (clone pred))))
			(declare (type cl:vector buffer))
			(declare (type cl:function pred))
			(for (nil (< idx1 idx2) (incf idx1) :returns t)
			  (when (funcall pred (aref buffer idx1))
				(return-from none_of nil))))))))



; first   : input_iterator
; last    : input_iterator
; func    : unary_function
; returns : copy of func
(locally (declare (optimize speed))

  ;;PTN; for_each : 0 -  i
  (defmethod for_each ((first input_iterator) (last input_iterator) func)
	(with-operators
		(let ((func @~func))
		  (if (_== first last)
			  func
			  (let ((fnc (functor_function func)))
				(declare (type cl:function fnc))
				(for (((itr @~first)) (_/= itr last) ++itr :returns func)
				  (funcall fnc *itr)))))))

  ;;PTN; for_each : 1 - cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod for_each ((first cons_const_iterator) (last cons_const_iterator) func)
	;;(format t "specialized for_each for cons_const_iterator is invoked.~%")
	(let ((fnc (clone func)))
	  (let ((cons1 (__cons-itr-cons first))
			(cons2 (__cons-itr-cons  last))
			(uf    (functor_function  fnc)))
		(declare (type cl:function uf))
		(for (nil (not (eq cons1 cons2)) (setf cons1 (cdr cons1)))
		  (funcall uf (car cons1))))
	  fnc))

  ;;PTN; for_each : 2 - cvp
  (defmethod for_each ((first const-vector-pointer) (last const-vector-pointer) func)
	;;(format t "specialized for_each for const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((fnc (clone func)))
	  (let ((idx1   (opr::vec-ptr-index  first))
			(idx2   (opr::vec-ptr-index   last))
			(buffer (opr::vec-ptr-buffer first))
			(uf     (functor_function fnc)))
		(declare (type fixnum idx1 idx2))
		(declare (type cl:vector buffer))
		(declare (type cl:function uf))
		(for (nil (< idx1 idx2) (incf idx1))
		  (funcall uf (aref buffer idx1))))
	  fnc)))



; first   : input_iterator
; last    : input_iterator
; returns : iterator point to found element.
(locally (declare (optimize speed))

  ;;PTN; find : 0 -  i
  (labels ((__find-imp-0 (first last v eql-bf)
			 (declare (type cl:function eql-bf))
			 (with-operators
				 (for (((itr @~first)) (_/= itr last) ++itr :returns itr)
				   (when (funcall eql-bf v *itr)
					 (return-from __find-imp-0 itr))))))

	(defmethod-overload find ((first input_iterator) (last input_iterator) v)
	  (__find-imp-0 first last v #'operator_==))

	#-cl-stl-noextra
	(defmethod-overload find ((first input_iterator) (last input_iterator) v eql-bf)
	  (__find-imp-0 first last v (functor_function (clone eql-bf)))))

  ;;PTN; find : 1 - cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__find-imp-1 (cons1 cons2 v eql-bf)
			 (declare (type cl:function eql-bf))
			 (for (nil (not (eq cons1 cons2)) (setf cons1 (cdr cons1)) :returns cons2)
			   (when (funcall eql-bf v (car cons1))
				 (return-from __find-imp-1 cons1)))))

	(defmethod-overload find ((first cons_const_iterator) (last cons_const_iterator) v)
	  ;;(format t "specialized find for cons_const_iterator is invoked.~%")
	  (__algo-make-cns-iterator first (__find-imp-1 (__cons-itr-cons first)
													 (__cons-itr-cons  last) v #'operator_==)))

	#-cl-stl-noextra
	(defmethod-overload find ((first cons_const_iterator) (last cons_const_iterator) v eql-bf)
	  ;;(format t "specialized find for cons_const_iterator is invoked.~%")
	  (__algo-make-cns-iterator first
								 (__find-imp-1 (__cons-itr-cons first)
											   (__cons-itr-cons  last) v (functor_function (clone eql-bf))))))

  ;;PTN; find : 2 - cvp
  (labels ((__find-imp-2 (idx1 idx2 buffer v eql-bf)
			 (declare (type fixnum idx1 idx2))
			 (declare (type cl:vector buffer))
			 (declare (type cl:function eql-bf))
			 (for (nil (< idx1 idx2) (incf idx1) :returns idx2)
			   (when (funcall eql-bf v (aref buffer idx1))
				 (return-from __find-imp-2 idx1)))))

	(defmethod-overload find ((first const-vector-pointer) (last const-vector-pointer) v)
	  ;;(format t "specialized find for const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__algo-make-vct-iterator first (__find-imp-2 (opr::vec-ptr-index  first)
													 (opr::vec-ptr-index  last)
													 (opr::vec-ptr-buffer first) v #'operator_==)))

	#-cl-stl-noextra
	(defmethod-overload find ((first const-vector-pointer) (last const-vector-pointer) v eql-bf)
	  ;;(format t "specialized find for const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__algo-make-vct-iterator first
								 (__find-imp-2 (opr::vec-ptr-index  first)
											   (opr::vec-ptr-index  last)
											   (opr::vec-ptr-buffer first) v (functor_function (clone eql-bf)))))))


	
; first   : input_iterator
; last    : input_iterator
; pred    : unary_function
; returns : iterator point to found element.
(locally (declare (optimize speed))

  ;;PTN; find_if : 0 -  i
  (defmethod find_if ((first input_iterator) (last input_iterator) pred)
	(if (_== first last)
		(clone last)
		(__find_if-imp-0 first last (functor_function (clone pred)))))

  ;;PTN; find_if : 1 - cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod find_if ((first cons_const_iterator) (last cons_const_iterator) pred)
	;;(format t "specialized find_if for cons_const_iterator is invoked.~%")
	(if (_== first last)
		(clone last)
		(__algo-make-cns-iterator first
								   (__find_if-imp-1 (__cons-itr-cons first)
													(__cons-itr-cons  last)
													(functor_function (clone pred))))))

  ;;PTN; find_if : 2 - cvp
  (defmethod find_if ((first const-vector-pointer) (last const-vector-pointer) pred)
	;;(format t "specialized find_if for const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(if (_== first last)
		(clone last)
		(__algo-make-vct-iterator first
								   (__find_if-imp-2 (opr::vec-ptr-index  first)
													(opr::vec-ptr-index  last)
													(opr::vec-ptr-buffer first)
													(functor_function (clone pred)))))))



; first   : input_iterator
; last    : input_iterator
; pred    : unary_function
; returns : iterator point to found element.
#-cl-stl-0x98    ; find_if_not
(locally (declare (optimize speed))

  ;;PTN; find_if_not : 0 -  i
  (defmethod find_if_not ((first input_iterator) (last input_iterator) pred)
	(with-operators
		(if (_== first last)
			@~last
			(let ((pred (functor_function @~pred)))
			  (declare (type cl:function pred))
			  (for (((itr @~first)) (_/= itr last) ++itr :returns itr)
				(unless (funcall pred *itr)
				  (return-from find_if_not itr)))))))

  ;;PTN; find_if_not : 1 - cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod find_if_not ((first cons_const_iterator) (last cons_const_iterator) pred)
	;;(format t "specialized find_if_not for cons_const_iterator is invoked.~%")
	(let ((cons1 (__cons-itr-cons first))
		  (cons2 (__cons-itr-cons  last)))
	  (if (eq cons1 cons2)
		  (clone last)
		  (let ((pred (functor_function (clone pred))))
			(declare (type cl:function pred))
			(let ((ret (for (nil (not (eq cons1 cons2)) (setf cons1 (cdr cons1)) :returns cons2)
						 (unless (funcall pred (car cons1))
						   (return cons1)))))
			  (__algo-make-cns-iterator first ret))))))

  ;;PTN; find_if_not : 2 - cvp
  (defmethod find_if_not ((first const-vector-pointer) (last const-vector-pointer) pred)
	;;(format t "specialized find_if_not for const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index first))
		  (idx2 (opr::vec-ptr-index  last)))
	  (declare (type fixnum idx1 idx2))
	  (if (= idx1 idx2)
		  (clone last)
		  (let ((buffer (opr::vec-ptr-buffer first))
				(pred   (functor_function (clone pred))))
			(declare (type cl:vector buffer))
			(declare (type cl:function pred))
			(let ((ret (for (nil (< idx1 idx2) (incf idx1) :returns idx2)
						 (unless (funcall pred (aref buffer idx1))
						   (return idx1)))))
			  (__algo-make-vct-iterator first ret)))))))



; first1  : forward_iterator
; last1   : forward_iterator
; first2  : forward_iterator
; last2   : forward_iterator
; eql-bf  : binary_function ( default : #'operator_== )
; returns : iterator point to found element.
(locally (declare (optimize speed))

  ;;PTN; find_end : 0 -  f  x  f 
  (defmethod-overload find_end ((first1 forward_iterator) (last1 forward_iterator)
								(first2 forward_iterator) (last2 forward_iterator))
	(__find_end-imp-0 first1 last1 first2 last2 #'operator_==))

  (defmethod-overload find_end ((first1 forward_iterator) (last1 forward_iterator)
								(first2 forward_iterator) (last2 forward_iterator) eql-bf)
	(__find_end-imp-0 first1 last1 first2 last2 (functor_function (clone eql-bf))))

  ;;PTN; find_end : 1 - cci x  f 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload find_end ((first1 cons_const_iterator)
								(last1  cons_const_iterator)
								(first2 forward_iterator) (last2 forward_iterator))
	;;(format t "specialized find_end for cons_const_iterator & forward_iterator is invoked.~%")
	(__algo-make-cns-iterator first1
							   (__find_end-imp-1 (__cons-itr-cons first1)
												 (__cons-itr-cons  last1) first2 last2 #'operator_==)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload find_end ((first1 cons_const_iterator)
								(last1  cons_const_iterator)
								(first2 forward_iterator) (last2 forward_iterator) eql-bf)
	;;(format t "specialized find_end for cons_const_iterator & forward_iterator is invoked.~%")
	(__algo-make-cns-iterator first1
							   (__find_end-imp-1 (__cons-itr-cons first1)
												 (__cons-itr-cons  last1)
												 first2 last2 (functor_function (clone eql-bf)))))
  
  ;;PTN; find_end : 2 - cvp x  f 
  (defmethod-overload find_end ((first1 const-vector-pointer)
								(last1  const-vector-pointer)
								(first2 forward_iterator) (last2 forward_iterator))
	;;(format t "specialized find_end for const-vector-pointer & forward_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-vct-iterator first1
							   (__find_end-imp-2 (opr::vec-ptr-index  first1)
												 (opr::vec-ptr-index  last1)
												 (opr::vec-ptr-buffer first1) first2 last2 #'operator_==)))

  (defmethod-overload find_end ((first1 const-vector-pointer)
								(last1  const-vector-pointer)
								(first2 forward_iterator) (last2 forward_iterator) eql-bf)
	;;(format t "specialized find_end for const-vector-pointer & forward_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-vct-iterator first1
							   (__find_end-imp-2 (opr::vec-ptr-index  first1)
												 (opr::vec-ptr-index  last1)
												 (opr::vec-ptr-buffer first1)
												 first2 last2 (functor_function (clone eql-bf)))))

  ;;PTN; find_end : 3 -  f  x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload find_end ((first1 forward_iterator)    (last1 forward_iterator)
								(first2 cons_const_iterator) (last2 cons_const_iterator))
	;;(format t "specialized find_end for forward_iterator & cons_const_iterator is invoked.~%")
	(__find_end-imp-3 first1 last1
					  (__cons-itr-cons first2)
					  (__cons-itr-cons  last2) #'operator_==))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload find_end ((first1 forward_iterator)    (last1 forward_iterator)
								(first2 cons_const_iterator) (last2 cons_const_iterator) eql-bf)
	;;(format t "specialized find_end for forward_iterator & cons_const_iterator is invoked.~%")
	(__find_end-imp-3 first1 last1
					  (__cons-itr-cons first2)
					  (__cons-itr-cons  last2) (functor_function (clone eql-bf))))

  ;;PTN; find_end : 4 - cci x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload find_end ((first1 cons_const_iterator) (last1 cons_const_iterator)
								(first2 cons_const_iterator) (last2 cons_const_iterator))
	;;(format t "specialized find_end for cons_const_iterator & cons_const_iterator is invoked.~%")
	(__algo-make-cns-iterator first1
							   (__find_end-imp-4 (__cons-itr-cons first1)
												 (__cons-itr-cons  last1)
												 (__cons-itr-cons first2)
												 (__cons-itr-cons  last2) #'operator_==)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload find_end ((first1 cons_const_iterator) (last1 cons_const_iterator)
								(first2 cons_const_iterator) (last2 cons_const_iterator) eql-bf)
	;;(format t "specialized find_end for cons_const_iterator & cons_const_iterator is invoked.~%")
	(__algo-make-cns-iterator first1
							   (__find_end-imp-4 (__cons-itr-cons first1)
												 (__cons-itr-cons  last1)
												 (__cons-itr-cons first2)
												 (__cons-itr-cons  last2) (functor_function (clone eql-bf)))))

  ;;PTN; find_end : 5 - cvp x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload find_end ((first1 const-vector-pointer) (last1 const-vector-pointer)
								(first2 cons_const_iterator)  (last2 cons_const_iterator))
	;;(format t "specialized find_end for const-vector-pointer & cons_const_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-vct-iterator first1
							   (__find_end-imp-5 (opr::vec-ptr-index  first1)
												 (opr::vec-ptr-index   last1)
												 (opr::vec-ptr-buffer first1)
												 (__cons-itr-cons     first2)
												 (__cons-itr-cons      last2) #'operator_==)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload find_end ((first1 const-vector-pointer) (last1 const-vector-pointer)
								(first2 cons_const_iterator)  (last2 cons_const_iterator) eql-bf)
	;;(format t "specialized find_end for const-vector-pointer & cons_const_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-vct-iterator first1
							   (__find_end-imp-5 (opr::vec-ptr-index  first1)
												 (opr::vec-ptr-index   last1)
												 (opr::vec-ptr-buffer first1)
												 (__cons-itr-cons     first2)
												 (__cons-itr-cons      last2) (functor_function (clone eql-bf)))))

  ;;PTN; find_end : 6 -  f  x cvp
  (defmethod-overload find_end ((first1 forward_iterator)     (last1 forward_iterator)
								(first2 const-vector-pointer) (last2 const-vector-pointer))
	;;(format t "specialized find_end for forward_iterator & const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__find_end-imp-6 first1 last1
					  (opr::vec-ptr-index  first2)
					  (opr::vec-ptr-index  last2)
					  (opr::vec-ptr-buffer first2) #'operator_==))

  (defmethod-overload find_end ((first1 forward_iterator)     (last1 forward_iterator)
								(first2 const-vector-pointer) (last2 const-vector-pointer) eql-bf)
	;;(format t "specialized find_end for forward_iterator & const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__find_end-imp-6 first1 last1
					  (opr::vec-ptr-index  first2)
					  (opr::vec-ptr-index  last2)
					  (opr::vec-ptr-buffer first2) (functor_function (clone eql-bf))))

  ;;PTN; find_end : 7 - cci x cvp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload find_end ((first1 cons_const_iterator) (last1 cons_const_iterator)
								(first2 const-vector-pointer) (last2 const-vector-pointer))
	;;(format t "specialized find_end for cons_const_iterator & const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-cns-iterator first1
							   (__find_end-imp-7 (__cons-itr-cons first1)
												 (__cons-itr-cons  last1)
												 (opr::vec-ptr-index  first2)
												 (opr::vec-ptr-index   last2)
												 (opr::vec-ptr-buffer first2) #'operator_==)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload find_end ((first1 cons_const_iterator) (last1 cons_const_iterator)
								(first2 const-vector-pointer) (last2 const-vector-pointer) eql-bf)
	;;(format t "specialized find_end for cons_const_iterator & const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-cns-iterator first1
							   (__find_end-imp-7 (__cons-itr-cons first1)
												 (__cons-itr-cons  last1)
												 (opr::vec-ptr-index  first2)
												 (opr::vec-ptr-index   last2)
												 (opr::vec-ptr-buffer first2) (functor_function (clone eql-bf)))))

  ;;PTN; find_end : 8 - cvp x cvp
  (defmethod-overload find_end ((first1 const-vector-pointer) (last1 const-vector-pointer)
								(first2 const-vector-pointer) (last2 const-vector-pointer))
	;;(format t "specialized find_end for const-vector-pointer & const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-vct-iterator first1
							   (__find_end-imp-8 (opr::vec-ptr-index  first1)
												 (opr::vec-ptr-index  last1)
												 (opr::vec-ptr-buffer first1)
												 (opr::vec-ptr-index  first2)
												 (opr::vec-ptr-index  last2)
												 (opr::vec-ptr-buffer first2) #'operator_==)))

  (defmethod-overload find_end ((first1 const-vector-pointer) (last1 const-vector-pointer)
								(first2 const-vector-pointer) (last2 const-vector-pointer) eql-bf)
	;;(format t "specialized find_end for const-vector-pointer & const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-vct-iterator first1
							   (__find_end-imp-8 (opr::vec-ptr-index  first1)
												 (opr::vec-ptr-index  last1)
												 (opr::vec-ptr-buffer first1)
												 (opr::vec-ptr-index  first2)
												 (opr::vec-ptr-index  last2)
												 (opr::vec-ptr-buffer first2) (functor_function (clone eql-bf))))))


; first1  : [0x98] forward_iterator / [0x11] input_iterator
; last1   : [0x98] forward_iterator / [0x11] input_iterator
; first2  : forward_iterator
; last2   : forward_iterator
; eql-bf  : binary_function ( default : #'operator_== )
; returns : iterator point to found element. [0x98] forward_iterator / [0x11] input_iterator
(locally (declare (optimize speed))

  ;;PTN; find_first_of : 0 -  f  x  f
  (labels ((__find_first_of-imp-0 (first1 last1 first2 last2 eql-bf)
			 (declare (type cl:function eql-bf))
			 (with-operators
				 (if (or (_== first1 last1) (_== first2 last2))
					 @~last1
					 (for (((itr1 @~first1)
							(itr2 @~first2)) (_/= itr1 last1) ++itr1 :returns itr1)
					   (when (let ((val *itr1))
							   (_= itr2 first2)
							   (for (nil (_/= itr2 last2) ++itr2 :returns nil)
								 (when (funcall eql-bf val *itr2)
								   (return t))))
						 (return-from __find_first_of-imp-0 itr1)))))))

	(defmethod-overload find_first_of ((first1 #+cl-stl-0x98 forward_iterator
											   #-cl-stl-0x98 input_iterator)
									   (last1  #+cl-stl-0x98 forward_iterator
											   #-cl-stl-0x98 input_iterator)
									   (first2 forward_iterator) (last2 forward_iterator))
	  (__find_first_of-imp-0 first1 last1 first2 last2 #'operator_==))

	(defmethod-overload find_first_of ((first1 #+cl-stl-0x98 forward_iterator
											   #-cl-stl-0x98 input_iterator)
									   (last1  #+cl-stl-0x98 forward_iterator
											   #-cl-stl-0x98 input_iterator)
									   (first2 forward_iterator) (last2 forward_iterator) eql-bf)
	  (__find_first_of-imp-0 first1 last1 first2 last2 (functor_function (clone eql-bf)))))
		  

  ;;PTN; find_first_of : 1 - cci x  f 
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__find_first_of-imp-1 (cons1 end1 first2 last2 eql-bf)
			 (declare (type cl:list cons1 end1))
			 (declare (type cl:function eql-bf))
			 (labels ((imp (val itr2)
						(with-operators
							(for (nil (_/= itr2 last2) ++itr2)
							  (when (funcall eql-bf val *itr2)
								(return-from imp t))))))
			   (if (or (eq cons1 end1)
					   (_== first2 last2))
				   end1
				   (let ((cns1 cons1)
						 (itr2 (clone first2)))
					 (declare (type cl:list cns1))
					 (for (nil (not (eq cns1 end1)) (setf cns1 (cdr cns1)) :returns cns1)
					   (when (imp (car cns1) (_= itr2 first2))
						 (return-from __find_first_of-imp-1 cns1))))))))

	(defmethod-overload find_first_of ((first1 cons_const_iterator)
									   (last1  cons_const_iterator)
									   (first2 forward_iterator) (last2 forward_iterator))
	  ;;(format t "specialized find_first_of for cons_const_iterator & forward_iterator is invoked.~%")
	  (__algo-make-cns-iterator first1
								 (__find_first_of-imp-1 (__cons-itr-cons first1)
														(__cons-itr-cons  last1) first2 last2 #'operator_==)))

	(defmethod-overload find_first_of ((first1 cons_const_iterator)
									   (last1  cons_const_iterator)
									   (first2 forward_iterator) (last2 forward_iterator) eql-bf)
	  ;;(format t "specialized find_first_of for cons_const_iterator & forward_iterator is invoked.~%")
	  (__algo-make-cns-iterator first1
								 (__find_first_of-imp-1 (__cons-itr-cons first1)
														(__cons-itr-cons  last1)
														first2 last2 (functor_function (clone eql-bf))))))

  ;;PTN; find_first_of : 2 - cvp x  f 
  (labels ((__find_first_of-imp-2 (begin1 end1 buffer1 first2 last2 eql-bf)
			 (declare (type fixnum begin1 end1))
			 (declare (type cl:vector buffer1))
			 (declare (type cl:function eql-bf))
			 (labels ((imp (val itr2)
						(with-operators
							(for (nil (_/= itr2 last2) ++itr2)
							  (when (funcall eql-bf val *itr2)
								(return-from imp t))))))
			   (if (or (= begin1 end1)
					   (_== first2 last2))
				   end1
				   (let ((idx1 begin1)
						 (itr2 (clone first2)))
					 (declare (type fixnum idx1))
					 (for (nil (< idx1 end1) (incf idx1) :returns idx1)
					   (when (imp (aref buffer1 idx1) (_= itr2 first2))
						 (return-from __find_first_of-imp-2 idx1))))))))

	(defmethod-overload find_first_of ((first1 const-vector-pointer)
									   (last1  const-vector-pointer)
									   (first2 forward_iterator) (last2 forward_iterator))
	  ;;(format t "specialized find_first_of for const-vector-pointer & forward_iterator is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__algo-make-vct-iterator first1
								 (__find_first_of-imp-2 (opr::vec-ptr-index  first1)
														(opr::vec-ptr-index   last1)
														(opr::vec-ptr-buffer first1) first2 last2 #'operator_==)))

	(defmethod-overload find_first_of ((first1 const-vector-pointer)
									   (last1  const-vector-pointer)
									   (first2 forward_iterator) (last2 forward_iterator) eql-bf)
	  ;;(format t "specialized find_first_of for const-vector-pointer & forward_iterator is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__algo-make-vct-iterator first1
								 (__find_first_of-imp-2 (opr::vec-ptr-index  first1)
														(opr::vec-ptr-index   last1)
														(opr::vec-ptr-buffer first1)
														first2 last2 (functor_function (clone eql-bf))))))

  ;;PTN; find_first_of : 3 -  f  x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__find_first_of-imp-3 (first1 last1 cons2 end2 eql-bf)
			 (declare (type cl:list cons2 end2))
			 (declare (type cl:function eql-bf))
			 (labels ((imp (val)
						(let ((cns2 cons2))
						  (declare (type cl:list cns2))
						  (for (nil (not (eq cns2 end2)) (setf cns2 (cdr cns2)))
							(when (funcall eql-bf val (car cns2))
							  (return-from imp t))))))
			   (with-operators
				   (if (or (_== first1 last1) (eq cons2 end2))
					   @~last1
					   (for (((itr1 @~first1)) (_/= itr1 last1) ++itr1 :returns itr1)
						 (when (imp *itr1)
						   (return-from __find_first_of-imp-3 itr1))))))))

	(defmethod-overload find_first_of ((first1 #+cl-stl-0x98 forward_iterator
											   #-cl-stl-0x98 input_iterator)
									   (last1  #+cl-stl-0x98 forward_iterator
											   #-cl-stl-0x98 input_iterator)
									   (first2 cons_const_iterator) (last2 cons_const_iterator))
	  ;;(format t "specialized find_first_of for ~A & cons_const_iterator is invoked.~%" #+cl-stl-0x98 forward_iterator #-cl-stl-0x98 input_iterator))
	  (__find_first_of-imp-3 first1 last1
							 (__cons-itr-cons first2)
							 (__cons-itr-cons  last2) #'operator_==))

	(defmethod-overload find_first_of ((first1 #+cl-stl-0x98 forward_iterator
											   #-cl-stl-0x98 input_iterator)
									   (last1  #+cl-stl-0x98 forward_iterator
											   #-cl-stl-0x98 input_iterator)
									   (first2 cons_const_iterator) (last2 cons_const_iterator) eql-bf)
	  ;;(format t "specialized find_first_of for ~A & cons_const_iterator is invoked.~%" #+cl-stl-0x98 forward_iterator #-cl-stl-0x98 input_iterator))
	  (__find_first_of-imp-3 first1 last1
							 (__cons-itr-cons first2)
							 (__cons-itr-cons  last2) (functor_function (clone eql-bf)))))


  ;;PTN; find_first_of : 4 - cci x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__find_first_of-imp-4 (cons1 end1 cons2 end2 eql-bf)
			 (declare (type cl:list cons1 end1 cons2 end2))
			 (declare (type cl:function eql-bf))
			 (labels ((imp (val)
						(let ((cns2 cons2))
						  (declare (type cl:list cns2))
						  (for (nil (not (eq cns2 end2)) (setf cns2 (cdr cns2)))
							(when (funcall eql-bf val (car cns2))
							  (return-from imp t))))))
			   (if (or (eq cons1 end1)
					   (eq cons2 end2))
				   end1
				   (let ((cns1 cons1))
					 (declare (type cl:list cns1))
					 (for (nil (not (eq cns1 end1)) (setf cns1 (cdr cns1)) :returns cns1)
					   (when (imp (car cns1))
						 (return-from __find_first_of-imp-4 cns1))))))))

	(defmethod-overload find_first_of ((first1 cons_const_iterator) (last1 cons_const_iterator)
									   (first2 cons_const_iterator) (last2 cons_const_iterator))
	  ;;(format t "specialized find_first_of for cons_const_iterator & cons_const_iterator is invoked.~%")
	  (__algo-make-cns-iterator first1
								 (__find_first_of-imp-4 (__cons-itr-cons first1)
														(__cons-itr-cons  last1)
														(__cons-itr-cons first2)
														(__cons-itr-cons  last2) #'operator_==)))

	(defmethod-overload find_first_of ((first1 cons_const_iterator) (last1 cons_const_iterator)
									   (first2 cons_const_iterator) (last2 cons_const_iterator) eql-bf)
	  ;;(format t "specialized find_first_of for cons_const_iterator & cons_const_iterator is invoked.~%")
	  (__algo-make-cns-iterator first1
								 (__find_first_of-imp-4 (__cons-itr-cons first1)
														(__cons-itr-cons  last1)
														(__cons-itr-cons first2)
														(__cons-itr-cons  last2) (functor_function (clone eql-bf))))))

  ;;PTN; find_first_of : 5 - cvp x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__find_first_of-imp-5 (begin1 end1 buffer1 cons2 end2 eql-bf)
			 (declare (type fixnum begin1 end1))
			 (declare (type cl:vector buffer1))
			 (declare (type cl:list cons2 end2))
			 (declare (type cl:function eql-bf))
			 (labels ((imp (val)
						(let ((cns2 cons2))
						  (declare (type cl:list cns2))
						  (for (nil (not (eq cns2 end2)) (setf cns2 (cdr cns2)))
							(when (funcall eql-bf val (car cns2))
							  (return-from imp t))))))
			   (if (or (= begin1 end1)
					   (eq cons2 end2))
				   end1
				   (let ((idx1 begin1))
					 (declare (type fixnum idx1))
					 (for (nil (< idx1 end1) (incf idx1) :returns idx1)
					   (when (imp (aref buffer1 idx1))
						 (return-from __find_first_of-imp-5 idx1))))))))

	(defmethod-overload find_first_of ((first1 const-vector-pointer) (last1 const-vector-pointer)
									   (first2  cons_const_iterator) (last2  cons_const_iterator))
	  ;;(format t "specialized find_first_of for const-vector-pointer & cons_const_iterator is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__algo-make-vct-iterator first1
								 (__find_first_of-imp-5 (opr::vec-ptr-index  first1)
														(opr::vec-ptr-index  last1)
														(opr::vec-ptr-buffer first1)
														(__cons-itr-cons  first2)
														(__cons-itr-cons  last2) #'operator_==)))

	(defmethod-overload find_first_of ((first1 const-vector-pointer) (last1 const-vector-pointer)
									   (first2 cons_const_iterator) (last2 cons_const_iterator) eql-bf)
	  ;;(format t "specialized find_first_of for const-vector-pointer & cons_const_iterator is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__algo-make-vct-iterator first1
								 (__find_first_of-imp-5 (opr::vec-ptr-index  first1)
														(opr::vec-ptr-index  last1)
														(opr::vec-ptr-buffer first1)
														(__cons-itr-cons  first2)
														(__cons-itr-cons  last2) (functor_function (clone eql-bf))))))

  ;;PTN; find_first_of : 6 -  f  x cvp
  (labels ((__find_first_of-imp-6 (first1 last1 begin2 end2 buffer2 eql-bf)
			 (declare (type fixnum begin2 end2))
			 (declare (type cl:vector buffer2))
			 (declare (type cl:function eql-bf))
			 (labels ((imp (val)
						(let ((idx2 begin2))
						  (declare (type fixnum idx2))
						  (for (nil (< idx2 end2) (incf idx2))
							(when (funcall eql-bf val (aref buffer2 idx2))
							  (return-from imp t))))))
			   (with-operators
				   (if (or (_== first1 last1)
						   (= begin2 end2))
					   @~last1
					   (for (((itr1 @~first1)) (_/= itr1 last1) ++itr1 :returns itr1)
						 (when (imp *itr1)
						   (return-from __find_first_of-imp-6 itr1))))))))

	(defmethod-overload find_first_of ((first1 #+cl-stl-0x98 forward_iterator
											   #-cl-stl-0x98 input_iterator)
									   (last1  #+cl-stl-0x98 forward_iterator
											   #-cl-stl-0x98 input_iterator)
									   (first2 const-vector-pointer) (last2 const-vector-pointer))
	  ;;(format t "specialized find_first_of for ~A & const-vector-pointer is invoked.~%" #+cl-stl-0x98 forward_iterator #-cl-stl-0x98 input_iterator))
	  (__pointer-check-iterator-range first2 last2)
	  (__find_first_of-imp-6 first1 last1
							 (opr::vec-ptr-index  first2)
							 (opr::vec-ptr-index   last2)
							 (opr::vec-ptr-buffer first2) #'operator_==))

	(defmethod-overload find_first_of ((first1 #+cl-stl-0x98 forward_iterator
											   #-cl-stl-0x98 input_iterator)
									   (last1  #+cl-stl-0x98 forward_iterator
											   #-cl-stl-0x98 input_iterator)
									   (first2 const-vector-pointer) (last2 const-vector-pointer) eql-bf)
	  ;;(format t "specialized find_first_of for ~A & const-vector-pointer is invoked.~%" #+cl-stl-0x98 forward_iterator #-cl-stl-0x98 input_iterator))
	  (__pointer-check-iterator-range first2 last2)
	  (__find_first_of-imp-6 first1 last1
							 (opr::vec-ptr-index  first2)
							 (opr::vec-ptr-index   last2)
							 (opr::vec-ptr-buffer first2) (functor_function (clone eql-bf)))))

  ;;PTN; find_first_of : 7 - cci x cvp
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__find_first_of-imp-7 (cons1 end1 begin2 end2 buffer2 eql-bf)
			 (declare (type cl:list cons1 end1))
			 (declare (type fixnum begin2 end2))
			 (declare (type cl:vector buffer2))
			 (declare (type cl:function eql-bf))
			 (labels ((imp (val)
						(let ((idx2 begin2))
						  (declare (type fixnum idx2))
						  (for (nil (< idx2 end2) (incf idx2))
							(when (funcall eql-bf val (aref buffer2 idx2))
							  (return-from imp t))))))
			   (if (or (eq cons1 end1)
					   (= begin2 end2))
				   end1
				   (let ((cns1 cons1))
					 (declare (type cl:list cns1))
					 (for (nil (not (eq cns1 end1)) (setf cns1 (cdr cns1)) :returns cns1)
					   (when (imp (car cns1))
						 (return-from __find_first_of-imp-7 cns1))))))))

	(defmethod-overload find_first_of ((first1 cons_const_iterator) (last1 cons_const_iterator)
									   (first2 const-vector-pointer) (last2 const-vector-pointer))
	  ;;(format t "specialized find_first_of for cons_const_iterator & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first2 last2)
	  (__algo-make-cns-iterator first1
								 (__find_first_of-imp-7 (__cons-itr-cons first1)
														(__cons-itr-cons  last1)
														(opr::vec-ptr-index  first2)
														(opr::vec-ptr-index   last2)
														(opr::vec-ptr-buffer first2) #'operator_==)))

	(defmethod-overload find_first_of ((first1 cons_const_iterator) (last1 cons_const_iterator)
									   (first2 const-vector-pointer) (last2 const-vector-pointer) eql-bf)
	  ;;(format t "specialized find_first_of for cons_const_iterator & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first2 last2)
	  (__algo-make-cns-iterator first1
								 (__find_first_of-imp-7 (__cons-itr-cons first1)
														(__cons-itr-cons  last1)
														(opr::vec-ptr-index  first2)
														(opr::vec-ptr-index   last2)
														(opr::vec-ptr-buffer first2)
														(functor_function (clone eql-bf))))))

  
  ;;PTN; find_first_of : 8 - cvp x cvp
  (labels ((__find_first_of-imp-8 (begin1 end1 buffer1 begin2 end2 buffer2 eql-bf)
			 (declare (type fixnum begin1 end1 begin2 end2))
			 (declare (type cl:vector buffer1 buffer2))
			 (declare (type cl:function eql-bf))
			 (labels ((imp (val)
						(let ((idx2 begin2))
						  (declare (type fixnum idx2))
						  (for (nil (< idx2 end2) (incf idx2))
							(when (funcall eql-bf val (aref buffer2 idx2))
							  (return-from imp t))))))
			   (if (or (= begin1 end1)
					   (= begin2 end2))
				   end1
				   (let ((idx1 begin1))
					 (declare (type fixnum idx1))
					 (for (nil (< idx1 end1) (incf idx1) :returns idx1)
					   (when (imp (aref buffer1 idx1))
						 (return-from __find_first_of-imp-8 idx1))))))))

	(defmethod-overload find_first_of ((first1 const-vector-pointer) (last1 const-vector-pointer)
									   (first2 const-vector-pointer) (last2 const-vector-pointer))
	  ;;(format t "specialized find_first_of for const-vector-pointer & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__pointer-check-iterator-range first2 last2)
	  (__algo-make-vct-iterator first1
								 (__find_first_of-imp-8 (opr::vec-ptr-index  first1)
														(opr::vec-ptr-index  last1)
														(opr::vec-ptr-buffer first1)
														(opr::vec-ptr-index  first2)
														(opr::vec-ptr-index  last2)
														(opr::vec-ptr-buffer first2) #'operator_==)))

	(defmethod-overload find_first_of ((first1 const-vector-pointer) (last1 const-vector-pointer)
									   (first2 const-vector-pointer) (last2 const-vector-pointer) eql-bf)
	  ;;(format t "specialized find_first_of for const-vector-pointer & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__pointer-check-iterator-range first2 last2)
	  (__algo-make-vct-iterator first1
								 (__find_first_of-imp-8 (opr::vec-ptr-index  first1)
														(opr::vec-ptr-index  last1)
														(opr::vec-ptr-buffer first1)
														(opr::vec-ptr-index  first2)
														(opr::vec-ptr-index  last2)
														(opr::vec-ptr-buffer first2)
														(functor_function (clone eql-bf)))))))


; first   : forward_iterator
; last    : forward_iterator
; eql-bf  : binary_function ( default : #'operator_== )
; returns : iterator point to found element.
(locally (declare (optimize speed))

  ;;PTN; adjacent_find : 0 -  f
  (defmethod-overload adjacent_find ((first forward_iterator) (last forward_iterator))
	(__adjacent_find-imp-0 first last #'operator_==))
  
  (defmethod-overload adjacent_find ((first forward_iterator) (last forward_iterator) eql-bf)
	(__adjacent_find-imp-0 first last (functor_function (clone eql-bf))))

  ;;PTN; adjacent_find : 1 - cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload adjacent_find ((first cons_const_iterator) (last cons_const_iterator))
	;;(format t "specialized adjacent_find for cons_const_iterator is invoked.~%")
	(__algo-make-cns-iterator first
							   (__adjacent_find-imp-1 (__cons-itr-cons first)
													  (__cons-itr-cons  last) #'operator_==)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload adjacent_find ((first cons_const_iterator)
									 (last  cons_const_iterator) eql-bf)
	;;(format t "specialized adjacent_find for cons_const_iterator is invoked.~%")
	(__algo-make-cns-iterator first
							   (__adjacent_find-imp-1 (__cons-itr-cons first)
													  (__cons-itr-cons  last)
													  (functor_function (clone eql-bf)))))

  ;;PTN; adjacent_find : 2 - cvp
  (defmethod-overload adjacent_find ((first const-vector-pointer) (last const-vector-pointer))
	;;(format t "specialized adjacent_find for const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(__algo-make-vct-iterator first
							   (__adjacent_find-imp-2 (opr::vec-ptr-index  first)
													  (opr::vec-ptr-index   last)
													  (opr::vec-ptr-buffer first) #'operator_==)))

  (defmethod-overload adjacent_find ((first const-vector-pointer)
									 (last  const-vector-pointer) eql-bf)
	;;(format t "specialized adjacent_find for const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(__algo-make-vct-iterator first
							   (__adjacent_find-imp-2 (opr::vec-ptr-index  first)
													  (opr::vec-ptr-index   last)
													  (opr::vec-ptr-buffer first)
													  (functor_function (clone eql-bf))))))



; first   : input_iterator
; last    : input_iterator
; eql-bf  : binary_function ( default : #'operator_== )
; returns : fixnum
(locally (declare (optimize speed))

  ;;PTN; count : 0 -  i
  (labels ((__count-imp-0 (first last val eql-bf)
			 (declare (type cl:function eql-bf))
			 (if (_== first last)
				 0
				 (let ((cnt 0))
				   (declare (type fixnum cnt))
				   (with-operators
					   (for (((itr @~first)) (_/= itr last) ++itr :returns cnt)
						 (when (funcall eql-bf val *itr)
						   (incf cnt))))))))
	
	(defmethod-overload count ((first input_iterator) (last input_iterator) val)
	  (__count-imp-0 first last val #'operator_==))

	#-cl-stl-noextra
	(defmethod-overload count ((first input_iterator) (last input_iterator) val eql-bf)
	  (__count-imp-0 first last val (functor_function (clone eql-bf)))))


  ;;PTN; count : 1 - cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__count-imp-1 (cons1 cons2 val eql-bf)
			 (declare (type cl:function eql-bf))
			 (let ((cnt 0))
			   (declare (type fixnum cnt))
			   (for (nil (not (eq cons1 cons2)) (setf cons1 (cdr cons1)) :returns cnt)
				 (when (funcall eql-bf val (car cons1))
				   (incf cnt))))))

	(defmethod-overload count ((first cons_const_iterator)
							   (last  cons_const_iterator) val)
	  ;;(format t "specialized count for cons_const_iterator is invoked.~%")
	  (__count-imp-1 (__cons-itr-cons first)
					 (__cons-itr-cons  last) val #'operator_==))

	#-cl-stl-noextra
	(defmethod-overload count ((first cons_const_iterator)
							   (last  cons_const_iterator) val eql-bf)
	  ;;(format t "specialized count for cons_const_iterator is invoked.~%")
	  (__count-imp-1 (__cons-itr-cons first)
					 (__cons-itr-cons  last) val (functor_function (clone eql-bf)))))


  ;;PTN; count : 2 - cvp
  (labels ((__count-imp-2 (idx1 idx2 buffer val eql-bf)
			 (declare (type fixnum idx1 idx2))
			 (declare (type cl:vector buffer))
			 (declare (type cl:function eql-bf))
			 (let ((cnt 0))
			   (declare (type fixnum cnt))
			   (for (nil (< idx1 idx2) (incf idx1) :returns cnt)
				 (when (funcall eql-bf val (aref buffer idx1))
				   (incf cnt))))))

	(defmethod-overload count ((first const-vector-pointer)
							   (last  const-vector-pointer) val)
	  ;;(format t "specialized count for const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__count-imp-2 (opr::vec-ptr-index  first)
					 (opr::vec-ptr-index  last)
					 (opr::vec-ptr-buffer first) val #'operator_==))

	#-cl-stl-noextra
	(defmethod-overload count ((first const-vector-pointer)
							   (last  const-vector-pointer) val eql-bf)
	  ;;(format t "specialized count for const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__count-imp-2 (opr::vec-ptr-index  first)
					 (opr::vec-ptr-index  last)
					 (opr::vec-ptr-buffer first) val (functor_function (clone eql-bf))))))



; first   : input_iterator
; last    : input_iterator
; pred    : unary_function
; returns : fixnum
(locally (declare (optimize speed))

  ;;PTN; count_if : 0 -  i
  (defmethod count_if ((first input_iterator) (last input_iterator) pred)
	(if (_== first last)
		0
		(__count_if-imp-0 first last (functor_function (clone pred)))))

  ;;PTN; count_if : 1 - cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod count_if ((first cons_const_iterator) (last cons_const_iterator) pred)
	;;(format t "specialized count_if for cons_const_iterator is invoked.~%")
	(let ((cons1 (__cons-itr-cons first))
		  (cons2 (__cons-itr-cons  last)))
	  (if (eq cons1 cons2)
		  0
		  (__count_if-imp-1 cons1 cons2 (functor_function (clone pred))))))

  ;;PTN; count_if : 2 - cvp
  (defmethod count_if ((first const-vector-pointer) (last const-vector-pointer) pred)
	;;(format t "specialized count_if for const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index first))
		  (idx2 (opr::vec-ptr-index  last)))
	  (declare (type fixnum idx1 idx2))
	  (if (= idx1 idx2)
		  0
		  (__count_if-imp-2 idx1 idx2 (opr::vec-ptr-buffer first) (functor_function (clone pred)))))))


; first1  : forward_iterator
; last1   : forward_iterator
; first2  : forward_iterator
; eql-bf  : binary_function ( default : #'operator_== )
; returns : pair of iterators.
(locally (declare (optimize speed))

  ;;PTN; mismatch : 0 -  f  x  f 
  (labels ((__mismatch-imp-0 (first1 last1 first2 eql-bf)
			 (declare (type cl:function eql-bf))
			 (with-operators
				 (for (((itr1 @~first1) (itr2 @~first2))
					   (and (_/= itr1 last1) (funcall eql-bf *itr1 *itr2))
					   (progn ++itr1 ++itr2)
					   :returns (make_pair itr1 itr2))))))

	(defmethod-overload mismatch ((first1 forward_iterator)
								  (last1  forward_iterator) (first2 forward_iterator))
	  (__mismatch-imp-0 first1 last1 first2 #'operator_==))

	(defmethod-overload mismatch ((first1 forward_iterator)
								  (last1  forward_iterator) (first2 forward_iterator) eql-bf)
	  (__mismatch-imp-0 first1 last1 first2 (functor_function (clone eql-bf)))))

  ;;PTN; mismatch : 1 -  f  x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__mismatch-imp-1 (first1 last1 cns2 eql-bf)
			 (declare (type cl:list cns2))
			 (declare (type cl:function eql-bf))
			 (with-operators
				 (for (((itr1 @~first1)) (and (_/= itr1 last1)
											  (funcall eql-bf *itr1 (car cns2))) nil :returns (values itr1 cns2))
				   ++itr1
				   (setf cns2 (cdr cns2))))))

	(defmethod-overload mismatch ((first1 forward_iterator)
								  (last1  forward_iterator) (first2 cons_const_iterator))
	  ;;(format t "specialized mismatch for forward_iterator & cons_const_iterator is invoked.~%")
	  (multiple-value-bind (itr1 cns2) (__mismatch-imp-1 first1 last1
														 (__cons-itr-cons first2) #'operator_==)
		(make_pair itr1 (__algo-make-cns-iterator first2 cns2))))

	(defmethod-overload mismatch ((first1 forward_iterator)
								  (last1  forward_iterator) (first2 cons_const_iterator) eql-bf)
	  ;;(format t "specialized mismatch for forward_iterator & cons_const_iterator is invoked.~%")
	  (multiple-value-bind (itr1 cns2) (__mismatch-imp-1 first1 last1
														 (__cons-itr-cons first2) (functor_function (clone eql-bf)))
		(make_pair itr1 (__algo-make-cns-iterator first2 cns2)))))

  ;;PTN; mismatch : 2 -  f  x cvp
  (labels ((__mismatch-imp-2 (first1 last1 idx2 buffer2 eql-bf)
			 (declare (type fixnum idx2))
			 (declare (type cl:vector buffer2))
			 (declare (type cl:function eql-bf))
			 (with-operators
				 (for (((itr1 @~first1)) (and (_/= itr1 last1)
											  (funcall eql-bf *itr1 (aref buffer2 idx2))) nil :returns (values itr1 idx2))
				   ++itr1
				   (incf idx2)))))

	(defmethod-overload mismatch ((first1 forward_iterator)
								  (last1  forward_iterator) (first2 const-vector-pointer))
	  ;;(format t "specialized mismatch for forward_iterator & const-vector-pointer is invoked.~%")
	  (multiple-value-bind (itr1 idx2) (__mismatch-imp-2 first1 last1
														 (opr::vec-ptr-index  first2)
														 (opr::vec-ptr-buffer first2) #'operator_==)
		(make_pair itr1 (__algo-make-vct-iterator first2 idx2))))

	(defmethod-overload mismatch ((first1 forward_iterator)
								  (last1  forward_iterator) (first2 const-vector-pointer) eql-bf)
	  ;;(format t "specialized mismatch for forward_iterator & const-vector-pointer is invoked.~%")
	  (multiple-value-bind (itr1 idx2) (__mismatch-imp-2 first1 last1
														 (opr::vec-ptr-index  first2)
														 (opr::vec-ptr-buffer first2) (functor_function (clone eql-bf)))
		(make_pair itr1 (__algo-make-vct-iterator first2 idx2)))))
  

  ;;PTN; mismatch : 3 - cci x  f
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__mismatch-imp-3 (cns1 last1 first2 eql-bf)
			 (declare (type cl:list cns1 last1))
			 (declare (type cl:function eql-bf))
			 (with-operators
				 (for (((itr2 @~first2)) (and (not (eq cns1 last1))
											  (funcall eql-bf (car cns1) *itr2)) nil :returns (values cns1 itr2))
				   (setf cns1 (cdr cns1))
				   ++itr2))))

	(defmethod-overload mismatch ((first1 cons_const_iterator)
								  (last1  cons_const_iterator) (first2 forward_iterator))
	  ;;(format t "specialized mismatch for cons_const_iterator & forward_iterator is invoked.~%")
	  (multiple-value-bind (cns1 itr2) (__mismatch-imp-3 (__cons-itr-cons first1)
														 (__cons-itr-cons  last1) first2 #'operator_==)
		(make_pair (__algo-make-cns-iterator first1 cns1) itr2)))

	(defmethod-overload mismatch ((first1 cons_const_iterator)
								  (last1  cons_const_iterator) (first2 forward_iterator) eql-bf)
	  ;;(format t "specialized mismatch for cons_const_iterator & forward_iterator is invoked.~%")
	  (multiple-value-bind (cns1 itr2) (__mismatch-imp-3 (__cons-itr-cons first1)
														 (__cons-itr-cons  last1) first2 (functor_function (clone eql-bf)))
		(make_pair (__algo-make-cns-iterator first1 cns1) itr2))))

  ;;PTN; mismatch : 4 - cci x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__mismatch-imp-4 (cns1 last1 cns2 eql-bf)
			 (declare (type cl:list cns1 last1 cns2))
			 (declare (type cl:function eql-bf))
			 (for (nil (and (not (eq cns1 last1))
							(funcall eql-bf (car cns1)
											(car cns2))) nil :returns (values cns1 cns2))
			   (setf cns1 (cdr cns1))
			   (setf cns2 (cdr cns2)))))

	(defmethod-overload mismatch ((first1 cons_const_iterator)
								  (last1  cons_const_iterator) (first2 cons_const_iterator))
	  ;;(format t "specialized mismatch for cons_const_iterator & cons_const_iterator is invoked.~%")
	  (multiple-value-bind (cns1 cns2) (__mismatch-imp-4 (__cons-itr-cons first1)
														 (__cons-itr-cons  last1)
														 (__cons-itr-cons first2) #'operator_==)
		(make_pair (__algo-make-cns-iterator first1 cns1)
				   (__algo-make-cns-iterator first2 cns2))))

	(defmethod-overload mismatch ((first1 cons_const_iterator)
								  (last1  cons_const_iterator) (first2 cons_const_iterator) eql-bf)
	  ;;(format t "specialized mismatch for cons_const_iterator & cons_const_iterator is invoked.~%")
	  (multiple-value-bind (cns1 cns2) (__mismatch-imp-4 (__cons-itr-cons first1)
														 (__cons-itr-cons  last1)
														 (__cons-itr-cons first2) (functor_function (clone eql-bf)))
		(make_pair (__algo-make-cns-iterator first1 cns1)
				   (__algo-make-cns-iterator first2 cns2)))))

  ;;PTN; mismatch : 5 - cci x cvp
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__mismatch-imp-5 (cns1 last1 idx2 buf2 eql-bf)
			 (declare (type cl:list cns1 last1))
			 (declare (type fixnum idx2))
			 (declare (type cl:vector buf2))
			 (declare (type cl:function eql-bf))
			 (for (nil (and (not (eq cns1 last1))
							(funcall eql-bf (car       cns1)
											(aref buf2 idx2))) nil :returns (values cns1 idx2))
			   (setf cns1 (cdr cns1))
			   (incf idx2))))

	(defmethod-overload mismatch ((first1 cons_const_iterator)
								  (last1  cons_const_iterator) (first2 const-vector-pointer))
	  ;;(format t "specialized mismatch for cons_const_iterator & const-vector-pointer is invoked.~%")
	  (multiple-value-bind (cns1 idx2) (__mismatch-imp-5 (__cons-itr-cons first1)
														 (__cons-itr-cons  last1)
														 (opr::vec-ptr-index  first2)
														 (opr::vec-ptr-buffer first2) #'operator_==)
		(make_pair (__algo-make-cns-iterator first1 cns1)
				   (__algo-make-vct-iterator first2 idx2))))

	(defmethod-overload mismatch ((first1 cons_const_iterator)
								  (last1  cons_const_iterator) (first2 const-vector-pointer) eql-bf)
	  ;;(format t "specialized mismatch for cons_const_iterator & const-vector-pointer is invoked.~%")
	  (multiple-value-bind (cns1 idx2) (__mismatch-imp-5 (__cons-itr-cons first1)
														 (__cons-itr-cons  last1)
														 (opr::vec-ptr-index  first2)
														 (opr::vec-ptr-buffer first2) (functor_function (clone eql-bf)))
		(make_pair (__algo-make-cns-iterator first1 cns1)
				   (__algo-make-vct-iterator first2 idx2)))))

  ;;PTN; mismatch : 6 - cvp x  f 
  (labels ((__mismatch-imp-6 (idx1 last1 buffer1 first2 eql-bf)
			 (declare (type fixnum idx1 last1))
			 (declare (type cl:vector buffer1))
			 (declare (type cl:function eql-bf))
			 (with-operators
				 (for (((itr2 @~first2)) (and (< idx1 last1)
											  (funcall eql-bf (aref buffer1 idx1) *itr2)) nil :returns (values idx1 itr2))
				   (incf idx1)
				   ++itr2))))

	(defmethod-overload mismatch ((first1 const-vector-pointer)
								  (last1  const-vector-pointer) (first2 forward_iterator))
	  ;;(format t "specialized mismatch for const-vector-pointer & forward_iterator is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (multiple-value-bind (idx1 itr2) (__mismatch-imp-6 (opr::vec-ptr-index  first1)
														 (opr::vec-ptr-index  last1)
														 (opr::vec-ptr-buffer first1) first2 #'operator_==)
		(make_pair (__algo-make-vct-iterator first1 idx1) itr2)))

	(defmethod-overload mismatch ((first1 const-vector-pointer)
								  (last1  const-vector-pointer) (first2 forward_iterator) eql-bf)
	  ;;(format t "specialized mismatch for const-vector-pointer & forward_iterator is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (multiple-value-bind (idx1 itr2) (__mismatch-imp-6 (opr::vec-ptr-index  first1)
														 (opr::vec-ptr-index  last1)
														 (opr::vec-ptr-buffer first1)
														 first2 (functor_function (clone eql-bf)))
		(make_pair (__algo-make-vct-iterator first1 idx1) itr2))))

  ;;PTN; mismatch : 7 - cvp x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__mismatch-imp-7 (idx1 last1 buf1 cns2 eql-bf)
			 (declare (type fixnum idx1 last1))
			 (declare (type cl:vector buf1))
			 (declare (type cl:list cns2))
			 (declare (type cl:function eql-bf))
			 (for (nil (and (< idx1 last1)
							(funcall eql-bf (aref buf1 idx1)
											(car       cns2))) nil :returns (values idx1 cns2))
			   (incf idx1)
			   (setf cns2 (cdr cns2)))))

	(defmethod-overload mismatch ((first1 const-vector-pointer)
								  (last1  const-vector-pointer) (first2 cons_const_iterator))
	  ;;(format t "specialized mismatch for const-vector-pointer & cons_const_iterator is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (multiple-value-bind (idx1 cns2) (__mismatch-imp-7 (opr::vec-ptr-index  first1)
														 (opr::vec-ptr-index  last1)
														 (opr::vec-ptr-buffer first1)
														 (__cons-itr-cons     first2) #'operator_==)
		(make_pair (__algo-make-vct-iterator first1 idx1)
				   (__algo-make-cns-iterator first2 cns2))))

	(defmethod-overload mismatch ((first1 const-vector-pointer)
								  (last1  const-vector-pointer) (first2 cons_const_iterator) eql-bf)
	  ;;(format t "specialized mismatch for const-vector-pointer & cons_const_iterator is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (multiple-value-bind (idx1 cns2) (__mismatch-imp-7 (opr::vec-ptr-index  first1)
														 (opr::vec-ptr-index  last1)
														 (opr::vec-ptr-buffer first1)
														 (__cons-itr-cons     first2) (functor_function (clone eql-bf)))
		(make_pair (__algo-make-vct-iterator first1 idx1)
				   (__algo-make-cns-iterator first2 cns2)))))

  ;;PTN; mismatch : 8 - cvp x cvp
  (labels ((__mismatch-imp-8 (idx1 last1 buf1 idx2 buf2 eql-bf)
			 (declare (type fixnum idx1 last1 idx2))
			 (declare (type cl:vector buf1 buf2))
			 (declare (type cl:function eql-bf))
			 (for (nil (and (< idx1 last1)
							(funcall eql-bf (aref buf1 idx1)
											(aref buf2 idx2))) nil :returns (values idx1 idx2))
			   (incf idx1)
			   (incf idx2))))

	(defmethod-overload mismatch ((first1 const-vector-pointer)
								  (last1  const-vector-pointer) (first2 const-vector-pointer))
	  ;;(format t "specialized mismatch for const-vector-pointer & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (multiple-value-bind (idx1 idx2) (__mismatch-imp-8 (opr::vec-ptr-index  first1)
														 (opr::vec-ptr-index  last1)
														 (opr::vec-ptr-buffer first1)
														 (opr::vec-ptr-index  first2)
														 (opr::vec-ptr-buffer first2) #'operator_==)
		(make_pair (__algo-make-vct-iterator first1 idx1)
				   (__algo-make-vct-iterator first2 idx2))))

	(defmethod-overload mismatch ((first1 const-vector-pointer)
								  (last1  const-vector-pointer) (first2 const-vector-pointer) eql-bf)
	  ;;(format t "specialized mismatch for const-vector-pointer & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (multiple-value-bind (idx1 idx2) (__mismatch-imp-8 (opr::vec-ptr-index  first1)
														 (opr::vec-ptr-index  last1)
														 (opr::vec-ptr-buffer first1)
														 (opr::vec-ptr-index  first2)
														 (opr::vec-ptr-buffer first2) (functor_function (clone eql-bf)))
		(make_pair (__algo-make-vct-iterator first1 idx1)
				   (__algo-make-vct-iterator first2 idx2))))))



; first1  : forward_iterator
; last1   : forward_iterator
; first2  : forward_iterator
; last2   : forward_iterator
; eql-bf  : binary_function ( default : #'operator_== )
; returns : pair of iterators.
#-(or cl-stl-0x98 cl-stl-0x11)
(locally (declare (optimize speed))

  ;;PTN; mismatch(0x14) : 0 -  f  x  f 
  (labels ((__mismatch-imp-0 (first1 last1 first2 last2 eql-bf)
			 (declare (type cl:function eql-bf))
			 (with-operators
				 (for (((itr1 @~first1) (itr2 @~first2))
					   (and (_/= itr1 last1) (_/= itr2 last2) (funcall eql-bf *itr1 *itr2))
					   (progn ++itr1 ++itr2)
					   :returns (make_pair itr1 itr2))))))

	(defmethod-overload mismatch ((first1 forward_iterator) (last1 forward_iterator)
								  (first2 forward_iterator) (last2 forward_iterator))
	  (__mismatch-imp-0 first1 last1 first2 last2 #'operator_==))

	(defmethod-overload mismatch ((first1 forward_iterator) (last1 forward_iterator)
								  (first2 forward_iterator) (last2 forward_iterator) eql-bf)
	  (__mismatch-imp-0 first1 last1 first2 last2 (functor_function (clone eql-bf)))))


  ;;PTN; mismatch(0x14) : 1 -  f  x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__mismatch-imp-2 (first1 last1 cons2 last2 eql-bf)
			 (declare (type cl:list cons2 last2))
			 (declare (type cl:function eql-bf))
			 (with-operators
				 (for (((itr1 @~first1)) (and (_/= itr1 last1)
											  (not (eq cons2 last2))
											  (funcall eql-bf *itr1 (car cons2))) nil :returns (values itr1 cons2))
				   ++itr1
				   (setf cons2 (cdr cons2))))))

	(defmethod-overload mismatch ((first1 forward_iterator) (last1 forward_iterator)
								  (first2 cons_const_iterator) (last2 cons_const_iterator))
	  ;;(format t "specialized mismatch for forward_iterator & cons_const_iterator is invoked.~%")
	  (multiple-value-bind (itr1 cns2) (__mismatch-imp-2 first1 last1
														 (__cons-itr-cons first2)
														 (__cons-itr-cons  last2) #'operator_==)
		(make_pair itr1 (__algo-make-cns-iterator first2 cns2))))

	(defmethod-overload mismatch ((first1 forward_iterator) (last1 forward_iterator)
								  (first2 cons_const_iterator) (last2 cons_const_iterator) eql-bf)
	  ;;(format t "specialized mismatch for forward_iterator & cons_const_iterator is invoked.~%")
	  (multiple-value-bind (itr1 cns2) (__mismatch-imp-2 first1 last1
														 (__cons-itr-cons first2)
														 (__cons-itr-cons  last2) (functor_function (clone eql-bf)))
		(make_pair itr1 (__algo-make-cns-iterator first2 cns2)))))


  ;;PTN; mismatch(0x14) : 2 -  f  x cvp
  (labels ((__mismatch-imp-2 (first1 last1 idx2 last2 buffer2 eql-bf)
			 (declare (type fixnum idx2 last2))
			 (declare (type cl:vector buffer2))
			 (declare (type cl:function eql-bf))
			 (with-operators
				 (for (((itr1 @~first1)) (and (_/= itr1 last1)
											  (< idx2 last2)
											  (funcall eql-bf *itr1 (aref buffer2 idx2))) nil :returns (values itr1 idx2))
				   ++itr1
				   (incf idx2)))))

	(defmethod-overload mismatch ((first1 forward_iterator) (last1 forward_iterator)
								  (first2 const-vector-pointer) (last2 const-vector-pointer))
	  ;;(format t "specialized mismatch for forward_iterator & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first2 last2)
	  (multiple-value-bind (itr1 idx2) (__mismatch-imp-2 first1 last1
														 (opr::vec-ptr-index  first2)
														 (opr::vec-ptr-index  last2)
														 (opr::vec-ptr-buffer first2) #'operator_==)
		(make_pair itr1 (__algo-make-vct-iterator first2 idx2))))

	(defmethod-overload mismatch ((first1 forward_iterator) (last1 forward_iterator)
								  (first2 const-vector-pointer) (last2 const-vector-pointer) eql-bf)
	  ;;(format t "specialized mismatch for forward_iterator & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first2 last2)
	  (multiple-value-bind (itr1 idx2) (__mismatch-imp-2 first1 last1
														 (opr::vec-ptr-index  first2)
														 (opr::vec-ptr-index  last2)
														 (opr::vec-ptr-buffer first2) (functor_function (clone eql-bf)))
		(make_pair itr1 (__algo-make-vct-iterator first2 idx2)))))

  ;;PTN; mismatch(0x14) : 3 - cci x  f 
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__mismatch-imp-3 (cns1 last1 first2 last2 eql-bf)
			 (declare (type cl:list cns1 last1))
			 (declare (type cl:function eql-bf))
			 (with-operators
				 (for (((itr2 @~first2)) (and (not (eq cns1 last1))
											  (_/= itr2 last2)
											  (funcall eql-bf (car cns1) *itr2)) nil :returns (values cns1 itr2))
				   (setf cns1 (cdr cns1))
				   ++itr2))))

	(defmethod-overload mismatch ((first1 cons_const_iterator) (last1 cons_const_iterator)
								  (first2 forward_iterator) (last2 forward_iterator))
	  ;;(format t "specialized mismatch for cons_const_iterator & forward_iterator is invoked.~%")
	  (multiple-value-bind (cns1 itr2) (__mismatch-imp-3 (__cons-itr-cons first1)
														 (__cons-itr-cons  last1) first2 last2 #'operator_==)
		(make_pair (__algo-make-cns-iterator first1 cns1) itr2)))

	(defmethod-overload mismatch ((first1 cons_const_iterator) (last1 cons_const_iterator)
								  (first2 forward_iterator) (last2 forward_iterator) eql-bf)
	  ;;(format t "specialized mismatch for cons_const_iterator & forward_iterator is invoked.~%")
	  (multiple-value-bind (cns1 itr2) (__mismatch-imp-3 (__cons-itr-cons first1)
														 (__cons-itr-cons  last1)
														 first2 last2 (functor_function (clone eql-bf)))
		(make_pair (__algo-make-cns-iterator first1 cns1) itr2))))

  ;;PTN; mismatch(0x14) : 4 - cci x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__mismatch-imp-4 (cns1 last1 cns2 last2 eql-bf)
			 (declare (type cl:list cns1 last1 cns2 last2))
			 (declare (type cl:function eql-bf))
			 (for (nil (and (not (eq cns1 last1))
							(not (eq cns2 last2))
							(funcall eql-bf (car cns1)
											(car cns2))) nil :returns (values cns1 cns2))
			   (setf cns1 (cdr cns1))
			   (setf cns2 (cdr cns2)))))

	(defmethod-overload mismatch ((first1 cons_const_iterator) (last1 cons_const_iterator)
								  (first2 cons_const_iterator) (last2 cons_const_iterator))
	  ;;(format t "specialized mismatch for cons_const_iterator & cons_const_iterator is invoked.~%")
	  (multiple-value-bind (cns1 cns2) (__mismatch-imp-4 (__cons-itr-cons first1)
														 (__cons-itr-cons  last1)
														 (__cons-itr-cons first2)
														 (__cons-itr-cons  last2) #'operator_==)
		(make_pair (__algo-make-cns-iterator first1 cns1)
				   (__algo-make-cns-iterator first2 cns2))))

	(defmethod-overload mismatch ((first1 cons_const_iterator) (last1 cons_const_iterator)
								  (first2 cons_const_iterator) (last2 cons_const_iterator) eql-bf)
	  ;;(format t "specialized mismatch for cons_const_iterator & cons_const_iterator is invoked.~%")
	  (multiple-value-bind (cns1 cns2) (__mismatch-imp-4 (__cons-itr-cons first1)
														 (__cons-itr-cons  last1)
														 (__cons-itr-cons first2)
														 (__cons-itr-cons  last2) (functor_function (clone eql-bf)))
		(make_pair (__algo-make-cns-iterator first1 cns1)
				   (__algo-make-cns-iterator first2 cns2)))))

  ;;PTN; mismatch(0x14) : 5 - cci x cvp
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__mismatch-imp-5 (cns1 last1 idx2 last2 buf2 eql-bf)
			 (declare (type cl:list cns1 last1))
			 (declare (type fixnum idx2 last2))
			 (declare (type cl:vector buf2))
			 (declare (type cl:function eql-bf))
			 (for (nil (and (not (eq cns1 last1))
							(< idx2 last2)
							(funcall eql-bf (car       cns1)
											(aref buf2 idx2))) nil :returns (values cns1 idx2))
			   (setf cns1 (cdr cns1))
			   (incf idx2))))

	(defmethod-overload mismatch ((first1  cons_const_iterator) (last1  cons_const_iterator)
								  (first2 const-vector-pointer) (last2 const-vector-pointer))
	  ;;(format t "specialized mismatch for cons_const_iterator & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first2 last2)
	  (multiple-value-bind (cns1 idx2) (__mismatch-imp-5 (__cons-itr-cons first1)
														 (__cons-itr-cons  last1)
														 (opr::vec-ptr-index  first2)
														 (opr::vec-ptr-index   last2)
														 (opr::vec-ptr-buffer first2) #'operator_==)
		(make_pair (__algo-make-cns-iterator first1 cns1)
				   (__algo-make-vct-iterator first2 idx2))))

	(defmethod-overload mismatch ((first1  cons_const_iterator) (last1  cons_const_iterator)
								  (first2 const-vector-pointer) (last2 const-vector-pointer) eql-bf)
	  ;;(format t "specialized mismatch for cons_const_iterator & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first2 last2)
	  (multiple-value-bind (cns1 idx2) (__mismatch-imp-5 (__cons-itr-cons first1)
														 (__cons-itr-cons  last1)
														 (opr::vec-ptr-index  first2)
														 (opr::vec-ptr-index   last2)
														 (opr::vec-ptr-buffer first2) (functor_function (clone eql-bf)))
		(make_pair (__algo-make-cns-iterator first1 cns1)
				   (__algo-make-vct-iterator first2 idx2)))))

  ;;PTN; mismatch(0x14) : 6 - cvp x  f 
  (labels ((__mismatch-imp-6 (idx1 last1 buffer1 first2 last2 eql-bf)
			 (declare (type fixnum idx1 last1))
			 (declare (type cl:vector buffer1))
			 (declare (type cl:function eql-bf))
			 (with-operators
				 (for (((itr2 @~first2)) (and (< idx1 last1)
											  (_/= itr2 last2)
											  (funcall eql-bf (aref buffer1 idx1) *itr2)) nil :returns (values idx1 itr2))
				   (incf idx1)
				   ++itr2))))

	(defmethod-overload mismatch ((first1 const-vector-pointer) (last1 const-vector-pointer)
								  (first2 forward_iterator) (last2 forward_iterator))
	  ;;(format t "specialized mismatch for const-vector-pointer & forward_iterator is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (multiple-value-bind (idx1 itr2) (__mismatch-imp-6 (opr::vec-ptr-index  first1)
														 (opr::vec-ptr-index  last1)
														 (opr::vec-ptr-buffer first1) first2 last2 #'operator_==)
		(make_pair (__algo-make-vct-iterator first1 idx1) itr2)))

	(defmethod-overload mismatch ((first1 const-vector-pointer) (last1 const-vector-pointer)
								  (first2 forward_iterator) (last2 forward_iterator) eql-bf)
	  ;;(format t "specialized mismatch for const-vector-pointer & forward_iterator is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (multiple-value-bind (idx1 itr2) (__mismatch-imp-6 (opr::vec-ptr-index  first1)
														 (opr::vec-ptr-index  last1)
														 (opr::vec-ptr-buffer first1)
														 first2 last2 (functor_function (clone eql-bf)))
		(make_pair (__algo-make-vct-iterator first1 idx1) itr2))))

  ;;PTN; mismatch(0x14) : 7 - cvp x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__mismatch-imp-7 (idx1 last1 buffer1 cns2 last2 eql-bf)
			 (declare (type fixnum  idx1 last1))
			 (declare (type cl:vector buffer1))
			 (declare (type cl:list cns2 last2))
			 (declare (type cl:function eql-bf))
			 (for (nil (and (< idx1 last1)
							(not (eq cns2 last2))
							(funcall eql-bf (aref buffer1 idx1)
											(car          cns2))) nil :returns (values idx1 cns2))
			   (incf idx1)
			   (setf cns2 (cdr cns2)))))

	(defmethod-overload mismatch ((first1 const-vector-pointer) (last1 const-vector-pointer)
								  (first2  cons_const_iterator) (last2  cons_const_iterator))
	  ;;(format t "specialized mismatch for const-vector-pointer & cons_const_iterator is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (multiple-value-bind (idx1 cns2) (__mismatch-imp-7 (opr::vec-ptr-index  first1)
														 (opr::vec-ptr-index   last1)
														 (opr::vec-ptr-buffer first1)
														 (__cons-itr-cons first2)
														 (__cons-itr-cons  last2) #'operator_==)
		(make_pair (__algo-make-vct-iterator first1 idx1)
				   (__algo-make-cns-iterator first2 cns2))))

	(defmethod-overload mismatch ((first1 const-vector-pointer) (last1 const-vector-pointer)
								  (first2  cons_const_iterator) (last2  cons_const_iterator) eql-bf)
	  ;;(format t "specialized mismatch for const-vector-pointer & cons_const_iterator is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (multiple-value-bind (idx1 cns2) (__mismatch-imp-7 (opr::vec-ptr-index  first1)
														 (opr::vec-ptr-index   last1)
														 (opr::vec-ptr-buffer first1)
														 (__cons-itr-cons first2)
														 (__cons-itr-cons  last2) (functor_function (clone eql-bf)))
		(make_pair (__algo-make-vct-iterator first1 idx1)
				   (__algo-make-cns-iterator first2 cns2)))))

  ;;PTN; mismatch(0x14) : 8 - cvp x cvp
  (labels ((__mismatch-imp-8 (idx1 last1 buffer1 idx2 last2 buffer2 eql-bf)
			 (declare (type fixnum idx1 last1 idx2 last2))
			 (declare (type cl:vector buffer1 buffer2))
			 (declare (type cl:function eql-bf))
			 (for (nil (and (< idx1 last1)
							(< idx2 last2)
							(funcall eql-bf (aref buffer1 idx1)
											(aref buffer2 idx2))) nil :returns (values idx1 idx2))
			   (incf idx1)
			   (incf idx2))))

	(defmethod-overload mismatch ((first1 const-vector-pointer) (last1 const-vector-pointer)
								  (first2 const-vector-pointer) (last2 const-vector-pointer))
	  ;;(format t "specialized mismatch for const-vector-pointer & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__pointer-check-iterator-range first2 last2)
	  (multiple-value-bind (idx1 idx2) (__mismatch-imp-8 (opr::vec-ptr-index  first1)
														 (opr::vec-ptr-index  last1)
														 (opr::vec-ptr-buffer first1)
														 (opr::vec-ptr-index  first2)
														 (opr::vec-ptr-index  last2)
														 (opr::vec-ptr-buffer first2) #'operator_==)
		(make_pair (__algo-make-vct-iterator first1 idx1)
				   (__algo-make-vct-iterator first2 idx2))))

	(defmethod-overload mismatch ((first1 const-vector-pointer) (last1 const-vector-pointer)
								  (first2 const-vector-pointer) (last2 const-vector-pointer) eql-bf)
	  ;;(format t "specialized mismatch for const-vector-pointer & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__pointer-check-iterator-range first2 last2)
	  (multiple-value-bind (idx1 idx2) (__mismatch-imp-8 (opr::vec-ptr-index  first1)
														 (opr::vec-ptr-index  last1)
														 (opr::vec-ptr-buffer first1)
														 (opr::vec-ptr-index  first2)
														 (opr::vec-ptr-index  last2)
														 (opr::vec-ptr-buffer first2) (functor_function (clone eql-bf)))
		(make_pair (__algo-make-vct-iterator first1 idx1)
				   (__algo-make-vct-iterator first2 idx2))))))




; first1  : input_iterator
; last1   : input_iterator
; first2  : input_iterator
; last2   : input_iterator
; eql-bf  : binary_function ( default : #'operator_== )
; returns : boolean value.
(locally (declare (optimize speed))

  ;;PTN; equal : 0 -  i  x  i 
  (labels ((__equal-imp-0 (first1 last1 first2 eql-bf)
			 (declare (type cl:function eql-bf))
			 (if (_== first1 last1)
				 t
				 (with-operators
					 (for (((itr1 @~first1) (itr2 @~first2))
						   (_/= itr1 last1) (progn ++itr1 ++itr2) :returns t)
					   (unless (funcall eql-bf *itr1 *itr2)
						 (return-from __equal-imp-0 nil)))))))

	(defmethod-overload equal ((first1 input_iterator)
							   (last1  input_iterator) (first2 input_iterator))
	  (__equal-imp-0 first1 last1 first2 #'operator_==))

	(defmethod-overload equal ((first1 input_iterator)
							   (last1  input_iterator)
							   (first2 input_iterator) eql-bf)
	  (__equal-imp-0 first1 last1 first2 (functor_function (clone eql-bf)))))

  ;;PTN; equal : 1 - cci x  i 
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__equal-imp-1 (cons1 cons2 first2 eql-bf)
			 (declare (type cl:list cons1 cons2))
			 (declare (type cl:function eql-bf))
			 (with-operators
				 (for (((itr2 @~first2)) (not (eq cons1 cons2)) (progn (setf cons1 (cdr cons1)) ++itr2) :returns t)
				   (unless (funcall eql-bf (car cons1) *itr2)
					 (return-from __equal-imp-1 nil))))))

	(defmethod-overload equal ((first1 cons_const_iterator)
							   (last1  cons_const_iterator) (first2 input_iterator))
	  ;;(format t "specialized equal for cons_const_iterator & input_iterator is invoked.~%")
	  (__equal-imp-1 (__cons-itr-cons first1)
					 (__cons-itr-cons  last1) first2 #'operator_==))

	(defmethod-overload equal ((first1 cons_const_iterator)
							   (last1  cons_const_iterator) (first2 input_iterator) eql-bf)
	  ;;(format t "specialized equal for cons_const_iterator & input_iterator is invoked.~%")
	  (__equal-imp-1 (__cons-itr-cons first1)
					 (__cons-itr-cons  last1) first2 (functor_function (clone eql-bf)))))
  
  ;;PTN; equal : 2 - cvp x  i 
  (labels ((__equal-imp-2 (idx1 last1 buffer1 first2 eql-bf)
			 (declare (type fixnum idx1 last1))
			 (declare (type cl:vector buffer1))
			 (declare (type cl:function eql-bf))
			 (with-operators
				 (for (((itr2 @~first2)) (< idx1 last1) (progn (incf idx1) ++itr2) :returns t)
				   (unless (funcall eql-bf (aref buffer1 idx1) *itr2)
					 (return-from __equal-imp-2 nil))))))

	(defmethod-overload equal ((first1 const-vector-pointer)
							   (last1  const-vector-pointer) (first2 input_iterator))
	  ;;(format t "specialized equal for const-vector-pointer & input_iterator is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__equal-imp-2 (opr::vec-ptr-index  first1)
					 (opr::vec-ptr-index   last1)
					 (opr::vec-ptr-buffer first1) first2 #'operator_==))

	(defmethod-overload equal ((first1 const-vector-pointer)
							   (last1  const-vector-pointer) (first2 input_iterator) eql-bf)
	  ;;(format t "specialized equal for const-vector-pointer & input_iterator is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__equal-imp-2 (opr::vec-ptr-index  first1)
					 (opr::vec-ptr-index   last1)
					 (opr::vec-ptr-buffer first1) first2 (functor_function (clone eql-bf)))))

  ;;PTN; equal : 3 -  i  x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__equal-imp-3 (first1 last1 cns2 eql-bf)
			 (declare (type cl:list     cns2))
			 (declare (type cl:function eql-bf))
			 (with-operators
				 (for (((itr1 @~first1)) (_/= itr1 last1) (progn ++itr1 (setf cns2 (cdr cns2))) :returns t)
				   (unless (funcall eql-bf *itr1 (car cns2))
					 (return-from __equal-imp-3 nil))))))

	(defmethod-overload equal ((first1 input_iterator)
							   (last1  input_iterator) (first2 cons_const_iterator))
	  ;;(format t "specialized equal for input_iterator & cons_const_iterator is invoked.~%")
	  (__equal-imp-3 first1 last1 (__cons-itr-cons first2) #'operator_==))

	(defmethod-overload equal ((first1 input_iterator)
							   (last1  input_iterator) (first2 cons_const_iterator) eql-bf)
	  ;;(format t "specialized equal for input_iterator & cons_const_iterator is invoked.~%")
	  (__equal-imp-3 first1 last1 (__cons-itr-cons first2) (functor_function (clone eql-bf)))))

  ;;PTN; equal : 4 - cci x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__equal-imp-4 (cons1 end1 cons2 eql-bf)
			 (declare (type cl:list     cons1 end1 cons2))
			 (declare (type cl:function eql-bf))
			 (for (nil (not (eq cons1 end1)) (progn (setf cons1 (cdr cons1))
													(setf cons2 (cdr cons2))) :returns t)
			   (unless (funcall eql-bf (car cons1) (car cons2))
				 (return-from __equal-imp-4 nil)))))

	(defmethod-overload equal ((first1 cons_const_iterator)
							   (last1  cons_const_iterator) (first2 cons_const_iterator))
	  ;;(format t "specialized equal for cons_const_iterator & cons_const_iterator is invoked.~%")
	  (__equal-imp-4 (__cons-itr-cons first1)
					 (__cons-itr-cons  last1) (__cons-itr-cons first2) #'operator_==))

	(defmethod-overload equal ((first1 cons_const_iterator)
							   (last1  cons_const_iterator)
							   (first2 cons_const_iterator) eql-bf)
	  ;;(format t "specialized equal for cons_const_iterator & cons_const_iterator is invoked.~%")
	  (__equal-imp-4 (__cons-itr-cons first1) (__cons-itr-cons last1)
					 (__cons-itr-cons first2) (functor_function (clone eql-bf)))))

  ;;PTN; equal : 5 - cvp x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__equal-imp-5 (idx1 last1 buffer1 cons2 eql-bf)
			 (declare (type fixnum idx1 last1))
			 (declare (type cl:vector   buffer1))
			 (declare (type cl:list     cons2))
			 (declare (type cl:function eql-bf))
			 (for (nil (< idx1 last1) (progn (incf idx1)
											 (setf cons2 (cdr cons2))) :returns t)
			   (unless (funcall eql-bf (aref buffer1 idx1) (car cons2))
				 (return-from __equal-imp-5 nil)))))

	(defmethod-overload equal ((first1 const-vector-pointer)
							   (last1  const-vector-pointer) (first2 cons_const_iterator))
	  ;;(format t "specialized equal for const-vector-pointer & cons_const_iterator is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__equal-imp-5 (opr::vec-ptr-index  first1)
					 (opr::vec-ptr-index   last1)
					 (opr::vec-ptr-buffer first1) (__cons-itr-cons first2) #'operator_==))

	(defmethod-overload equal ((first1 const-vector-pointer)
							   (last1  const-vector-pointer)
							   (first2 cons_const_iterator) eql-bf)
	  ;;(format t "specialized equal for const-vector-pointer & cons_const_iterator is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__equal-imp-5 (opr::vec-ptr-index  first1)
					 (opr::vec-ptr-index   last1)
					 (opr::vec-ptr-buffer first1) (__cons-itr-cons first2) (functor_function (clone eql-bf)))))

  ;;PTN; equal : 6 -  i  x cvp
  (labels ((__equal-imp-6 (first1 last1 idx2 buffer2 eql-bf)
			 (declare (type fixnum idx2))
			 (declare (type cl:vector buffer2))
			 (declare (type cl:function eql-bf))
			 (with-operators
				 (for (((itr1 @~first1)) (_/= itr1 last1) (progn ++itr1 (incf idx2)) :returns t)
				   (unless (funcall eql-bf *itr1 (aref buffer2 idx2))
					 (return-from __equal-imp-6 nil))))))

	(defmethod-overload equal ((first1 input_iterator)
							   (last1  input_iterator) (first2 const-vector-pointer))
	  ;;(format t "specialized equal for input_iterator & const-vector-pointer is invoked.~%")
	  (__equal-imp-6 first1 last1
					 (opr::vec-ptr-index  first2) (opr::vec-ptr-buffer first2) #'operator_==))

	(defmethod-overload equal ((first1 input_iterator)
							   (last1  input_iterator) (first2 const-vector-pointer) eql-bf)
	  ;;(format t "specialized equal for input_iterator & const-vector-pointer is invoked.~%")
	  (__equal-imp-6 first1 last1
					 (opr::vec-ptr-index  first2)
					 (opr::vec-ptr-buffer first2) (functor_function (clone eql-bf)))))

  ;;PTN; equal : 7 - cci x cvp
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__equal-imp-7 (cons1 end1 idx2 buffer2 eql-bf)
			 (declare (type fixnum      idx2))
			 (declare (type cl:list     cons1 end1))
			 (declare (type cl:vector   buffer2))
			 (declare (type cl:function eql-bf))
			 (for (nil (not (eq cons1 end1)) (progn (setf cons1 (cdr cons1)) (incf idx2)) :returns t)
			   (unless (funcall eql-bf (car cons1) (aref buffer2 idx2))
				 (return-from __equal-imp-7 nil)))))

	(defmethod-overload equal ((first1 cons_const_iterator)
							   (last1  cons_const_iterator) (first2 const-vector-pointer))
	  ;;(format t "specialized equal for cons_const_iterator & const-vector-pointer is invoked.~%")
	  (__equal-imp-7 (__cons-itr-cons first1) (__cons-itr-cons last1)
					 (opr::vec-ptr-index first2) (opr::vec-ptr-buffer first2) #'operator_==))

	(defmethod-overload equal ((first1 cons_const_iterator)
							   (last1  cons_const_iterator)
							   (first2 const-vector-pointer) eql-bf)
	  ;;(format t "specialized equal for cons_const_iterator & const-vector-pointer is invoked.~%")
	  (__equal-imp-7 (__cons-itr-cons first1) (__cons-itr-cons last1)
					 (opr::vec-ptr-index first2) (opr::vec-ptr-buffer first2) (functor_function (clone eql-bf)))))

  ;;PTN; equal : 8 - cvp x cvp
  (labels ((__equal-imp-8 (idx1 last1 buffer1 idx2 buffer2 eql-bf)
			 (declare (type fixnum idx1 last1 idx2))
			 (declare (type cl:vector buffer1 buffer2))
			 (declare (type cl:function eql-bf))
			 (for (nil (< idx1 last1) (progn (incf idx1) (incf idx2)) :returns t)
			   (unless (funcall eql-bf (aref buffer1 idx1) (aref buffer2 idx2))
				 (return-from __equal-imp-8 nil)))))

	(defmethod-overload equal ((first1 const-vector-pointer)
							   (last1  const-vector-pointer) (first2 const-vector-pointer))
	  ;;(format t "specialized equal for const-vector-pointer & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__equal-imp-8 (opr::vec-ptr-index  first1)
					 (opr::vec-ptr-index   last1) (opr::vec-ptr-buffer first1)
					 (opr::vec-ptr-index  first2) (opr::vec-ptr-buffer first2) #'operator_==))

	(defmethod-overload equal ((first1 const-vector-pointer)
							   (last1  const-vector-pointer)
							   (first2 const-vector-pointer) eql-bf)
	  ;;(format t "specialized equal for const-vector-pointer & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__equal-imp-8 (opr::vec-ptr-index  first1)
					 (opr::vec-ptr-index   last1) (opr::vec-ptr-buffer first1)
					 (opr::vec-ptr-index  first2) (opr::vec-ptr-buffer first2) (functor_function (clone eql-bf))))))



; first1  : input_iterator
; last1   : input_iterator
; first2  : input_iterator
; last2   : input_iterator
; eql-bf  : binary_function ( default : #'operator_== )
; returns : boolean value.
#-(or cl-stl-0x98 cl-stl-0x11)
(locally (declare (optimize speed))

  ;;PTN; equal(0x14) : 0 -  i  x  i 
  (labels ((__equal-imp-0a (first1 last1 first2 eql-bf)
			 (declare (type cl:function eql-bf))
			 (if (_== first1 last1)
				 t
				 (with-operators
					 (for (((itr1 @~first1) (itr2 @~first2))
						   (_/= itr1 last1) (progn ++itr1 ++itr2) :returns t)
					   (unless (funcall eql-bf *itr1 *itr2)
						 (return-from __equal-imp-0a nil))))))

		   (__equal-imp-0b (first1 last1 first2 last2 eql-bf)
			 (declare (type cl:function eql-bf))
			 (if (and (_== first1 last1) (_== first2 last2))
				 t
				 (with-operators
					 (for (((itr1 @~first1) (itr2 @~first2))
						   (and (_/= itr1 last1) (_/= itr2 last2))
						   (progn ++itr1 ++itr2) :returns (and (_== itr1 last1) (_== itr2 last2)))
					   (unless (funcall eql-bf *itr1 *itr2)
						 (return-from __equal-imp-0b nil)))))))
  
	(defmethod-overload equal ((first1 input_iterator) (last1 input_iterator)
							   (first2 input_iterator) (last2 input_iterator))
	  (__equal-imp-0b first1 last1 first2 last2 #'operator_==))

	(defmethod-overload equal ((first1 input_iterator) (last1 input_iterator)
							   (first2 input_iterator) (last2 input_iterator) eql-bf)
	  (__equal-imp-0b first1 last1 first2 last2 (functor_function (clone eql-bf))))

	(defmethod-overload equal ((first1 randomaccess_iterator) (last1 randomaccess_iterator)
							   (first2 randomaccess_iterator) (last2 randomaccess_iterator))
	  (if (/= (the fixnum (distance first1 last1))
			  (the fixnum (distance first2 last2)))
		  nil
		  (__equal-imp-0a first1 last1 first2 #'operator_==)))

	(defmethod-overload equal ((first1 randomaccess_iterator) (last1 randomaccess_iterator)
							   (first2 randomaccess_iterator) (last2 randomaccess_iterator) eql-bf)
	  (if (/= (the fixnum (distance first1 last1))
			  (the fixnum (distance first2 last2)))
		  nil
		  (__equal-imp-0a first1 last1 first2 (functor_function (clone eql-bf))))))


  ;;PTN; equal(0x14) : 1 - cci x  i 
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__equal-imp-1 (cons1 end1 first2 last2 eql-bf)
			 (declare (type cl:list     cons1 end1))
			 (declare (type cl:function eql-bf))
			 (if (and (eq cons1 end1) (_== first2 last2))
				 t
				 (with-operators
					 (for (((itr2 @~first2)) (and (not (eq cons1 end1)) (_/= itr2 last2))
											 (progn (setf cons1 (cdr cons1)) ++itr2)
											 :returns (and (eq cons1 end1) (_== itr2 last2)))
					   (unless (funcall eql-bf (car cons1) *itr2)
						 (return-from __equal-imp-1 nil)))))))

	(defmethod-overload equal ((first1 cons_const_iterator)
							   (last1  cons_const_iterator)
							   (first2 input_iterator) (last2 input_iterator))
	  ;;(format t "specialized equal for cons_const_iterator & input_iterator is invoked.~%")
	  (__equal-imp-1 (__cons-itr-cons first1)
					 (__cons-itr-cons  last1) first2 last2 #'operator_==))

	(defmethod-overload equal ((first1 cons_const_iterator)
							   (last1  cons_const_iterator)
							   (first2 input_iterator) (last2 input_iterator) eql-bf)
	  ;;(format t "specialized equal for cons_const_iterator & input_iterator is invoked.~%")
	  (__equal-imp-1 (__cons-itr-cons first1)
					 (__cons-itr-cons  last1) first2 last2 (functor_function (clone eql-bf)))))


  ;;PTN; equal(0x14) : 2 - cvp x  i 
  (labels ((__equal-imp-2 (idx1 last1 buffer1 first2 last2 eql-bf)
			 (declare (type fixnum idx1 last1))
			 (declare (type cl:vector buffer1))
			 (declare (type cl:function eql-bf))
			 (if (and (= idx1 last1) (_== first2 last2))
				 t
				 (with-operators
					 (for (((itr2 @~first2)) (and (< idx1 last1) (_/= itr2 last2))
											 (progn (incf idx1) ++itr2)
											 :returns (and (= idx1 last1) (_== itr2 last2)))
					   (unless (funcall eql-bf (aref buffer1 idx1) *itr2)
						 (return-from __equal-imp-2 nil)))))))

	(defmethod-overload equal ((first1 const-vector-pointer)
							   (last1  const-vector-pointer)
							   (first2 input_iterator) (last2 input_iterator))
	  ;;(format t "specialized equal for const-vector-pointer & input_iterator is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__equal-imp-2 (opr::vec-ptr-index  first1)
					 (opr::vec-ptr-index   last1)
					 (opr::vec-ptr-buffer first1) first2 last2 #'operator_==))

	(defmethod-overload equal ((first1 const-vector-pointer)
							   (last1  const-vector-pointer)
							   (first2 input_iterator) (last2 input_iterator) eql-bf)
	  ;;(format t "specialized equal for const-vector-pointer & input_iterator is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__equal-imp-2 (opr::vec-ptr-index  first1)
					 (opr::vec-ptr-index   last1)
					 (opr::vec-ptr-buffer first1) first2 last2 (functor_function (clone eql-bf)))))


  ;;PTN; equal(0x14) : 3 -  i  x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__equal-imp-3 (first1 last1 cons2 end2 eql-bf)
			 (declare (type cl:list cons2 end2))
			 (declare (type cl:function eql-bf))
			 (if (and (_== first1 last1) (eq cons2 end2))
				 t
				 (with-operators
					 (for (((itr1 @~first1)) (and (_/= itr1 last1) (not (eq cons2 end2)))
											 (progn ++itr1 (setf cons2 (cdr cons2)))
											 :returns (and (_== itr1 last1) (eq cons2 end2)))
					   (unless (funcall eql-bf *itr1 (car cons2))
						 (return-from __equal-imp-3 nil)))))))

	(defmethod-overload equal ((first1 input_iterator) (last1 input_iterator)
							   (first2 cons_const_iterator) (last2 cons_const_iterator))
	  ;;(format t "specialized equal for input_iterator & cons_const_iterator is invoked.~%")
	  (__equal-imp-3 first1 last1 (__cons-itr-cons first2) (__cons-itr-cons last2) #'operator_==))

	(defmethod-overload equal ((first1 input_iterator) (last1 input_iterator)
							   (first2 cons_const_iterator) (last2 cons_const_iterator) eql-bf)
	  ;;(format t "specialized equal for input_iterator & cons_const_iterator is invoked.~%")
	  (__equal-imp-3 first1 last1
					 (__cons-itr-cons first2) (__cons-itr-cons last2) (functor_function (clone eql-bf)))))


  ;;PTN; equal(0x14) : 4 - cci x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__equal-imp-4 (cons1 end1 cons2 end2 eql-bf)
			 (declare (type cl:list cons1 end1 cons2 end2))
			 (declare (type cl:function eql-bf))
			 (if (and (eq cons1 end1) (eq cons2 end2))
				 t
				 (for (nil (and (not (eq cons1 end1))
								(not (eq cons2 end2))) (progn (setf cons1 (cdr cons1))
															  (setf cons2 (cdr cons2)))
													   :returns (and (eq cons1 end1) (eq cons2 end2)))
					   (unless (funcall eql-bf (car cons1) (car cons2))
						 (return-from __equal-imp-4 nil))))))

	(defmethod-overload equal ((first1 cons_const_iterator) (last1 cons_const_iterator)
							   (first2 cons_const_iterator) (last2 cons_const_iterator))
	  ;;(format t "specialized equal for cons_const_iterator & cons_const_iterator is invoked.~%")
	  (__equal-imp-4 (__cons-itr-cons first1) (__cons-itr-cons last1)
					 (__cons-itr-cons first2) (__cons-itr-cons last2) #'operator_==))

	(defmethod-overload equal ((first1 cons_const_iterator) (last1 cons_const_iterator)
							   (first2 cons_const_iterator) (last2 cons_const_iterator) eql-bf)
	  ;;(format t "specialized equal for cons_const_iterator & cons_const_iterator is invoked.~%")
	  (__equal-imp-4 (__cons-itr-cons first1) (__cons-itr-cons last1)
					 (__cons-itr-cons first2) (__cons-itr-cons last2) (functor_function (clone eql-bf)))))


  ;;PTN; equal(0x14) : 5 - cvp x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__equal-imp-5 (idx1 last1 buffer1 cons2 end2 eql-bf)
			 (declare (type fixnum idx1 last1))
			 (declare (type cl:vector buffer1))
			 (declare (type cl:list cons2 end2))
			 (declare (type cl:function eql-bf))
			 (if (and (= idx1 last1) (eq cons2 end2))
				 t
				 (for (nil (and (< idx1 last1) (not (eq cons2 end2)))
						   (progn (incf idx1) (setf cons2 (cdr cons2)))
											  :returns (and (= idx1 last1) (eq cons2 end2)))
				   (unless (funcall eql-bf (aref buffer1 idx1) (car cons2))
					 (return-from __equal-imp-5 nil))))))

	(defmethod-overload equal ((first1 const-vector-pointer) (last1 const-vector-pointer)
							   (first2 cons_const_iterator)  (last2 cons_const_iterator))
	  ;;(format t "specialized equal for const-vector-pointer & cons_const_iterator is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__equal-imp-5 (opr::vec-ptr-index first1)
					 (opr::vec-ptr-index  last1) (opr::vec-ptr-buffer first1)
					 (__cons-itr-cons first2) (__cons-itr-cons last2) #'operator_==))

	(defmethod-overload equal ((first1 const-vector-pointer) (last1 const-vector-pointer)
							   (first2 cons_const_iterator)  (last2 cons_const_iterator) eql-bf)
	  ;;(format t "specialized equal for const-vector-pointer & cons_const_iterator is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__equal-imp-5 (opr::vec-ptr-index first1)
					 (opr::vec-ptr-index  last1) (opr::vec-ptr-buffer first1)
					 (__cons-itr-cons first2) (__cons-itr-cons last2) (functor_function (clone eql-bf)))))


  ;;PTN; equal(0x14) : 6 -  i  x cvp
  (labels ((__equal-imp-6 (first1 last1 idx2 last2 buffer2 eql-bf)
			 (declare (type fixnum idx2 last2))
			 (declare (type cl:vector buffer2))
			 (declare (type cl:function eql-bf))
			 (if (and (_== first1 last1) (= idx2 last2))
				 t
				 (with-operators
					 (for (((itr1 @~first1)) (and (_/= itr1 last1) (< idx2 last2))
											 (progn ++itr1 (incf idx2))
											 :returns (and (_== itr1 last1) (= idx2 last2)))
					   (unless (funcall eql-bf *itr1 (aref buffer2 idx2))
						 (return-from __equal-imp-6 nil)))))))

	(defmethod-overload equal ((first1 input_iterator) (last1 input_iterator)
							   (first2 const-vector-pointer) (last2 const-vector-pointer))
	  ;;(format t "specialized equal for const-vector-pointer & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first2 last2)
	  (__equal-imp-6 first1 last1
					 (opr::vec-ptr-index first2)
					 (opr::vec-ptr-index  last2) (opr::vec-ptr-buffer first2) #'operator_==))

	(defmethod-overload equal ((first1 input_iterator) (last1 input_iterator)
							   (first2 const-vector-pointer) (last2 const-vector-pointer) eql-bf)
	  ;;(format t "specialized equal for const-vector-pointer & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first2 last2)
	  (__equal-imp-6 first1 last1
					 (opr::vec-ptr-index first2)
					 (opr::vec-ptr-index  last2) (opr::vec-ptr-buffer first2) (functor_function (clone eql-bf)))))


  ;;PTN; equal(0x14) : 7 - cci x cvp
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__equal-imp-7 (cons1 end1 idx2 last2 buffer2 eql-bf)
			 (declare (type cl:list cons1 end1))
			 (declare (type fixnum idx2 last2))
			 (declare (type cl:vector buffer2))
			 (declare (type cl:function eql-bf))
			 (if (and (eq cons1 end1) (= idx2 last2))
				 t
				 (for (nil (and (not (eq cons1 end1)) (< idx2 last2))
						   (progn (setf cons1 (cdr cons1)) (incf idx2)) :returns (and (eq cons1 end1) (= idx2 last2)))
				   (unless (funcall eql-bf (car cons1) (aref buffer2 idx2))
					 (return-from __equal-imp-7 nil))))))

	(defmethod-overload equal ((first1  cons_const_iterator) (last1  cons_const_iterator)
							   (first2 const-vector-pointer) (last2 const-vector-pointer))
	  ;;(format t "specialized equal for cons_const_iterator & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first2 last2)
	  (__equal-imp-7 (__cons-itr-cons first1) (__cons-itr-cons last1)
					 (opr::vec-ptr-index first2) (opr::vec-ptr-index last2)
					 (opr::vec-ptr-buffer first2) #'operator_==))

	(defmethod-overload equal ((first1  cons_const_iterator) (last1  cons_const_iterator)
							   (first2 const-vector-pointer) (last2 const-vector-pointer) eql-bf)
	  ;;(format t "specialized equal for cons_const_iterator & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first2 last2)
	  (__equal-imp-7 (__cons-itr-cons first1) (__cons-itr-cons last1)
					 (opr::vec-ptr-index first2) (opr::vec-ptr-index last2)
					 (opr::vec-ptr-buffer first2) (functor_function (clone eql-bf)))))

  
  ;;PTN; equal(0x14) : 8 - cvp x cvp
  (labels ((__equal-imp-8 (idx1 last1 buffer1 idx2 last2 buffer2 eql-bf)
			 (declare (type fixnum idx1 last1 idx2 last2))
			 (declare (type cl:vector buffer1 buffer2))
			 (declare (type cl:function eql-bf))
			 (if (/= (- last1 idx1) (- last2 idx2))
				 nil
				 (for (nil (< idx1 last1) (progn (incf idx1) (incf idx2)) :returns t)
				   (unless (funcall eql-bf (aref buffer1 idx1) (aref buffer2 idx2))
					 (return-from __equal-imp-8 nil))))))

	(defmethod-overload equal ((first1 const-vector-pointer) (last1 const-vector-pointer)
							   (first2 const-vector-pointer) (last2 const-vector-pointer))
	  ;;(format t "specialized equal for const-vector-pointer & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__pointer-check-iterator-range first2 last2)
	  (__equal-imp-8 (opr::vec-ptr-index first1)
					 (opr::vec-ptr-index  last1) (opr::vec-ptr-buffer first1)
					 (opr::vec-ptr-index first2)
					 (opr::vec-ptr-index  last2) (opr::vec-ptr-buffer first2) #'operator_==))

	(defmethod-overload equal ((first1 const-vector-pointer) (last1 const-vector-pointer)
							   (first2 const-vector-pointer) (last2 const-vector-pointer) eql-bf)
	  ;;(format t "specialized equal for const-vector-pointer & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__pointer-check-iterator-range first2 last2)
	  (__equal-imp-8 (opr::vec-ptr-index first1)
					 (opr::vec-ptr-index  last1) (opr::vec-ptr-buffer first1)
					 (opr::vec-ptr-index first2)
					 (opr::vec-ptr-index  last2) (opr::vec-ptr-buffer first2) (functor_function (clone eql-bf))))))



; first1  : forward_iterator
; last1   : forward_iterator
; first2  : forward_iterator
; last2   : forward_iterator
; pred    : binary_function ( default : #'operator_== )
; returns : boolean value
#-cl-stl-0x98    ; is_permutation
(locally (declare (optimize speed))

  ;;PTN; is_permutation : 0 -  f  x  f
  (defmethod-overload is_permutation ((first1 forward_iterator)
									  (last1  forward_iterator) (first2 forward_iterator))
	(__is_permutation-imp-0a first1 last1 first2 #'operator_==))

  (defmethod-overload is_permutation ((first1 forward_iterator)
									  (last1  forward_iterator) (first2 forward_iterator) pred)
	(__is_permutation-imp-0a first1 last1 first2 (functor_function (clone pred))))

  ;;PTN; is_permutation : 1 - cci x  f
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload is_permutation ((first1 cons_const_iterator)
									  (last1  cons_const_iterator) (first2 forward_iterator))
	;;(format t "specialized is_permutation for cons_const_iterator & forward_iterator is invoked.~%")
	(__is_permutation-imp-1a (__cons-itr-cons first1)
							 (__cons-itr-cons  last1) first2 #'operator_==))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload is_permutation ((first1 cons_const_iterator)
									  (last1  cons_const_iterator) (first2 forward_iterator) eql-bf)
	;;(format t "specialized is_permutation for cons_const_iterator & forward_iterator is invoked.~%")
	(__is_permutation-imp-1a (__cons-itr-cons first1)
							 (__cons-itr-cons  last1) first2 (functor_function (clone eql-bf))))

  ;;PTN; is_permutation : 2 - cvp x  f 
  (defmethod-overload is_permutation ((first1 const-vector-pointer)
									  (last1  const-vector-pointer) (first2 forward_iterator))
	;;(format t "specialized is_permutation for const-vector-pointer & forward_iterator is invoked.~%")
	(__is_permutation-imp-2a (opr::vec-ptr-index  first1)
							 (opr::vec-ptr-index   last1)
							 (opr::vec-ptr-buffer first1) first2 #'operator_==))

  (defmethod-overload is_permutation ((first1 const-vector-pointer)
									  (last1  const-vector-pointer) (first2 forward_iterator) eql-bf)
	;;(format t "specialized is_permutation for const-vector-pointer & forward_iterator is invoked.~%")
	(__is_permutation-imp-2a (opr::vec-ptr-index  first1)
							 (opr::vec-ptr-index   last1)
							 (opr::vec-ptr-buffer first1) first2 (functor_function (clone eql-bf))))

  ;;PTN; is_permutation : 3 -  f  x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload is_permutation ((first1 forward_iterator)
									  (last1  forward_iterator) (first2 cons_const_iterator))
	;;(format t "specialized is_permutation for forward_iterator & cons_const_iterator is invoked.~%")
	(__is_permutation-imp-3a first1 last1
							 (__cons-itr-cons first2) #'operator_==))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload is_permutation ((first1 forward_iterator)
									  (last1  forward_iterator) (first2 cons_const_iterator) eql-bf)
	;;(format t "specialized is_permutation for forward_iterator & cons_const_iterator is invoked.~%")
	(__is_permutation-imp-3a first1 last1
							 (__cons-itr-cons first2) (functor_function (clone eql-bf))))

  ;;PTN; is_permutation : 4 - cci x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload is_permutation ((first1 cons_const_iterator)
									  (last1  cons_const_iterator) (first2 cons_const_iterator))
	;;(format t "specialized is_permutation for cons_const_iterator & cons_const_iterator is invoked.~%")
	(__is_permutation-imp-4a (__cons-itr-cons first1)
							 (__cons-itr-cons  last1)
							 (__cons-itr-cons first2) #'operator_==))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload is_permutation ((first1 cons_const_iterator)
									  (last1  cons_const_iterator) (first2 cons_const_iterator) eql-bf)
	;;(format t "specialized is_permutation for cons_const_iterator & cons_const_iterator is invoked.~%")
	(__is_permutation-imp-4a (__cons-itr-cons first1)
							 (__cons-itr-cons  last1)
							 (__cons-itr-cons first2) (functor_function (clone eql-bf))))

  ;;PTN; is_permutation : 5 - cvp x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload is_permutation ((first1 const-vector-pointer)
									  (last1  const-vector-pointer) (first2 cons_const_iterator))
	;;(format t "specialized is_permutation for const-vector-pointer & cons_const_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__is_permutation-imp-5a (opr::vec-ptr-index  first1)
							 (opr::vec-ptr-index   last1)
							 (opr::vec-ptr-buffer first1)
							 (__cons-itr-cons     first2) #'operator_==))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload is_permutation ((first1 const-vector-pointer)
									  (last1  const-vector-pointer) (first2 cons_const_iterator) eql-bf)
	;;(format t "specialized is_permutation for const-vector-pointer & cons_const_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__is_permutation-imp-5a (opr::vec-ptr-index  first1)
							 (opr::vec-ptr-index   last1)
							 (opr::vec-ptr-buffer first1)
							 (__cons-itr-cons     first2) (functor_function (clone eql-bf))))

  ;;PTN; is_permutation : 6 -  f  x cvp
  (defmethod-overload is_permutation ((first1 forward_iterator)
									  (last1  forward_iterator) (first2 const-vector-pointer))
	;;(format t "specialized is_permutation for forward_iterator & const-vector-pointer is invoked.~%")
	(__is_permutation-imp-6a first1 last1
							 (opr::vec-ptr-index  first2)
							 (opr::vec-ptr-buffer first2) #'operator_==))

  (defmethod-overload is_permutation ((first1 forward_iterator)
									  (last1  forward_iterator) (first2 const-vector-pointer) eql-bf)
	;;(format t "specialized is_permutation for forward_iterator & const-vector-pointer is invoked.~%")
	(__is_permutation-imp-6a first1 last1
							 (opr::vec-ptr-index  first2)
							 (opr::vec-ptr-buffer first2) (functor_function (clone eql-bf))))

  ;;PTN; is_permutation : 7 - cci x cvp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload is_permutation ((first1 cons_const_iterator)
									  (last1  cons_const_iterator) (first2 const-vector-pointer))
	;;(format t "specialized is_permutation for cons_const_iterator & const-vector-pointer is invoked.~%")
	(__is_permutation-imp-7a (__cons-itr-cons first1)
							 (__cons-itr-cons  last1)
							 (opr::vec-ptr-index  first2)
							 (opr::vec-ptr-buffer first2) #'operator_==))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload is_permutation ((first1 cons_const_iterator)
									  (last1  cons_const_iterator) (first2 const-vector-pointer) eql-bf)
	;;(format t "specialized is_permutation for cons_const_iterator & const-vector-pointer is invoked.~%")
	(__is_permutation-imp-7a (__cons-itr-cons first1)
							 (__cons-itr-cons  last1)
							 (opr::vec-ptr-index  first2)
							 (opr::vec-ptr-buffer first2) (functor_function (clone eql-bf))))

  ;;PTN; is_permutation : 8 - cvp x cvp
  (defmethod-overload is_permutation ((first1 const-vector-pointer)
									  (last1  const-vector-pointer) (first2 const-vector-pointer))
	;;(format t "specialized is_permutation for const-vector-pointer & const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__is_permutation-imp-8a (opr::vec-ptr-index  first1)
							 (opr::vec-ptr-index  last1)
							 (opr::vec-ptr-buffer first1)
							 (opr::vec-ptr-index  first2)
							 (opr::vec-ptr-buffer first2) #'operator_==))

  (defmethod-overload is_permutation ((first1 const-vector-pointer)
									  (last1  const-vector-pointer) (first2 const-vector-pointer) eql-bf)
	;;(format t "specialized is_permutation for const-vector-pointer & const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__is_permutation-imp-8a (opr::vec-ptr-index  first1)
							 (opr::vec-ptr-index  last1)
							 (opr::vec-ptr-buffer first1)
							 (opr::vec-ptr-index  first2)
							 (opr::vec-ptr-buffer first2) (functor_function (clone eql-bf)))))



; first1  : forward_iterator
; last1   : forward_iterator
; first2  : forward_iterator
; last2   : forward_iterator
; pred    : binary_function ( default : #'operator_== )
; returns : boolean value
#-(or cl-stl-0x98 cl-stl-0x11)    ; is_permutation ( 0x14 )
(locally (declare (optimize speed))

  ;;PTN; is_permutation(0x14) : 0 -  f  x  f ( and r x r )
  (defmethod-overload is_permutation ((first1 forward_iterator) (last1 forward_iterator)
									  (first2 forward_iterator) (last2 forward_iterator))
	(__is_permutation-imp-0b first1 last1 first2 last2 #'operator_==))

  (defmethod-overload is_permutation ((first1 forward_iterator) (last1 forward_iterator)
									  (first2 forward_iterator) (last2 forward_iterator) pred)
	(__is_permutation-imp-0b first1 last1 first2 last2 (functor_function (clone pred))))

  (defmethod-overload is_permutation ((first1 randomaccess_iterator) (last1 randomaccess_iterator)
									  (first2 randomaccess_iterator) (last2 randomaccess_iterator))
	(__is_permutation-imp-0c first1 last1 first2 last2 #'operator_==))

  (defmethod-overload is_permutation ((first1 randomaccess_iterator) (last1 randomaccess_iterator)
									  (first2 randomaccess_iterator) (last2 randomaccess_iterator) pred)
	(__is_permutation-imp-0c first1 last1 first2 last2 (functor_function (clone pred))))

  ;;PTN; is_permutation(0x14) : 1 - cci x  f
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload is_permutation ((first1 cons_const_iterator) (last1 cons_const_iterator)
									  (first2 forward_iterator) (last2 forward_iterator))
	;;(format t "specialized is_permutation for cons_const_iterator & forward_iterator is invoked.~%")
	(__is_permutation-imp-1b (__cons-itr-cons first1)
							 (__cons-itr-cons  last1) first2 last2 #'operator_==))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload is_permutation ((first1 cons_const_iterator) (last1 cons_const_iterator)
									  (first2 forward_iterator) (last2 forward_iterator) eql-bf)
	;;(format t "specialized is_permutation for cons_const_iterator & forward_iterator is invoked.~%")
	(__is_permutation-imp-1b (__cons-itr-cons first1)
							 (__cons-itr-cons  last1) first2 last2 (functor_function (clone eql-bf))))

  ;;PTN; is_permutation(0x14) : 2 - cvp x  f 
  (defmethod-overload is_permutation ((first1 const-vector-pointer) (last1 const-vector-pointer)
									  (first2 forward_iterator) (last2 forward_iterator))
	;;(format t "specialized is_permutation for const-vector-pointer & forward_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__is_permutation-imp-2b (opr::vec-ptr-index  first1)
							 (opr::vec-ptr-index  last1)
							 (opr::vec-ptr-buffer first1) first2 last2 #'operator_==))

  (defmethod-overload is_permutation ((first1 const-vector-pointer) (last1 const-vector-pointer)
									  (first2 forward_iterator) (last2 forward_iterator) eql-bf)
	;;(format t "specialized is_permutation for const-vector-pointer & forward_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__is_permutation-imp-2b (opr::vec-ptr-index  first1)
							 (opr::vec-ptr-index  last1)
							 (opr::vec-ptr-buffer first1) first2 last2 (functor_function (clone eql-bf))))

  ;;PTN; is_permutation(0x14) : 3 -  f  x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload is_permutation ((first1 forward_iterator) (last1 forward_iterator)
									  (first2 cons_const_iterator) (last2 cons_const_iterator))
	;;(format t "specialized is_permutation for forward_iterator & cons_const_iterator is invoked.~%")
	(__is_permutation-imp-3b first1 last1
							 (__cons-itr-cons first2)
							 (__cons-itr-cons  last2) #'operator_==))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload is_permutation ((first1 forward_iterator) (last1 forward_iterator)
									  (first2 cons_const_iterator) (last2 cons_const_iterator) eql-bf)
	;;(format t "specialized is_permutation for forward_iterator & cons_const_iterator is invoked.~%")
	(__is_permutation-imp-3b first1 last1
							 (__cons-itr-cons first2)
							 (__cons-itr-cons  last2) (functor_function (clone eql-bf))))

  ;;PTN; is_permutation(0x14) : 4 - cci x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload is_permutation ((first1 cons_const_iterator) (last1 cons_const_iterator)
									  (first2 cons_const_iterator) (last2 cons_const_iterator))
	;;(format t "specialized is_permutation for cons_const_iterator & cons_const_iterator is invoked.~%")
	(__is_permutation-imp-4b (__cons-itr-cons first1)
							 (__cons-itr-cons  last1)
							 (__cons-itr-cons first2)
							 (__cons-itr-cons  last2) #'operator_==))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload is_permutation ((first1 cons_const_iterator) (last1 cons_const_iterator)
									  (first2 cons_const_iterator) (last2 cons_const_iterator) eql-bf)
	;;(format t "specialized is_permutation for cons_const_iterator & cons_const_iterator is invoked.~%")
	(__is_permutation-imp-4b (__cons-itr-cons first1)
							 (__cons-itr-cons  last1)
							 (__cons-itr-cons first2)
							 (__cons-itr-cons  last2) (functor_function (clone eql-bf))))

  ;;PTN; is_permutation(0x14) : 5 - cvp x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload is_permutation ((first1 const-vector-pointer) (last1 const-vector-pointer)
									  (first2 cons_const_iterator) (last2 cons_const_iterator))
	;;(format t "specialized is_permutation for const-vector-pointer & cons_const_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__is_permutation-imp-5b (opr::vec-ptr-index  first1)
							 (opr::vec-ptr-index   last1)
							 (opr::vec-ptr-buffer first1)
							 (__cons-itr-cons first2)
							 (__cons-itr-cons  last2) #'operator_==))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload is_permutation ((first1 const-vector-pointer) (last1 const-vector-pointer)
									  (first2 cons_const_iterator) (last2 cons_const_iterator) eql-bf)
	;;(format t "specialized is_permutation for const-vector-pointer & cons_const_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__is_permutation-imp-5b (opr::vec-ptr-index  first1)
							 (opr::vec-ptr-index   last1)
							 (opr::vec-ptr-buffer first1)
							 (__cons-itr-cons first2)
							 (__cons-itr-cons  last2) (functor_function (clone eql-bf))))

  ;;PTN; is_permutation(0x14) : 6 -  f  x cvp
  (defmethod-overload is_permutation ((first1 forward_iterator) (last1 forward_iterator)
									  (first2 const-vector-pointer) (last2 const-vector-pointer))
	;;(format t "specialized is_permutation for forward_iterator & const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__is_permutation-imp-6b first1 last1
							 (opr::vec-ptr-index  first2)
							 (opr::vec-ptr-index  last2)
							 (opr::vec-ptr-buffer first2) #'operator_==))

  (defmethod-overload is_permutation ((first1 forward_iterator) (last1 forward_iterator)
									  (first2 const-vector-pointer) (last2 const-vector-pointer) eql-bf)
	;;(format t "specialized is_permutation for forward_iterator & const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__is_permutation-imp-6b first1 last1
							 (opr::vec-ptr-index  first2)
							 (opr::vec-ptr-index  last2)
							 (opr::vec-ptr-buffer first2) (functor_function (clone eql-bf))))

  ;;PTN; is_permutation(0x14) : 7 - cci x cvp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload is_permutation ((first1 cons_const_iterator) (last1 cons_const_iterator)
									  (first2 const-vector-pointer) (last2 const-vector-pointer))
	;;(format t "specialized is_permutation for cons_const_iterator & const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__is_permutation-imp-7b (__cons-itr-cons  first1)
							 (__cons-itr-cons  last1)
							 (opr::vec-ptr-index  first2)
							 (opr::vec-ptr-index  last2)
							 (opr::vec-ptr-buffer first2) #'operator_==))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload is_permutation ((first1 cons_const_iterator) (last1 cons_const_iterator)
									  (first2 const-vector-pointer) (last2 const-vector-pointer) eql-bf)
	;;(format t "specialized is_permutation for cons_const_iterator & const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__is_permutation-imp-7b (__cons-itr-cons  first1)
							 (__cons-itr-cons  last1)
							 (opr::vec-ptr-index  first2)
							 (opr::vec-ptr-index  last2)
							 (opr::vec-ptr-buffer first2) (functor_function (clone eql-bf))))

  ;;PTN; is_permutation(0x14) : 8 - cvp x cvp
  (defmethod-overload is_permutation ((first1 const-vector-pointer) (last1 const-vector-pointer)
									  (first2 const-vector-pointer) (last2 const-vector-pointer))
	;;(format t "specialized is_permutation for const-vector-pointer & const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__is_permutation-imp-8b (opr::vec-ptr-index  first1)
							 (opr::vec-ptr-index  last1)
							 (opr::vec-ptr-buffer first1)
							 (opr::vec-ptr-index  first2)
							 (opr::vec-ptr-index  last2)
							 (opr::vec-ptr-buffer first2) #'operator_==))

  (defmethod-overload is_permutation ((first1 const-vector-pointer) (last1 const-vector-pointer)
									  (first2 const-vector-pointer) (last2 const-vector-pointer) eql-bf)
	;;(format t "specialized is_permutation for const-vector-pointer & const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__is_permutation-imp-8b (opr::vec-ptr-index  first1)
							 (opr::vec-ptr-index  last1)
							 (opr::vec-ptr-buffer first1)
							 (opr::vec-ptr-index  first2)
							 (opr::vec-ptr-index  last2)
							 (opr::vec-ptr-buffer first2) (functor_function (clone eql-bf)))))



; first1  : forward_iterator
; last1   : forward_iterator
; first2  : forward_iterator
; last2   : forward_iterator
; eql-bf  : binary_function ( default : #'operator_== )
; returns : iterator in [first1, last1)
(locally (declare (optimize speed))

  ;;PTN; search : 0 -  f  x  f 
  (labels ((__search-imp-0 (first1 last1 first2 last2 eql-bf)
			 (declare (type cl:function eql-bf))
			 (labels ((imp1 (itr1 itr2)
						(with-operators
							(for (nil (_/= itr2 last2) (progn ++itr1 ++itr2) :returns t)
							  (unless (funcall eql-bf *itr1 *itr2)
								(return-from imp1 nil)))))
					  (imp2 (itr1 wk1 wk2 len1 len2)
						(declare (type fixnum len1 len2))
						(with-operators
							(for (nil (<= len2 len1) (progn ++itr1 (decf len1)) :returns (_= itr1 last1))
							  (when (imp1 (_= wk1 itr1) (_= wk2 first2))
								(return-from imp2 itr1))))))
			   (let ((len1 (the fixnum (distance first1 last1)))
					 (len2 (the fixnum (distance first2 last2))))
				 (declare (type fixnum len1 len2))
				 (with-operators
					 (if (<= len2 0)
						 @~last1
						 (imp2 @~first1 @~first1 @~first2 len1 len2)))))))

	(defmethod-overload search ((first1 forward_iterator) (last1 forward_iterator)
								(first2 forward_iterator) (last2 forward_iterator))
	  (__search-imp-0 first1 last1 first2 last2 #'operator_==))

	(defmethod-overload search ((first1 forward_iterator) (last1 forward_iterator)
								(first2 forward_iterator) (last2 forward_iterator) eql-bf)
	  (__search-imp-0 first1 last1 first2 last2 (functor_function (clone eql-bf)))))


  ;;PTN; search : 1 -  f  x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__search-imp-1 (first1 last1 cons2 end2 eql-bf)
			 (declare (type cl:list cons2 end2))
			 (declare (type cl:function eql-bf))
			 (labels ((imp1 (itr1)
						(let ((cns2 cons2))
						  (declare (type cl:list cns2))
						  (with-operators
							  (for (nil (not (eq cns2 end2)) (progn ++itr1
																	(setf cns2 (cdr cns2))) :returns t)
								(unless (funcall eql-bf *itr1 (car cns2))
								  (return-from imp1 nil))))))
					  (imp2 (itr1 wk1 len1 len2)
						(declare (type fixnum len1 len2))
						(with-operators
							(for (nil (<= len2 len1) (progn ++itr1 (decf len1)) :returns (_= itr1 last1))
							  (when (imp1 (_= wk1 itr1))
								(return-from imp2 itr1))))))
			   (let ((len1 (the fixnum (distance first1 last1)))
					 (len2 (the fixnum (__conslist-count-nodes cons2 end2))))
				 (declare (type fixnum len1 len2))
				 (with-operators
					 (if (<= len2 0)
						 @~last1
						 (imp2 @~first1 @~first1 len1 len2)))))))

	(defmethod-overload search ((first1 forward_iterator) (last1 forward_iterator)
								(first2 cons_const_iterator) (last2 cons_const_iterator))
	  ;;(format t "specialized search for forward_iterator & cons_const_iterator is invoked.~%")
	  (__search-imp-1 first1 last1
					  (__cons-itr-cons first2)
					  (__cons-itr-cons  last2) #'operator_==))

	(defmethod-overload search ((first1 forward_iterator) (last1 forward_iterator)
								(first2 cons_const_iterator) (last2 cons_const_iterator) eql-bf)
	  ;;(format t "specialized search for forward_iterator & cons_const_iterator is invoked.~%")
	  (__search-imp-1 first1 last1
					  (__cons-itr-cons first2)
					  (__cons-itr-cons  last2) (functor_function (clone eql-bf)))))


  ;;PTN; search : 2 -  f  x cvp
  (labels ((__search-imp-2 (first1 last1 begin2 end2 buffer2 eql-bf)
			 (declare (type fixnum begin2 end2))
			 (declare (type cl:vector buffer2))
			 (declare (type cl:function eql-bf))
			 (labels ((imp1 (itr1)
						(let ((idx2 begin2))
						  (declare (type fixnum idx2))
						  (with-operators
							  (for (nil (< idx2 end2) (progn ++itr1 (incf idx2)) :returns t)
								(unless (funcall eql-bf *itr1 (aref buffer2 idx2))
								  (return-from imp1 nil))))))
					  (imp2 (itr1 wk1 len1 len2)
						(declare (type fixnum len1 len2))
						(with-operators
							(for (nil (<= len2 len1) (progn ++itr1 (decf len1)) :returns (_= itr1 last1))
							  (when (imp1 (_= wk1 itr1))
								(return-from imp2 itr1))))))
			   (let ((len1 (the fixnum (distance first1 last1)))
					 (len2 (the fixnum (- end2 begin2))))
				 (declare (type fixnum len1 len2))
				 (with-operators
					 (if (<= len2 0)
						 @~last1
						 (imp2 @~first1 @~first1 len1 len2)))))))

	(defmethod-overload search ((first1 forward_iterator)
								(last1  forward_iterator)
								(first2 const-vector-pointer) (last2 const-vector-pointer))
	  ;;(format t "specialized search for forward_iterator & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first2 last2)
	  (__search-imp-2 first1 last1
					  (opr::vec-ptr-index  first2)
					  (opr::vec-ptr-index  last2)
					  (opr::vec-ptr-buffer first2) #'operator_==))

	(defmethod-overload search ((first1 forward_iterator)
								(last1  forward_iterator)
								(first2 const-vector-pointer) (last2 const-vector-pointer) eql-bf)
	  ;;(format t "specialized search for forward_iterator & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first2 last2)
	  (__search-imp-2 first1 last1
					  (opr::vec-ptr-index  first2)
					  (opr::vec-ptr-index  last2)
					  (opr::vec-ptr-buffer first2) (functor_function (clone eql-bf)))))

  ;;PTN; search : 3 - cci x  f
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__search-imp-3 (cons1 end1 first2 last2 eql-bf)
			 (declare (type cl:list cons1 end1))
			 (declare (type cl:function eql-bf))
			 (labels ((imp1 (cns1 itr2)
						(declare (type cl:list cns1))
						(with-operators
							(for (nil (_/= itr2 last2) (progn (setf cns1 (cdr cns1)) ++itr2) :returns t)
							  (unless (funcall eql-bf (car cns1) *itr2)
								(return-from imp1 nil)))))
					  (imp2 (wk2 len2)
						(declare (type fixnum len2))
						(let ((cns1 cons1)
							  (len1 (__conslist-count-nodes cons1 end1)))
						  (declare (type cl:list cns1))
						  (declare (type fixnum len1))
						  (for (nil (<= len2 len1) (progn (setf cns1 (cdr cns1)) (decf len1)) :returns end1)
							(when (imp1 cns1 (_= wk2 first2))
							  (return-from imp2 cns1))))))
			   (let ((len2 (the fixnum (distance first2 last2))))
				 (declare (type fixnum len2))
				 (if (<= len2 0)
					 end1
					 (imp2 (clone first2) len2))))))

	(defmethod-overload search ((first1 cons_const_iterator)
								(last1  cons_const_iterator)
								(first2 forward_iterator) (last2 forward_iterator))
	  ;;(format t "specialized search for cons_const_iterator & forward_iterator is invoked.~%")
	  (__algo-make-cns-iterator first1
								 (__search-imp-3 (__cons-itr-cons first1)
												 (__cons-itr-cons  last1) first2 last2 #'operator_==)))

	(defmethod-overload search ((first1 cons_const_iterator)
								(last1  cons_const_iterator)
								(first2 forward_iterator) (last2 forward_iterator) eql-bf)
	  ;;(format t "specialized search for cons_const_iterator & forward_iterator is invoked.~%")
	  (__algo-make-cns-iterator first1
								 (__search-imp-3 (__cons-itr-cons first1)
												 (__cons-itr-cons  last1)
												 first2 last2 (functor_function (clone eql-bf))))))


  ;;PTN; search : 4 - cci x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__search-imp-4 (cons1 end1 cons2 end2 eql-bf)
			 (declare (type cl:list cons1 end1 cons2 end2))
			 (declare (type cl:function eql-bf))
			 (labels ((imp1 (cns1)
						(declare (type cl:list cns1))
						(let ((cns2 cons2))
						  (declare (type cl:list cns2))
						  (for (nil (not (eq cns2 end2)) (progn (setf cns1 (cdr cns1))
																(setf cns2 (cdr cns2))) :returns t)
							(unless (funcall eql-bf (car cns1) (car cns2))
							  (return-from imp1 nil)))))
					  (imp2 (len2)
						(declare (type fixnum len2))
						(let ((cns1 cons1)
							  (len1 (__conslist-count-nodes cons1 end1)))
						  (declare (type cl:list cns1))
						  (declare (type fixnum len1))
						  (for (nil (<= len2 len1) (progn (setf cns1 (cdr cns1)) (decf len1)) :returns end1)
							(when (imp1 cns1)
							  (return-from imp2 cns1))))))
			   (let ((len2 (__conslist-count-nodes cons2 end2)))
				 (declare (type fixnum len2))
				 (if (<= len2 0)
					 end1
					 (imp2 len2))))))

	(defmethod-overload search ((first1 cons_const_iterator) (last1 cons_const_iterator)
								(first2 cons_const_iterator) (last2 cons_const_iterator))
	  ;;(format t "specialized search for cons_const_iterator & cons_const_iterator is invoked.~%")
	  (__algo-make-cns-iterator first1
								 (__search-imp-4 (__cons-itr-cons first1)
												 (__cons-itr-cons  last1)
												 (__cons-itr-cons first2)
												 (__cons-itr-cons  last2) #'operator_==)))

	(defmethod-overload search ((first1 cons_const_iterator) (last1 cons_const_iterator)
								(first2 cons_const_iterator) (last2 cons_const_iterator) eql-bf)
	  ;;(format t "specialized search for cons_const_iterator & cons_const_iterator is invoked.~%")
	  (__algo-make-cns-iterator first1
								 (__search-imp-4 (__cons-itr-cons first1)
												 (__cons-itr-cons  last1)
												 (__cons-itr-cons first2)
												 (__cons-itr-cons  last2) (functor_function (clone eql-bf))))))

  ;;PTN; search : 5 - cci x cvp
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__search-imp-5 (cons1 end1 begin2 end2 buffer2 eql-bf)
			 (declare (type cl:list cons1 end1))
			 (declare (type fixnum begin2 end2))
			 (declare (type cl:vector buffer2))
			 (declare (type cl:function eql-bf))
			 (labels ((imp1 (cns1)
						(declare (type cl:list cns1))
						(let ((idx2 begin2))
						  (declare (type fixnum idx2))
						  (for (nil (< idx2 end2) (progn (setf cns1 (cdr cns1)) (incf idx2)) :returns t)
							(unless (funcall eql-bf (car cns1) (aref buffer2 idx2))
							  (return-from imp1 nil)))))
					  (imp2 (len2)
						(declare (type fixnum len2))
						(let ((cns1 cons1)
							  (len1 (__conslist-count-nodes cons1 end1)))
						  (declare (type cl:list cns1))
						  (declare (type fixnum len1))
						  (for (nil (<= len2 len1) (progn (setf cns1 (cdr cns1)) (decf len1)) :returns end1)
							(when (imp1 cns1)
							  (return-from imp2 cns1))))))
			   (let ((len2 (- end2 begin2)))
				 (declare (type fixnum len2))
				 (if (<= len2 0)
					 end1
					 (imp2 len2))))))

	(defmethod-overload search ((first1  cons_const_iterator) (last1  cons_const_iterator)
								(first2 const-vector-pointer) (last2 const-vector-pointer))
	  ;;(format t "specialized search for cons_const_iterator & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first2 last2)
	  (__algo-make-cns-iterator first1
								 (__search-imp-5 (__cons-itr-cons first1)
												 (__cons-itr-cons  last1)
												 (opr::vec-ptr-index  first2)
												 (opr::vec-ptr-index   last2)
												 (opr::vec-ptr-buffer first2) #'operator_==)))

	(defmethod-overload search ((first1  cons_const_iterator) (last1  cons_const_iterator)
								(first2 const-vector-pointer) (last2 const-vector-pointer) eql-bf)
	  ;;(format t "specialized search for cons_const_iterator & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first2 last2)
	  (__algo-make-cns-iterator first1
								 (__search-imp-5 (__cons-itr-cons first1)
												 (__cons-itr-cons  last1)
												 (opr::vec-ptr-index  first2)
												 (opr::vec-ptr-index   last2)
												 (opr::vec-ptr-buffer first2) (functor_function (clone eql-bf))))))

  ;;PTN; search : 6 - cvp x  f 
  (labels ((__search-imp-6 (begin1 end1 buffer1 first2 last2 eql-bf)
			 (declare (type fixnum begin1 end1))
			 (declare (type cl:vector buffer1))
			 (declare (type cl:function eql-bf))
			 (labels ((imp1 (idx1 itr2)
						(declare (type fixnum idx1))
						(with-operators
							(for (nil (_/= itr2 last2) (progn (incf idx1) ++itr2) :returns t)
							  (unless (funcall eql-bf (aref buffer1 idx1) *itr2)
								(return-from imp1 nil)))))
					  (imp2 (wk2 len2)
						(declare (type fixnum len2))
						(let ((idx1 begin1)
							  (len1 (- end1 begin1)))
						  (declare (type fixnum idx1 len1))
						  (for (nil (<= len2 len1) (progn (incf idx1) (decf len1)) :returns end1)
							(when (imp1 idx1 (_= wk2 first2))
							  (return-from imp2 idx1))))))
			   (let ((len2 (the fixnum (distance first2 last2))))
				 (declare (type fixnum len2))
				 (if (<= len2 0)
					 end1
					 (imp2 (clone first2) len2))))))

	(defmethod-overload search ((first1 const-vector-pointer)
								(last1  const-vector-pointer)
								(first2 forward_iterator) (last2 forward_iterator))
	  ;;(format t "specialized search for const-vector-pointer & forward_iterator is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__algo-make-vct-iterator first1
								 (__search-imp-6 (opr::vec-ptr-index  first1)
												 (opr::vec-ptr-index  last1)
												 (opr::vec-ptr-buffer first1) first2 last2 #'operator_==)))

	(defmethod-overload search ((first1 const-vector-pointer)
								(last1  const-vector-pointer)
								(first2 forward_iterator) (last2 forward_iterator) eql-bf)
	  ;;(format t "specialized search for const-vector-pointer & forward_iterator is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__algo-make-vct-iterator first1
								 (__search-imp-6 (opr::vec-ptr-index  first1)
												 (opr::vec-ptr-index  last1)
												 (opr::vec-ptr-buffer first1) first2 last2 (functor_function (clone eql-bf))))))


  ;;PTN; search : 7 - cvp x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__search-imp-7 (begin1 end1 buffer1 cons2 end2 eql-bf)
			 (declare (type fixnum begin1 end1))
			 (declare (type cl:vector buffer1))
			 (declare (type cl:list cons2 end2))
			 (declare (type cl:function eql-bf))
			 (labels ((imp1 (idx1)
						(declare (type fixnum idx1))
						(let ((cns2 cons2))
						  (declare (type cl:list cns2))
						  (for (nil (not (eq cns2 end2)) (progn (incf idx1)
																(setf cns2 (cdr cns2))) :returns t)
							(unless (funcall eql-bf (aref buffer1 idx1) (car cns2))
							  (return-from imp1 nil)))))
					  (imp2 (len2)
						(declare (type fixnum len2))
						(let ((idx1 begin1)
							  (len1 (- end1 begin1)))
						  (declare (type fixnum idx1 len1))
						  (for (nil (<= len2 len1) (progn (incf idx1) (decf len1)) :returns end1)
							(when (imp1 idx1)
							  (return-from imp2 idx1))))))
			   (let ((len2 (__conslist-count-nodes cons2 end2)))
				 (declare (type fixnum len2))
				 (if (<= len2 0)
					 end1
					 (imp2 len2))))))

	(defmethod-overload search ((first1 const-vector-pointer) (last1 const-vector-pointer)
								(first2  cons_const_iterator) (last2  cons_const_iterator))
	  ;;(format t "specialized search for const-vector-pointer & cons_const_iterator is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__algo-make-vct-iterator first1
								 (__search-imp-7 (opr::vec-ptr-index  first1)
												 (opr::vec-ptr-index   last1)
												 (opr::vec-ptr-buffer first1)
												 (__cons-itr-cons first2)
												 (__cons-itr-cons  last2) #'operator_==)))

	(defmethod-overload search ((first1 const-vector-pointer) (last1 const-vector-pointer)
								(first2  cons_const_iterator) (last2  cons_const_iterator) eql-bf)
	  ;;(format t "specialized search for const-vector-pointer & cons_const_iterator is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__algo-make-vct-iterator first1
								 (__search-imp-7 (opr::vec-ptr-index  first1)
												 (opr::vec-ptr-index   last1)
												 (opr::vec-ptr-buffer first1)
												 (__cons-itr-cons first2)
												 (__cons-itr-cons  last2) (functor_function (clone eql-bf))))))

  ;;PTN; search : 8 - cvp x cvp
  (labels ((__search-imp-8 (begin1 end1 buffer1 begin2 end2 buffer2 eql-bf)
			 (declare (type fixnum begin1 end1 begin2 end2))
			 (declare (type cl:vector buffer1 buffer2))
			 (declare (type cl:function eql-bf))
			 (labels ((imp1 (idx1)
						(declare (type fixnum idx1))
						(let ((idx2 begin2))
						  (declare (type fixnum idx2))
						  (for (nil (< idx2 end2) (progn (incf idx1) (incf idx2)) :returns t)
							(unless (funcall eql-bf (aref buffer1 idx1)
											 (aref buffer2 idx2))
							  (return-from imp1 nil)))))
					  (imp2 (len2)
						(declare (type fixnum len2))
						(let ((idx1 begin1)
							  (len1 (- end1 begin1)))
						  (declare (type fixnum idx1 len1))
						  (for (nil (<= len2 len1) (progn (incf idx1) (decf len1)) :returns end1)
							(when (imp1 idx1)
							  (return-from imp2 idx1))))))
			   (let ((len2 (- end2 begin2)))
				 (declare (type fixnum len2))
				 (if (<= len2 0)
					 end1
					 (imp2 len2))))))

	(defmethod-overload search ((first1 const-vector-pointer) (last1 const-vector-pointer)
								(first2 const-vector-pointer) (last2 const-vector-pointer))
	  ;;(format t "specialized search for const-vector-pointer & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__pointer-check-iterator-range first2 last2)
	  (__algo-make-vct-iterator first1
								 (__search-imp-8 (opr::vec-ptr-index  first1)
												 (opr::vec-ptr-index  last1)
												 (opr::vec-ptr-buffer first1)
												 (opr::vec-ptr-index  first2)
												 (opr::vec-ptr-index  last2)
												 (opr::vec-ptr-buffer first2) #'operator_==)))

	(defmethod-overload search ((first1 const-vector-pointer) (last1 const-vector-pointer)
								(first2 const-vector-pointer) (last2 const-vector-pointer) eql-bf)
	  ;;(format t "specialized search for const-vector-pointer & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__pointer-check-iterator-range first2 last2)
	  (__algo-make-vct-iterator first1
								 (__search-imp-8 (opr::vec-ptr-index  first1)
												 (opr::vec-ptr-index  last1)
												 (opr::vec-ptr-buffer first1)
												 (opr::vec-ptr-index  first2)
												 (opr::vec-ptr-index  last2)
												 (opr::vec-ptr-buffer first2) (functor_function (clone eql-bf)))))))




; first   : forward_iterator
; last    : forward_iterator
; count   : fixnum
; eql-bf  : binary_function ( default : #'operator_== )
; returns : iterator in [first, last)
(locally (declare (optimize speed))

  ;;PTN; search_n : 0 -  f 
  (labels ((__search_n-imp-0 (first last cnt val eql-bf)
			 (declare (type fixnum cnt))
			 (declare (type cl:function eql-bf))
			 (__error-unless-non-negative-fixnum search_n cnt)
			 (labels ((imp-1 (itr n)
						(declare (type fixnum n))
						(with-operators
							(for (nil (< 0 n) (progn ++itr (decf n)) :returns t)
							  (unless (funcall eql-bf val *itr)
								(return-from imp-1 nil)))))
					  (imp-2 (itr wk-itr dist)
						(declare (type fixnum dist))
						(with-operators
							(for (nil (<= cnt dist) (progn ++itr (decf dist)) :returns (_= itr last))
							  (when (imp-1 (_= wk-itr itr) cnt)
								(return-from imp-2 itr))))))
			   (let ((distance (the fixnum (distance first last))))
				 (declare (type fixnum distance))
				 (with-operators
					 (if (< distance cnt)
						 @~last
						 (imp-2 @~first @~first distance)))))))

	(defmethod-overload search_n ((first forward_iterator)
								  (last  forward_iterator) (cnt integer) val)
	  (__search_n-imp-0 first last cnt val #'operator_==))

	(defmethod-overload search_n ((first forward_iterator)
								  (last  forward_iterator) (cnt integer) val eql-bf)
	  (__search_n-imp-0 first last cnt val (functor_function (clone eql-bf)))))

  
  ;;PTN; search_n : 1 - cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__search_n-imp-1 (cons1 cons2 cnt val eql-bf)
			 (declare (type fixnum cnt))
			 (declare (type cl:list cons1 cons2))
			 (declare (type cl:function eql-bf))
			 (labels ((imp-1 (cns)
						(declare (type cl:list cns))
						(let ((n cnt))
						  (declare (type fixnum n))
						  (for (nil (< 0 n) (progn (decf n)
												   (setf cns (cdr cns))) :returns t)
							(unless (funcall eql-bf val (car cns))
							  (return-from imp-1 nil)))))
					  (imp-2 (dist)
						(declare (type fixnum dist))
						(let ((cns cons1))
						  (declare (type cl:list cns))
						  (for (nil (<= cnt dist) (progn (decf dist)
														 (setf cns (cdr cns))) :returns cons2)
							(when (imp-1 cns)
							  (return-from imp-2 cns))))))
			   (let ((distance (__conslist-count-nodes cons1 cons2)))
				 (declare (type fixnum distance))
				 (if (< distance cnt)
					 cons2
					 (imp-2 distance))))))

	(defmethod-overload search_n ((first cons_const_iterator)
								  (last  cons_const_iterator) (count integer) val)
	  (__error-unless-non-negative-fixnum search_n count)
	  ;;(format t "specialized search_n for cons_const_iterator is invoked.~%")
	  (__algo-make-cns-iterator first
								 (__search_n-imp-1 (__cons-itr-cons first)
												   (__cons-itr-cons  last) count val #'operator_==)))

	(defmethod-overload search_n ((first cons_const_iterator)
								  (last  cons_const_iterator) (count integer) val eql-bf)
	  (__error-unless-non-negative-fixnum search_n count)
	  ;;(format t "specialized search_n for cons_const_iterator is invoked.~%")
	  (__algo-make-cns-iterator first
								 (__search_n-imp-1 (__cons-itr-cons first)
												   (__cons-itr-cons  last)
												   count val (functor_function (clone eql-bf))))))

  
  ;;PTN; search_n : 2 - cvp
  (labels ((__search_n-imp-2 (idx1 idx2 buffer cnt val eql-bf)
			 (declare (type fixnum idx1 idx2 cnt))
			 (declare (type cl:vector buffer))
			 (declare (type cl:function eql-bf))
			 (labels ((imp-2 (idx)
						(declare (type fixnum idx))
						(let ((n cnt))
						  (declare (type fixnum n))
						  (for (nil (< 0 n) (progn (incf idx) (decf n)) :returns t)
							(unless (funcall eql-bf val (aref buffer idx))
							  (return-from imp-2 nil)))))
					  (imp-2 (dist)
						(declare (type fixnum dist))
						(let ((idx idx1))
						  (declare (type fixnum idx))
						  (for (nil (<= cnt dist) (progn (incf idx) (decf dist)) :returns idx2)
							(when (imp-2 idx)
							  (return-from imp-2 idx))))))
			   (let ((distance (- idx2 idx1)))
				 (declare (type fixnum distance))
				 (if (< distance cnt)
					 idx2
					 (imp-2 distance))))))

	(defmethod-overload search_n ((first const-vector-pointer)
								  (last  const-vector-pointer) (count integer) val)
	  (__error-unless-non-negative-fixnum search_n count)
	  ;;(format t "specialized search_n for const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__algo-make-vct-iterator first
								 (__search_n-imp-2 (opr::vec-ptr-index  first)
												   (opr::vec-ptr-index  last)
												   (opr::vec-ptr-buffer first) count val #'operator_==)))

	(defmethod-overload search_n ((first const-vector-pointer)
								  (last  const-vector-pointer) (count integer) val eql-bf)
	  (__error-unless-non-negative-fixnum search_n count)
	  ;;(format t "specialized search_n for const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__algo-make-vct-iterator first
								 (__search_n-imp-2 (opr::vec-ptr-index  first)
												   (opr::vec-ptr-index  last)
												   (opr::vec-ptr-buffer first)
												   count val (functor_function (clone eql-bf)))))))




;;------------------------------------------------------------------------------
;; 25.2, modifying sequence operations:
;;------------------------------------------------------------------------------
;; 25.2.1, copy:

; first   : input_iterator
; last    : input_iterator
; result  : output_iterator
; returns : copy of result.
(locally (declare (optimize speed))

  ;;PTN; copy : 0 -  i  x  o 
  (defmethod copy ((first input_iterator) (last input_iterator) (result output_iterator))
	(__copy-imp-0 first last result))

  ;;PTN; copy : 1 - cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod copy ((first cons_const_iterator) (last cons_const_iterator) (result output_iterator))
	;;(format t "specialized copy for cons_const_iterator & output_iterator is invoked.~%")
	(__copy-imp-1 (__cons-itr-cons first) (__cons-itr-cons last) (clone result)))

  ;;PTN; copy : 2 - cvp x  o 
  (defmethod copy ((first const-vector-pointer) (last const-vector-pointer) (result output_iterator))
	;;(format t "specialized copy for const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first last)
	(__copy-imp-2 (opr::vec-ptr-index  first)
				  (opr::vec-ptr-index  last)
				  (opr::vec-ptr-buffer first) (clone result)))

  ;;PTN; copy : 3 -  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod copy ((first input_iterator) (last input_iterator) (result cons_iterator))
	;;(format t "specialized copy for input_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__copy-imp-3 first last (__cons-itr-cons result))))

  ;;PTN; copy : 4 - cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod copy ((first cons_const_iterator) (last cons_const_iterator) (result cons_iterator))
	;;(format t "specialized copy for cons_const_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__copy-imp-4 (__cons-itr-cons first)
											 (__cons-itr-cons  last) (__cons-itr-cons result))))

  ;;PTN; copy : 5 - cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod copy ((first const-vector-pointer) (last const-vector-pointer) (result cons_iterator))
	;;(format t "specialized copy for const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first last)
	(__algo-make-cns-iterator result
							   (__copy-imp-5 (opr::vec-ptr-index  first)
											 (opr::vec-ptr-index   last)
											 (opr::vec-ptr-buffer first) (__cons-itr-cons result))))

  ;;PTN; copy : 6 -  i  x  vp
  (defmethod copy ((first input_iterator) (last input_iterator) (result vector-pointer))
	;;(format t "specialized copy for input_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__copy-imp-6 first last
											 (opr::vec-ptr-index  result)
											 (opr::vec-ptr-buffer result))))

  ;;PTN; copy : 7 - cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod copy ((first cons_const_iterator) (last cons_const_iterator) (result vector-pointer))
	;;(format t "specialized copy for cons_const_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__copy-imp-7 (__cons-itr-cons      first)
											 (__cons-itr-cons       last)
											 (opr::vec-ptr-index  result)
											 (opr::vec-ptr-buffer result))))

  ;;PTN; copy : 8 - cvp x  vp
  (defmethod copy ((first const-vector-pointer) (last const-vector-pointer) (result vector-pointer))
	;;(format t "specialized copy for const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(__algo-make-vct-iterator result
							   (__copy-imp-8 (opr::vec-ptr-index  first)
											 (opr::vec-ptr-index  last)
											 (opr::vec-ptr-buffer first)
											 (opr::vec-ptr-index  result)
											 (opr::vec-ptr-buffer result)))))



; first   : input_iterator
; n       : fixnum
; result  : output_iterator
; returns : copy of result.
#-cl-stl-0x98    ; copy_n
(locally (declare (optimize speed))

  ;;PTN; copy_n : 0 -  i  x  o 
  (defmethod copy_n ((first input_iterator) (n integer) (result output_iterator))
	(declare (type fixnum n))
	(__error-unless-non-negative-fixnum copy_n n)
	(with-operators
		(let ((dest @~result))
		  (if (<= n 0)
			  dest
			  (for (((itr @~first)) (< 0 n) (progn ++itr ++dest (decf n)) :returns dest)
				(_= *dest *itr))))))

  ;;PTN; copy_n : 1 - cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod copy_n ((first cons_const_iterator) (n integer) (result output_iterator))
	;;(format t "specialized copy_n for cons_const_iterator & output_iterator is invoked.~%")
	(__error-unless-non-negative-fixnum copy_n n)
	(let ((cns  (__cons-itr-cons first))
		  (oitr (clone result)))
	  (declare (type fixnum    n))
	  (declare (type cl:list   cns))
	  (with-operators
		  (for (nil (< 0 n) (progn (setf cns (cdr cns)) ++oitr (decf n)) :returns oitr)
			(_= *oitr (car cns))))))

  ;;PTN; copy_n : 2 - cvp x  o 
  (defmethod copy_n ((first const-vector-pointer) (n integer) (result output_iterator))
	;;(format t "specialized copy_n for const-vector-pointer & output_iterator is invoked.~%")
	(__error-unless-non-negative-fixnum copy_n n)
	(let ((idx    (opr::vec-ptr-index first))
		  (buffer (opr::vec-ptr-buffer first))
		  (oitr   (clone result)))
	  (declare (type fixnum idx n))
	  (declare (type cl:vector buffer))
	  (with-operators
		  (for (nil (< 0 n) (progn (incf idx) ++oitr (decf n)) :returns oitr)
			(_= *oitr (aref buffer idx))))))

  ;;PTN; copy_n : 3 -  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod copy_n ((first input_iterator) (n integer) (result cons_iterator))
	;;(format t "specialized copy_n for input_iterator & cons_iterator is invoked.~%")
	(__error-unless-non-negative-fixnum copy_n n)
	(let ((dest (__cons-itr-cons result)))
	  (declare (type fixnum  n))
	  (declare (type cl:list dest))
	  (with-operators
		  (for (((itr @~first)) (< 0 n) (progn ++itr (setf dest (cdr dest)) (decf n))
										:returns (__algo-make-cns-iterator result dest))
			(_= (car dest) *itr)))))

  ;;PTN; copy_n : 4 - cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod copy_n ((first cons_const_iterator) (n integer) (result cons_iterator))
	;;(format t "specialized copy_n for cons_const_iterator & cons_iterator is invoked.~%")
	(__error-unless-non-negative-fixnum copy_n n)
	(let ((cns  (__cons-itr-cons first))
		  (dest (__cons-itr-cons result)))
	  (declare (type fixnum  n))
	  (declare (type cl:list cns dest))
	  (for (nil (< 0 n) (progn (setf cns (cdr cns)) (setf dest (cdr dest)) (decf n))
						:returns (__algo-make-cns-iterator result dest))
		(_= (car dest) (car cns)))))

  ;;PTN; copy_n : 5 - cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod copy_n ((first const-vector-pointer) (n integer) (result cons_iterator))
	;;(format t "specialized copy_n for const-vector-pointer & cons_iterator is invoked.~%")
	(__error-unless-non-negative-fixnum copy_n n)
	(let ((idx  (opr::vec-ptr-index  first))
		  (buf  (opr::vec-ptr-buffer first))
		  (dest (__cons-itr-cons     result)))
	  (declare (type fixnum    idx n))
	  (declare (type cl:list   dest))
	  (declare (type cl:vector buf))
	  (for (nil (< 0 n) (progn (incf idx) (setf dest (cdr dest)) (decf n))
						:returns (__algo-make-cns-iterator result dest))
		(_= (car dest) (aref buf idx)))))
  
  ;;PTN; copy_n : 6 -  i  x  vp
  (defmethod copy_n ((first input_iterator) (n integer) (result vector-pointer))
	;;(format t "specialized copy_n for input_iterator & vector-pointer is invoked.~%")
	(__error-unless-non-negative-fixnum copy_n n)
	(let ((out-idx (opr::vec-ptr-index  result))
		  (out-buf (opr::vec-ptr-buffer result)))
	  (declare (type fixnum n out-idx))
	  (declare (type cl:vector out-buf))
	  (with-operators
		  (for (((itr @~first)) (< 0 n) (progn ++itr (incf out-idx) (decf n))
										:returns (__algo-make-vct-iterator result out-idx))
			(_= (aref out-buf out-idx) *itr)))))

  ;;PTN; copy_n : 7 - cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod copy_n ((first cons_const_iterator) (n integer) (result vector-pointer))
	;;(format t "specialized copy_n for cons_const_iterator & vector-pointer is invoked.~%")
	(__error-unless-non-negative-fixnum copy_n n)
	(let ((cns     (__cons-itr-cons     first))
		  (out-idx (opr::vec-ptr-index  result))
		  (out-buf (opr::vec-ptr-buffer result)))
	  (declare (type fixnum    n out-idx))
	  (declare (type cl:list   cns))
	  (declare (type cl:vector out-buf))
	  (for (nil (< 0 n) (progn (setf cns (cdr cns)) (incf out-idx) (decf n))
						:returns (__algo-make-vct-iterator result out-idx))
		(_= (aref out-buf out-idx) (car cns)))))

  ;;PTN; copy_n : 8 - cvp x  vp
  (defmethod copy_n ((first const-vector-pointer) (n integer) (result vector-pointer))
	;;(format t "specialized copy_n for const-vector-pointer & vector-pointer is invoked.~%")
	(__error-unless-non-negative-fixnum copy_n n)
	(let ((idx     (opr::vec-ptr-index  first))
		  (src-buf (opr::vec-ptr-buffer first))
		  (out-idx (opr::vec-ptr-index  result))
		  (out-buf (opr::vec-ptr-buffer result)))
	  (declare (type fixnum idx n out-idx))
	  (declare (type cl:vector src-buf out-buf))
	  (for (nil (< 0 n) (progn (incf idx) (incf out-idx) (decf n))
						:returns (__algo-make-vct-iterator result out-idx))
		(_= (aref out-buf out-idx) (aref src-buf idx))))))




; first   : input_iterator
; last    : input_iterator
; result  : output_iterator
; pred    : unary_function
; returns : copy of result.
#-cl-stl-0x98    ; copy_if
(locally (declare (optimize speed))

  ;;PTN; copy_if : 0 -  i  x  o 
  (defmethod copy_if ((first input_iterator)
					  (last  input_iterator) (result output_iterator) pred)
	(with-operators
		(let ((dest @~result))
		  (if (_== first last)
			  dest
			  (let ((pred (functor_function @~pred)))
				(declare (type cl:function pred))
				(for (((itr @~first)) (_/= itr last) ++itr :returns dest)
				  (let ((v *itr))
					(when (funcall pred v)
					  (_= *dest v)
					  ++dest))))))))

  ;;PTN; copy_if : 1 - cci x  o
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod copy_if ((first cons_const_iterator)
					  (last  cons_const_iterator) (result output_iterator) pred)
	;;(format t "specialized copy_if for cons_const_iterator & output_iterator is invoked.~%")
	(with-operators
		(let ((cons1 (__cons-itr-cons first))
			  (cons2 (__cons-itr-cons  last))
			  (dest  @~result))
		  (declare (type cl:list cons1 cons2))
		  (if (eq cons1 cons2)
			  dest
			  (let ((pred (functor_function @~pred)))
				(declare (type cl:function pred))
				(for (nil (not (eq cons1 cons2)) (setf cons1 (cdr cons1)) :returns dest)
				  (let ((v (car cons1)))
					(when (funcall pred v)
					  (_= *dest v)
					  ++dest))))))))
	

  ;;PTN; copy_if : 2 - cvp x  o 
  (defmethod copy_if ((first const-vector-pointer)
					  (last  const-vector-pointer) (result output_iterator) pred)
	;;(format t "specialized copy_if for const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1   (opr::vec-ptr-index  first))
		  (idx2   (opr::vec-ptr-index   last))
		  (buffer (opr::vec-ptr-buffer first))
		  (dest   (clone result)))
	  (declare (type fixnum idx1 idx2))
	  (declare (type cl:vector buffer))
	  (if (= idx1 idx2)
		  dest
		  (let ((pred (functor_function (clone pred))))
			(declare (type cl:function pred))
			(with-operators
				(for (nil (< idx1 idx2) (incf idx1) :returns dest)
				  (let ((v (aref buffer idx1)))
					(when (funcall pred v)
					  (_= *dest v)
					  ++dest))))))))

  ;;PTN; copy_if : 3 -  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod copy_if ((first input_iterator)
					  (last  input_iterator) (result cons_iterator) pred)
	;;(format t "specialized copy_if for input_iterator & cons_iterator is invoked.~%")
	(if (_== first last)
		(clone result)
		(let ((dest (__cons-itr-cons result))
			  (pred (functor_function (clone pred))))
		  (declare (type cl:list     dest))
		  (declare (type cl:function pred))
		  (with-operators
			  (for (((itr @~first)) (_/= itr last) ++itr :returns (__algo-make-cns-iterator result dest))
				(let ((v *itr))
				  (when (funcall pred v)
					(_= (car dest) v)
					(setf dest (cdr dest)))))))))


  ;;PTN; copy_if : 4 - cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod copy_if ((first cons_const_iterator)
					  (last  cons_const_iterator) (result cons_iterator) pred)
	;;(format t "specialized copy_if for cons_const_iterator & cons_iterator is invoked.~%")
	(let ((cons1 (__cons-itr-cons first))
		  (cons2 (__cons-itr-cons  last)))
	  (declare (type cl:list cons1 cons2))
	  (if (eq cons1 cons2)
		  (clone result)
		  (let ((dest (__cons-itr-cons result))
				(pred (functor_function (clone pred))))
			(declare (type cl:list     dest))
			(declare (type cl:function pred))
			(for (nil (not (eq cons1 cons2)) (setf cons1 (cdr cons1)) :returns (__algo-make-cns-iterator result dest))
			  (let ((v (car cons1)))
				(when (funcall pred v)
				  (_= (car dest) v)
				  (setf dest (cdr dest)))))))))


  ;;PTN; copy_if : 5 - cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod copy_if ((first const-vector-pointer)
					  (last  const-vector-pointer) (result cons_iterator) pred)
	;;(format t "specialized copy_if for const-vector-pointer & cons_iterator is invoked.~%")
	(let ((idx1    (opr::vec-ptr-index  first))
		  (idx2    (opr::vec-ptr-index   last))
		  (src-buf (opr::vec-ptr-buffer first)))
	  (declare (type fixnum idx1 idx2))
	  (declare (type cl:vector src-buf))
	  (if (= idx1 idx2)
		  (clone result)
		  (let ((dest (__cons-itr-cons result))
				(pred (functor_function (clone pred))))
			(declare (type cl:list     dest))
			(declare (type cl:function pred))
			(for (nil (< idx1 idx2) (incf idx1) :returns (__algo-make-cns-iterator result dest))
			  (let ((v (aref src-buf idx1)))
				(when (funcall pred v)
				  (_= (car dest) v)
				  (setf dest (cdr dest)))))))))


  ;;PTN; copy_if : 6 -  i  x  vp
  (defmethod copy_if ((first input_iterator)
					  (last  input_iterator) (result vector-pointer) pred)
	;;(format t "specialized copy_if for input_iterator & vector-pointer is invoked.~%")
	(if (_== first last)
		(clone result)
		(let ((out-idx (opr::vec-ptr-index  result))
			  (out-buf (opr::vec-ptr-buffer result))
			  (pred    (functor_function (clone pred))))
		  (declare (type fixnum out-idx))
		  (declare (type cl:vector out-buf))
		  (declare (type cl:function pred))
		  (with-operators
			  (for (((itr @~first)) (_/= itr last) ++itr
									:returns (__algo-make-vct-iterator result out-idx))
				(let ((v *itr))
				  (when (funcall pred v)
					(_= (aref out-buf out-idx) v)
					(incf out-idx))))))))


  ;;PTN; copy_if : 7 - cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod copy_if ((first cons_const_iterator)
					  (last  cons_const_iterator) (result vector-pointer) pred)
	;;(format t "specialized copy_if for cons_const_iterator & vector-pointer is invoked.~%")
	(let ((cons1 (__cons-itr-cons first))
		  (cons2 (__cons-itr-cons  last)))
	  (declare (type cl:list cons1 cons2))
	  (if (eq cons1 cons2)
		  (clone result)
		  (let ((out-idx (opr::vec-ptr-index  result))
				(out-buf (opr::vec-ptr-buffer result))
				(pred    (functor_function (clone pred))))
			(declare (type fixnum      out-idx))
			(declare (type cl:vector   out-buf))
			(declare (type cl:function pred))
			(for (nil (not (eq cons1 cons2)) (setf cons1 (cdr cons1))
											 :returns (__algo-make-vct-iterator result out-idx))
			  (let ((v (car cons1)))
				(when (funcall pred v)
				  (_= (aref out-buf out-idx) v)
				  (incf out-idx))))))))
	

  ;;PTN; copy_if : 8 - cvp x  vp
  (defmethod copy_if ((first const-vector-pointer)
					  (last  const-vector-pointer) (result vector-pointer) pred)
	;;(format t "specialized copy_if for const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1    (opr::vec-ptr-index  first))
		  (idx2    (opr::vec-ptr-index   last))
		  (src-buf (opr::vec-ptr-buffer first)))
	  (declare (type fixnum idx1 idx2))
	  (declare (type cl:vector src-buf))
	  (if (= idx1 idx2)
		  (clone result)
		  (let ((out-idx (opr::vec-ptr-index  result))
				(out-buf (opr::vec-ptr-buffer result))
				(pred    (functor_function (clone pred))))
			(declare (type fixnum out-idx))
			(declare (type cl:vector out-buf))
			(declare (type cl:function pred))
			(for (nil (< idx1 idx2) (incf idx1)
									:returns (__algo-make-vct-iterator result out-idx))
			  (let ((v (aref src-buf idx1)))
				(when (funcall pred v)
				  (_= (aref out-buf out-idx) v)
				  (incf out-idx)))))))))



; first   : bidirectional_iterator
; last    : bidirectional_iterator
; result  : bidirectional_iterator
; returns : copy of result.
(locally (declare (optimize speed))

  ;;PTN; copy_backward : 0 -  b  x  b 
  (defmethod copy_backward ((first bidirectional_iterator)
							(last  bidirectional_iterator) (result bidirectional_iterator))
	(__copy_backward-imp-0 first last result))

  ;;PTN; copy_backward : 1 - cvp x  b 
  (defmethod copy_backward ((first const-vector-pointer)
							(last  const-vector-pointer) (result bidirectional_iterator))
	;;(format t "specialized copy_backward for const-vector-pointer & bidirectional_iterator is invoked.~%")
	(__pointer-check-iterator-range first last)
	(__copy_backward-imp-1 (opr::vec-ptr-index  first)
						   (opr::vec-ptr-index  last)
						   (opr::vec-ptr-buffer first) (clone result)))

  ;;PTN; copy_backward : 2 -  b  x  vp
  (defmethod copy_backward ((first bidirectional_iterator)
							(last  bidirectional_iterator) (result vector-pointer))
	;;(format t "specialized copy_backward for bidirectional_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__copy_backward-imp-2 first last
													  (opr::vec-ptr-index  result)
													  (opr::vec-ptr-buffer result))))

  ;;PTN; copy_backward : 3 - cvp x  vp
  (defmethod copy_backward ((first const-vector-pointer)
							(last  const-vector-pointer) (result vector-pointer))
	;;(format t "specialized copy_backward for const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(__algo-make-vct-iterator result
							   (__copy_backward-imp-3 (opr::vec-ptr-index  first)
													  (opr::vec-ptr-index  last)
													  (opr::vec-ptr-buffer first)
													  (opr::vec-ptr-index  result)
													  (opr::vec-ptr-buffer result)))))




; first   : input_iterator
; last    : input_iterator
; result  : output_iterator
; returns : copy of result.
#-cl-stl-0x98
(locally (declare (optimize speed))

  ;;PTN; move : 0 -  i  x  o 
  (defmethod move ((first input_iterator)
				   (last  input_iterator) (result output_iterator))
	(__move-imp-0 first last result))

  ;;PTN; move : 1 - cci x  o
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod move ((first cons_const_iterator)
				   (last  cons_const_iterator) (result output_iterator))
	(__move-imp-1 (__cons-itr-cons first)
				  (__cons-itr-cons  last) (clone result)))

  ;;PTN; move : 2 - cvp x  o 
  (defmethod move ((first const-vector-pointer)
				   (last  const-vector-pointer) (result output_iterator))
	;;(format t "specialized move for const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first last)
	(__move-imp-2 (opr::vec-ptr-index  first)
				  (opr::vec-ptr-index  last)
				  (opr::vec-ptr-buffer first) (clone result)))

  ;;PTN; move : 3 -  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod move ((first input_iterator)
				   (last  input_iterator) (result cons_iterator))
	;;(format t "specialized move for input_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__move-imp-3 first last
											 (__cons-itr-cons result))))

  ;;PTN; move : 4 - cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod move ((first cons_const_iterator)
				   (last  cons_const_iterator) (result cons_iterator))
	;;(format t "specialized move for const-vector-pointer & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__move-imp-4 (__cons-itr-cons  first)
											 (__cons-itr-cons   last)
											 (__cons-itr-cons result))))

  ;;PTN; move : 5 - cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod move ((first const-vector-pointer)
				   (last  const-vector-pointer) (result cons_iterator))
	;;(format t "specialized move for const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first last)
	(__algo-make-cns-iterator result
							   (__move-imp-5 (opr::vec-ptr-index  first)
											 (opr::vec-ptr-index  last)
											 (opr::vec-ptr-buffer first)
											 (__cons-itr-cons result))))

  ;;PTN; move : 6 -  i  x  vp
  (defmethod move ((first input_iterator)
				   (last  input_iterator) (result vector-pointer))
	;;(format t "specialized move for input_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__move-imp-6 first last
											 (opr::vec-ptr-index  result)
											 (opr::vec-ptr-buffer result))))

  ;;PTN; move : 7 - cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod move ((first cons_const_iterator)
				   (last  cons_const_iterator) (result vector-pointer))
	;;(format t "specialized move for const-vector-pointer & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__move-imp-7 (__cons-itr-cons first)
											 (__cons-itr-cons  last)
											 (opr::vec-ptr-index  result)
											 (opr::vec-ptr-buffer result))))

  ;;PTN; move : 8 - cvp x  vp
  (defmethod move ((first const-vector-pointer)
				   (last  const-vector-pointer) (result vector-pointer))
	;;(format t "specialized move for const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(__algo-make-vct-iterator result
							   (__move-imp-8 (opr::vec-ptr-index  first)
											 (opr::vec-ptr-index  last)
											 (opr::vec-ptr-buffer first)
											 (opr::vec-ptr-index  result)
											 (opr::vec-ptr-buffer result)))))




; first   : bidirectional_iterator
; last    : bidirectional_iterator
; result  : bidirectional_iterator
; returns : copy of result.
#-cl-stl-0x98
(locally (declare (optimize speed))

  ;;PTN; move_backward : 0 -  b  x  b 
  (defmethod move_backward ((first bidirectional_iterator)
							(last  bidirectional_iterator) (result bidirectional_iterator))
	(__move_backward-imp-0 first last result))

  ;;PTN; move_backward : 1 - cvp x  b 
  (defmethod move_backward ((first const-vector-pointer)
							(last  const-vector-pointer) (result bidirectional_iterator))
	;;(format t "specialized move_backward for const-vector-pointer & bidirectional_iterator is invoked.~%")
	(__pointer-check-iterator-range first last)
	(__move_backward-imp-1 (opr::vec-ptr-index  first)
						   (opr::vec-ptr-index  last)
						   (opr::vec-ptr-buffer first) (clone result)))

  ;;PTN; move_backward : 2 -  b  x  vp
  (defmethod move_backward ((first bidirectional_iterator)
							(last  bidirectional_iterator) (result vector-pointer))
	;;(format t "specialized move_backward for bidirectional_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__move_backward-imp-2 first last
													  (opr::vec-ptr-index  result)
													  (opr::vec-ptr-buffer result))))

  ;;PTN; move_backward : 3 - cvp x  vp
  (defmethod move_backward ((first const-vector-pointer)
							(last  const-vector-pointer) (result vector-pointer))
	;;(format t "specialized move_backward for const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(__algo-make-vct-iterator result
							   (__move_backward-imp-3 (opr::vec-ptr-index  first)
													  (opr::vec-ptr-index  last)
													  (opr::vec-ptr-buffer first)
													  (opr::vec-ptr-index  result)
													  (opr::vec-ptr-buffer result)))))





;; 25.2.2, swap:

;#+cl-stl-0x98 (defmethod-overload swap (a b) ...)    ; move to CL-OPERATOR
;#-cl-stl-0x98 (defmethod-overload swap (a b) ... )   ; move to CL-OPERATOR
;#-cl-stl-0x98 (defmethod-overload swap ((arr1 cl:vector) (arr2 cl:vector)) ... )  ; move to CL-OPERATOR
;#-cl-stl-0x98 (defmethod-overload swap ((str1 cl:string) (str2 cl:string)) ... )) ; move to CL-OPERATOR
;#-cl-stl-noextra (defmethod-overload swap ((lst1 cl:list) (lst2 cl:list)) ...))   ; move to CL-OPERATOR




; first1  : forward_iterator
; last1   : forward_iterator
; first2  : forward_iterator
; returns : copy of first2.
(locally (declare (optimize speed))

  ;;PTN; swap_ranges : 0 -  f  x  f 
  (defmethod swap_ranges ((first1 forward_iterator)
						  (last1  forward_iterator) (first2 forward_iterator))
	(__swap_ranges-imp-0 first1 last1 first2))

  ;;PTN; swap_ranges : 1 -  ci x  f 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod swap_ranges ((first1 cons_iterator)
						  (last1  cons_iterator) (first2 forward_iterator))
	;;(format t "specialized swap_ranges for cons_iterator & forward_iterator is invoked.~%")
	(__swap_ranges-imp-1 (__cons-itr-cons first1)
						 (__cons-itr-cons  last1) first2))

  ;;PTN; swap_ranges : 2 -  vp x  f 
  (defmethod swap_ranges ((first1 vector-pointer)
						  (last1  vector-pointer) (first2 forward_iterator))
	;;(format t "specialized swap_ranges for vector-pointer & forward_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__swap_ranges-imp-2 (opr::vec-ptr-index  first1)
						 (opr::vec-ptr-index  last1)
						 (opr::vec-ptr-buffer first1) first2))

  ;;PTN; swap_ranges : 3 -  f  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod swap_ranges ((first1 forward_iterator)
						  (last1  forward_iterator) (first2 cons_iterator))
	;;(format t "specialized swap_ranges for forward_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator first2
							   (__swap_ranges-imp-3 first1 last1
													(__cons-itr-cons first2))))


  ;;PTN; swap_ranges : 4 -  ci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod swap_ranges ((first1 cons_iterator)
						  (last1  cons_iterator) (first2 cons_iterator))
	;;(format t "specialized swap_ranges for cons_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator first2
							   (__swap_ranges-imp-4 (__cons-itr-cons first1)
													(__cons-itr-cons  last1)
													(__cons-itr-cons first2))))

  ;;PTN; swap_ranges : 5 -  vp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod swap_ranges ((first1 vector-pointer)
						  (last1  vector-pointer) (first2 cons_iterator))
	;;(format t "specialized swap_ranges for vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-cns-iterator first2
							   (__swap_ranges-imp-5 (opr::vec-ptr-index  first1)
													(opr::vec-ptr-index  last1)
													(opr::vec-ptr-buffer first1)
													(__cons-itr-cons first2))))

  ;;PTN; swap_ranges : 6 -  f  x  vp
  (defmethod swap_ranges ((first1 forward_iterator)
						  (last1  forward_iterator) (first2 vector-pointer))
	;;(format t "specialized swap_ranges for forward_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator first2
							   (__swap_ranges-imp-6 first1 last1
													(opr::vec-ptr-index  first2)
													(opr::vec-ptr-buffer first2))))

  ;;PTN; swap_ranges : 7 -  ci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod swap_ranges ((first1 cons_iterator)
						  (last1  cons_iterator) (first2 vector-pointer))
	;;(format t "specialized swap_ranges for cons_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator first2
							   (__swap_ranges-imp-7 (__cons-itr-cons  first1)
													(__cons-itr-cons  last1)
													(opr::vec-ptr-index  first2)
													(opr::vec-ptr-buffer first2))))

  ;;PTN; swap_ranges : 8 -  vp x  vp
  (defmethod swap_ranges ((first1 vector-pointer)
						  (last1  vector-pointer) (first2 vector-pointer))
	;;(format t "specialized swap_ranges for vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-vct-iterator first2
							   (__swap_ranges-imp-8 (opr::vec-ptr-index  first1)
													(opr::vec-ptr-index  last1)
													(opr::vec-ptr-buffer first1)
													(opr::vec-ptr-index  first2)
													(opr::vec-ptr-buffer first2)))))




; a       : forward_iterator
; b       : forward_iterator
; returns : nil.
(locally (declare (optimize speed))

  ;;PTN; iter_swap : 0 -  i 
  (defmethod iter_swap ((a forward_iterator) (b forward_iterator))
	(with-operators
		(swap *a *b)
	  nil))

  ;;PTN; iter_swap : 1 -  ci <- commented out.
  ;;#-(and cl-stl-noextra cl-stl-0x98)
  ;;(defmethod iter_swap ((a cons_iterator) (b cons_iterator))
  ;;  (with-operators
  ;;	(swap *a *b)
  ;;	nil))

  ;;PTN; iter_swap : 2 -  vp
  (defmethod iter_swap ((a vector-pointer) (b vector-pointer))
	(with-operators
		(swap *a *b)
	  nil)))




;; first     : input_iterator
;; last      : input_iterator
;; result    : output_iterator
;; op        : unary_function
;; returns   : copy of result.
(locally (declare (optimize speed))

  ;;PTN; transform : 0 -  i  x  o 
  (defmethod-overload transform ((first input_iterator)
								 (last  input_iterator) (result output_iterator) op)
	(with-operators
		(if (_== first last)
			@~result
			(let ((op (functor_function @~op)))
			  (declare (type cl:function op))
			  (for (((itr @~first) (dest @~result)) (_/= itr last) (progn ++itr ++dest) :returns dest)
				(_= *dest (funcall op *itr)))))))


  ;;PTN; transform : 1 - cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload transform ((first cons_const_iterator)
								 (last  cons_const_iterator) (result output_iterator) op)
	;;(format t "specialized transform for cons_const_iterator & output_iterator is invoked.~%" ')
	(let ((cons1 (__cons-itr-cons first))
		  (cons2 (__cons-itr-cons  last)))
	  (declare (type cl:list cons1 cons2))
	  (if (eq cons1 cons2)
		  (clone result)
		  (let ((oitr (clone result))
				(uf   (functor_function (clone op))))
			(declare (type cl:function uf))
			(with-operators
				(for (nil (not (eq cons1 cons2)) (progn (setf cons1 (cdr cons1)) ++oitr) :returns oitr)
				  (_= *oitr (funcall uf (car cons1)))))))))


  ;;PTN; transform : 2 - cvp x  o 
  (defmethod-overload transform ((first const-vector-pointer)
								 (last  const-vector-pointer) (result output_iterator) op)
	;;(format t "specialized transform for const-vector-pointer & output_iterator is invoked.~%" ')
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index first))
		  (idx2 (opr::vec-ptr-index  last)))
	  (declare (type fixnum idx1 idx2))
	  (if (= idx1 idx2)
		  (clone result)
		  (let ((buffer (opr::vec-ptr-buffer first))
				(oitr   (clone result))
				(uf     (functor_function (clone op))))
			(declare (type cl:vector buffer))
			(declare (type cl:function uf))
			(with-operators
				(for (nil (< idx1 idx2) (progn (incf idx1) ++oitr) :returns oitr)
				  (_= *oitr (funcall uf (aref buffer idx1)))))))))


  ;;PTN; transform : 3 -  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload transform ((first input_iterator)
								 (last  input_iterator) (result cons_iterator) op)
	;;(format t "specialized transform for input_iterator & cons_iterator is invoked.~%" ')
	(if (_== first last)
		(clone result)
		(let ((out (__cons-itr-cons result))
			  (uf  (functor_function (clone op))))
		  (declare (type cl:list out))
		  (declare (type cl:function uf))
		  (with-operators
			  (for (((itr @~first)) (_/= itr last) (progn ++itr (setf out (cdr out)))
												   :returns (__algo-make-cns-iterator result out))
				(_= (car out) (funcall uf *itr)))))))


  ;;PTN; transform : 4 - cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload transform ((first cons_const_iterator)
								 (last  cons_const_iterator) (result cons_iterator) op)
	;;(format t "specialized transform for cons_const_iterator & cons_iterator is invoked.~%" ')
	(let ((cons1 (__cons-itr-cons first))
		  (cons2 (__cons-itr-cons  last)))
	  (declare (type cl:list cons1 cons2))
	  (if (eq cons1 cons2)
		  (clone result)
		  (let ((out (__cons-itr-cons result))
				(uf  (functor_function (clone op))))
			(declare (type cl:list out))
			(declare (type cl:function uf))
			(for (nil (not (eq cons1 cons2)) (progn (setf cons1 (cdr cons1))
													(setf out   (cdr out)))
											 :returns (__algo-make-cns-iterator result out))
			  (_= (car out) (funcall uf (car cons1))))))))

  
  ;;PTN; transform : 5 - cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload transform ((first const-vector-pointer)
								 (last  const-vector-pointer) (result cons_iterator) op)
	;;(format t "specialized transform for const-vector-pointer & cons_iterator is invoked.~%" ')
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index first))
		  (idx2 (opr::vec-ptr-index  last)))
	  (declare (type fixnum idx1 idx2))
	  (if (= idx1 idx2)
		  (clone result)
		  (let ((buf (opr::vec-ptr-buffer first))
				(out (__cons-itr-cons result))
				(uf  (functor_function (clone op))))
			(declare (type cl:vector buf))
			(declare (type cl:list out))
			(declare (type cl:function uf))
			(for (nil (< idx1 idx2) (progn (incf idx1) (setf out (cdr out)))
									:returns (__algo-make-cns-iterator result out))
			  (_= (car out) (funcall uf (aref buf idx1))))))))


  ;;PTN; transform : 6 -  i  x  vp
  (defmethod-overload transform ((first input_iterator)
								 (last  input_iterator) (result vector-pointer) op)
	;;(format t "specialized transform for input_iterator & vector-pointer is invoked.~%" ')
	(if (_== first last)
		(clone result)
		(let ((out-idx (opr::vec-ptr-index  result))
			  (out-buf (opr::vec-ptr-buffer result))
			  (uf      (functor_function (clone op))))
		  (declare (type fixnum out-idx))
		  (declare (type cl:vector out-buf))
		  (declare (type cl:function uf))
		  (with-operators
			  (for (((itr @~first)) (_/= itr last) (progn ++itr (incf out-idx))
												   :returns (__algo-make-vct-iterator result out-idx))
				(_= (aref out-buf out-idx) (funcall uf *itr)))))))


  ;;PTN; transform : 7 - cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload transform ((first cons_const_iterator)
								 (last  cons_const_iterator) (result vector-pointer) op)
	;;(format t "specialized transform for cons_const_iterator & vector-pointer is invoked.~%" ')
	(let ((cons1 (__cons-itr-cons first))
		  (cons2 (__cons-itr-cons  last)))
	  (declare (type cl:list cons1 cons2))
	  (if (eq cons1 cons2)
		  (clone result)
		  (let ((out-idx (opr::vec-ptr-index  result))
				(out-buf (opr::vec-ptr-buffer result))
				(uf      (functor_function (clone op))))
			(declare (type fixnum out-idx))
			(declare (type cl:vector out-buf))
			(declare (type cl:function uf))
			(for (nil (not (eq cons1 cons2)) (progn (setf cons1 (cdr cons1)) (incf out-idx))
											 :returns (__algo-make-vct-iterator result out-idx))
			  (_= (aref out-buf out-idx) (funcall uf (car cons1))))))))


  ;;PTN; transform : 8 - cvp x  vp
  (defmethod-overload transform ((first const-vector-pointer)
								 (last  const-vector-pointer) (result vector-pointer) op)
	;;(format t "specialized transform for const-vector-pointer & vector-pointer is invoked.~%" ')
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index first))
		  (idx2 (opr::vec-ptr-index  last)))
	  (declare (type fixnum idx1 idx2))
	  (if (= idx1 idx2)
		  (clone result)
		  (let ((src-buf (opr::vec-ptr-buffer first))
				(out-idx (opr::vec-ptr-index  result))
				(out-buf (opr::vec-ptr-buffer result))
				(uf      (functor_function (clone op))))
			(declare (type fixnum out-idx))
			(declare (type cl:vector src-buf out-buf))
			(declare (type cl:function uf))
			(for (nil (< idx1 idx2) (progn (incf idx1) (incf out-idx))
									:returns (__algo-make-vct-iterator result out-idx))
			  (_= (aref out-buf out-idx) (funcall uf (aref src-buf idx1)))))))))




;; first1    : input_iterator
;; last1     : input_iterator
;; first2    : input_iterator
;; result    : output_iterator
;; binary-op : binary_function
;; returns   : copy of result.
(locally (declare (optimize speed))

  ;;PTN; transform : 00 -  i  x  i  x  o 
  (defmethod-overload transform ((first1 input_iterator) (last1  input_iterator)
								 (first2 input_iterator) (result output_iterator) binary-op)
	(with-operators
		(if (_== first1 last1)
			@~result
			(let ((bf (functor_function @~binary-op)))
			  (declare (type cl:function bf))
			  (for (((itr1 @~first1) (itr2 @~first2) (dest @~result))
					(_/= itr1 last1) (progn ++itr1 ++itr2 ++dest) :returns dest)
				(_= *dest (funcall bf *itr1 *itr2)))))))

  ;;PTN; transform : 01 -  i  x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload transform ((first1 input_iterator) (last1 input_iterator)
								 (first2 input_iterator) (result cons_iterator) binary-op)
	;;(format t "specialized transform for input_iterator & input_iterator & cons_iterator is invoked.~%")
	(with-operators
		(if (_== first1 last1)
			@~result
			(let ((out (__cons-itr-cons result))
				  (bf  (functor_function @~binary-op)))
			  (declare (type cl:list out))
			  (declare (type cl:function bf))
			  (for (((itr1 @~first1)
					 (itr2 @~first2)) (_/= itr1 last1) (progn ++itr1
															  ++itr2
															  (setf out (cdr out)))
													   :returns (__algo-make-cns-iterator result out))
				(_= (car out) (funcall bf *itr1 *itr2)))))))

  ;;PTN; transform : 02 -  i  x  i  x  vp
  (defmethod-overload transform ((first1 input_iterator) (last1 input_iterator)
								 (first2 input_iterator) (result vector-pointer) binary-op)
	;;(format t "specialized transform for input_iterator & input_iterator & vector-pointer is invoked.~%")
	(with-operators
		(if (_== first1 last1)
			@~result
			(let ((out-idx (opr::vec-ptr-index  result))
				  (out-buf (opr::vec-ptr-buffer result))
				  (bf      (functor_function @~binary-op)))
			  (declare (type fixnum out-idx))
			  (declare (type cl:vector out-buf))
			  (declare (type cl:function bf))
			  (for (((itr1 @~first1)
					 (itr2 @~first2)) (_/= itr1 last1) (progn ++itr1
															  ++itr2
															  (incf out-idx))
													   :returns (__algo-make-vct-iterator result out-idx))
				(_= (aref out-buf out-idx) (funcall bf *itr1 *itr2)))))))

  ;;PTN; transform : 03 -  i  x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload transform ((first1 input_iterator) (last1 input_iterator)
								 (first2 cons_const_iterator) (result output_iterator) binary-op)
	;;(format t "specialized transform for input_iterator & cons_const_iterator & output_iterator is invoked.~%")
	(with-operators
		(if (_== first1 last1)
			@~result
			(let ((cons2 (__cons-itr-cons first2))
				  (oitr @~result)
				  (bf   (functor_function @~binary-op)))
			  (declare (type cl:list cons2))
			  (declare (type cl:function bf))
			  (for (((itr1 @~first1)) (_/= itr1 last1) (progn ++itr1 (setf cons2 (cdr cons2)) ++oitr) :returns oitr)
				   (_= *oitr (funcall bf *itr1 (car cons2))))))))

  ;;PTN; transform : 04 -  i  x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload transform ((first1 input_iterator) (last1 input_iterator)
								 (first2 cons_const_iterator) (result cons_iterator) binary-op)
	;;(format t "specialized transform for input_iterator & cons_const_iterator & cons_iterator is invoked.~%")
	(with-operators
		(if (_== first1 last1)
			@~result
			(let ((cons2 (__cons-itr-cons  first2))
				  (out   (__cons-itr-cons  result))
				  (bf    (functor_function @~binary-op)))
			  (declare (type cl:list cons2 out))
			  (declare (type cl:function bf))
			  (for (((itr1 @~first1)) (_/= itr1 last1) (progn ++itr1
															  (setf cons2 (cdr cons2))
															  (setf out (cdr out)))
													   :returns (__algo-make-cns-iterator result out))
				(_= (car out) (funcall bf *itr1 (car cons2))))))))

  ;;PTN; transform : 05 -  i  x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload transform ((first1 input_iterator) (last1 input_iterator)
								 (first2 cons_const_iterator) (result vector-pointer) binary-op)
	;;(format t "specialized transform for input_iterator & cons_const_iterator & vector-pointer is invoked.~%")
	(with-operators
		(if (_== first1 last1)
			@~result
			(let ((cons2   (__cons-itr-cons     first2))
				  (out-idx (opr::vec-ptr-index  result))
				  (out-buf (opr::vec-ptr-buffer result))
				  (bf      (functor_function @~binary-op)))
			  (declare (type cl:list cons2))
			  (declare (type fixnum out-idx))
			  (declare (type cl:vector out-buf))
			  (declare (type cl:function bf))
			  (for (((itr1 @~first1)) (_/= itr1 last1) (progn ++itr1
															  (setf cons2 (cdr cons2))
															  (incf out-idx))
													   :returns (__algo-make-vct-iterator result out-idx))
				(_= (aref out-buf out-idx) (funcall bf *itr1 (car cons2))))))))

  ;;PTN; transform : 06 -  i  x cvp x  o 
  (defmethod-overload transform ((first1 input_iterator) (last1 input_iterator)
								 (first2 const-vector-pointer) (result output_iterator) binary-op)
	;;(format t "specialized transform for input_iterator & const-vector-pointer & output_iterator is invoked.~%")
	(with-operators
		(if (_== first1 last1)
			@~result
			(let ((idx2 (opr::vec-ptr-index  first2))
				  (buf2 (opr::vec-ptr-buffer first2))
				  (oitr @~result)
				  (bf   (functor_function @~binary-op)))
			  (declare (type fixnum idx2))
			  (declare (type cl:vector buf2))
			  (declare (type cl:function bf))
			  (for (((itr1 @~first1)) (_/= itr1 last1) (progn ++itr1 (incf idx2) ++oitr) :returns oitr)
				   (_= *oitr (funcall bf *itr1 (aref buf2 idx2))))))))

  ;;PTN; transform : 07 -  i  x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload transform ((first1 input_iterator) (last1 input_iterator)
								 (first2 const-vector-pointer) (result cons_iterator) binary-op)
	;;(format t "specialized transform for input_iterator & const-vector-pointer & cons_iterator is invoked.~%")
	(with-operators
		(if (_== first1 last1)
			@~result
			(let ((idx2 (opr::vec-ptr-index  first2))
				  (buf2 (opr::vec-ptr-buffer first2))
				  (out  (__cons-itr-cons     result))
				  (bf   (functor_function    @~binary-op)))
			  (declare (type fixnum idx2))
			  (declare (type cl:vector buf2))
			  (declare (type cl:list out))
			  (declare (type cl:function bf))
			  (for (((itr1 @~first1)) (_/= itr1 last1) (progn ++itr1
															  (incf idx2)
															  (setf out (cdr out)))
													   :returns (__algo-make-cns-iterator result out))
				(_= (car out) (funcall bf *itr1 (aref buf2 idx2))))))))

  ;;PTN; transform : 08 -  i  x cvp x  vp
  (defmethod-overload transform ((first1 input_iterator) (last1 input_iterator)
								 (first2 const-vector-pointer) (result vector-pointer) binary-op)
	;;(format t "specialized transform for input_iterator & const-vector-pointer & vector-pointer is invoked.~%")
	(with-operators
		(if (_== first1 last1)
			@~result
			(let ((idx2    (opr::vec-ptr-index  first2))
				  (buf2    (opr::vec-ptr-buffer first2))
				  (out-idx (opr::vec-ptr-index  result))
				  (out-buf (opr::vec-ptr-buffer result))
				  (bf      (functor_function @~binary-op)))
			  (declare (type fixnum idx2 out-idx))
			  (declare (type cl:vector buf2 out-buf))
			  (declare (type cl:function bf))
			  (for (((itr1 @~first1)) (_/= itr1 last1) (progn ++itr1
															  (incf idx2)
															  (incf out-idx))
													   :returns (__algo-make-vct-iterator result out-idx))
				(_= (aref out-buf out-idx) (funcall bf *itr1 (aref buf2 idx2))))))))

  ;;PTN; transform : 09 - cci x  i  x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload transform ((first1 cons_const_iterator) (last1 cons_const_iterator)
								 (first2 input_iterator) (result output_iterator) binary-op)
	;;(format t "specialized transform2 for cons_const_iterator & input_iterator & output_iterator is invoked.~%")
	(let ((cons1 (__cons-itr-cons first1))
		  (last1 (__cons-itr-cons  last1)))
	  (declare (type cl:list cons1 last1))
	  (with-operators
		  (if (eq cons1 last1)
			  @~result
			  (let ((oitr @~result)
					(bf   (functor_function @~binary-op)))
				(declare (type cl:function bf))
				(for (((itr2 @~first2)) (not (eq cons1 last1)) (progn (setf cons1 (cdr cons1))
																	  ++itr2 ++oitr) :returns oitr)
				  (_= *oitr (funcall bf (car cons1) *itr2))))))))

  ;;PTN; transform : 10 - cci x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload transform ((first1 cons_const_iterator) (last1 cons_const_iterator)
								 (first2 input_iterator) (result cons_iterator) binary-op)
	;;(format t "specialized transform2 for cons_const_iterator & input_iterator & cons_iterator is invoked.~%")
	(let ((cons1 (__cons-itr-cons first1))
		  (last1 (__cons-itr-cons  last1)))
	  (declare (type cl:list cons1 last1))
	  (with-operators
		  (if (eq cons1 last1)
			  @~result
			  (let ((out (__cons-itr-cons  result))
					(bf  (functor_function @~binary-op)))
				(declare (type cl:list out))
				(declare (type cl:function bf))
				(for (((itr2 @~first2)) (not (eq cons1 last1)) (progn (setf cons1 (cdr cons1))
																	  ++itr2
																	  (setf out (cdr out)))
															   :returns (__algo-make-cns-iterator result out))
				  (_= (car out) (funcall bf (car cons1) *itr2))))))))

  ;;PTN; transform : 11 - cci x  i  x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload transform ((first1 cons_const_iterator) (last1 cons_const_iterator)
								 (first2 input_iterator) (result vector-pointer) binary-op)
	;;(format t "specialized transform2 for cons_const_iterator & input_iterator & vector-pointer is invoked.~%")
	(let ((cons1 (__cons-itr-cons first1))
		  (last1 (__cons-itr-cons  last1)))
	  (declare (type cl:list cons1 last1))
	  (with-operators
		  (if (eq cons1 last1)
			  @~result
			  (let ((out-idx (opr::vec-ptr-index  result))
					(out-buf (opr::vec-ptr-buffer result))
					(bf      (functor_function @~binary-op)))
				(declare (type fixnum out-idx))
				(declare (type cl:vector out-buf))
				(declare (type cl:function bf))
				(for (((itr2 @~first2)) (not (eq cons1 last1)) (progn (setf cons1 (cdr cons1))
																	  ++itr2
																	  (incf out-idx))
															   :returns (__algo-make-vct-iterator result out-idx))
				  (_= (aref out-buf out-idx) (funcall bf (car cons1) *itr2))))))))

  ;;PTN; transform : 12 - cci x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload transform ((first1 cons_const_iterator) (last1 cons_const_iterator)
								 (first2 cons_const_iterator) (result output_iterator) binary-op)
	;;(format t "specialized transform2 for cons_const_iterator & cons_const_iterator & output_iterator is invoked.~%")
	(let ((cons1 (__cons-itr-cons first1))
		  (last1 (__cons-itr-cons  last1)))
	  (declare (type cl:list cons1 last1))
	  (with-operators
		  (if (eq cons1 last1)
			  @~result
			  (let ((cons2 (__cons-itr-cons  first2))
					(oitr  @~result)
					(bf    (functor_function @~binary-op)))
				(declare (type cl:list cons2))
				(declare (type cl:function bf))
				(for (nil (not (eq cons1 last1)) (progn (setf cons1 (cdr cons1))
														(setf cons2 (cdr cons2)) ++oitr) :returns oitr)
				  (_= *oitr (funcall bf (car cons1) (car cons2)))))))))

  ;;PTN; transform : 13 - cci x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload transform ((first1 cons_const_iterator) (last1 cons_const_iterator)
								 (first2 cons_const_iterator) (result cons_iterator) binary-op)
	;;(format t "specialized transform2 for cons_const_iterator & cons_const_iterator & cons_iterator is invoked.~%")
	(let ((cons1 (__cons-itr-cons first1))
		  (last1 (__cons-itr-cons  last1)))
	  (declare (type cl:list cons1 last1))
	  (with-operators
		  (if (eq cons1 last1)
			  @~result
			  (let ((cons2 (__cons-itr-cons first2))
					(out   (__cons-itr-cons result))
					(bf    (functor_function @~binary-op)))
				(declare (type cl:list cons2 out))
				(declare (type cl:function bf))
				(for (nil (not (eq cons1 last1)) (progn (setf cons1 (cdr cons1))
														(setf cons2 (cdr cons2))
														(setf out (cdr out)))
												 :returns (__algo-make-cns-iterator result out))
				  (_= (car out) (funcall bf (car cons1) (car cons2)))))))))

  ;;PTN; transform : 14 - cci x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload transform ((first1 cons_const_iterator) (last1 cons_const_iterator)
								 (first2 cons_const_iterator) (result vector-pointer) binary-op)
	;;(format t "specialized transform2 for cons_const_iterator & cons_const_iterator & vector-pointer is invoked.~%")
	(let ((cons1 (__cons-itr-cons first1))
		  (last1 (__cons-itr-cons  last1)))
	  (declare (type cl:list cons1 last1))
	  (with-operators
		  (if (eq cons1 last1)
			  @~result
			  (let ((cons2   (__cons-itr-cons     first2))
					(out-idx (opr::vec-ptr-index  result))
					(out-buf (opr::vec-ptr-buffer result))
					(bf      (functor_function @~binary-op)))
				(declare (type cl:list cons2))
				(declare (type fixnum out-idx))
				(declare (type cl:vector out-buf))
				(declare (type cl:function bf))
				(for (nil (not (eq cons1 last1)) (progn (setf cons1 (cdr cons1))
														(setf cons2 (cdr cons2))
														(incf out-idx))
												 :returns (__algo-make-vct-iterator result out-idx))
				  (_= (aref out-buf out-idx) (funcall bf (car cons1) (car cons2)))))))))

  ;;PTN; transform : 15 - cci x cvp x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload transform ((first1 cons_const_iterator) (last1 cons_const_iterator)
								 (first2 const-vector-pointer) (result output_iterator) binary-op)
	;;(format t "specialized transform2 for cons_const_iterator & const-vector-pointer & output_iterator is invoked.~%")
	(let ((cons1 (__cons-itr-cons first1))
		  (last1 (__cons-itr-cons  last1)))
	  (declare (type cl:list cons1 last1))
	  (with-operators
		  (if (eq cons1 last1)
			  @~result
			  (let ((idx2 (opr::vec-ptr-index  first2))
					(buf2 (opr::vec-ptr-buffer first2))
					(oitr @~result)
					(bf   (functor_function @~binary-op)))
				(declare (type fixnum idx2))
				(declare (type cl:vector buf2))
				(declare (type cl:function bf))
				(for (nil (not (eq cons1 last1)) (progn (setf cons1 (cdr cons1)) (incf idx2) ++oitr) :returns oitr)
				  (_= *oitr (funcall bf (car cons1) (aref buf2 idx2)))))))))

  ;;PTN; transform : 16 - cci x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload transform ((first1 cons_const_iterator) (last1 cons_const_iterator)
								 (first2 const-vector-pointer) (result cons_iterator) binary-op)
	;;(format t "specialized transform2 for cons_const_iterator & const-vector-pointer & cons_iterator is invoked.~%")
	(let ((cons1 (__cons-itr-cons first1))
		  (last1 (__cons-itr-cons  last1)))
	  (declare (type cl:list cons1 last1))
	  (with-operators
		  (if (eq cons1 last1)
			  @~result
			  (let ((idx2 (opr::vec-ptr-index  first2))
					(buf2 (opr::vec-ptr-buffer first2))
					(out  (__cons-itr-cons result))
					(bf   (functor_function @~binary-op)))
				(declare (type fixnum idx2))
				(declare (type cl:list out))
				(declare (type cl:vector buf2))
				(declare (type cl:function bf))
				(for (nil (not (eq cons1 last1)) (progn (setf cons1 (cdr cons1))
														(incf idx2)
														(setf out (cdr out)))
												 :returns (__algo-make-cns-iterator result out))
				  (_= (car out) (funcall bf (car cons1) (aref buf2 idx2)))))))))

  ;;PTN; transform : 17 - cci x cvp x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload transform ((first1 cons_const_iterator) (last1 cons_const_iterator)
								 (first2 const-vector-pointer) (result vector-pointer) binary-op)
	;;(format t "specialized transform2 for cons_const_iterator & const-vector-pointer & vector-pointer is invoked.~%")
	(let ((cons1 (__cons-itr-cons first1))
		  (last1 (__cons-itr-cons  last1)))
	  (declare (type cl:list cons1 last1))
	  (with-operators
		  (if (eq cons1 last1)
			  @~result
			  (let ((idx2    (opr::vec-ptr-index  first2))
					(buf2    (opr::vec-ptr-buffer first2))
					(out-idx (opr::vec-ptr-index  result))
					(out-buf (opr::vec-ptr-buffer result))
					(bf      (functor_function @~binary-op)))
				(declare (type fixnum idx2 out-idx))
				(declare (type cl:vector buf2 out-buf))
				(declare (type cl:function bf))
				(for (nil (not (eq cons1 last1)) (progn (setf cons1 (cdr cons1))
														(incf idx2)
														(incf out-idx))
												 :returns (__algo-make-vct-iterator result out-idx))
				  (_= (aref out-buf out-idx) (funcall bf (car cons1) (aref buf2 idx2)))))))))

  ;;PTN; transform : 18 - cvp x  i  x  o 
  (defmethod-overload transform ((first1 const-vector-pointer) (last1 const-vector-pointer)
								 (first2 input_iterator) (result output_iterator) binary-op)
	;;(format t "specialized transform2 for const-vector-pointer & input_iterator & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(let ((idx1  (opr::vec-ptr-index first1))
		  (last1 (opr::vec-ptr-index  last1)))
	  (declare (type fixnum idx1 last1))
	  (with-operators
		  (if (= idx1 last1)
			  @~result
			  (let ((buf1 (opr::vec-ptr-buffer first1))
					(oitr @~result)
					(bf   (functor_function @~binary-op)))
				(declare (type cl:vector buf1))
				(declare (type cl:function bf))
				(for (((itr2 @~first2)) (< idx1 last1) (progn (incf idx1) ++itr2 ++oitr) :returns oitr)
				  (_= *oitr (funcall bf (aref buf1 idx1) *itr2))))))))

  ;;PTN; transform : 19 - cvp x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload transform ((first1 const-vector-pointer) (last1 const-vector-pointer)
								 (first2 input_iterator) (result cons_iterator) binary-op)
	;;(format t "specialized transform2 for const-vector-pointer & input_iterator & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(let ((idx1  (opr::vec-ptr-index first1))
		  (last1 (opr::vec-ptr-index  last1)))
	  (declare (type fixnum idx1 last1))
	  (with-operators
		  (if (= idx1 last1)
			  @~result
			  (let ((buf1 (opr::vec-ptr-buffer first1))
					(out  (__cons-itr-cons  result))
					(bf   (functor_function @~binary-op)))
				(declare (type cl:vector buf1))
				(declare (type cl:list out))
				(declare (type cl:function bf))
				(for (((itr2 @~first2)) (< idx1 last1) (progn (incf idx1)
															  ++itr2
															  (setf out (cdr out)))
													   :returns (__algo-make-cns-iterator result out))
				  (_= (car out) (funcall bf (aref buf1 idx1) *itr2))))))))

  ;;PTN; transform : 20 - cvp x  i  x  vp
  (defmethod-overload transform ((first1 const-vector-pointer) (last1 const-vector-pointer)
								 (first2 input_iterator) (result vector-pointer) binary-op)
	;;(format t "specialized transform2 for const-vector-pointer & input_iterator & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(let ((idx1  (opr::vec-ptr-index first1))
		  (last1 (opr::vec-ptr-index  last1)))
	  (declare (type fixnum idx1 last1))
	  (with-operators
		  (if (= idx1 last1)
			  @~result
			  (let ((buf1    (opr::vec-ptr-buffer first1))
					(out-idx (opr::vec-ptr-index  result))
					(out-buf (opr::vec-ptr-buffer result))
					(bf      (functor_function @~binary-op)))
				(declare (type fixnum out-idx))
				(declare (type cl:vector buf1 out-buf))
				(declare (type cl:function bf))
				(for (((itr2 @~first2)) (< idx1 last1) (progn (incf idx1)
															  ++itr2
															  (incf out-idx))
													   :returns (__algo-make-vct-iterator result out-idx))
				  (_= (aref out-buf out-idx) (funcall bf (aref buf1 idx1) *itr2))))))))

  ;;PTN; transform : 21 - cvp x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload transform ((first1 const-vector-pointer) (last1 const-vector-pointer)
								 (first2 cons_const_iterator) (result output_iterator) binary-op)
	;;(format t "specialized transform2 for const-vector-pointer & cons_const_iterator & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(let ((idx1  (opr::vec-ptr-index first1))
		  (last1 (opr::vec-ptr-index  last1)))
	  (declare (type fixnum idx1 last1))
	  (with-operators
		  (if (= idx1 last1)
			  @~result
			  (let ((buf1  (opr::vec-ptr-buffer first1))
					(cons2 (__cons-itr-cons  first2))
					(oitr  @~result)
					(bf    (functor_function @~binary-op)))
				(declare (type cl:list cons2))
				(declare (type cl:vector buf1))
				(declare (type cl:function bf))
				(for (nil (< idx1 last1) (progn (incf idx1) (setf cons2 (cdr cons2)) ++oitr) :returns oitr)
				  (_= *oitr (funcall bf (aref buf1 idx1) (car cons2)))))))))

  ;;PTN; transform : 22 - cvp x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload transform ((first1 const-vector-pointer) (last1 const-vector-pointer)
								 (first2 cons_const_iterator) (result cons_iterator) binary-op)
	;;(format t "specialized transform2 for const-vector-pointer & cons_const_iterator & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(let ((idx1  (opr::vec-ptr-index first1))
		  (last1 (opr::vec-ptr-index  last1)))
	  (declare (type fixnum idx1 last1))
	  (with-operators
		  (if (= idx1 last1)
			  @~result
			  (let ((buf1  (opr::vec-ptr-buffer first1))
					(cons2 (__cons-itr-cons first2))
					(out   (__cons-itr-cons result))
					(bf    (functor_function @~binary-op)))
				(declare (type cl:list cons2 out))
				(declare (type cl:vector buf1))
				(declare (type cl:function bf))
				(for (nil (< idx1 last1) (progn (incf idx1)
												(setf cons2 (cdr cons2))
												(setf out (cdr out))) :returns (__algo-make-cns-iterator result out))
				  (_= (car out) (funcall bf (aref buf1 idx1) (car cons2)))))))))

  ;;PTN; transform : 23 - cvp x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload transform ((first1 const-vector-pointer) (last1 const-vector-pointer)
								 (first2 cons_const_iterator) (result vector-pointer) binary-op)
	;;(format t "specialized transform2 for const-vector-pointer & cons_const_iterator & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(let ((idx1  (opr::vec-ptr-index first1))
		  (last1 (opr::vec-ptr-index  last1)))
	  (declare (type fixnum idx1 last1))
	  (with-operators
		  (if (= idx1 last1)
			  @~result
			  (let ((buf1    (opr::vec-ptr-buffer first1))
					(cons2   (__cons-itr-cons     first2))
					(out-idx (opr::vec-ptr-index  result))
					(out-buf (opr::vec-ptr-buffer result))
					(bf      (functor_function @~binary-op)))
				(declare (type cl:list cons2))
				(declare (type fixnum out-idx))
				(declare (type cl:vector buf1 out-buf))
				(declare (type cl:function bf))
				(for (nil (< idx1 last1) (progn (incf idx1)
												(setf cons2 (cdr cons2))
												(incf out-idx)) :returns (__algo-make-vct-iterator result out-idx))
				  (_= (aref out-buf out-idx) (funcall bf (aref buf1 idx1) (car cons2)))))))))

  ;;PTN; transform : 24 - cvp x cvp x  o 
  (defmethod-overload transform ((first1 const-vector-pointer) (last1 const-vector-pointer)
								 (first2 const-vector-pointer) (result output_iterator) binary-op)
	;;(format t "specialized transform2 for const-vector-pointer & const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(let ((idx1  (opr::vec-ptr-index first1))
		  (last1 (opr::vec-ptr-index  last1)))
	  (declare (type fixnum idx1 last1))
	  (with-operators
		  (if (= idx1 last1)
			  @~result
			  (let ((buf1 (opr::vec-ptr-buffer first1))
					(idx2 (opr::vec-ptr-index  first2))
					(buf2 (opr::vec-ptr-buffer first2))
					(oitr @~result)
					(bf   (functor_function @~binary-op)))
				(declare (type fixnum idx2))
				(declare (type cl:vector buf1 buf2))
				(declare (type cl:function bf))
				(for (nil (< idx1 last1) (progn (incf idx1) (incf idx2) ++oitr) :returns oitr)
				  (_= *oitr (funcall bf (aref buf1 idx1) (aref buf2 idx2)))))))))

  ;;PTN; transform : 25 - cvp x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload transform ((first1 const-vector-pointer) (last1 const-vector-pointer)
								 (first2 const-vector-pointer) (result cons_iterator) binary-op)
	;;(format t "specialized transform2 for const-vector-pointer & const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(let ((idx1  (opr::vec-ptr-index first1))
		  (last1 (opr::vec-ptr-index  last1)))
	  (declare (type fixnum idx1 last1))
	  (with-operators
		  (if (= idx1 last1)
			  @~result
			  (let ((buf1 (opr::vec-ptr-buffer first1))
					(idx2 (opr::vec-ptr-index  first2))
					(buf2 (opr::vec-ptr-buffer first2))
					(out  (__cons-itr-cons result))
					(bf   (functor_function @~binary-op)))
				(declare (type fixnum idx2))
				(declare (type cl:list out))
				(declare (type cl:vector buf1 buf2))
				(declare (type cl:function bf))
				(for (nil (< idx1 last1) (progn (incf idx1)
												(incf idx2)
												(setf out (cdr out))) :returns (__algo-make-cns-iterator result out))
				  (_= (car out) (funcall bf (aref buf1 idx1) (aref buf2 idx2)))))))))

  ;;PTN; transform : 26 - cvp x cvp x  vp
  (defmethod-overload transform ((first1 const-vector-pointer) (last1 const-vector-pointer)
								 (first2 const-vector-pointer) (result vector-pointer) binary-op)
	;;(format t "specialized transform2 for const-vector-pointer & const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(let ((idx1  (opr::vec-ptr-index first1))
		  (last1 (opr::vec-ptr-index  last1)))
	  (declare (type fixnum idx1 last1))
	  (with-operators
		  (if (= idx1 last1)
			  @~result
			  (let ((buf1    (opr::vec-ptr-buffer first1))
					(idx2    (opr::vec-ptr-index  first2))
					(buf2    (opr::vec-ptr-buffer first2))
					(out-idx (opr::vec-ptr-index  result))
					(out-buf (opr::vec-ptr-buffer result))
					(bf      (functor_function @~binary-op)))
				(declare (type fixnum idx2 out-idx))
				(declare (type cl:vector buf1 buf2 out-buf))
				(declare (type cl:function bf))
				(for (nil (< idx1 last1) (progn (incf idx1)
												(incf idx2)
												(incf out-idx)) :returns (__algo-make-vct-iterator result out-idx))
				  (_= (aref out-buf out-idx) (funcall bf (aref buf1 idx1) (aref buf2 idx2))))))))))





; first     : forward_iterator
; last      : forward_iterator
; eql-bf    : binary_function ( added by CL-STL. default : #'operator_== )
; returns   : nil.
(locally (declare (optimize speed))

  ;;PTN; replace : 0 -  f
  (labels ((__replace-imp-0 (first last old-val new-val eql-bf)
			 (declare (type cl:function eql-bf))
			 (if (_== first last)
				 nil
				 (with-operators
					 (for (((itr @~first)) (_/= itr last) ++itr)
					   (when (funcall eql-bf old-val *itr)
						 (_= *itr new-val)))))))

  (defmethod-overload replace ((first forward_iterator)
							   (last  forward_iterator) old-val new-val)
	(__replace-imp-0 first last old-val new-val #'operator_==))

  #-cl-stl-noextra
  (defmethod-overload replace ((first forward_iterator)
							   (last  forward_iterator) old-val new-val eql-bf)
	(__replace-imp-0 first last old-val new-val (functor_function (clone eql-bf)))))


  ;;PTN; replace : 1 -  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__replace-imp-1 (cons1 cons2 old-val new-val eql-bf)
			 (declare (type cl:function eql-bf))
			 (for (nil (not (eq cons1 cons2)) (setf cons1 (cdr cons1)))
			   (when (funcall eql-bf old-val (car cons1))
				 (_= (car cons1) new-val)))))

	(defmethod-overload replace ((first cons_iterator)
								 (last  cons_iterator) old-val new-val)
	  ;;(format t "specialized replace for cons_iterator is invoked.~%")
	  (__replace-imp-1 (__cons-itr-cons first)
					   (__cons-itr-cons  last) old-val new-val #'operator_==))

	#-cl-stl-noextra
	(defmethod-overload replace ((first cons_iterator)
								 (last  cons_iterator) old-val new-val eql-bf)
	  ;;(format t "specialized replace for cons_iterator is invoked.~%")
	  (__replace-imp-1 (__cons-itr-cons first)
					   (__cons-itr-cons  last)
					   old-val new-val (functor_function (clone eql-bf)))))


  ;;PTN; replace : 2 -  vp
  (labels ((__replace-imp-2 (idx1 idx2 buffer old-val new-val eql-bf)
			 (declare (type fixnum idx1 idx2))
			 (declare (type cl:vector buffer))
			 (declare (type cl:function eql-bf))
			 (for (nil (< idx1 idx2) (incf idx1))
			   (when (funcall eql-bf old-val (aref buffer idx1))
				 (_= (aref buffer idx1) new-val)))))

	(defmethod-overload replace ((first vector-pointer)
								 (last  vector-pointer) old-val new-val)
	  ;;(format t "specialized replace for vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__replace-imp-2 (opr::vec-ptr-index  first)
					   (opr::vec-ptr-index  last)
					   (opr::vec-ptr-buffer first) old-val new-val #'operator_==))

	#-cl-stl-noextra
	(defmethod-overload replace ((first vector-pointer)
								 (last  vector-pointer) old-val new-val eql-bf)
	  ;;(format t "specialized replace for vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__replace-imp-2 (opr::vec-ptr-index  first)
					   (opr::vec-ptr-index  last)
					   (opr::vec-ptr-buffer first) old-val new-val (clone eql-bf)))))




; first     : forward_iterator
; last      : forward_iterator
; pred      : unary_function
; returns   : nil.
(locally (declare (optimize speed))

  ;;PTN; replace_if : 0 -  f
  (defmethod replace_if ((first forward_iterator) (last forward_iterator) pred new-val)
	(if (_== first last)
		nil
		(with-operators
			(let ((pred (functor_function @~pred)))
			  (declare (type cl:function pred))
			  (for (((itr @~first)) (_/= itr last) ++itr)
				(when (funcall pred *itr)
				  (_= *itr new-val)))))))

  ;;PTN; replace_if : 1 -  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod replace_if ((first cons_iterator) (last cons_iterator) pred new-val)
	  ;;(format t "specialized replace_if for cons_iterator is invoked.~%")
	  (let ((cons1 (__cons-itr-cons first))
			(cons2 (__cons-itr-cons  last)))
		(declare (type cl:list cons1 cons2))
		(if (eq cons1 cons2)
			nil
			(let ((uf (functor_function (clone pred))))
			  (declare (type cl:function uf))
			  (for (nil (not (eq cons1 cons2)) (setf cons1 (cdr cons1)))
				(when (funcall uf (car cons1))
				  (_= (car cons1) new-val)))))))

  ;;PTN; replace_if : 2 -  vp
  (defmethod replace_if ((first vector-pointer) (last vector-pointer) pred new-val)
	  ;;(format t "specialized replace_if for vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (let ((idx1 (opr::vec-ptr-index  first))
			(idx2 (opr::vec-ptr-index   last)))
		(declare (type fixnum idx1 idx2))
		(if (= idx1 idx2)
			nil
			(let ((buf1 (opr::vec-ptr-buffer first))
				  (uf   (functor_function (clone pred))))
			  (declare (type cl:vector buf1))
			  (declare (type cl:function uf))
			  (for (nil (< idx1 idx2) (incf idx1))
				(when (funcall uf (aref buf1 idx1))
				  (_= (aref buf1 idx1) new-val))))))))




; first     : input_iterator
; last      : input_iterator
; result    : output_iterator
; eql-bf    : binary_function ( added by CL-STL. default : #'operator_== )
; returns   : copy of result.
(locally (declare (optimize speed))

  ;;PTN; replace_copy : 0 -   i  x  o 
  (labels ((__replace_copy-imp-0 (first last result old-val new-val eql-bf)
			 (declare (type cl:function eql-bf))
			 (with-operators
				 (if (_== first last)
					 @~result
					 (for (((itr @~first) (dest @~result)) (_/= itr last) (progn ++itr ++dest) :returns dest)
					   (let ((cur-val *itr))
						 (_= *dest (if (funcall eql-bf cur-val old-val)
									   new-val
									   cur-val))))))))

	(defmethod-overload replace_copy ((first  input_iterator)
									  (last   input_iterator) (result output_iterator) old-val new-val)
	  (__replace_copy-imp-0 first last result old-val new-val #'operator_==))

	#-cl-stl-noextra
	(defmethod-overload replace_copy ((first  input_iterator)
									  (last   input_iterator) (result output_iterator) old-val new-val eql-bf)
	  (__replace_copy-imp-0 first last result old-val new-val (functor_function (clone eql-bf)))))


  ;;PTN; replace_copy : 1 -  cci x  o
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__replace_copy-imp-1 (cons1 end1 oitr old-val new-val eql-bf)
			 (declare (type cl:list cons1 end1))
			 (declare (type cl:function eql-bf))
			 (with-operators
				 (for (nil (not (eq cons1 end1)) (progn (setf cons1 (cdr cons1)) ++oitr) :returns oitr)
				   (let ((cur-val (car cons1)))
					 (if (funcall eql-bf cur-val old-val)
						 (_= *oitr new-val)
						 (_= *oitr cur-val)))))))

	(defmethod-overload replace_copy ((first cons_const_iterator)
									  (last  cons_const_iterator) (result output_iterator) old-val new-val)
	  ;;(format t "specialized replace_copy for cons_const_iterator & output_iterator is invoked.~%")
	  (__replace_copy-imp-1 (__cons-itr-cons first)
							(__cons-itr-cons  last) (clone result) old-val new-val #'operator_==))

	#-cl-stl-noextra
	(defmethod-overload replace_copy ((first cons_const_iterator)
									  (last  cons_const_iterator) (result output_iterator) old-val new-val eql-bf)
	  ;;(format t "specialized replace_copy for cons_const_iterator & output_iterator is invoked.~%")
	  (__replace_copy-imp-1 (__cons-itr-cons first)
							(__cons-itr-cons  last)
							(clone result) old-val new-val (functor_function (clone eql-bf)))))


  ;;PTN; replace_copy : 2 -  cvp x  o 
  (labels ((__replace_copy-imp-2 (idx1 idx2 buffer oitr old-val new-val eql-bf)
			 (declare (type fixnum idx1 idx2))
			 (declare (type cl:vector buffer))
			 (declare (type cl:function eql-bf))
			 (with-operators
				 (for (nil (< idx1 idx2) (progn (incf idx1) ++oitr) :returns oitr)
				   (let ((cur-val (aref buffer idx1)))
					 (if (funcall eql-bf cur-val old-val)
						 (_= *oitr new-val)
						 (_= *oitr cur-val)))))))

	(defmethod-overload replace_copy ((first const-vector-pointer)
									  (last  const-vector-pointer) (result output_iterator) old-val new-val)
	  ;;(format t "specialized replace_copy for const-vector-pointer & output_iterator is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__replace_copy-imp-2 (opr::vec-ptr-index  first)
							(opr::vec-ptr-index  last)
							(opr::vec-ptr-buffer first) (clone result) old-val new-val #'operator_==))

	#-cl-stl-noextra
	(defmethod-overload replace_copy ((first const-vector-pointer)
									  (last  const-vector-pointer) (result output_iterator) old-val new-val eql-bf)
	  ;;(format t "specialized replace_copy for const-vector-pointer & output_iterator is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__replace_copy-imp-2 (opr::vec-ptr-index  first)
							(opr::vec-ptr-index  last)
							(opr::vec-ptr-buffer first)
							(clone result) old-val new-val (functor_function (clone eql-bf)))))


  ;;PTN; replace_copy : 3 -   i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__replace_copy-imp-3 (first last out-cns old-val new-val eql-bf)
			 (declare (type cl:list out-cns))
			 (declare (type cl:function eql-bf))
			 (with-operators
				 (for (((itr @~first)) (_/= itr last) (progn ++itr (setf out-cns (cdr out-cns))) :returns out-cns)
				   (let ((cur-val *itr))
					 (if (funcall eql-bf cur-val old-val)
						 (_= (car out-cns) new-val)
						 (_= (car out-cns) cur-val)))))))

	(defmethod-overload replace_copy ((first  input_iterator)
									  (last   input_iterator) (result cons_iterator) old-val new-val)
	  ;;(format t "specialized replace_copy for input_iterator & cons_iterator is invoked.~%")
	  (__algo-make-cns-iterator result
								 (__replace_copy-imp-3 first last
													   (__cons-itr-cons result) old-val new-val #'operator_==)))

	#-cl-stl-noextra
	(defmethod-overload replace_copy ((first  input_iterator)
									  (last   input_iterator) (result cons_iterator) old-val new-val eql-bf)
	  ;;(format t "specialized replace_copy for input_iterator & cons_iterator is invoked.~%")
	  (__algo-make-cns-iterator result
								 (__replace_copy-imp-3 first last (__cons-itr-cons result)
													   old-val new-val (functor_function (clone eql-bf))))))


  ;;PTN; replace_copy : 4 -  cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__replace_copy-imp-4 (cons1 end1 out-cns old-val new-val eql-bf)
			 (declare (type cl:list cons1 end1 out-cns))
			 (declare (type cl:function eql-bf))
			 (for (nil (not (eq cons1 end1)) (progn (setf cons1   (cdr cons1))
													(setf out-cns (cdr out-cns))) :returns out-cns)
			   (let ((cur-val (car cons1)))
				 (if (funcall eql-bf cur-val old-val)
					 (_= (car out-cns) new-val)
					 (_= (car out-cns) cur-val))))))

	(defmethod-overload replace_copy ((first  cons_const_iterator)
									  (last   cons_const_iterator) (result cons_iterator) old-val new-val)
	  ;;(format t "specialized replace_copy for cons_const_iterator & cons_iterator is invoked.~%")
	  (__algo-make-cns-iterator result
								 (__replace_copy-imp-4 (__cons-itr-cons  first)
													   (__cons-itr-cons   last)
													   (__cons-itr-cons result) old-val new-val #'operator_==)))

	#-cl-stl-noextra
	(defmethod-overload replace_copy ((first  cons_const_iterator)
									  (last   cons_const_iterator) (result cons_iterator) old-val new-val eql-bf)
	  ;;(format t "specialized replace_copy for cons_const_iterator & cons_iterator is invoked.~%")
	  (__algo-make-cns-iterator result
								 (__replace_copy-imp-4 (__cons-itr-cons  first)
													   (__cons-itr-cons   last)
													   (__cons-itr-cons result)
													   old-val new-val (functor_function (clone eql-bf))))))


  ;;PTN; replace_copy : 5 -  cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__replace_copy-imp-5 (idx1 idx2 src-buf out-cns old-val new-val eql-bf)
			 (declare (type fixnum idx1 idx2))
			 (declare (type cl:list out-cns))
			 (declare (type cl:vector src-buf))
			 (declare (type cl:function eql-bf))
			 (for (nil (< idx1 idx2) (progn (incf idx1) (setf out-cns (cdr out-cns))) :returns out-cns)
			   (let ((cur-val (aref src-buf idx1)))
				 (if (funcall eql-bf cur-val old-val)
					 (_= (car out-cns) new-val)
					 (_= (car out-cns) cur-val))))))

	(defmethod-overload replace_copy ((first  const-vector-pointer)
									  (last   const-vector-pointer) (result cons_iterator) old-val new-val)
	  ;;(format t "specialized replace_copy for const-vector-pointer & cons_iterator is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__algo-make-cns-iterator result
								 (__replace_copy-imp-5 (opr::vec-ptr-index  first)
													   (opr::vec-ptr-index   last)
													   (opr::vec-ptr-buffer first)
													   (__cons-itr-cons result) old-val new-val #'operator_==)))

	#-cl-stl-noextra
	(defmethod-overload replace_copy ((first  const-vector-pointer)
									  (last   const-vector-pointer) (result cons_iterator) old-val new-val eql-bf)
	  ;;(format t "specialized replace_copy for const-vector-pointer & cons_iterator is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__algo-make-cns-iterator result
								 (__replace_copy-imp-5 (opr::vec-ptr-index  first)
													   (opr::vec-ptr-index   last)
													   (opr::vec-ptr-buffer first)
													   (__cons-itr-cons    result)
													   old-val new-val (functor_function (clone eql-bf))))))

  
  ;;PTN; replace_copy : 6 -   i  x  vp
  (labels ((__replace_copy-imp-6 (first last out-idx out-buf old-val new-val eql-bf)
			 (declare (type fixnum out-idx))
			 (declare (type cl:vector out-buf))
			 (declare (type cl:function eql-bf))
			 (with-operators
				 (for (((itr @~first)) (_/= itr last) (progn ++itr (incf out-idx)) :returns out-idx)
				   (let ((cur-val *itr))
					 (if (funcall eql-bf cur-val old-val)
						 (_= (aref out-buf out-idx) new-val)
						 (_= (aref out-buf out-idx) cur-val)))))))

	(defmethod-overload replace_copy ((first  input_iterator)
									  (last   input_iterator) (result vector-pointer) old-val new-val)
	  ;;(format t "specialized replace_copy for input_iterator & vector-pointer is invoked.~%")
	  (__algo-make-vct-iterator result
								 (__replace_copy-imp-6 first last
													   (opr::vec-ptr-index  result)
													   (opr::vec-ptr-buffer result) old-val new-val #'operator_==)))

	#-cl-stl-noextra
	(defmethod-overload replace_copy ((first  input_iterator)
									  (last   input_iterator) (result vector-pointer) old-val new-val eql-bf)
	  ;;(format t "specialized replace_copy for input_iterator & vector-pointer is invoked.~%")
	  (__algo-make-vct-iterator result
								 (__replace_copy-imp-6 first last
													   (opr::vec-ptr-index  result)
													   (opr::vec-ptr-buffer result)
													   old-val new-val (functor_function (clone eql-bf))))))


  ;;PTN; replace_copy : 7 -  cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__replace_copy-imp-7 (cons1 end1 out-idx out-buf old-val new-val eql-bf)
			 (declare (type cl:list cons1 end1))
			 (declare (type fixnum out-idx))
			 (declare (type cl:vector out-buf))
			 (declare (type cl:function eql-bf))
			 (for (nil (not (eq cons1 end1)) (progn (setf cons1 (cdr cons1)) (incf out-idx)) :returns out-idx)
			   (let ((cur-val (car cons1)))
				 (if (funcall eql-bf cur-val old-val)
					 (_= (aref out-buf out-idx) new-val)
					 (_= (aref out-buf out-idx) cur-val))))))

	(defmethod-overload replace_copy ((first  cons_const_iterator)
									  (last   cons_const_iterator) (result vector-pointer) old-val new-val)
	  ;;(format t "specialized replace_copy for cons_const_iterator & vector-pointer is invoked.~%")
	  (__algo-make-vct-iterator result
								 (__replace_copy-imp-7 (__cons-itr-cons first)
													   (__cons-itr-cons  last)
													   (opr::vec-ptr-index  result)
													   (opr::vec-ptr-buffer result) old-val new-val #'operator_==)))

	#-cl-stl-noextra
	(defmethod-overload replace_copy ((first  cons_const_iterator)
									  (last   cons_const_iterator) (result vector-pointer) old-val new-val eql-bf)
	  ;;(format t "specialized replace_copy for cons_const_iterator & vector-pointer is invoked.~%")
	  (__algo-make-vct-iterator result
								 (__replace_copy-imp-7 (__cons-itr-cons first)
													   (__cons-itr-cons  last)
													   (opr::vec-ptr-index  result)
													   (opr::vec-ptr-buffer result)
													   old-val new-val (functor_function (clone eql-bf))))))

  ;;PTN; replace_copy : 8 -  cvp x  vp
  (labels ((__replace_copy-imp-8 (idx1 idx2 src-buf out-idx out-buf old-val new-val eql-bf)
			 (declare (type fixnum idx1 idx2 out-idx))
			 (declare (type cl:vector src-buf out-buf))
			 (declare (type cl:function eql-bf))
			 (for (nil (< idx1 idx2) (progn (incf idx1) (incf out-idx)) :returns out-idx)
			   (let ((cur-val (aref src-buf idx1)))
				 (if (funcall eql-bf cur-val old-val)
					 (_= (aref out-buf out-idx) new-val)
					 (_= (aref out-buf out-idx) cur-val))))))

	(defmethod-overload replace_copy ((first  const-vector-pointer)
									  (last   const-vector-pointer) (result vector-pointer) old-val new-val)
	  ;;(format t "specialized replace_copy for const-vector-pointer & vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__algo-make-vct-iterator result
								 (__replace_copy-imp-8 (opr::vec-ptr-index  first)
													   (opr::vec-ptr-index  last)
													   (opr::vec-ptr-buffer first)
													   (opr::vec-ptr-index  result)
													   (opr::vec-ptr-buffer result) old-val new-val #'operator_==)))

	#-cl-stl-noextra
	(defmethod-overload replace_copy ((first  const-vector-pointer)
									  (last   const-vector-pointer) (result vector-pointer) old-val new-val eql-bf)
	  ;;(format t "specialized replace_copy for const-vector-pointer & vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__algo-make-vct-iterator result
								 (__replace_copy-imp-8 (opr::vec-ptr-index  first)
													   (opr::vec-ptr-index  last)
													   (opr::vec-ptr-buffer first)
													   (opr::vec-ptr-index  result)
													   (opr::vec-ptr-buffer result)
													   old-val new-val (functor_function (clone eql-bf)))))))





; first     : input_iterator
; last      : input_iterator
; result    : output_iterator
; pred      : unary_function
; returns   : copy of result.
(locally (declare (optimize speed))

  ;;PTN; replace_copy_if : 0 -   i  x  o 
  (defmethod replace_copy_if ((first  input_iterator)
							  (last   input_iterator) (result output_iterator) pred new-val)
	(with-operators
		(if (_== first last)
			@~result
			(let ((pred (functor_function @~pred)))
			  (declare (type cl:function pred))
			  (for (((itr @~first) (dest @~result)) (_/= itr last) (progn ++itr ++dest) :returns dest)
				(let ((cur-val *itr))
				  (_= *dest (if (funcall pred cur-val) new-val cur-val))))))))


  ;;PTN; replace_copy_if : 1 -  cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod replace_copy_if ((first cons_const_iterator)
							  (last  cons_const_iterator) (result output_iterator) pred new-val)
	;;(format t "specialized replace_copy_if for cons_const_iterator & output_iterator is invoked.~%")
	(let ((cons1 (__cons-itr-cons first))
		  (cons2 (__cons-itr-cons  last)))
	  (declare (type cl:list cons1 cons2))
	  (with-operators
		  (if (eq cons1 cons2)
			  @~result
			  (let ((oitr (clone result))
					(uf   (functor_function (clone pred))))
				(declare (type cl:function uf))
				(for (nil (not (eq cons1 cons2)) (progn (setf cons1 (cdr cons1)) ++oitr) :returns oitr)
				  (let ((cur-val (car cons1)))
					(_= *oitr (if (funcall uf cur-val) new-val cur-val)))))))))

  
  ;;PTN; replace_copy_if : 2 -  cvp x  o 
  (defmethod replace_copy_if ((first const-vector-pointer)
							  (last  const-vector-pointer) (result output_iterator) pred new-val)
	;;(format t "specialized replace_copy_if for const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index  first))
		  (idx2 (opr::vec-ptr-index   last)))
	  (declare (type fixnum idx1 idx2))
	  (with-operators
		  (if (= idx1 idx2)
			  @~result
			  (let ((buf1 (opr::vec-ptr-buffer first))
					(oitr (clone result))
					(uf   (functor_function (clone pred))))
				(declare (type cl:vector buf1))
				(declare (type cl:function uf))
				(for (nil (< idx1 idx2) (progn (incf idx1) ++oitr) :returns oitr)
				  (let ((cur-val (aref buf1 idx1)))
					(_= *oitr (if (funcall uf cur-val) new-val cur-val)))))))))


  ;;PTN; replace_copy_if : 3 -   i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod replace_copy_if ((first input_iterator)
							  (last  input_iterator) (result cons_iterator) pred new-val)
	;;(format t "specialized replace_copy_if for input_iterator & cons_iterator is invoked.~%")
	(with-operators
		(if (_== first last)
			@~result
			(let ((out-cns (__cons-itr-cons result))
				  (uf      (functor_function (clone pred))))
			  (declare (type cl:list out-cns))
			  (declare (type cl:function uf))
			  (for (((itr1 @~first)) (_/= itr1 last) (progn ++itr1 (setf out-cns (cdr out-cns)))
													 :returns (__algo-make-cns-iterator result out-cns))
				(let ((cur-val *itr1))
				  (_= (car out-cns)
					  (if (funcall uf cur-val) new-val cur-val))))))))


  ;;PTN; replace_copy_if : 4 -  cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod replace_copy_if ((first cons_const_iterator)
							  (last  cons_const_iterator) (result cons_iterator) pred new-val)
	;;(format t "specialized replace_copy_if for cons_const_iterator & cons_iterator is invoked.~%")
	(let ((cons1 (__cons-itr-cons first))
		  (cons2 (__cons-itr-cons  last)))
	  (declare (type cl:list cons1 cons2))
	  (with-operators
		  (if (eq cons1 cons2)
			  @~result
			  (let ((out-cns (__cons-itr-cons result))
					(uf      (functor_function (clone pred))))
				(declare (type cl:list out-cns))
				(declare (type cl:function uf))
				(for (nil (not (eq cons1 cons2)) (progn (setf cons1   (cdr cons1))
														(setf out-cns (cdr out-cns)))
												 :returns (__algo-make-cns-iterator result out-cns))
				  (let ((cur-val (car cons1)))
					(_= (car out-cns)
						(if (funcall uf cur-val) new-val cur-val)))))))))

  
  ;;PTN; replace_copy_if : 5 -  cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod replace_copy_if ((first const-vector-pointer)
							  (last  const-vector-pointer) (result cons_iterator) pred new-val)
	;;(format t "specialized replace_copy_if for const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index first))
		  (idx2 (opr::vec-ptr-index  last)))
	  (declare (type fixnum idx1 idx2))
	  (with-operators
		  (if (= idx1 idx2)
			  @~result
			  (let ((src-buf (opr::vec-ptr-buffer first))
					(out-cns (__cons-itr-cons result))
					(uf      (functor_function (clone pred))))
				(declare (type cl:list out-cns))
				(declare (type cl:function uf))
				(for (nil (< idx1 idx2) (progn (incf idx1) (setf out-cns (cdr out-cns)))
										:returns (__algo-make-cns-iterator result out-cns))
				  (let ((cur-val (aref src-buf idx1)))
					(_= (car out-cns)
						(if (funcall uf cur-val) new-val cur-val)))))))))


  ;;PTN; replace_copy_if : 6 -   i  x  vp
  (defmethod replace_copy_if ((first input_iterator)
							  (last  input_iterator) (result vector-pointer) pred new-val)
	;;(format t "specialized replace_copy_if for input_iterator & vector-pointer is invoked.~%")
	(with-operators
		(if (_== first last)
			@~result
			(let ((out-idx (opr::vec-ptr-index  result))
				  (out-buf (opr::vec-ptr-buffer result))
				  (uf      (functor_function (clone pred))))
			  (declare (type fixnum out-idx))
			  (declare (type cl:vector out-buf))
			  (declare (type cl:function uf))
			  (for (((itr1 @~first)) (_/= itr1 last) (progn ++itr1 (incf out-idx))
													 :returns (__algo-make-vct-iterator result out-idx))
				(let ((cur-val *itr1))
				  (_= (aref out-buf out-idx)
					  (if (funcall uf cur-val) new-val cur-val))))))))


  ;;PTN; replace_copy_if : 7 -  cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod replace_copy_if ((first cons_const_iterator)
							  (last  cons_const_iterator) (result vector-pointer) pred new-val)
	;;(format t "specialized replace_copy_if for cons_const_iterator & vector-pointer is invoked.~%")
	(let ((cons1 (__cons-itr-cons first))
		  (cons2 (__cons-itr-cons  last)))
	  (declare (type cl:list cons1 cons2))
	  (with-operators
		  (if (eq cons1 cons2)
			  @~result
			  (let ((out-idx (opr::vec-ptr-index  result))
					(out-buf (opr::vec-ptr-buffer result))
					(uf      (functor_function (clone pred))))
				(declare (type fixnum out-idx))
				(declare (type cl:vector out-buf))
				(declare (type cl:function uf))
				(for (nil (not (eq cons1 cons2)) (progn (setf cons1 (cdr cons1)) (incf out-idx))
												 :returns (__algo-make-vct-iterator result out-idx))
				  (let ((cur-val (car cons1)))
					(_= (aref out-buf out-idx)
						(if (funcall uf cur-val) new-val cur-val)))))))))

  
  ;;PTN; replace_copy_if : 8 -  cvp x  vp
  (defmethod replace_copy_if ((first const-vector-pointer)
							  (last  const-vector-pointer) (result vector-pointer) pred new-val)
	;;(format t "specialized replace_copy_if for const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index first))
		  (idx2 (opr::vec-ptr-index  last)))
	  (declare (type fixnum idx1 idx2))
	  (with-operators
		  (if (= idx1 idx2)
			  @~result
			  (let ((src-buf (opr::vec-ptr-buffer  first))
					(out-idx (opr::vec-ptr-index  result))
					(out-buf (opr::vec-ptr-buffer result))
					(uf      (functor_function (clone pred))))
				(declare (type fixnum out-idx))
				(declare (type cl:vector src-buf out-buf))
				(declare (type cl:function uf))
				(for (nil (< idx1 idx2) (progn (incf idx1) (incf out-idx))
										:returns (__algo-make-vct-iterator result out-idx))
				  (let ((cur-val (aref src-buf idx1)))
					(_= (aref out-buf out-idx)
						(if (funcall uf cur-val) new-val cur-val))))))))))




; first     : forward_iterator
; last      : forward_iterator
; returns   : nil.
(locally (declare (optimize speed))

  ;;PTN; fill : 0 -   f 
  (defmethod-overload fill ((first forward_iterator) (last forward_iterator) value)
	(if (_== first last)
		nil
		(with-operators
			(for (((itr @~first)) (_/= itr last) ++itr)
			  (_= *itr value)))))

  ;;PTN; fill : 1 -   ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload fill ((first cons_iterator) (last cons_iterator) v)
	;;(format t "specialized fill for cons_iterator is invoked.~%")
	(let ((cons1 (__cons-itr-cons first))
		  (cons2 (__cons-itr-cons  last)))
	  (declare (type cl:list cons1 cons2))
	  (for (nil (not (eq cons1 cons2)) (setf cons1 (cdr cons1)))
		(_= (car cons1) v))))

  ;;PTN; fill : 2 -   vp
  (defmethod-overload fill ((first vector-pointer) (last vector-pointer) v)
	;;(format t "specialized fill for vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index  first))
		  (idx2 (opr::vec-ptr-index   last))
		  (buf  (opr::vec-ptr-buffer first)))
	  (declare (type fixnum idx1 idx2))
	  (declare (type cl:vector buf))
	  (for (nil (< idx1 idx2) (incf idx1))
		(_= (aref buf idx1) v)))))
			 



; first     : output_iterator
; n         : fixnum
; returns   : nil(0x98) / output_iterator(0x11).
(locally (declare (optimize speed))

  ;;PTN; fill_n : 0 -   o 
  (defmethod fill_n ((first output_iterator) (n integer) v)
	(declare (type fixnum n))
	(with-operators
		(if (<= n 0)
			#+cl-stl-0x98 nil
			#-cl-stl-0x98 @~first
			(for (((itr @~first)) (< 0 n) (progn ++itr (decf n)) :returns #+cl-stl-0x98 nil
																		  #-cl-stl-0x98 itr)
			  (_= *itr v)))))

  ;;PTN; fill_n : 1 -   ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod fill_n ((first cons_iterator) (n integer) v)
	;;(format t "specialized fill_n for cons_iterator is invoked.~%")
	(declare (type fixnum n))
	(let ((cons1 (__cons-itr-cons first)))
	  (declare (type cl:list cons1))
	  (let ((ret (for (nil (< 0 n) (progn (decf n)
										  (setf cons1 (cdr cons1))) :returns cons1)
				   (_= (car cons1) v))))
		(declare (ignorable ret))
		#+cl-stl-0x98 nil
		#-cl-stl-0x98 (__algo-make-cns-iterator first ret))))

  ;;PTN; fill_n : 2 -   vp
  (defmethod fill_n ((first vector-pointer) (n integer) v)
	;;(format t "specialized fill_n for vector-pointer is invoked.~%")
	(declare (type fixnum n))
	(let ((idx (opr::vec-ptr-index  first))
		  (buf (opr::vec-ptr-buffer first)))
	  (declare (type fixnum    idx))
	  (declare (type cl:vector buf))
	  (let ((ret (for (nil (< 0 n) (progn (incf idx) (decf n)) :returns idx)
				   (_= (aref buf idx) v))))
		(declare (ignorable ret))
		#+cl-stl-0x98 nil
		#-cl-stl-0x98 (__algo-make-vct-iterator first ret)))))




; first     : forward_iterator
; last      : forward_iterator
; gen       : function ( take zero argument )
; returns   : nil.
(locally (declare (optimize speed))

  ;;PTN; generate : 0 -   f 
  (defmethod generate ((first forward_iterator) (last forward_iterator) gen)
	; MEMO : gen must be function in 0x98.
	(if (_== first last)
		nil
		(with-operators
			(let ((gen (functor_function @~gen)))
			  (declare (type cl:function gen))
			  (for (((itr @~first)) (_/= itr last) ++itr)
				(_= *itr (funcall gen)))))))

  ;;PTN; generate : 1 -   ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod generate ((first cons_iterator) (last cons_iterator) gen)
	;; MEMO : in 0x98, gen must be function...
	;;(format t "specialized generate for cons_iterator is invoked.~%")
	(let ((cons1 (__cons-itr-cons first))
		  (cons2 (__cons-itr-cons  last)))
	  (declare (type cl:list cons1 cons2))
	  (if (eq cons1 cons2)
		  nil
		  (let ((gen (functor_function (clone gen))))
			(declare (type cl:function gen))
			(for (nil (not (eq cons1 cons2)) (setf cons1 (cdr cons1)))
			  (_= (car cons1) (funcall gen)))))))

  ;;PTN; generate : 2 -   vp
  (defmethod generate ((first vector-pointer) (last vector-pointer) gen)
	;; MEMO : in 0x98, gen must be function...
	;;(format t "specialized generate for vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index first))
		  (idx2 (opr::vec-ptr-index  last)))
	  (declare (type fixnum idx1 idx2))
	  (if (= idx1 idx2)
		  nil
		  (let ((buf (opr::vec-ptr-buffer first))
				(gen (functor_function (clone gen))))
			(declare (type cl:vector   buf))
			(declare (type cl:function gen))
			(for (nil (< idx1 idx2) (incf idx1))
			  (_= (aref buf idx1) (funcall gen))))))))




; first     : output_iterator
; n         : fixnum
; gen       : function ( take zero argument )
; returns   : nil(0x98) / output_iterator(0x11).
(locally (declare (optimize speed))

  ;;PTN; generate_n : 0 -   o 
  (defmethod generate_n ((first output_iterator) (n integer) gen)
	;; MEMO : gen must be function in 0x98.
	(declare (type fixnum n))
	(__error-unless-non-negative-fixnum generate_n n)
	(with-operators
		(if (<= n 0)
			#+cl-stl-0x98 nil
			#-cl-stl-0x98 @~first
			(let ((gen (functor_function @~gen)))
			  (declare (type cl:function gen))
			  (for (((itr @~first)) (< 0 n) (progn ++itr (decf n)) :returns #+cl-stl-0x98 nil
																			#-cl-stl-0x98 itr)
				(_= *itr (funcall gen)))))))

  ;;PTN; generate_n : 1 -   ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod generate_n ((first cons_iterator) (n integer) gen)
	;; MEMO : gen must be function in 0x98.
	;;(format t "specialized generate_n for cons_iterator is invoked.~%")
	(declare (type fixnum n))
	(__error-unless-non-negative-fixnum generate_n n)
	(if (<= n 0)
		#+cl-stl-0x98 nil
		#-cl-stl-0x98 (clone first)
		(let ((cns (__cons-itr-cons  first))
			  (gen (functor_function (clone gen))))
		  (declare (type cl:list     cns))
		  (declare (type cl:function gen))
		  (let ((ret (for (nil (< 0 n) (progn (decf n)
											  (setf cns (cdr cns))) :returns cns)
					   (_= (car cns) (funcall gen)))))
			(declare (ignorable ret))
			#+cl-stl-0x98 nil
			#-cl-stl-0x98 (__algo-make-cns-iterator first ret)))))

  ;;PTN; generate_n : 2 -   vp
  (defmethod generate_n ((first vector-pointer) (n integer) gen)
	;; MEMO : gen must be function in 0x98.
	;;(format t "specialized generate_n for vector-pointer is invoked.~%")
	(declare (type fixnum n))
	(__error-unless-non-negative-fixnum generate_n n)
	(if (<= n 0)
		#+cl-stl-0x98 nil
		#-cl-stl-0x98 (clone first)
		(let ((idx (opr::vec-ptr-index  first))
			  (buf (opr::vec-ptr-buffer first))
			  (gen (functor_function (clone gen))))
		  (declare (type fixnum      idx))
		  (declare (type cl:vector   buf))
		  (declare (type cl:function gen))
		  (let ((ret (for (nil (< 0 n) (progn (incf idx) (decf n)) :returns idx)
					   (_= (aref buf idx) (funcall gen)))))
			(declare (ignorable ret))
			#+cl-stl-0x98 nil
			#-cl-stl-0x98 (__algo-make-vct-iterator first ret))))))




; first     : forward_iterator
; last      : forward_iterator
; eql-bf    : binary_function ( added by CL-STL. default : #'operator_== )
; returns   : copy of first ( point to new end of sequence ).
(locally (declare (optimize speed))

  ;;PTN; remove : 0 -   f 
  (labels ((__remove-imp-0 (first last value eql-bf)
			 (with-operators
				 (if (_== first last)
					 @~last
					 (let ((in @~first)
						   (eql-bf (functor_function @~eql-bf)))
					   (declare (type cl:function eql-bf))
					   (for (nil (_/= in last) ++in)
						 (when (funcall eql-bf *in value)
						   (return)))
					   (if (_== in last)
						   in
						   (let ((out @~in))
							 ++in
							 (for (nil (_/= in last) ++in :returns out)
							   (let ((cur-val *in))
								 (unless (funcall eql-bf value cur-val)
								   #+cl-stl-0x98 (_= *out cur-val)
								   #-cl-stl-0x98 (multiple-value-bind (a b) (operator_move *out cur-val)
												   (setf *in  b)
												   (setf *out a))
								   ++out))))))))))

	(defmethod-overload remove ((first forward_iterator) (last forward_iterator) value)
	  (__remove-imp-0 first last value #'operator_==))

	#-cl-stl-noextra
	(defmethod-overload remove ((first forward_iterator) (last forward_iterator) value eql-bf)
	  (__remove-imp-0 first last value eql-bf)))

  ;;PTN; remove : 1 -   ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__remove-imp-1 (cons1 cons2 val eql-bf)
			 (declare (type cl:list cons1 cons2))
			 (if (eq cons1 cons2)
				 cons2
				 (let ((eql-bf (functor_function (clone eql-bf))))
				   (declare (type cl:function eql-bf))
				   (for (nil (not (eq cons1 cons2)) (setf cons1 (cdr cons1)))
					 (when (funcall eql-bf (car cons1) val)
					   (return)))
				   (if (eq cons1 cons2)
					   cons2
					   (let ((out cons1))
						 (declare (type cl:list out))
						 (setf cons1 (cdr cons1))
						 (for (nil (not (eq cons1 cons2)) (setf cons1 (cdr cons1)) :returns out)
						   (let ((cur-val (car cons1)))
							 (unless (funcall eql-bf val cur-val)
							   #+cl-stl-0x98 (_= (car out) cur-val)
							   #-cl-stl-0x98 (multiple-value-bind (a b) (operator_move (car out) cur-val)
											   (setf (car cons1) b)
											   (setf (car   out) a))
							   (setf out (cdr out)))))))))))

	(defmethod-overload remove ((first cons_iterator) (last cons_iterator) val)
	  ;;(format t "specialized remove for cons_iterator is invoked.~%")
	  (__algo-make-cns-iterator first
								 (__remove-imp-1 (__cons-itr-cons first)
												 (__cons-itr-cons  last) val #'operator_==)))

	#-cl-stl-noextra
	(defmethod-overload remove ((first cons_iterator) (last cons_iterator) val eql-bf)
	  ;;(format t "specialized remove for cons_iterator is invoked.~%")
	  (__algo-make-cns-iterator first
								 (__remove-imp-1 (__cons-itr-cons first)
												 (__cons-itr-cons  last) val eql-bf))))

  ;;PTN; remove : 2 -   vp
  (labels ((__remove-imp-2 (idx1 idx2 buffer val eql-bf)
			 (declare (type fixnum idx1 idx2))
			 (declare (type cl:vector buffer))
			 (if (= idx1 idx2)
				 idx2
				 (let ((eql-bf (functor_function (clone eql-bf))))
				   (declare (type cl:function eql-bf))
				   (for (nil (< idx1 idx2) (incf idx1))
					 (when (funcall eql-bf (aref buffer idx1) val)
					   (return)))
				   (if (= idx1 idx2)
					   idx2
					   (let ((out idx1))
						 (declare (type fixnum out))
						 (incf idx1)
						 (for (nil (< idx1 idx2) (incf idx1) :returns out)
						   (let ((cur-val (aref buffer idx1)))
							 (unless (funcall eql-bf val cur-val)
							   #+cl-stl-0x98 (_= (aref buffer out) cur-val)
							   #-cl-stl-0x98 (multiple-value-bind (a b) (operator_move (aref buffer out) cur-val)
											   (setf (aref buffer idx1) b)
											   (setf (aref buffer  out) a))
							   (incf out))))))))))

	(defmethod-overload remove ((first vector-pointer) (last vector-pointer) val)
	  ;;(format t "specialized remove for vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__algo-make-vct-iterator first
								 (__remove-imp-2 (opr::vec-ptr-index  first)
												 (opr::vec-ptr-index  last)
												 (opr::vec-ptr-buffer first) val #'operator_==)))

	#-cl-stl-noextra
	(defmethod-overload remove ((first vector-pointer) (last vector-pointer) val eql-bf)
	  ;;(format t "specialized remove for vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__algo-make-vct-iterator first
								 (__remove-imp-2 (opr::vec-ptr-index  first)
												 (opr::vec-ptr-index  last)
												 (opr::vec-ptr-buffer first) val eql-bf)))))




; first     : forward_iterator
; last      : forward_iterator
; pred      : unary_function
; returns   : copy of first ( point to new end of sequence ).
(locally (declare (optimize speed))

  ;;PTN; remove_if : 0 -   f 
  (defmethod-overload remove_if ((first forward_iterator) (last forward_iterator) pred)
	(with-operators
		(if (_== first last)
			@~last
			(let ((in   @~first)
				  (pred (functor_function @~pred)))
			  (declare (type cl:function pred))
			  (for (nil (_/= in last) ++in)
				(when (funcall pred *in)
				  (return)))
			  (if (_== in last)
				  in
				  (let ((out @~in))
					++in
					(for (nil (_/= in last) ++in :returns out)
					  (let ((cur-val *in))
						(unless (funcall pred cur-val)
						  #+cl-stl-0x98 (_= *out cur-val)
						  #-cl-stl-0x98 (multiple-value-bind (a b) (operator_move *out cur-val)
										  (setf *in  b)
										  (setf *out a))
						  ++out)))))))))

  ;;PTN; remove_if : 1 -   ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload remove_if ((first cons_iterator) (last cons_iterator) pred)
	;;(format t "specialized remove_if for cons_iterator is invoked.~%" ')
	(let ((cons1 (__cons-itr-cons first))
		  (cons2 (__cons-itr-cons  last)))
	  (declare (type cl:list cons1 cons2))
	  (if (eq cons1 cons2)
		  (clone last)
		  (let ((pred (functor_function (clone pred))))
			(declare (type cl:function pred))
			(for (nil (not (eq cons1 cons2)) (setf cons1 (cdr cons1)))
			  (when (funcall pred (car cons1))
				(return)))
			(if (eq cons1 cons2)
				(__algo-make-cns-iterator first cons2)
				(let ((out cons1))
				  (declare (type cl:list out))
				  (setf cons1 (cdr cons1))
				  (for (nil (not (eq cons1 cons2)) (setf cons1 (cdr cons1))
												   :returns (__algo-make-cns-iterator first out))
					(let ((cur-val (car cons1)))
					  (unless (funcall pred cur-val)
						#+cl-stl-0x98 (_= (car out) cur-val)
						#-cl-stl-0x98 (multiple-value-bind (a b)
										  (operator_move (car out) cur-val)
										(setf (car cons1) b)
										(setf (car   out) a))
						(setf out (cdr out)))))))))))

  ;;PTN; remove_if : 2 -   vp
  (defmethod-overload remove_if ((first vector-pointer) (last vector-pointer) pred)
	;;(format t "specialized remove_if for vector-pointer is invoked.~%" ')
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index first))
		  (idx2 (opr::vec-ptr-index  last)))
	  (declare (type fixnum idx1 idx2))
	  (if (= idx1 idx2)
		  (clone last)
		  (let ((buf  (opr::vec-ptr-buffer first))
				(pred (functor_function (clone pred))))
			(declare (type cl:vector   buf))
			(declare (type cl:function pred))
			(for (nil (< idx1 idx2) (incf idx1))
			  (when (funcall pred (aref buf idx1))
				(return)))
			(if (= idx1 idx2)
				(__algo-make-vct-iterator first idx1)
				(let ((out idx1))
				  (declare (type fixnum out))
				  (incf idx1)
				  (for (nil (< idx1 idx2) (incf idx1) :returns (__algo-make-vct-iterator first out))
					(let ((cur-val (aref buf idx1)))
					  (unless (funcall pred cur-val)
						#+cl-stl-0x98 (_= (aref buf out) cur-val)
						#-cl-stl-0x98 (multiple-value-bind (a b)
										  (operator_move (aref buf out) cur-val)
										(setf (aref buf idx1) b)
										(setf (aref buf  out) a))
						(incf out)))))))))))




; first     : input_iterator
; last      : input_iterator
; eql-bf    : binary_function ( added by CL-STL. default : #'operator_== )
; result    : output_iterator
; returns   : copy of result ( point to end of sequence ).
(locally (declare (optimize speed))

  ;;PTN; remove_copy : 0 -   i  x  o 
  (labels ((__remove_copy-imp-0 (first last result val eql-bf)
			 (declare (type cl:function eql-bf))
			 (with-operators
				 (if (_== first last)
					 @~result
					 (for (((itr @~first) (dest @~result)) (_/= itr last) ++itr :returns dest)
					   (let ((cur-val *itr))
						 (unless (funcall eql-bf val cur-val)
						   (_= *dest cur-val)
						   ++dest)))))))
			 
	(defmethod-overload remove_copy ((first input_iterator)
									 (last  input_iterator) (result output_iterator) val)
	  (__remove_copy-imp-0 first last result val #'operator_==))

	#-cl-stl-noextra
	(defmethod-overload remove_copy ((first input_iterator)
									 (last  input_iterator) (result output_iterator) val eql-bf)
	  (__remove_copy-imp-0 first last result val (functor_function (clone eql-bf)))))


  ;;PTN; remove_copy : 1 -  cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__remove_copy-imp-1 (cons1 end1 oitr val eql-bf)
			 (declare (type cl:list cons1 end1))
			 (declare (type cl:function eql-bf))
			 (with-operators
				 (for (nil (not (eq cons1 end1)) (setf cons1 (cdr cons1)) :returns oitr)
				   (let ((cur-val (car cons1)))
					 (unless (funcall eql-bf val cur-val)
					   (_= *oitr cur-val)
					   ++oitr))))))

	(defmethod-overload remove_copy ((first cons_const_iterator)
									 (last  cons_const_iterator) (result output_iterator) val)
	  ;;(format t "specialized remove_copy for cons_const_iterator & output_iterator is invoked.~%")
	  (__remove_copy-imp-1 (__cons-itr-cons first)
						   (__cons-itr-cons  last) (clone result) val #'operator_==))
	#-cl-stl-noextra
	(defmethod-overload remove_copy ((first cons_const_iterator)
									 (last  cons_const_iterator) (result output_iterator) val eql-bf)
	  ;;(format t "specialized remove_copy for cons_const_iterator & output_iterator is invoked.~%")
	  (__remove_copy-imp-1 (__cons-itr-cons first)
						   (__cons-itr-cons  last) (clone result) val (functor_function (clone eql-bf)))))


  ;;PTN; remove_copy : 2 -  cvp x  o 
  (labels ((__remove_copy-imp-2 (idx1 idx2 buffer oitr val eql-bf)
			 (declare (type fixnum idx1 idx2))
			 (declare (type cl:vector buffer))
			 (declare (type cl:function eql-bf))
			 (with-operators
				 (for (nil (< idx1 idx2) (incf idx1) :returns oitr)
				   (let ((cur-val (aref buffer idx1)))
					 (unless (funcall eql-bf val cur-val)
					   (_= *oitr cur-val)
					   ++oitr))))))

	(defmethod-overload remove_copy ((first const-vector-pointer)
									 (last  const-vector-pointer) (result output_iterator) val)
	  ;;(format t "specialized remove_copy for const-vector-pointer & output_iterator is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__remove_copy-imp-2 (opr::vec-ptr-index  first)
						   (opr::vec-ptr-index  last)
						   (opr::vec-ptr-buffer first)
						   (clone result) val #'operator_==))
	#-cl-stl-noextra
	(defmethod-overload remove_copy ((first const-vector-pointer)
									 (last  const-vector-pointer) (result output_iterator) val eql-bf)
	  ;;(format t "specialized remove_copy for const-vector-pointer & output_iterator is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__remove_copy-imp-2 (opr::vec-ptr-index  first)
						   (opr::vec-ptr-index  last)
						   (opr::vec-ptr-buffer first)
						   (clone result) val (functor_function (clone eql-bf)))))

  
  ;;PTN; remove_copy : 3 -   i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__remove_copy-imp-3 (first1 last1 out val eql-bf)
			 (declare (type cl:list out))
			 (declare (type cl:function eql-bf))
			 (with-operators
				 (for (((itr1 @~first1)) (_/= itr1 last1) ++itr1 :returns out)
				   (let ((cur-val *itr1))
					 (unless (funcall eql-bf val cur-val)
					   (_= (car out) cur-val)
					   (setf out (cdr out))))))))

	(defmethod-overload remove_copy ((first input_iterator)
									 (last  input_iterator) (result cons_iterator) val)
	  ;;(format t "specialized remove_copy for input_iterator & cons_iterator is invoked.~%")
	  (__algo-make-cns-iterator result
								 (__remove_copy-imp-3 first last
													  (__cons-itr-cons result) val #'operator_==)))
	#-cl-stl-noextra
	(defmethod-overload remove_copy ((first input_iterator)
									 (last  input_iterator) (result cons_iterator) val eql-bf)
	  ;;(format t "specialized remove_copy for input_iterator & cons_iterator is invoked.~%")
	  (__algo-make-cns-iterator result
								 (__remove_copy-imp-3 first last
													  (__cons-itr-cons result)
													  val (functor_function (clone eql-bf))))))
  
  ;;PTN; remove_copy : 4 -  cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__remove_copy-imp-4 (cons1 end1 out val eql-bf)
			 (declare (type cl:list cons1 end1 out))
			 (declare (type cl:function eql-bf))
			 (for (nil (not (eq cons1 end1)) (setf cons1 (cdr cons1)) :returns out)
			   (let ((cur-val (car cons1)))
				 (unless (funcall eql-bf val cur-val)
				   (_= (car out) cur-val)
				   (setf out (cdr out)))))))

	(defmethod-overload remove_copy ((first cons_const_iterator)
									 (last  cons_const_iterator) (result cons_iterator) val)
	  ;;(format t "specialized remove_copy for cons_const_iterator & cons_iterator is invoked.~%")
	  (__algo-make-cns-iterator result
								 (__remove_copy-imp-4 (__cons-itr-cons  first)
													  (__cons-itr-cons   last)
													  (__cons-itr-cons result) val #'operator_==)))
	#-cl-stl-noextra
	(defmethod-overload remove_copy ((first cons_const_iterator)
									 (last  cons_const_iterator) (result cons_iterator) val eql-bf)
	  ;;(format t "specialized remove_copy for cons_const_iterator & cons_iterator is invoked.~%")
	  (__algo-make-cns-iterator result
								 (__remove_copy-imp-4 (__cons-itr-cons  first)
													  (__cons-itr-cons   last)
													  (__cons-itr-cons result)
													  val (functor_function (clone eql-bf))))))
  
  ;;PTN; remove_copy : 5 -  cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__remove_copy-imp-5 (idx1 idx2 src-buf out val eql-bf)
			 (declare (type fixnum idx1 idx2))
			 (declare (type cl:vector src-buf))
			 (declare (type cl:list out))
			 (declare (type cl:function eql-bf))
			 (for (nil (< idx1 idx2) (incf idx1) :returns out)
			   (let ((cur-val (aref src-buf idx1)))
				 (unless (funcall eql-bf val cur-val)
				   (_= (car out) cur-val)
				   (setf out (cdr out)))))))

	(defmethod-overload remove_copy ((first const-vector-pointer)
									 (last  const-vector-pointer) (result cons_iterator) val)
	  ;;(format t "specialized remove_copy for const-vector-pointer & cons_iterator is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__algo-make-cns-iterator result
								 (__remove_copy-imp-5 (opr::vec-ptr-index  first)
													  (opr::vec-ptr-index  last)
													  (opr::vec-ptr-buffer first)
													  (__cons-itr-cons    result) val #'operator_==)))
	#-cl-stl-noextra
	(defmethod-overload remove_copy ((first const-vector-pointer)
									 (last  const-vector-pointer) (result cons_iterator) val eql-bf)
	  ;;(format t "specialized remove_copy for const-vector-pointer & cons_iterator is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__algo-make-cns-iterator result
								 (__remove_copy-imp-5 (opr::vec-ptr-index  first)
													  (opr::vec-ptr-index  last)
													  (opr::vec-ptr-buffer first)
													  (__cons-itr-cons    result)
													  val (functor_function (clone eql-bf))))))

  ;;PTN; remove_copy : 6 -   i  x  vp
  (labels ((__remove_copy-imp-6 (first1 last1 out-idx out-buf val eql-bf)
			 (declare (type fixnum out-idx))
			 (declare (type cl:vector out-buf))
			 (declare (type cl:function eql-bf))
			 (with-operators
				 (for (((itr1 @~first1)) (_/= itr1 last1) ++itr1 :returns out-idx)
				   (let ((cur-val *itr1))
					 (unless (funcall eql-bf val cur-val)
					   (_= (aref out-buf out-idx) cur-val)
					   (incf out-idx)))))))

	(defmethod-overload remove_copy ((first input_iterator)
									 (last  input_iterator) (result vector-pointer) val)
	  ;;(format t "specialized remove_copy for input_iterator & vector-pointer is invoked.~%")
	  (__algo-make-vct-iterator result
								 (__remove_copy-imp-6 first last
													  (opr::vec-ptr-index  result)
													  (opr::vec-ptr-buffer result) val #'operator_==)))

	#-cl-stl-noextra
	(defmethod-overload remove_copy ((first input_iterator)
									 (last  input_iterator) (result vector-pointer) val eql-bf)
	  ;;(format t "specialized remove_copy for input_iterator & vector-pointer is invoked.~%")
	  (__algo-make-vct-iterator result
								 (__remove_copy-imp-6 first last
													  (opr::vec-ptr-index  result)
													  (opr::vec-ptr-buffer result)
													  val (functor_function (clone eql-bf))))))
  
  ;;PTN; remove_copy : 7 -  cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__remove_copy-imp-7 (cons1 end1 out-idx out-buf val eql-bf)
			 (declare (type cl:list cons1 end1))
			 (declare (type fixnum out-idx))
			 (declare (type cl:vector out-buf))
			 (declare (type cl:function eql-bf))
			 (for (nil (not (eq cons1 end1)) (setf cons1 (cdr cons1)) :returns out-idx)
			   (let ((cur-val (car cons1)))
				 (unless (funcall eql-bf val cur-val)
				   (_= (aref out-buf out-idx) cur-val)
				   (incf out-idx))))))

	(defmethod-overload remove_copy ((first cons_const_iterator)
									 (last  cons_const_iterator) (result vector-pointer) val)
	  ;;(format t "specialized remove_copy for cons_const_iterator & vector-pointer is invoked.~%")
	  (__algo-make-vct-iterator result
								 (__remove_copy-imp-7 (__cons-itr-cons first)
													  (__cons-itr-cons  last)
													  (opr::vec-ptr-index  result)
													  (opr::vec-ptr-buffer result) val #'operator_==)))

	#-cl-stl-noextra
	(defmethod-overload remove_copy ((first cons_const_iterator)
									 (last  cons_const_iterator) (result vector-pointer) val eql-bf)
	  ;;(format t "specialized remove_copy for cons_const_iterator & vector-pointer is invoked.~%")
	  (__algo-make-vct-iterator result
								 (__remove_copy-imp-7 (__cons-itr-cons first)
													  (__cons-itr-cons  last)
													  (opr::vec-ptr-index  result)
													  (opr::vec-ptr-buffer result) val (functor_function (clone eql-bf))))))
  
  ;;PTN; remove_copy : 8 -  cvp x  vp
  (labels ((__remove_copy-imp-8 (idx1 idx2 src-buf out-idx out-buf val eql-bf)
			 (declare (type fixnum idx1 idx2 out-idx))
			 (declare (type cl:vector src-buf out-buf))
			 (declare (type cl:function eql-bf))
			 (for (nil (< idx1 idx2) (incf idx1) :returns out-idx)
			   (let ((cur-val (aref src-buf idx1)))
				 (unless (funcall eql-bf val cur-val)
				   (_= (aref out-buf out-idx) cur-val)
				   (incf out-idx))))))

	(defmethod-overload remove_copy ((first const-vector-pointer)
									 (last  const-vector-pointer) (result vector-pointer) val)
	  ;;(format t "specialized remove_copy for const-vector-pointer & vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__algo-make-vct-iterator result
								 (__remove_copy-imp-8 (opr::vec-ptr-index  first)
													  (opr::vec-ptr-index  last)
													  (opr::vec-ptr-buffer first)
													  (opr::vec-ptr-index  result)
													  (opr::vec-ptr-buffer result) val #'operator_==)))

	#-cl-stl-noextra
	(defmethod-overload remove_copy ((first const-vector-pointer)
									 (last  const-vector-pointer) (result vector-pointer) val eql-bf)
	  ;;(format t "specialized remove_copy for const-vector-pointer & vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__algo-make-vct-iterator result
								 (__remove_copy-imp-8 (opr::vec-ptr-index  first)
													  (opr::vec-ptr-index  last)
													  (opr::vec-ptr-buffer first)
													  (opr::vec-ptr-index  result)
													  (opr::vec-ptr-buffer result)
													  val (functor_function (clone eql-bf)))))))


; first     : input_iterator
; last      : input_iterator
; result    : output_iterator
; pred      : unary_function
; returns   : copy of result ( point to end of sequence ).
(locally (declare (optimize speed))

  ;;PTN; remove_copy_if : 0 -   i  x  o 
  (defmethod remove_copy_if ((first input_iterator) (last input_iterator) (result output_iterator) pred)
	(with-operators
		(if (_== first last)
			@~result
			(let ((pred (functor_function @~pred)))
			  (declare (type cl:function pred))
			  (for (((itr @~first) (dest @~result)) (_/= itr last) ++itr :returns dest)
				(let ((cur-val *itr))
				  (unless (funcall pred cur-val)
					(_= *dest cur-val)
					++dest)))))))


  ;;PTN; remove_copy_if : 1 -  cci x  o
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod remove_copy_if ((first cons_const_iterator)
							 (last  cons_const_iterator) (result output_iterator) pred)
	;;(format t "specialized remove_copy_if for cons_const_iterator & output_iterator is invoked.~%")
	(let ((cons1 (__cons-itr-cons first))
		  (cons2 (__cons-itr-cons  last)))
	  (declare (type cl:list cons1 cons2))
	  (with-operators
		  (if (eq cons1 cons2)
			  @~result
			  (let ((oitr (clone result))
					(uf   (functor_function (clone pred))))
				(declare (type cl:function uf))
				(for (nil (not (eq cons1 cons2)) (setf cons1 (cdr cons1)) :returns oitr)
				  (let ((cur-val (car cons1)))
					(unless (funcall uf cur-val)
					  (_= *oitr cur-val)
					  ++oitr))))))))


  ;;PTN; remove_copy_if : 2 -  cvp x  o 
  (defmethod remove_copy_if ((first const-vector-pointer)
							 (last  const-vector-pointer) (result output_iterator) pred)
	;;(format t "specialized remove_copy_if for const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index  first))
		  (idx2 (opr::vec-ptr-index   last)))
	  (declare (type fixnum idx1 idx2))
	  (with-operators
		  (if (= idx1 idx2)
			  @~result
			  (let ((buf1 (opr::vec-ptr-buffer first))
					(oitr (clone result))
					(uf   (functor_function (clone pred))))
				(declare (type cl:vector   buf1))
				(declare (type cl:function uf))
				(for (nil (< idx1 idx2) (incf idx1) :returns oitr)
				  (let ((cur-val (aref buf1 idx1)))
					(unless (funcall uf cur-val)
					  (_= *oitr cur-val)
					  ++oitr))))))))

  ;;PTN; remove_copy_if : 3 -   i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod remove_copy_if ((first input_iterator) (last input_iterator) (result cons_iterator) pred)
	;;(format t "specialized remove_copy_if for input_iterator & cons_iterator is invoked.~%")
	(with-operators
		(if (_== first last)
			@~result
			(let ((out-cns (__cons-itr-cons  result))
				  (uf      (functor_function @~pred)))
			  (declare (type cl:list out-cns))
			  (declare (type cl:function uf))
			  (for (((itr1 @~first)) (_/= itr1 last) ++itr1
									 :returns (__algo-make-cns-iterator result out-cns))
				(let ((cur-val *itr1))
				  (unless (funcall uf cur-val)
					(_= (car out-cns) cur-val)
					(setf out-cns (cdr out-cns)))))))))


  ;;PTN; remove_copy_if : 4 -  cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod remove_copy_if ((first cons_const_iterator)
							 (last  cons_const_iterator) (result cons_iterator) pred)
	;;(format t "specialized remove_copy_if for cons_const_iterator & cons_iterator is invoked.~%")
	(let ((cons1 (__cons-itr-cons first))
		  (cons2 (__cons-itr-cons  last)))
	  (declare (type cl:list cons1 cons2))
	  (if (eq cons1 cons2)
		  (clone result)
		  (let ((out-cns (__cons-itr-cons  result))
				(uf      (functor_function (clone pred))))
			(declare (type cl:list     out-cns))
			(declare (type cl:function uf))
			(for (nil (not (eq cons1 cons2)) (setf cons1 (cdr cons1))
											 :returns (__algo-make-cns-iterator result out-cns))
			  (let ((cur-val (car cons1)))
				(unless (funcall uf cur-val)
				  (_= (car out-cns) cur-val)
				  (setf out-cns (cdr out-cns)))))))))

  
  ;;PTN; remove_copy_if : 5 -  cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod remove_copy_if ((first const-vector-pointer)
							 (last  const-vector-pointer) (result cons_iterator) pred)
	;;(format t "specialized remove_copy_if for const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index  first))
		  (idx2 (opr::vec-ptr-index   last)))
	  (declare (type fixnum idx1 idx2))
	  (if (= idx1 idx2)
		  (clone result)
		  (let ((src-buf (opr::vec-ptr-buffer first))
				(out-cns (__cons-itr-cons     result))
				(uf      (functor_function (clone pred))))
			(declare (type cl:list     out-cns))
			(declare (type cl:vector   src-buf))
			(declare (type cl:function uf))
			(for (nil (< idx1 idx2) (incf idx1)
									:returns (__algo-make-cns-iterator result out-cns))
			  (let ((cur-val (aref src-buf idx1)))
				(unless (funcall uf cur-val)
				  (_= (car out-cns) cur-val)
				  (setf out-cns (cdr out-cns)))))))))


  ;;PTN; remove_copy_if : 6 -   i  x  vp
  (defmethod remove_copy_if ((first input_iterator) (last input_iterator) (result vector-pointer) pred)
	;;(format t "specialized remove_copy_if for input_iterator & vector-pointer is invoked.~%")
	(with-operators
		(if (_== first last)
			@~result
			(let ((out-idx (opr::vec-ptr-index  result))
				  (out-buf (opr::vec-ptr-buffer result))
				  (uf      (functor_function @~pred)))
			  (declare (type fixnum out-idx))
			  (declare (type cl:vector out-buf))
			  (declare (type cl:function uf))
			  (for (((itr1 @~first)) (_/= itr1 last) ++itr1
									 :returns (__algo-make-vct-iterator result out-idx))
				(let ((cur-val *itr1))
				  (unless (funcall uf cur-val)
					(_= (aref out-buf out-idx) cur-val)
					(incf out-idx))))))))


  ;;PTN; remove_copy_if : 7 -  cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod remove_copy_if ((first cons_const_iterator)
							 (last  cons_const_iterator) (result vector-pointer) pred)
	;;(format t "specialized remove_copy_if for cons_const_iterator & vector-pointer is invoked.~%")
	(let ((cons1 (__cons-itr-cons first))
		  (cons2 (__cons-itr-cons  last)))
	  (declare (type cl:list cons1 cons2))
	  (if (eq cons1 cons2)
		  (clone result)
		  (let ((out-idx (opr::vec-ptr-index  result))
				(out-buf (opr::vec-ptr-buffer result))
				(uf      (functor_function (clone pred))))
			(declare (type fixnum    out-idx))
			(declare (type cl:vector out-buf))
			(declare (type cl:function uf))
			(for (nil (not (eq cons1 cons2)) (setf cons1 (cdr cons1))
											 :returns (__algo-make-vct-iterator result out-idx))
			  (let ((cur-val (car cons1)))
				(unless (funcall uf cur-val)
				  (_= (aref out-buf out-idx) cur-val)
				  (incf out-idx))))))))

  
  ;;PTN; remove_copy_if : 8 -  cvp x  vp
  (defmethod remove_copy_if ((first const-vector-pointer)
							 (last  const-vector-pointer) (result vector-pointer) pred)
	;;(format t "specialized remove_copy_if for const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index  first))
		  (idx2 (opr::vec-ptr-index   last)))
	  (declare (type fixnum idx1 idx2))
	  (if (= idx1 idx2)
		  (clone result)
		  (let ((src-buf (opr::vec-ptr-buffer first))
				(out-idx (opr::vec-ptr-index  result))
				(out-buf (opr::vec-ptr-buffer result))
				(uf      (functor_function (clone pred))))
			(declare (type fixnum      out-idx))
			(declare (type cl:vector   src-buf out-buf))
			(declare (type cl:function uf))
			(for (nil (< idx1 idx2) (incf idx1)
									:returns (__algo-make-vct-iterator result out-idx))
			  (let ((cur-val (aref src-buf idx1)))
				(unless (funcall uf cur-val)
				  (_= (aref out-buf out-idx) cur-val)
				  (incf out-idx)))))))))




; first     : forward_iterator
; last      : forward_iterator
; pred      : binary_function ( default : #'operator_== )
; returns   : copy of first ( point to new end of sequence ).
(locally (declare (optimize speed))

  ;;PTN; unique : 0 -   f 
  (labels ((__unique-imp-0 (first last eql-bf)
		   (with-operators
			   (if (_== first last)
				   @~last
				   (let ((eql-bf (functor_function (clone eql-bf))))
					 (declare (type cl:function eql-bf))
					 (let ((first (__adjacent_find-imp-0 first last eql-bf)))
					   (if (_== first last)
						   first
						   (let ((dest @~first))
							 ++first
							 (for (nil (_/= ++first last) nil)
							   (unless (funcall eql-bf *dest *first)
								 ++dest
								 #+cl-stl-0x98 (_= *dest *first)
								 #-cl-stl-0x98 (multiple-value-bind (a b) (operator_move *dest *first)
												 (setf *first b)
												 (setf *dest  a))))
							 ++dest))))))))

	(defmethod-overload unique ((first forward_iterator) (last forward_iterator))
	  (__unique-imp-0 first last #'operator_==))

	(defmethod-overload unique ((first forward_iterator) (last forward_iterator) pred)
	  (__unique-imp-0 first last pred)))

  ;;PTN; unique : 1 -   ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__unique-imp-1 (cons1 cons2 eql-bf)
			 (declare (type cl:list cons1 cons2))
			 (if (eq cons1 cons2)
				 cons2
				 (let ((eql-bf (functor_function (clone eql-bf))))
				   (declare (type cl:function eql-bf))
				   (let ((cons1 (__adjacent_find-imp-1 cons1 cons2 eql-bf)))
					 (if (eq cons1 cons2)
						 cons2
						 (let ((dest cons1))
						   (declare (type cl:list dest))
						   (setf cons1 (cdr cons1))
						   (for (nil (not (eq (setf cons1 (cdr cons1)) cons2)) nil)
							 (unless (funcall eql-bf (car dest) (car cons1))
							   (setf dest (cdr dest))
							   #+cl-stl-0x98 (_= (car dest) (car cons1))
							   #-cl-stl-0x98 (multiple-value-bind (a b) (operator_move (car dest) (car cons1))
											   (setf (car cons1) b)
											   (setf (car dest)  a))))
						   (setf dest (cdr dest)))))))))

	(defmethod-overload unique ((first cons_iterator) (last cons_iterator))
	  ;;(format t "specialized unique for cons_iterator is invoked.~%")
	  (__algo-make-cns-iterator first
								 (__unique-imp-1 (__cons-itr-cons first)
												 (__cons-itr-cons  last) #'operator_==)))

	(defmethod-overload unique ((first cons_iterator) (last cons_iterator) pred)
	  ;;(format t "specialized unique for cons_iterator is invoked.~%")
	  (__algo-make-cns-iterator first
								 (__unique-imp-1 (__cons-itr-cons first)
												 (__cons-itr-cons  last) pred))))

  ;;PTN; unique : 2 -   vp
  (labels ((__unique-imp-2 (idx1 idx2 buffer eql-bf)
			 (declare (type fixnum idx1 idx2))
			 (declare (type cl:vector buffer))
			 (if (= idx1 idx2)
				 idx2
				 (let ((eql-bf (functor_function (clone eql-bf))))
				   (declare (type cl:function eql-bf))
				   (let ((idx1 (__adjacent_find-imp-2 idx1 idx2 buffer eql-bf)))
					 (declare (type fixnum idx1))
					 (if (= idx1 idx2)
						 idx2
						 (let ((dest idx1))
						   (declare (type fixnum dest))
						   (incf idx1)
						   (for (nil (/= (incf idx1) idx2) nil)
							 (unless (funcall eql-bf (aref buffer dest) (aref buffer idx1))
							   (incf dest)
							   #+cl-stl-0x98 (_= (aref buffer dest) (aref buffer idx1))
							   #-cl-stl-0x98 (multiple-value-bind (a b) (operator_move (aref buffer dest)
																					   (aref buffer idx1))
											   (setf (aref buffer idx1) b)
											   (setf (aref buffer dest) a))))
						   (incf dest))))))))

	(defmethod-overload unique ((first vector-pointer) (last vector-pointer))
	  ;;(format t "specialized unique for vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__algo-make-vct-iterator first
								 (__unique-imp-2 (opr::vec-ptr-index  first)
												 (opr::vec-ptr-index  last)
												 (opr::vec-ptr-buffer first) #'operator_==)))

	(defmethod-overload unique ((first vector-pointer) (last vector-pointer) pred)
	  ;;(format t "specialized unique for vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__algo-make-vct-iterator first
								 (__unique-imp-2 (opr::vec-ptr-index  first)
												 (opr::vec-ptr-index  last)
												 (opr::vec-ptr-buffer first) pred)))))




; first     : input_iterator
; last      : input_iterator
; result    : output_iterator
; pred      : binary_function ( default : #'operator_== )
; returns   : copy of result ( point to end of sequence ).
(locally (declare (optimize speed))

  ;;PTN; unique_copy : 0 -   i  x  o 
  (labels ((__unique_copy-imp-0 (first last result pred)
			 (declare (type cl:function pred))
			 (with-operators
				 (let ((oitr @~result))
				   (if (_== first last)
					   oitr
					   (let ((last-val nil))
						 (_= last-val *first)
						 (_= *oitr last-val)
						 ++oitr
						 (for (((itr (next first))) (_/= itr last) ++itr :returns oitr)
						   (let ((cur-val *itr))
							 (unless (funcall pred last-val cur-val)
							   (_= *oitr cur-val)
							   ++oitr
							   (setq last-val cur-val))))))))))

	(defmethod-overload unique_copy ((first input_iterator) (last input_iterator) (result output_iterator))
	  (__unique_copy-imp-0 first last result #'operator_==))

	(defmethod-overload unique_copy ((first input_iterator) (last input_iterator) (result output_iterator) pred)
	  (__unique_copy-imp-0 first last result (functor_function (clone pred)))))


  ;;PTN; unique_copy : 1 -  cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__unique_copy-imp-1 (cons1 end1 oitr eql-bf)
			 (declare (type cl:list cons1 end1))
			 (declare (type cl:function eql-bf))
			 (if (eq cons1 end1)
				 oitr
				 (with-operators
					 (let ((last-val (car cons1)))
					   (_= *oitr last-val)
					   ++oitr
					   (for (nil (not (eq cons1 end1)) (setf cons1 (cdr cons1)) :returns oitr)
						 (let ((cur-val (car cons1)))
						   (unless (funcall eql-bf last-val cur-val)
							 (_= *oitr cur-val)
							 ++oitr
							 (setq last-val cur-val)))))))))

	(defmethod-overload unique_copy ((first cons_const_iterator) (last cons_const_iterator) (result output_iterator))
	  ;;(format t "specialized unique_copy for cons_const_iterator & output_iterator is invoked.~%")
	  (__unique_copy-imp-1 (__cons-itr-cons first)
						   (__cons-itr-cons  last) (clone result) #'operator_==))

	(defmethod-overload unique_copy ((first cons_const_iterator) (last cons_const_iterator) (result output_iterator) pred)
	  ;;(format t "specialized unique_copy for cons_const_iterator & output_iterator is invoked.~%")
	  (__unique_copy-imp-1 (__cons-itr-cons first)
						   (__cons-itr-cons  last) (clone result) (functor_function (clone pred)))))


  ;;PTN; unique_copy : 2 -  cvp x  o 
  (labels ((__unique_copy-imp-2 (idx1 idx2 buffer oitr eql-bf)
			 (declare (type fixnum idx1 idx2))
			 (declare (type cl:vector buffer))
			 (declare (type cl:function eql-bf))
			 (if (= idx1 idx2)
				 oitr
				 (with-operators
					 (let ((last-val (aref buffer idx1)))
					   (_= *oitr last-val)
					   ++oitr
					   (let ((idx idx1))
						 (declare (type fixnum idx))
						 (for (nil (< idx idx2) (incf idx) :returns oitr)
						   (let ((cur-val (aref buffer idx)))
							 (unless (funcall eql-bf last-val cur-val)
							   (_= *oitr cur-val)
							   ++oitr
							   (setq last-val cur-val))))))))))

	(defmethod-overload unique_copy ((first const-vector-pointer) (last const-vector-pointer) (result output_iterator))
	  ;;(format t "specialized unique_copy for const-vector-pointer & output_iterator is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__unique_copy-imp-2 (opr::vec-ptr-index  first)
						   (opr::vec-ptr-index  last)
						   (opr::vec-ptr-buffer first) (clone result) #'operator_==))

	(defmethod-overload unique_copy ((first const-vector-pointer) (last const-vector-pointer) (result output_iterator) pred)
	  ;;(format t "specialized unique_copy for const-vector-pointer & output_iterator is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__unique_copy-imp-2 (opr::vec-ptr-index  first)
						   (opr::vec-ptr-index  last)
						   (opr::vec-ptr-buffer first)
						   (clone result) (functor_function (clone pred)))))

  ;;PTN; unique_copy : 3 -   i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__unique_copy-imp-3 (first last out eql-bf)
			 (declare (type cl:list out))
			 (declare (type cl:function eql-bf))
			 (if (_== first last)
				 out
				 (with-operators
					 (let ((last-val *first))
					   (_= (car out) last-val)
					   (setf out (cdr out))
					   (for (((itr @~first)) (_/= itr last) ++itr :returns out)
						 (let ((cur-val *itr))
						   (unless (funcall eql-bf last-val cur-val)
							 (_= (car out) cur-val)
							 (setf out (cdr out))
							 (setq last-val cur-val)))))))))

	(defmethod-overload unique_copy ((first input_iterator) (last input_iterator) (result cons_iterator))
	  ;;(format t "specialized unique_copy for input_iterator & cons_iterator is invoked.~%")
	  (__algo-make-cns-iterator result
								 (__unique_copy-imp-3 first last
													  (__cons-itr-cons  result) #'operator_==)))


	(defmethod-overload unique_copy ((first input_iterator) (last input_iterator) (result cons_iterator) pred)
	  ;;(format t "specialized unique_copy for input_iterator & cons_iterator is invoked.~%")
	  (__algo-make-cns-iterator result
								 (__unique_copy-imp-3 first last
													  (__cons-itr-cons result)
													  (functor_function (clone pred))))))


  ;;PTN; unique_copy : 4 -  cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__unique_copy-imp-4 (cons1 cons2 out eql-bf)
			 (declare (type cl:list cons1 cons2 out))
			 (declare (type cl:function eql-bf))
			 (if (eq cons1 cons2)
				 out
				 (let ((last-val (car cons1)))
				   (_= (car out) last-val)
				   (setf out (cdr out))
				   (for (nil (not (eq cons1 cons2)) (setf cons1 (cdr cons1)) :returns out)
					 (let ((cur-val (car cons1)))
					   (unless (funcall eql-bf last-val cur-val)
						 (_= (car out) cur-val)
						 (setf out (cdr out))
						 (setq last-val cur-val))))))))

	(defmethod-overload unique_copy ((first cons_const_iterator) (last cons_const_iterator) (result cons_iterator))
	  ;;(format t "specialized unique_copy for cons_const_iterator & cons_iterator is invoked.~%")
	  (__algo-make-cns-iterator result
								 (__unique_copy-imp-4 (__cons-itr-cons  first)
													  (__cons-itr-cons   last)
													  (__cons-itr-cons result) #'operator_==)))


	(defmethod-overload unique_copy ((first cons_const_iterator) (last cons_const_iterator) (result cons_iterator) pred)
	  ;;(format t "specialized unique_copy for cons_const_iterator & cons_iterator is invoked.~%")
	  (__algo-make-cns-iterator result
								 (__unique_copy-imp-4 (__cons-itr-cons  first)
													  (__cons-itr-cons   last)
													  (__cons-itr-cons result) (functor_function (clone pred))))))

  
  ;;PTN; unique_copy : 5 -  cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__unique_copy-imp-5 (idx1 idx2 src-buf out eql-bf)
			 (declare (type fixnum idx1 idx2))
			 (declare (type cl:vector src-buf))
			 (declare (type cl:list out))
			 (declare (type cl:function eql-bf))
			 (if (= idx1 idx2)
				 out
				 (let ((last-val (aref src-buf idx1)))
				   (_= (car out) last-val)
				   (setf out (cdr out))
				   (let ((idx idx1))
					 (declare (type fixnum idx))
					 (for (nil (< idx idx2) (incf idx) :returns out)
					   (let ((cur-val (aref src-buf idx)))
						 (unless (funcall eql-bf last-val cur-val)
						   (_= (car out) cur-val)
						   (setf out (cdr out))
						   (setq last-val cur-val)))))))))

	(defmethod-overload unique_copy ((first const-vector-pointer) (last const-vector-pointer) (result cons_iterator))
	  ;;(format t "specialized unique_copy for const-vector-pointer & cons_iterator is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__algo-make-cns-iterator result
								 (__unique_copy-imp-5 (opr::vec-ptr-index  first)
													  (opr::vec-ptr-index  last)
													  (opr::vec-ptr-buffer first)
													  (__cons-itr-cons result) #'operator_==)))


	(defmethod-overload unique_copy ((first const-vector-pointer) (last const-vector-pointer) (result cons_iterator) pred)
	  ;;(format t "specialized unique_copy for const-vector-pointer & cons_iterator is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__algo-make-cns-iterator result
								 (__unique_copy-imp-5 (opr::vec-ptr-index  first)
													  (opr::vec-ptr-index  last)
													  (opr::vec-ptr-buffer first)
													  (__cons-itr-cons result) (functor_function (clone pred))))))
  

  ;;PTN; unique_copy : 6 -   i  x  vp
  (labels ((__unique_copy-imp-6 (first last out-idx out-buf eql-bf)
			 (declare (type fixnum out-idx))
			 (declare (type cl:vector out-buf))
			 (declare (type cl:function eql-bf))
			 (if (_== first last)
				 out-idx
				 (with-operators
					 (let ((last-val *first))
					   (_= (aref out-buf out-idx) last-val)
					   (incf out-idx)
					   (for (((itr @~first)) (_/= itr last) ++itr :returns out-idx)
						 (let ((cur-val *itr))
						   (unless (funcall eql-bf last-val cur-val)
							 (_= (aref out-buf out-idx) cur-val)
							 (incf out-idx)
							 (setq last-val cur-val)))))))))

	(defmethod-overload unique_copy ((first input_iterator) (last input_iterator) (result vector-pointer))
	  ;;(format t "specialized unique_copy for input_iterator & vector-pointer is invoked.~%")
	  (__algo-make-vct-iterator result
								 (__unique_copy-imp-6 first last
													  (opr::vec-ptr-index  result)
													  (opr::vec-ptr-buffer result) #'operator_==)))


	(defmethod-overload unique_copy ((first input_iterator) (last input_iterator) (result vector-pointer) pred)
	  ;;(format t "specialized unique_copy for input_iterator & vector-pointer is invoked.~%")
	  (__algo-make-vct-iterator result
								 (__unique_copy-imp-6 first last
													  (opr::vec-ptr-index  result)
													  (opr::vec-ptr-buffer result) (functor_function (clone pred))))))


  ;;PTN; unique_copy : 7 -  cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__unique_copy-imp-7 (cons1 cons2 out-idx out-buf eql-bf)
			 (declare (type cl:list cons1 cons2))
			 (declare (type fixnum out-idx))
			 (declare (type cl:vector out-buf))
			 (declare (type cl:function eql-bf))
			 (if (eq cons1 cons2)
				 out-idx
				 (let ((last-val (car cons1)))
				   (_= (aref out-buf out-idx) last-val)
				   (incf out-idx)
				   (for (nil (not (eq cons1 cons2)) (setf cons1 (cdr cons1)) :returns out-idx)
					 (let ((cur-val (car cons1)))
					   (unless (funcall eql-bf last-val cur-val)
						 (_= (aref out-buf out-idx) cur-val)
						 (incf out-idx)
						 (setq last-val cur-val))))))))

	(defmethod-overload unique_copy ((first cons_const_iterator) (last cons_const_iterator) (result vector-pointer))
	  ;;(format t "specialized unique_copy for cons_const_iterator & vector-pointer is invoked.~%")
	  (__algo-make-vct-iterator result
								 (__unique_copy-imp-7 (__cons-itr-cons first)
													  (__cons-itr-cons  last)
													  (opr::vec-ptr-index  result)
													  (opr::vec-ptr-buffer result) #'operator_==)))


	(defmethod-overload unique_copy ((first cons_const_iterator) (last cons_const_iterator) (result vector-pointer) pred)
	  ;;(format t "specialized unique_copy for cons_const_iterator & vector-pointer is invoked.~%")
	  (__algo-make-vct-iterator result
								 (__unique_copy-imp-7 (__cons-itr-cons first)
													  (__cons-itr-cons  last)
													  (opr::vec-ptr-index  result)
													  (opr::vec-ptr-buffer result) (functor_function (clone pred))))))


  ;;PTN; unique_copy : 8 -  cvp x  vp
  (labels ((__unique_copy-imp-8 (idx1 idx2 src-buf out-idx out-buf eql-bf)
			 (declare (type fixnum idx1 idx2 out-idx))
			 (declare (type cl:vector src-buf out-buf))
			 (declare (type cl:function eql-bf))
			 (if (= idx1 idx2)
				 out-idx
				 (let ((last-val (aref src-buf idx1)))
				   (_= (aref out-buf out-idx) last-val)
				   (incf out-idx)
				   (let ((idx idx1))
					 (declare (type fixnum idx))
					 (for (nil (< idx idx2) (incf idx) :returns out-idx)
					   (let ((cur-val (aref src-buf idx)))
						 (unless (funcall eql-bf last-val cur-val)
						   (_= (aref out-buf out-idx) cur-val)
						   (incf out-idx)
						   (setq last-val cur-val)))))))))

	(defmethod-overload unique_copy ((first const-vector-pointer) (last const-vector-pointer) (result vector-pointer))
	  ;;(format t "specialized unique_copy for const-vector-pointer & vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__algo-make-vct-iterator result
								 (__unique_copy-imp-8 (opr::vec-ptr-index  first)
													  (opr::vec-ptr-index  last)
													  (opr::vec-ptr-buffer first)
													  (opr::vec-ptr-index  result)
													  (opr::vec-ptr-buffer result) #'operator_==)))


	(defmethod-overload unique_copy ((first const-vector-pointer) (last const-vector-pointer) (result vector-pointer) pred)
	  ;;(format t "specialized unique_copy for const-vector-pointer & vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__algo-make-vct-iterator result
								 (__unique_copy-imp-8 (opr::vec-ptr-index  first)
													  (opr::vec-ptr-index  last)
													  (opr::vec-ptr-buffer first)
													  (opr::vec-ptr-index  result)
													  (opr::vec-ptr-buffer result) (functor_function (clone pred)))))))



; first     : bidirectional_iterator
; last      : bidirectional_iterator
; returns   : nil.
(locally (declare (optimize speed))

  ;;PTN; reverse : 0 -   b
  (defmethod-overload reverse ((first bidirectional_iterator) (last bidirectional_iterator))
	(__reverse-imp-0b first last))

  ;;PTN; reverse : 0 -   r
  (defmethod-overload reverse ((first randomaccess_iterator) (last randomaccess_iterator))
	(__reverse-imp-0r first last))

  ;;PTN; reverse : 1 -   vp
  (defmethod-overload reverse ((first vector-pointer) (last vector-pointer))
	;;(format t "specialized reverse for vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(__reverse-imp-1 (opr::vec-ptr-index first)
					 (opr::vec-ptr-index  last) (opr::vec-ptr-buffer first))))




; first     : bidirectional_iterator
; last      : bidirectional_iterator
; result    : output_iterator
; returns   : copy of result ( point to end of sequence ).
(locally (declare (optimize speed))

  ;;PTN; reverse_copy : 0 -   b  x  o 
  (defmethod reverse_copy ((first bidirectional_iterator)
						   (last  bidirectional_iterator) (result output_iterator))
	(with-operators
		(let ((oitr @~result))
		  (if (_== first last)
			  oitr
			  (for (((itr @~last)) (_/= first itr) ++oitr :returns oitr)
				--itr
				(_= *oitr *itr))))))

  ;;PTN; reverse_copy : 1 -  cvp x  o 
  (defmethod reverse_copy ((first const-vector-pointer)
						   (last  const-vector-pointer) (result output_iterator))
	;;(format t "specialized reverse_copy for const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index  first))
		  (idx2 (opr::vec-ptr-index   last))
		  (buf  (opr::vec-ptr-buffer first))
		  (oitr (clone result)))
	  (declare (type fixnum idx1 idx2))
	  (declare (type cl:vector buf))
	  (with-operators
		  (for (nil (< idx1 idx2) ++oitr :returns oitr)
			(decf idx2)
			(_= *oitr (aref buf idx2))))))

  ;;PTN; reverse_copy : 2 -   b  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod reverse_copy ((first bidirectional_iterator)
						   (last  bidirectional_iterator) (result cons_iterator))
	;;(format t "specialized reverse_copy for bidirectional_iterator & cons_iterator is invoked.~%")
	(let ((out (__cons-itr-cons  result)))
	  (declare (type cl:list out))
	  (with-operators
		  (for (((itr @~last)) (_/= first itr) (setf out (cdr out))
							   :returns (__algo-make-cns-iterator result out))
			--itr
			(_= (car out) *itr)))))

  ;;PTN; reverse_copy : 3 -  cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod reverse_copy ((first const-vector-pointer)
						   (last  const-vector-pointer) (result cons_iterator))
	;;(format t "specialized reverse_copy for const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index   first))
		  (idx2 (opr::vec-ptr-index    last))
		  (buf  (opr::vec-ptr-buffer  first))
		  (out  (__cons-itr-cons     result)))
	  (declare (type fixnum idx1 idx2))
	  (declare (type cl:vector   buf))
	  (declare (type cl:list     out))
	  (for (nil (< idx1 idx2) (setf out (cdr out))
							  :returns (__algo-make-cns-iterator result out))
		(decf idx2)
		(_= (car out) (aref buf idx2)))))

  ;;PTN; reverse_copy : 4 -   b  x  vp
  (defmethod reverse_copy ((first bidirectional_iterator)
						   (last  bidirectional_iterator) (result vector-pointer))
	;;(format t "specialized reverse_copy for bidirectional_iterator & vector-pointer is invoked.~%")
	(let ((out-idx (opr::vec-ptr-index  result))
		  (out-buf (opr::vec-ptr-buffer result)))
	  (declare (type fixnum    out-idx))
	  (declare (type cl:vector out-buf))
	  (with-operators
		  (for (((itr @~last)) (_/= first itr) (incf out-idx)
							   :returns (__algo-make-vct-iterator result out-idx))
			--itr
			(_= (aref out-buf out-idx) *itr)))))

  ;;PTN; reverse_copy : 5 -  cvp x  vp
  (defmethod reverse_copy ((first const-vector-pointer)
						   (last  const-vector-pointer) (result vector-pointer))
	;;(format t "specialized reverse_copy for const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1    (opr::vec-ptr-index   first))
		  (idx2    (opr::vec-ptr-index    last))
		  (src-buf (opr::vec-ptr-buffer  first))
		  (out-idx (opr::vec-ptr-index  result))
		  (out-buf (opr::vec-ptr-buffer result)))
	  (declare (type fixnum idx1 idx2 out-idx))
	  (declare (type cl:vector src-buf out-buf))
	  (for (nil (< idx1 idx2) (incf out-idx)
							  :returns (__algo-make-vct-iterator result out-idx))
		(decf idx2)
		(_= (aref out-buf out-idx) (aref src-buf idx2))))))




; first     : forward_iterator
; middle    : forward_iterator
; last      : forward_iterator
; returns   : nil.
(locally (declare (optimize speed))

  ;;PTN; rotate : 0 -   f 
  (defmethod rotate ((first forward_iterator) (middle forward_iterator) (last forward_iterator))
	(with-operators
		(if (or (_== first middle) (_== middle last))
			(progn #+cl-stl-0x98 nil #-cl-stl-0x98 @~first)
			(let ((first  @~first)
				  (first2 @~middle)
				  (middle @~middle))
			  (for (nil t nil)
				(swap *first *first2)
				++first
				++first2
				(when (_== first middle)
				  (_= middle first2))
				(when (_== first2 last)
				  (return)))
			  (_= first2 middle)
			  (for ((#-cl-stl-0x98 (ret @~first))
					(_/= first2 last) nil :returns #+cl-stl-0x98 nil
												   #-cl-stl-0x98 ret)
				(swap *first *first2)
				++first
				++first2
				(if (_== first middle)
					(_= middle first2)
					(when (_== first2 last)
					  (_= first2 middle))))))))

  ;;PTN; rotate : 0 -   b 
  (defmethod rotate ((first bidirectional_iterator) (middle bidirectional_iterator) (last bidirectional_iterator))
	(with-operators
		(if (or (_== first middle) (_== middle last))
			(progn #+cl-stl-0x98 nil #-cl-stl-0x98 @~first)
			(progn
			  (__reverse-imp-0b first middle)
			  (__reverse-imp-0b middle  last)
			  (let ((first @~first)
					(last  @~last))
				(for (nil (and (_/= first middle) (_/= middle last)) nil)
				  --last
				  (swap *first *last)
				  ++first)
				(if (_== first middle)
					(progn (__reverse-imp-0b middle   last) #+cl-stl-0x98 nil #-cl-stl-0x98 last)
					(progn (__reverse-imp-0b  first middle) #+cl-stl-0x98 nil #-cl-stl-0x98 first)))))))

  ;;PTN; rotate : 0 -   r 
  (defmethod rotate ((first randomaccess_iterator) (middle randomaccess_iterator) (last randomaccess_iterator))
	(with-operators
		(if (or (_== first middle) (_== middle last))
			(progn #+cl-stl-0x98 nil
				   #-cl-stl-0x98 @~first)
			(let ((len1 (_- last   first))
				  (len2 (_- middle first)))
			  (declare (type fixnum len1 len2))
			  (if (= len2 (- len1 len2))
				  (progn (__swap_ranges-imp-0 first middle middle)
						 #+cl-stl-0x98 nil
						 #-cl-stl-0x98 @~middle)
				  (let ((itr1 @~first)
						(ret  #+cl-stl-0x98 nil
							  #-cl-stl-0x98 (next first (_- last middle))))
					(for (nil t nil)
					  (if (< len2 (- len1 len2))
						  (progn (when (= len2 1)
								   (let ((val *itr1))
									 #+cl-stl-0x98 (__copy-imp-0 (_+ itr1 1) (_+ itr1 len1) itr1)
									 #-cl-stl-0x98 (__move-imp-0 (_+ itr1 1) (_+ itr1 len1) itr1)
									 (_= (_[] itr1 (the fixnum (1- len1))) val))
								   (return-from rotate ret))
								 (let ((itr2 (_+ itr1 len2)))
								   (for (((cnt (- len1 len2)) (idx 0)) (< idx cnt) (progn (incf idx) ++itr1 ++itr2))
									 (swap *itr1 *itr2))
								   (setf len1 (mod len1 len2)))
								 (when (zerop len1)
								   (return-from rotate ret))
								 (rotatef len1 len2)
								 (setf len2 (- len1 len2)))

						  (progn (setf len2 (- len1 len2))
								 (when (= len2 1)
								   (let ((val (_[] itr1 (the fixnum (1- len1)))))
									 #+cl-stl-0x98 (__copy_backward-imp-0 itr1 (_+ itr1 (the fixnum (1- len1))) (_+ itr1 len1))
									 #-cl-stl-0x98 (__move_backward-imp-0 itr1 (_+ itr1 (the fixnum (1- len1))) (_+ itr1 len1))
									 (_= *itr1 val))
								   (return-from rotate ret))
								 (let ((itr2 (_+ itr1 len1)))
								   (_= itr1 itr2)
								   (_-= itr1 len2)
								   (for (((cnt (- len1 len2)) (idx 0)) (< idx cnt) (incf idx))
									 --itr1
									 --itr2
									 (swap *itr1 *itr2))
								   (setf len1 (mod len1 len2))
								   (when (zerop len1)
									 (return-from rotate ret))
								   (rotatef len1 len2)))))))))))

  ;;PTN; rotate : 1 -   ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod rotate ((first cons_iterator) (middle cons_iterator) (last cons_iterator))
	(format t "specialized rotate for cons_iterator is invoked.~%")
	(let ((cons (__rotate-imp-1 (__cons-itr-cons  first)
								(__cons-itr-cons middle)
								(__cons-itr-cons   last))))
	  (declare (ignorable cons))
	  #+cl-stl-0x98 nil
	  #-cl-stl-0x98 (__algo-make-cns-iterator first cons)))

  ;;PTN; rotate : 2 -   vp
  (defmethod rotate ((first vector-pointer) (middle vector-pointer) (last vector-pointer))
	;;(format t "specialized rotate for vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first middle)
	(__pointer-check-iterator-range middle last)
	(let ((idx (__rotate-imp-2 (opr::vec-ptr-index  first)
							   (opr::vec-ptr-index middle)
							   (opr::vec-ptr-index   last)
							   (opr::vec-ptr-buffer first))))
	  (declare (ignorable idx))
	  #+cl-stl-0x98 nil
	  #-cl-stl-0x98 (__algo-make-vct-iterator first idx))))




; first     : forward_iterator
; middle    : forward_iterator
; last      : forward_iterator
; result    : output_iterator
; returns   : copy of result ( point to end of sequence ).
(locally (declare (optimize speed))

  ;;PTN; rotate_copy : 0 -   f  x  o 
  (defmethod rotate_copy ((first  forward_iterator)
						  (middle forward_iterator)
						  (last   forward_iterator) (result output_iterator))
	(__copy-imp-0 first middle (__copy-imp-0 middle last result)))

  ;;PTN; rotate_copy : 1 -  cci x  o
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod rotate_copy ((first  cons_const_iterator)
						  (middle cons_const_iterator)
						  (last   cons_const_iterator) (result output_iterator))
	;;(format t "specialized rotate_copy for cons_const_iterator & output_iterator is invoked.~%")
	(let ((cons1 (__cons-itr-cons  first))
		  (cons2 (__cons-itr-cons middle))
		  (cons3 (__cons-itr-cons   last)))
	  (__copy-imp-1 cons1 cons2
					(__copy-imp-1 cons2 cons3 (clone result)))))

  ;;PTN; rotate_copy : 2 -  cvp x  o 
  (defmethod rotate_copy ((first  const-vector-pointer)
						  (middle const-vector-pointer)
						  (last   const-vector-pointer) (result output_iterator))
	;;(format t "specialized rotate_copy for const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first middle)
	(__pointer-check-iterator-range middle last)
	(let ((idx1   (opr::vec-ptr-index  first))
		  (idx2   (opr::vec-ptr-index  middle))
		  (idx3   (opr::vec-ptr-index  last))
		  (buffer (opr::vec-ptr-buffer first)))
	  (__copy-imp-2 idx1 idx2 buffer
					(__copy-imp-2 idx2 idx3 buffer (clone result)))))

  ;;PTN; rotate_copy : 3 -   f  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod rotate_copy ((first  forward_iterator)
						  (middle forward_iterator)
						  (last   forward_iterator) (result cons_iterator))
	;;(format t "specialized rotate_copy for forward_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__copy-imp-3 first middle
											 (__copy-imp-3 middle last
														   (__cons-itr-cons result)))))
  
  ;;PTN; rotate_copy : 4 -  cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod rotate_copy ((first  cons_const_iterator)
						  (middle cons_const_iterator)
						  (last   cons_const_iterator) (result cons_iterator))
	;;(format t "specialized rotate_copy for cons_const_iterator & cons_iterator is invoked.~%")
	(let ((cons1 (__cons-itr-cons  first))
		  (cons2 (__cons-itr-cons middle))
		  (cons3 (__cons-itr-cons   last)))
	  (__algo-make-cns-iterator result
								 (__copy-imp-4 cons1 cons2
											   (__copy-imp-4 cons2 cons3
															 (__cons-itr-cons result))))))
  
  ;;PTN; rotate_copy : 5 -  cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod rotate_copy ((first  const-vector-pointer)
						  (middle const-vector-pointer)
						  (last   const-vector-pointer) (result cons_iterator))
	;;(format t "specialized rotate_copy for const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first middle)
	(__pointer-check-iterator-range middle last)
	(let ((idx1 (opr::vec-ptr-index  first))
		  (idx2 (opr::vec-ptr-index  middle))
		  (idx3 (opr::vec-ptr-index  last))
		  (buf  (opr::vec-ptr-buffer first)))
	  (__algo-make-cns-iterator result
								 (__copy-imp-5 idx1 idx2 buf
											   (__copy-imp-5 idx2 idx3 buf
															 (__cons-itr-cons result))))))
  
  ;;PTN; rotate_copy : 6 -   f  x  vp
  (defmethod rotate_copy ((first  forward_iterator)
						  (middle forward_iterator)
						  (last   forward_iterator) (result vector-pointer))
	;;(format t "specialized rotate_copy for forward_iterator & vector-pointer is invoked.~%")
	(let ((out-idx (opr::vec-ptr-index  result))
		  (out-buf (opr::vec-ptr-buffer result)))
	  (setf out-idx (__copy-imp-6 middle  last out-idx out-buf))
	  (setf out-idx (__copy-imp-6 first middle out-idx out-buf))
	  (__algo-make-vct-iterator result out-idx)))

  ;;PTN; rotate_copy : 7 -  cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod rotate_copy ((first  cons_const_iterator)
						  (middle cons_const_iterator)
						  (last   cons_const_iterator) (result vector-pointer))
	;;(format t "specialized rotate_copy for cons_const_iterator & vector-pointer is invoked.~%")
	(let ((cons1   (__cons-itr-cons  first))
		  (cons2   (__cons-itr-cons middle))
		  (cons3   (__cons-itr-cons   last))
		  (out-idx (opr::vec-ptr-index  result))
		  (out-buf (opr::vec-ptr-buffer result)))
	  (setf out-idx (__copy-imp-7 cons2 cons3 out-idx out-buf))
	  (setf out-idx (__copy-imp-7 cons1 cons2 out-idx out-buf))
	  (__algo-make-vct-iterator result out-idx)))

  ;;PTN; rotate_copy : 8 -  cvp x  vp
  (defmethod rotate_copy ((first  const-vector-pointer)
						  (middle const-vector-pointer)
						  (last   const-vector-pointer) (result vector-pointer))
	;;(format t "specialized rotate_copy for const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first middle)
	(__pointer-check-iterator-range middle last)
	(let ((idx1    (opr::vec-ptr-index  first))
		  (idx2    (opr::vec-ptr-index  middle))
		  (idx3    (opr::vec-ptr-index  last))
		  (src-buf (opr::vec-ptr-buffer first))
		  (out-idx (opr::vec-ptr-index  result))
		  (out-buf (opr::vec-ptr-buffer result)))
	  (setf out-idx (__copy-imp-8 idx2 idx3 src-buf out-idx out-buf))
	  (setf out-idx (__copy-imp-8 idx1 idx2 src-buf out-idx out-buf))
	  (__algo-make-vct-iterator result out-idx))))





; first     : randomaccess_iterator
; last      : randomaccess_iterator
; rand      : unary_function ( default : #'random )
; returns   : nil.
(locally (declare (optimize speed))

  ;;PTN; random_shuffle : 0 -   r
  (labels ((__random_shuffle-imp-0 (first last rand)
			 (declare (type cl:function rand))
			 (if (_== first last)
				 nil
				 (with-operators
					 (let ((dist 1))
					   (declare (type fixnum dist))
					   (for (((tmp nil) (itr @~first)) (_/= itr last) (progn ++itr (incf dist)))
						 (let ((idx (funcall rand dist)))
						   (declare (type fixnum idx))
						   (_= tmp first[idx])
						   (_= first[idx] *itr)
						   (_= *itr tmp))))))))

  (defmethod-overload random_shuffle ((first randomaccess_iterator) (last randomaccess_iterator))
	(__random_shuffle-imp-0 first last #'random))

  (defmethod-overload random_shuffle ((first randomaccess_iterator) (last randomaccess_iterator) rand)
	(__random_shuffle-imp-0 first last (functor_function (clone rand)))))


  ;;PTN; random_shuffle : 1 -   vp
  (labels ((__random_shuffle-imp-1 (idx1 idx2 buffer random-uf)
			 (declare (type fixnum idx1 idx2))
			 (declare (type cl:vector buffer))
			 (declare (type cl:function random-uf))
			 (let((dist 1)
				  (idx  idx1))
			   (declare (type fixnum dist idx))
			   (for (nil (< idx idx2) (progn (incf idx) (incf dist)))
				 (let ((n (funcall random-uf dist)))
				   (declare (type fixnum n))
				   (swap (aref buffer (the fixnum (+ idx1 n))) (aref buffer idx)))))))

	(defmethod-overload random_shuffle ((first vector-pointer) (last vector-pointer))
	  ;;(format t "specialized random_shuffle for vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__random_shuffle-imp-1 (opr::vec-ptr-index  first)
							  (opr::vec-ptr-index  last)
							  (opr::vec-ptr-buffer first) #'random))

	(defmethod-overload random_shuffle ((first vector-pointer) (last vector-pointer) rand)
	  ;;(format t "specialized random_shuffle for vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__random_shuffle-imp-1 (opr::vec-ptr-index  first)
							  (opr::vec-ptr-index  last)
							  (opr::vec-ptr-buffer first) (functor_function (clone rand))))))




; first     : randomaccess_iterator
; last      : randomaccess_iterator
; gen       : functor
; returns   : nil.
#-cl-stl-0x98
(locally (declare (optimize speed))

  ;;PTN; shuffle : 0 -   r
  (labels ((__shuffle-imp-0 (first last gen)
			 (declare (type cl:function gen))
			 (if (_== first last)
				 nil
				 (let ((i (1- (the fixnum (_- last first)))))
				   (declare (type fixnum i))
				   (for (nil (< 0 i) (decf i))
					 (let ((n (funcall gen i)))
					   (multiple-value-bind (v1 v2)
						   (opr::__swap-2 (_[] first i) (_[] first n))
						 (setf (_[] first i) v1)
						 (setf (_[] first n) v2))))))))
	
	(defmethod shuffle ((first randomaccess_iterator) (last randomaccess_iterator) gen)
	  (__shuffle-imp-0 first last (functor_function (clone gen)))))


  ;;PTN; shuffle : 1 -   vp
  (labels ((__shuffle-imp-1 (idx1 idx2 buffer gen)
			 (declare (type fixnum idx1 idx2))
			 (declare (type cl:vector buffer))
			 (declare (type cl:function gen))
			 (if (= idx1 idx2)
				 nil
				 (let ((i (1- (the fixnum (- idx2 idx1)))))
				   (declare (type fixnum i))
				   (for (nil (< 0 i) (decf i))
					 (let ((n (funcall gen i)))
					   (multiple-value-bind (v1 v2)
						   (opr::__swap-2 (aref buffer i) (aref buffer n))
						 (setf (aref buffer i) v1)
						 (setf (aref buffer n) v2))))))))

	(defmethod shuffle ((first vector-pointer) (last vector-pointer) gen)
	  ;;(format t "specialized shuffle for vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__shuffle-imp-1 (opr::vec-ptr-index  first)
					   (opr::vec-ptr-index  last)
					   (opr::vec-ptr-buffer first) (functor_function (clone gen))))))




;; 25.2.12, partitions:

; first     : input_iterator
; last      : input_iterator
; pred      : unary_function
; returns   : boolean value.
#-cl-stl-0x98    ; is_partitioned
(locally (declare (optimize speed))

  ;;PTN; is_partitioned : 0 -   i
  (defmethod is_partitioned ((first input_iterator) (last input_iterator) pred)
	(if (_== first last)
		t
		(with-operators
			(let ((pred (functor_function @~pred)))
			  (declare (type cl:function pred))
			  (labels ((imp-nil (itr)
						 (for (nil (_/= itr last) ++itr :returns t)
						   (when (funcall pred *itr)
							 (return-from imp-nil nil))))
					   (imp-t (itr)
						 (for (nil (_/= itr last) ++itr :returns t)
						   (unless (funcall pred *itr)
							 (return-from imp-t (imp-nil ++itr))))))
				(imp-t @~first))))))

  ;;PTN; is_partitioned : 1 -  cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod is_partitioned ((first cons_const_iterator) (last cons_const_iterator) pred)
	;;(format t "specialized is_partitioned for cons_const_iterator is invoked.~%")
	(let ((cons1 (__cons-itr-cons first))
		  (cons2 (__cons-itr-cons  last)))
	  (declare (type cl:list cons1 cons2))
	  (if (eq cons1 cons2)
		  t
		  (let ((uf (functor_function (clone pred))))
			(declare (type cl:function uf))
			(labels ((imp-nil (cns)
					   (declare (type cl:list cns))
					   (for (nil (not (eq cns cons2)) (setf cns (cdr cns)) :returns t)
						 (when (funcall uf (car cns))
						   (return-from imp-nil nil))))
					 (imp-t (cns)
					   (declare (type cl:list cns))
					   (for (nil (not (eq cns cons2)) (setf cns (cdr cns)) :returns t)
						 (unless (funcall uf (car cns))
						   (return-from imp-t (imp-nil (cdr cns)))))))
			  (imp-t cons1))))))

  ;;PTN; is_partitioned : 2 -  cvp
  (defmethod is_partitioned ((first const-vector-pointer) (last const-vector-pointer) pred)
	;;(format t "specialized is_partitioned for const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index first))
		  (idx2 (opr::vec-ptr-index  last)))
	  (declare (type fixnum idx1 idx2))
	  (if (= idx1 idx2)
		  t
		  (let ((buf (opr::vec-ptr-buffer first))
				(uf  (functor_function (clone pred))))
			(declare (type cl:vector buf))
			(declare (type cl:function uf))
			(labels ((imp-nil (idx)
					   (declare (type fixnum idx))
					   (for (nil (< idx idx2) (incf idx) :returns t)
						 (when (funcall uf (aref buf idx))
						   (return-from imp-nil nil))))
					 (imp-t (idx)
					   (declare (type fixnum idx))
					   (for (nil (< idx idx2) (incf idx) :returns t)
						 (unless (funcall uf (aref buf idx))
						   (return-from imp-t (imp-nil (1+ idx)))))))
			  (imp-t idx1)))))))




; first     : bidirectional_iterator
; last      : bidirectional_iterator
; pred      : unary_function
; returns   : copy of first ( point to partition point ).
(locally (declare (optimize speed))

  ;;PTN; partition : 0 -   f   [0x11]
  #-cl-stl-0x98 ; partition for forward_iterator
  (defmethod partition ((first forward_iterator) (last forward_iterator) pred)
	(with-operators
		(if (_== first last)
			@~last
			(let ((first @~first)
				  (pred  (functor_function @~pred)))
			  (declare (type cl:function pred))
			  (for (nil (funcall pred *first) nil)
				(when (_== ++first last)
				  (return-from partition first)))
			  (for (((next @~first)) (_/= ++next last) nil :returns first)
				(when (funcall pred *next)
				  (swap *first *next)
				  ++first))))))

  ;;PTN; partition : 0 -   b
  (defmethod partition ((first bidirectional_iterator) (last bidirectional_iterator) pred)
	(with-operators
		(if (_== first last)
			@~first
			(let ((pred (functor_function @~pred)))
			  (declare (type cl:function pred))
			  (labels ((imp1 (itr1 itr2)
						 (for (nil (_/= itr1 itr2) ++itr1 :returns itr1)
						   (unless (funcall pred *itr1)
							 (return-from imp1 itr1))))
					   (imp2 (itr1 itr2)
						 (for (nil (_/= itr1 itr2) --itr2 :returns itr1)
						   (when (funcall pred *itr2)
							 (return-from imp2 itr2)))))
				(for (((itr1 @~first) (itr2 @~last)) t ++itr1)
				  (setf itr1 (imp1 itr1 itr2))
				  (if (_== itr1 itr2)
					  (return-from partition itr1)
					  (progn
						(setf itr2 (imp2 itr1 --itr2))
						(if (_== itr1 itr2)
							(return-from partition itr1)
							(swap *itr1 *itr2))))))))))

  ;;PTN; partition : 1 -   ci  [0x11]
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod partition ((first cons_iterator) (last cons_iterator) pred)
	;;(format t "specialized partition for cons_iterator is invoked.~%")
	(labels ((__partition-imp-1 (cons1 cons2)
			   (declare (type cl:list cons1 cons2))
			   (macrolet ((move-next (sym)
							`(setf ,sym (cdr ,sym))))
				 (if (eq cons1 cons2)
					 cons2
					 (let ((pred (functor_function (clone pred))))
					   (declare (type cl:function pred))
					   (for (nil (funcall pred (car cons1)) nil)
						 (move-next cons1)
						 (when (eq cons1 cons2)
						   (return-from __partition-imp-1 cons1)))
					   (for (((next cons1)) (not (eq (move-next next) cons2)) nil :returns cons1)
						 (when (funcall pred (car next))
						   (swap (car cons1) (car next))
						   (move-next cons1))))))))
	  (__algo-make-cns-iterator first (__partition-imp-1 (__cons-itr-cons first)
														  (__cons-itr-cons  last)))))

  ;;PTN; partition : 2 -   vp
  (defmethod partition ((first vector-pointer) (last vector-pointer) pred)
	;;(format t "specialized partition for vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index first))
		  (idx2 (opr::vec-ptr-index  last)))
	  (declare (type fixnum idx1 idx2))
	  (if (= idx1 idx2)
		  (clone last)
		  (let ((buffer  (opr::vec-ptr-buffer first))
				(pred-uf (functor_function (clone pred))))
			(declare (type cl:vector buffer))
			(declare (type cl:function pred-uf))
			(labels ((imp1 (idx1 idx2)
					   (declare (type fixnum idx1 idx2))
					   (for (nil (< idx1 idx2) (incf idx1) :returns idx1)
						 (unless (funcall pred-uf (aref buffer idx1))
						   (return-from imp1 idx1))))
					 (imp2 (idx1 idx2)
					   (declare (type fixnum idx1 idx2))
					   (for (nil (< idx1 idx2) (decf idx2) :returns idx1)
						 (when (funcall pred-uf (aref buffer idx2))
					 (return-from imp2 idx2)))))
			  (let ((ret (for (nil t (incf idx1))
						   (setf idx1 (imp1 idx1 idx2))
						   (if (= idx1 idx2)
							   (return idx1)
							   (progn
								 (setf idx2 (imp2 idx1 (1- idx2)))
								 (if (= idx1 idx2)
									 (return idx1)
									 (swap (aref buffer idx1) (aref buffer idx2))))))))
				(__algo-make-vct-iterator first ret))))))))




; first     : bidirectional_iterator
; last      : bidirectional_iterator
; pred      : unary_function
; returns   : copy of first ( point to partition point ).
(locally (declare (optimize speed))

  ;;PTN; stable_partition : 0 -   b 
  (defmethod stable_partition ((first bidirectional_iterator)
							   (last  bidirectional_iterator) pred)
	(if (_== first last)
		(clone last)
		(let ((n      (the fixnum (distance first last)))
			  (tmpitr (new-tmpitr)))
		  (declare (type fixnum n))
		  (tmpitr-set-buf-len tmpitr n)
		  (__recursive-stable_partition-0 first last
										  (functor_function (clone pred)) n tmpitr))))

  ;;PTN; stable_partition : 1 -   vp
  (defmethod stable_partition ((first vector-pointer) (last vector-pointer) pred)
	;;(format t "specialized stable_partition for vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index first))
		  (idx2 (opr::vec-ptr-index  last)))
	  (declare (type fixnum idx1 idx2))
	  (if (= idx1 idx2)
		  (clone last)
		  (let ((buffer  (opr::vec-ptr-buffer first))
				(pred-uf (functor_function (clone pred))))
			(declare (type cl:vector buffer))
			(declare (type cl:function pred-uf))
			(let ((ret (let ((n      (- idx2 idx1))
							 (tmpitr (new-tmpitr)))
						 (declare (type fixnum n))
						 (tmpitr-set-buf-len tmpitr n)
						 (__recursive-stable_partition-1 idx1 idx2 buffer pred-uf n tmpitr))))
			  (__algo-make-vct-iterator first ret)))))))




; first        : input_iterator
; last         : input_iterator
; result-true  : output_iterator
; result-false : output_iterator
; pred         : unary_function
; returns      : pair of output_iterator.
#-cl-stl-0x98    ; partition_copy
(locally (declare (optimize speed))

  ;;PTN; partition_copy : 00 -   i  x  o  x  o 
  (defmethod partition_copy ((first input_iterator) (last input_iterator)
							 (result-true output_iterator) (result-false output_iterator) pred)
	(with-operators
		(if (_== first last)
			(make_pair @~result-true @~result-false)
			(let ((pred (functor_function (clone pred))))
			  (declare (type cl:function pred))
			  (for (((itr     @~first)
					 (t-out   @~result-true)
					 (nil-out @~result-false)) (_/= itr last) ++itr :returns (make_pair t-out nil-out))
				(let ((val *itr))
				  (if (funcall pred val)
					  (progn (_= *t-out   val) ++t-out)
					  (progn (_= *nil-out val) ++nil-out))))))))


  ;;PTN; partition_copy : 01 -   i  x  o  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod partition_copy ((first input_iterator) (last input_iterator)
							 (result-true output_iterator) (result-false cons_iterator) pred-uf)
	;;(format t "specialized partition_copy for input_iterator & output_iterator & cons_iterator is invoked.~%")
	(with-operators
		(if (_== first last)
			(make_pair @~result-true @~result-false)
			(let ((t-out   @~result-true)
				  (nil-cns (__cons-itr-cons result-false))
				  (pred-uf (functor_function @~pred-uf)))
			  (declare (type cl:list nil-cns))
			  (declare (type cl:function pred-uf))
			  (for (((itr @~first)) (_/= itr last) ++itr
									:returns (make_pair t-out (__algo-make-cns-iterator result-false nil-cns)))
				(let ((val *itr))
				  (if (funcall pred-uf val)
					  (progn (_= *t-out        val) ++t-out)
					  (progn (_= (car nil-cns) val) (setf nil-cns (cdr nil-cns))))))))))

  ;;PTN; partition_copy : 02 -   i  x  o  x  vp
  (defmethod partition_copy ((first input_iterator) (last input_iterator)
							 (result-true output_iterator) (result-false vector-pointer) pred-uf)
	;;(format t "specialized partition_copy for input_iterator & output_iterator & vector-pointer is invoked.~%")
	(with-operators
		(if (_== first last)
			(make_pair @~result-true @~result-false)
			(let ((t-out   @~result-true)
				  (nil-idx (opr::vec-ptr-index  result-false))
				  (nil-buf (opr::vec-ptr-buffer result-false))
				  (pred-uf (functor_function @~pred-uf)))
			  (declare (type fixnum      nil-idx))
			  (declare (type cl:vector   nil-buf))
			  (declare (type cl:function pred-uf))
			  (for (((itr @~first)) (_/= itr last) ++itr
									:returns (make_pair t-out (__algo-make-vct-iterator result-false nil-idx)))
				(let ((val *itr))
				  (if (funcall pred-uf val)
					  (progn (_= *t-out                 val) ++t-out)
					  (progn (_= (aref nil-buf nil-idx) val) (incf nil-idx)))))))))

  ;;PTN; partition_copy : 03 -   i  x  ci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod partition_copy ((first input_iterator) (last input_iterator)
							 (result-true cons_iterator) (result-false output_iterator) pred-uf)
	;;(format t "specialized partition_copy for input_iterator & cons_iterator & output_iterator is invoked.~%")
	(with-operators
		(if (_== first last)
			(make_pair @~result-true @~result-false)
			(let ((t-cns   (__cons-itr-cons result-true))
				  (nil-out @~result-false)
				  (pred-uf (functor_function @~pred-uf)))
			  (declare (type cl:list     t-cns))
			  (declare (type cl:function pred-uf))
			  (for (((itr @~first)) (_/= itr last) ++itr
									:returns (make_pair (__algo-make-cns-iterator result-true t-cns) nil-out))
				(let ((val *itr))
				  (if (funcall pred-uf val)
					  (progn (_= (car t-cns) val) (setf t-cns (cdr t-cns)))
					  (progn (_= *nil-out    val) ++nil-out))))))))

  ;;PTN; partition_copy : 04 -   i  x  ci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod partition_copy ((first input_iterator) (last input_iterator)
							 (result-true cons_iterator) (result-false cons_iterator) pred-uf)
	;;(format t "specialized partition_copy for input_iterator & cons_iterator & cons_iterator is invoked.~%")
	(with-operators
		(if (_== first last)
			(make_pair @~result-true @~result-false)
			(let ((t-cns   (__cons-itr-cons  result-true))
				  (nil-cns (__cons-itr-cons  result-false))
				  (pred-uf (functor_function @~pred-uf)))
			  (declare (type cl:list t-cns nil-cns))
			  (declare (type cl:function pred-uf))
			  (for (((itr @~first)) (_/= itr last) ++itr
									:returns (make_pair (__algo-make-cns-iterator result-true  t-cns)
														(__algo-make-cns-iterator result-false nil-cns)))
				(let ((val *itr))
				  (if (funcall pred-uf val)
					  (progn (_= (car t-cns)   val) (setf t-cns   (cdr t-cns)))
					  (progn (_= (car nil-cns) val) (setf nil-cns (cdr nil-cns))))))))))

  ;;PTN; partition_copy : 05 -   i  x  ci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod partition_copy ((first input_iterator) (last input_iterator)
							 (result-true cons_iterator) (result-false vector-pointer) pred-uf)
	;;(format t "specialized partition_copy for input_iterator & cons_iterator & vector-pointer is invoked.~%")
	(with-operators
		(if (_== first last)
			(make_pair @~result-true @~result-false)
			(let ((t-cns   (__cons-itr-cons     result-true))
				  (nil-idx (opr::vec-ptr-index  result-false))
				  (nil-buf (opr::vec-ptr-buffer result-false))
				  (pred-uf (functor_function @~pred-uf)))
			  (declare (type cl:list   t-cns))
			  (declare (type fixnum    nil-idx))
			  (declare (type cl:vector nil-buf))
			  (declare (type cl:function pred-uf))
			  (for (((itr @~first)) (_/= itr last) ++itr
									:returns (make_pair (__algo-make-cns-iterator result-true  t-cns)
														(__algo-make-vct-iterator      result-false nil-idx)))
				(let ((val *itr))
				  (if (funcall pred-uf val)
					  (progn (_= (car t-cns)            val) (setf t-cns (cdr t-cns)))
					  (progn (_= (aref nil-buf nil-idx) val) (incf nil-idx)))))))))
  
  ;;PTN; partition_copy : 06 -   i  x  vp x  o 
  (defmethod partition_copy ((first input_iterator) (last input_iterator)
							 (result-true vector-pointer) (result-false output_iterator) pred-uf)
	;;(format t "specialized partition_copy for input_iterator & vector-pointer & output_iterator is invoked.~%")
	(with-operators
		(if (_== first last)
			(make_pair @~result-true @~result-false)
			(let ((t-idx   (opr::vec-ptr-index  result-true))
				  (t-buf   (opr::vec-ptr-buffer result-true))
				  (nil-out @~result-false)
				  (pred-uf (functor_function @~pred-uf)))
			  (declare (type fixnum      t-idx))
			  (declare (type cl:vector    t-buf))
			  (declare (type cl:function pred-uf))
			  (for (((itr @~first)) (_/= itr last) ++itr
									:returns (make_pair (__algo-make-vct-iterator result-true t-idx) nil-out))
				(let ((val *itr))
				  (if (funcall pred-uf val)
					  (progn (_= (aref t-buf t-idx) val) (incf t-idx))
					  (progn (_= *nil-out            val) ++nil-out))))))))

  ;;PTN; partition_copy : 07 -   i  x  vp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod partition_copy ((first input_iterator) (last input_iterator)
							 (result-true vector-pointer) (result-false cons_iterator) pred-uf)
	;;(format t "specialized partition_copy for input_iterator & vector-pointer & cons_iterator is invoked.~%")
	(with-operators
		(if (_== first last)
			(make_pair @~result-true @~result-false)
			(let ((t-idx   (opr::vec-ptr-index  result-true))
				  (t-buf   (opr::vec-ptr-buffer result-true))
				  (nil-cns (__cons-itr-cons     result-false))
				  (pred-uf (functor_function @~pred-uf)))
			  (declare (type fixnum t-idx))
			  (declare (type cl:vector t-buf))
			  (declare (type cl:list nil-cns))
			  (declare (type cl:function pred-uf))
			  (for (((itr @~first)) (_/= itr last) ++itr
									:returns (make_pair (__algo-make-vct-iterator result-true  t-idx)
														(__algo-make-cns-iterator result-false nil-cns)))
				(let ((val *itr))
				  (if (funcall pred-uf val)
					  (progn (_= (aref t-buf t-idx) val) (incf t-idx))
					  (progn (_= (car nil-cns)      val) (setf nil-cns (cdr nil-cns))))))))))

  ;;PTN; partition_copy : 08 -   i  x  vp x  vp
  (defmethod partition_copy ((first input_iterator) (last input_iterator)
							 (result-true vector-pointer) (result-false vector-pointer) pred-uf)
	;;(format t "specialized partition_copy for input_iterator & vector-pointer & vector-pointer is invoked.~%")
	(with-operators
		(if (_== first last)
			(make_pair @~result-true @~result-false)
			(let ((t-idx   (opr::vec-ptr-index  result-true))
				  (t-buf   (opr::vec-ptr-buffer result-true))
				  (nil-idx (opr::vec-ptr-index  result-false))
				  (nil-buf (opr::vec-ptr-buffer result-false))
				  (pred-uf (functor_function @~pred-uf)))
			  (declare (type fixnum    t-idx nil-idx))
			  (declare (type cl:vector t-buf nil-buf))
			  (declare (type cl:function pred-uf))
			  (for (((itr @~first)) (_/= itr last) ++itr
									:returns (make_pair (__algo-make-vct-iterator result-true  t-idx)
														(__algo-make-vct-iterator result-false nil-idx)))
				(let ((val *itr))
				  (if (funcall pred-uf val)
					  (progn (_= (aref   t-buf   t-idx) val) (incf   t-idx))
					  (progn (_= (aref nil-buf nil-idx) val) (incf nil-idx)))))))))
  
  ;;PTN; partition_copy : 09 -  cci x  o  x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod partition_copy ((first cons_const_iterator) (last cons_const_iterator)
							 (result-true output_iterator) (result-false output_iterator) pred-uf)
	;;(format t "specialized partition_copy for cons_const_iterator & output_iterator & output_iterator is invoked.~%")
	(let ((cns1 (__cons-itr-cons first))
		  (cns2 (__cons-itr-cons  last)))
	  (declare (type cl:list cns1 cns2))
	  (with-operators
		  (if (eq cns1 cns2)
			  (make_pair @~result-true @~result-false)
			  (let ((t-out   @~result-true)
					(nil-out @~result-false)
					(pred-uf (functor_function @~pred-uf)))
				(declare (type cl:function pred-uf))
				(for (nil (not (eq cns1 cns2)) (setf cns1 (cdr cns1)) :returns (make_pair t-out nil-out))
				   (let ((val (car cns1)))
					 (if (funcall pred-uf val)
						 (progn (_=   *t-out val) ++t-out)
						 (progn (_= *nil-out val) ++nil-out)))))))))

  ;;PTN; partition_copy : 10 -  cci x  o  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod partition_copy ((first cons_const_iterator) (last cons_const_iterator)
							 (result-true output_iterator) (result-false cons_iterator) pred-uf)
	;;(format t "specialized partition_copy for cons_const_iterator & output_iterator & cons_iterator is invoked.~%")
	(let ((cns1 (__cons-itr-cons first))
		  (cns2 (__cons-itr-cons  last)))
	  (declare (type cl:list cns1 cns2))
	  (with-operators
		  (if (eq cns1 cns2)
			  (make_pair @~result-true @~result-false)
			  (let ((t-out   @~result-true)
					(nil-cns (__cons-itr-cons result-false))
					(pred-uf (functor_function @~pred-uf)))
				(declare (type cl:list nil-cns))
				(declare (type cl:function pred-uf))
				(for (nil (not (eq cns1 cns2)) (setf cns1 (cdr cns1))
										:returns (make_pair t-out (__algo-make-cns-iterator result-false nil-cns)))
				  (let ((val (car cns1)))
					(if (funcall pred-uf val)
						(progn (_= *t-out val) ++t-out)
						(progn (_= (car nil-cns) val) (setf nil-cns (cdr nil-cns)))))))))))

  ;;PTN; partition_copy : 11 -  cci x  o  x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod partition_copy ((first cons_const_iterator) (last cons_const_iterator)
							 (result-true output_iterator) (result-false vector-pointer) pred-uf)
	;;(format t "specialized partition_copy for cons_const_iterator & output_iterator & vector-pointer is invoked.~%")
	(let ((cns1 (__cons-itr-cons first))
		  (cns2 (__cons-itr-cons  last)))
	  (declare (type cl:list cns1 cns2))
	  (with-operators
		  (if (eq cns1 cns2)
			  (make_pair @~result-true @~result-false)
			  (let ((t-out   @~result-true)
					(nil-idx (opr::vec-ptr-index  result-false))
					(nil-buf (opr::vec-ptr-buffer result-false))
					(pred-uf (functor_function @~pred-uf)))
				(declare (type fixnum nil-idx))
				(declare (type cl:vector nil-buf))
				(declare (type cl:function pred-uf))
				(for (nil (not (eq cns1 cns2)) (setf cns1 (cdr cns1))
										:returns (make_pair t-out (__algo-make-vct-iterator result-false nil-idx)))
				  (let ((val (car cns1)))
					(if (funcall pred-uf val)
						(progn (_= *t-out                 val) ++t-out)
						(progn (_= (aref nil-buf nil-idx) val) (incf nil-idx))))))))))

  ;;PTN; partition_copy : 12 -  cci x  ci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod partition_copy ((first cons_const_iterator) (last cons_const_iterator)
							 (result-true cons_iterator) (result-false output_iterator) pred-uf)
	;;(format t "specialized partition_copy for cons_const_iterator & cons_iterator & output_iterator is invoked.~%")
	(let ((cns1 (__cons-itr-cons first))
		  (cns2 (__cons-itr-cons  last)))
	  (declare (type cl:list cns1 cns2))
	  (with-operators
		  (if (eq cns1 cns2)
			  (make_pair @~result-true @~result-false)
			  (let ((t-cns   (__cons-itr-cons    result-true))
					(nil-out @~result-false)
					(pred-uf (functor_function @~pred-uf)))
				(declare (type cl:list   t-cns))
				(declare (type cl:function pred-uf))
				(for (nil (not (eq cns1 cns2)) (setf cns1 (cdr cns1))
										:returns (make_pair (__algo-make-cns-iterator result-true t-cns) nil-out))
				  (let ((val (car cns1)))
					(if (funcall pred-uf val)
						(progn (_= (car t-cns) val) (setf t-cns (cdr t-cns)))
						(progn (_= *nil-out    val) ++nil-out)))))))))

  ;;PTN; partition_copy : 13 -  cci x  ci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod partition_copy ((first cons_const_iterator) (last cons_const_iterator)
							 (result-true cons_iterator) (result-false cons_iterator) pred-uf)
	;;(format t "specialized partition_copy for cons_const_iterator & cons_iterator & cons_iterator is invoked.~%")
	(let ((cns1 (__cons-itr-cons first))
		  (cns2 (__cons-itr-cons  last)))
	  (declare (type cl:list cns1 cns2))
	  (with-operators
		  (if (eq cns1 cns2)
			  (make_pair @~result-true @~result-false)
			  (let ((t-cns   (__cons-itr-cons     result-true))
					(nil-cns (__cons-itr-cons     result-false))
					(pred-uf (functor_function    @~pred-uf)))
				(declare (type cl:list     t-cns nil-cns))
				(declare (type cl:function pred-uf))
				(for (nil (not (eq cns1 cns2)) (setf cns1 (cdr cns1))
										:returns (make_pair (__algo-make-cns-iterator result-true  t-cns)
															(__algo-make-cns-iterator result-false nil-cns)))
				  (let ((val (car cns1)))
					(if (funcall pred-uf val)
						(progn (_= (car t-cns)   val) (setf t-cns   (cdr t-cns)))
						(progn (_= (car nil-cns) val) (setf nil-cns (cdr nil-cns)))))))))))

  ;;PTN; partition_copy : 14 -  cci x  ci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod partition_copy ((first cons_const_iterator) (last cons_const_iterator)
							 (result-true cons_iterator) (result-false vector-pointer) pred-uf)
	;;(format t "specialized partition_copy for cons_const_iterator & cons_iterator & vector-pointer is invoked.~%")
	(let ((cns1 (__cons-itr-cons first))
		  (cns2 (__cons-itr-cons  last)))
	  (declare (type cl:list cns1 cns2))
	  (with-operators
		  (if (eq cns1 cns2)
			  (make_pair @~result-true @~result-false)
			  (let ((t-cns   (__cons-itr-cons     result-true))
					(nil-idx (opr::vec-ptr-index  result-false))
					(nil-buf (opr::vec-ptr-buffer result-false))
					(pred-uf (functor_function @~pred-uf)))
				(declare (type cl:list   t-cns))
				(declare (type fixnum    nil-idx))
				(declare (type cl:vector nil-buf))
				(declare (type cl:function pred-uf))
				(for (nil (not (eq cns1 cns2)) (setf cns1 (cdr cns1))
										:returns (make_pair (__algo-make-cns-iterator result-true  t-cns)
															(__algo-make-vct-iterator      result-false nil-idx)))
				  (let ((val (car cns1)))
					(if (funcall pred-uf val)
						(progn (_= (car t-cns)            val) (setf t-cns (cdr t-cns)))
						(progn (_= (aref nil-buf nil-idx) val) (incf nil-idx))))))))))

  ;;PTN; partition_copy : 15 -  cci x  vp x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod partition_copy ((first cons_const_iterator) (last cons_const_iterator)
							 (result-true vector-pointer) (result-false output_iterator) pred-uf)
	;;(format t "specialized partition_copy for cons_const_iterator & vector-pointer & output_iterator is invoked.~%")
	(let ((cns1 (__cons-itr-cons first))
		  (cns2 (__cons-itr-cons  last)))
	  (declare (type cl:list cns1 cns2))
	  (with-operators
		  (if (eq cns1 cns2)
			  (make_pair @~result-true @~result-false)
			  (let ((t-idx   (opr::vec-ptr-index  result-true))
					(t-buf   (opr::vec-ptr-buffer result-true))
					(nil-out @~result-false)
					(pred-uf (functor_function @~pred-uf)))
				(declare (type fixnum t-idx))
				(declare (type cl:vector t-buf))
				(declare (type cl:function pred-uf))
				(for (nil (not (eq cns1 cns2)) (setf cns1 (cdr cns1))
										:returns (make_pair (__algo-make-vct-iterator result-true t-idx) nil-out))
				  (let ((val (car cns1)))
					(if (funcall pred-uf val)
						(progn (_= (aref t-buf t-idx) val) (incf t-idx))
						(progn (_= *nil-out           val) ++nil-out)))))))))

  ;;PTN; partition_copy : 16 -  cci x  vp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod partition_copy ((first cons_const_iterator) (last cons_const_iterator)
							 (result-true vector-pointer) (result-false cons_iterator) pred-uf)
	;;(format t "specialized partition_copy for cons_const_iterator & vector-pointer & cons_iterator is invoked.~%")
	(let ((cns1 (__cons-itr-cons first))
		  (cns2 (__cons-itr-cons  last)))
	  (declare (type cl:list cns1 cns2))
	  (with-operators
		  (if (eq cns1 cns2)
			  (make_pair @~result-true @~result-false)
			  (let ((t-idx   (opr::vec-ptr-index  result-true))
					(t-buf   (opr::vec-ptr-buffer result-true))
					(nil-cns (__cons-itr-cons     result-false))
					(pred-uf (functor_function @~pred-uf)))
				(declare (type fixnum    t-idx))
				(declare (type cl:list   nil-cns))
				(declare (type cl:vector t-buf))
				(declare (type cl:function pred-uf))
				(for (nil (not (eq cns1 cns2)) (setf cns1 (cdr cns1))
										:returns (make_pair (__algo-make-vct-iterator result-true  t-idx)
															(__algo-make-cns-iterator result-false nil-cns)))
				  (let ((val (car cns1)))
					(if (funcall pred-uf val)
						(progn (_= (aref t-buf t-idx) val) (incf t-idx))
						(progn (_= (car  nil-cns)     val) (setf nil-cns (cdr nil-cns)))))))))))

  ;;PTN; partition_copy : 17 -  cci x  vp x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod partition_copy ((first cons_const_iterator) (last cons_const_iterator)
							 (result-true vector-pointer) (result-false vector-pointer) pred-uf)
	;;(format t "specialized partition_copy for cons_const_iterator & vector-pointer & vector-pointer is invoked.~%")
	(let ((cns1 (__cons-itr-cons first))
		  (cns2 (__cons-itr-cons  last)))
	  (declare (type cl:list cns1 cns2))
	  (with-operators
		  (if (eq cns1 cns2)
			  (make_pair @~result-true @~result-false)
			  (let ((t-idx   (opr::vec-ptr-index  result-true))
					(t-buf   (opr::vec-ptr-buffer result-true))
					(nil-idx (opr::vec-ptr-index  result-false))
					(nil-buf (opr::vec-ptr-buffer result-false))
					(pred-uf (functor_function @~pred-uf)))
				(declare (type fixnum    t-idx nil-idx))
				(declare (type cl:vector t-buf nil-buf))
				(declare (type cl:function pred-uf))
				(for (nil (not (eq cns1 cns2)) (setf cns1 (cdr cns1))
										:returns (make_pair (__algo-make-vct-iterator result-true  t-idx)
															(__algo-make-vct-iterator result-false nil-idx)))
				  (let ((val (car cns1)))
					(if (funcall pred-uf val)
						(progn (_= (aref   t-buf   t-idx) val) (incf t-idx))
						(progn (_= (aref nil-buf nil-idx) val) (incf nil-idx))))))))))

  ;;PTN; partition_copy : 18 -  cvp x  o  x  o 
  (defmethod partition_copy ((first const-vector-pointer) (last const-vector-pointer)
							 (result-true output_iterator) (result-false output_iterator) pred-uf)
	;;(format t "specialized partition_copy for const-vector-pointer & output_iterator & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index  first))
		  (idx2 (opr::vec-ptr-index   last)))
	  (declare (type fixnum idx1 idx2))
	  (with-operators
		  (if (= idx1 idx2)
			  (make_pair @~result-true @~result-false)
			  (let ((buffer  (opr::vec-ptr-buffer first))
					(t-out   @~result-true)
					(nil-out @~result-false)
					(pred-uf (functor_function @~pred-uf)))
				(declare (type cl:vector buffer))
				(declare (type cl:function pred-uf))
				(for (nil (< idx1 idx2) (incf idx1) :returns (make_pair t-out nil-out))
				   (let ((val (aref buffer idx1)))
					 (if (funcall pred-uf val)
						 (progn (_=   *t-out val) ++t-out)
						 (progn (_= *nil-out val) ++nil-out)))))))))

  
  ;;PTN; partition_copy : 19 -  cvp x  o  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod partition_copy ((first const-vector-pointer) (last const-vector-pointer)
							 (result-true output_iterator) (result-false cons_iterator) pred-uf)
	;;(format t "specialized partition_copy for const-vector-pointer & output_iterator & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index first))
		  (idx2 (opr::vec-ptr-index  last)))
	  (declare (type fixnum idx1 idx2))
	  (with-operators
		  (if (= idx1 idx2)
			  (make_pair @~result-true @~result-false)
			  (let ((src-buf (opr::vec-ptr-buffer first))
					(t-out   @~result-true)
					(nil-cns (__cons-itr-cons result-false))
					(pred-uf (functor_function @~pred-uf)))
				(declare (type cl:list nil-cns))
				(declare (type cl:vector src-buf))
				(declare (type cl:function pred-uf))
				(for (nil (< idx1 idx2) (incf idx1)
										:returns (make_pair t-out (__algo-make-cns-iterator result-false nil-cns)))
				  (let ((val (aref src-buf idx1)))
					(if (funcall pred-uf val)
						(progn (_= *t-out val) ++t-out)
						(progn (_= (car nil-cns) val) (setf nil-cns (cdr nil-cns)))))))))))
  
  ;;PTN; partition_copy : 20 -  cvp x  o  x  vp
  (defmethod partition_copy ((first const-vector-pointer) (last const-vector-pointer)
							 (result-true output_iterator) (result-false vector-pointer) pred-uf)
	;;(format t "specialized partition_copy for const-vector-pointer & output_iterator & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index  first))
		  (idx2 (opr::vec-ptr-index   last)))
	  (declare (type fixnum idx1 idx2))
	  (with-operators
		  (if (= idx1 idx2)
			  (make_pair @~result-true @~result-false)
			  (let ((src-buf (opr::vec-ptr-buffer first))
					(t-out   @~result-true)
					(nil-idx (opr::vec-ptr-index  result-false))
					(nil-buf (opr::vec-ptr-buffer result-false))
					(pred-uf (functor_function @~pred-uf)))
				(declare (type fixnum nil-idx))
				(declare (type cl:vector src-buf nil-buf))
				(declare (type cl:function pred-uf))
				(for (nil (< idx1 idx2) (incf idx1)
										:returns (make_pair t-out (__algo-make-vct-iterator result-false nil-idx)))
				  (let ((val (aref src-buf idx1)))
					(if (funcall pred-uf val)
						(progn (_= *t-out                 val) ++t-out)
						(progn (_= (aref nil-buf nil-idx) val) (incf nil-idx))))))))))
  
  ;;PTN; partition_copy : 21 -  cvp x  ci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod partition_copy ((first const-vector-pointer) (last const-vector-pointer)
							 (result-true cons_iterator) (result-false output_iterator) pred-uf)
	;;(format t "specialized partition_copy for const-vector-pointer & cons_iterator & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index  first))
		  (idx2 (opr::vec-ptr-index   last)))
	  (declare (type fixnum idx1 idx2))
	  (with-operators
		  (if (= idx1 idx2)
			  (make_pair @~result-true @~result-false)
			  (let ((src-buf (opr::vec-ptr-buffer first))
					(t-cns   (__cons-itr-cons    result-true))
					(nil-out @~result-false)
					(pred-uf (functor_function @~pred-uf)))
				(declare (type cl:list   t-cns))
				(declare (type cl:vector src-buf))
				(declare (type cl:function pred-uf))
				(for (nil (< idx1 idx2) (incf idx1)
										:returns (make_pair (__algo-make-cns-iterator result-true t-cns) nil-out))
				  (let ((val (aref src-buf idx1)))
					(if (funcall pred-uf val)
						(progn (_= (car t-cns) val) (setf t-cns (cdr t-cns)))
						(progn (_= *nil-out    val) ++nil-out)))))))))

  ;;PTN; partition_copy : 22 -  cvp x  ci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod partition_copy ((first const-vector-pointer) (last const-vector-pointer)
							 (result-true cons_iterator) (result-false cons_iterator) pred-uf)
	;;(format t "specialized partition_copy for const-vector-pointer & cons_iterator & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index  first))
		  (idx2 (opr::vec-ptr-index   last)))
	  (declare (type fixnum idx1 idx2))
	  (with-operators
		  (if (= idx1 idx2)
			  (make_pair @~result-true @~result-false)
			  (let ((src-buf (opr::vec-ptr-buffer first))
					(t-cns   (__cons-itr-cons     result-true))
					(nil-cns (__cons-itr-cons     result-false))
					(pred-uf (functor_function    @~pred-uf)))
				(declare (type cl:list     t-cns nil-cns))
				(declare (type cl:vector   src-buf))
				(declare (type cl:function pred-uf))
				(for (nil (< idx1 idx2) (incf idx1)
										:returns (make_pair (__algo-make-cns-iterator result-true  t-cns)
															(__algo-make-cns-iterator result-false nil-cns)))
				  (let ((val (aref src-buf idx1)))
					(if (funcall pred-uf val)
						(progn (_= (car t-cns)   val) (setf t-cns   (cdr t-cns)))
						(progn (_= (car nil-cns) val) (setf nil-cns (cdr nil-cns)))))))))))

  ;;PTN; partition_copy : 23 -  cvp x  ci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod partition_copy ((first const-vector-pointer) (last const-vector-pointer)
							 (result-true cons_iterator) (result-false vector-pointer) pred-uf)
	;;(format t "specialized partition_copy for const-vector-pointer & cons_iterator & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index  first))
		  (idx2 (opr::vec-ptr-index   last)))
	  (declare (type fixnum idx1 idx2))
	  (with-operators
		  (if (= idx1 idx2)
			  (make_pair @~result-true @~result-false)
			  (let ((src-buf (opr::vec-ptr-buffer first))
					(t-cns   (__cons-itr-cons     result-true))
					(nil-idx (opr::vec-ptr-index  result-false))
					(nil-buf (opr::vec-ptr-buffer result-false))
					(pred-uf (functor_function @~pred-uf)))
				(declare (type cl:list   t-cns))
				(declare (type fixnum    nil-idx))
				(declare (type cl:vector nil-buf src-buf))
				(declare (type cl:function pred-uf))
				(for (nil (< idx1 idx2) (incf idx1)
										:returns (make_pair (__algo-make-cns-iterator result-true  t-cns)
															(__algo-make-vct-iterator      result-false nil-idx)))
				  (let ((val (aref src-buf idx1)))
					(if (funcall pred-uf val)
						(progn (_= (car t-cns)            val) (setf t-cns (cdr t-cns)))
						(progn (_= (aref nil-buf nil-idx) val) (incf nil-idx))))))))))

  ;;PTN; partition_copy : 24 -  cvp x  vp x  o 
  (defmethod partition_copy ((first const-vector-pointer) (last const-vector-pointer)
							 (result-true vector-pointer) (result-false output_iterator) pred-uf)
	;;(format t "specialized partition_copy for const-vector-pointer & vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index  first))
		  (idx2 (opr::vec-ptr-index   last)))
	  (declare (type fixnum idx1 idx2))
	  (with-operators
		  (if (= idx1 idx2)
			  (make_pair @~result-true @~result-false)
			  (let ((src-buf (opr::vec-ptr-buffer first))
					(t-idx   (opr::vec-ptr-index  result-true))
					(t-buf   (opr::vec-ptr-buffer result-true))
					(nil-out @~result-false)
					(pred-uf (functor_function @~pred-uf)))
				(declare (type fixnum t-idx))
				(declare (type cl:vector src-buf t-buf))
				(declare (type cl:function pred-uf))
				(for (nil (< idx1 idx2) (incf idx1)
										:returns (make_pair (__algo-make-vct-iterator result-true t-idx) nil-out))
				  (let ((val (aref src-buf idx1)))
					(if (funcall pred-uf val)
						(progn (_= (aref t-buf t-idx) val) (incf t-idx))
						(progn (_= *nil-out           val) ++nil-out)))))))))

  ;;PTN; partition_copy : 25 -  cvp x  vp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod partition_copy ((first const-vector-pointer) (last const-vector-pointer)
							 (result-true vector-pointer) (result-false cons_iterator) pred-uf)
	;;(format t "specialized partition_copy for const-vector-pointer & vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index  first))
		  (idx2 (opr::vec-ptr-index   last)))
	  (declare (type fixnum idx1 idx2))
	  (with-operators
		  (if (= idx1 idx2)
			  (make_pair @~result-true @~result-false)
			  (let ((src-buf (opr::vec-ptr-buffer first))
					(t-idx   (opr::vec-ptr-index  result-true))
					(t-buf   (opr::vec-ptr-buffer result-true))
					(nil-cns (__cons-itr-cons     result-false))
					(pred-uf (functor_function @~pred-uf)))
				(declare (type fixnum    t-idx))
				(declare (type cl:list   nil-cns))
				(declare (type cl:vector t-buf src-buf))
				(declare (type cl:function pred-uf))
				(for (nil (< idx1 idx2) (incf idx1)
										:returns (make_pair (__algo-make-vct-iterator result-true  t-idx)
															(__algo-make-cns-iterator result-false nil-cns)))
				  (let ((val (aref src-buf idx1)))
					(if (funcall pred-uf val)
						(progn (_= (aref t-buf t-idx) val) (incf t-idx))
						(progn (_= (car  nil-cns)     val) (setf nil-cns (cdr nil-cns)))))))))))

  ;;PTN; partition_copy : 26 -  cvp x  vp x  vp
  (defmethod partition_copy ((first const-vector-pointer) (last const-vector-pointer)
							 (result-true vector-pointer) (result-false vector-pointer) pred-uf)
	;;(format t "specialized partition_copy for const-vector-pointer & vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index  first))
		  (idx2 (opr::vec-ptr-index   last)))
	  (declare (type fixnum idx1 idx2))
	  (with-operators
		  (if (= idx1 idx2)
			  (make_pair @~result-true @~result-false)
			  (let ((src-buf (opr::vec-ptr-buffer first))
					(t-idx   (opr::vec-ptr-index  result-true))
					(t-buf   (opr::vec-ptr-buffer result-true))
					(nil-idx (opr::vec-ptr-index  result-false))
					(nil-buf (opr::vec-ptr-buffer result-false))
					(pred-uf (functor_function @~pred-uf)))
				(declare (type fixnum    t-idx nil-idx))
				(declare (type cl:vector t-buf nil-buf src-buf))
				(declare (type cl:function pred-uf))
				(for (nil (< idx1 idx2) (incf idx1)
										:returns (make_pair (__algo-make-vct-iterator result-true  t-idx)
															(__algo-make-vct-iterator result-false nil-idx)))
				  (let ((val (aref src-buf idx1)))
					(if (funcall pred-uf val)
						(progn (_= (aref   t-buf   t-idx) val) (incf t-idx))
						(progn (_= (aref nil-buf nil-idx) val) (incf nil-idx)))))))))))




; first        : forward_iterator
; last         : forward_iterator
; pred         : unary_function
; returns      : copy of iterator ( points result ).
#-cl-stl-0x98    ; partition_point
(locally (declare (optimize speed))

  ;;PTN; partition_point : 0 -   f 
  (defmethod partition_point ((first forward_iterator) (last forward_iterator) pred)
	(with-operators
		(let ((dist (distance first last))
			  (pred (functor_function (clone pred))))
		  (declare (type fixnum dist))
		  (declare (type cl:function pred))
		  (for (((top  @~first) (mid  @~first)) (< 0 dist) nil :returns mid)
			(let ((half (ash dist -1)))
			  (declare (type fixnum half))
			  (_= mid top)
			  (advance mid half)
			  (if (not (funcall pred *mid))
				  (setf dist half)
				  (progn
					++mid
					(_= top mid)
					(setf dist (- dist half 1)))))))))

  ;;PTN; partition_point : 1 -  cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod partition_point ((first cons_const_iterator) (last cons_const_iterator) pred)
	(format t "specialized partition_point for cons_const_iterator is invoked.~%")
	(let ((cons1 (__cons-itr-cons first))
		  (cons2 (__cons-itr-cons  last)))
	  (declare (type cl:list cons1 cons2))
	  (let ((dist  (__conslist-count-nodes cons1 cons2))
			(pred (functor_function (clone pred))))
		(declare (type fixnum dist))
		(declare (type cl:function pred))
		(let ((top cons1)
			  (mid cons1))
		  (declare (type cl:list top mid))
		  (for (nil (< 0 dist) nil :returns (__algo-make-cns-iterator first mid))
			(let ((half (ash dist -1)))
			  (declare (type fixnum half))
			  (setf mid top)
			  (dotimes (v half) (setf mid (cdr mid)))
			  (if (not (funcall pred (car mid)))
				  (setf dist half)
				  (progn
					(setf mid (cdr mid))
					(setf top mid)
					(setf dist (- dist half 1))))))))))

  ;;PTN; partition_point : 2 -  cvp
  (defmethod partition_point ((first const-vector-pointer) (last const-vector-pointer) pred)
	;;(format t "specialized partition_point for const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1    (opr::vec-ptr-index  first))
		  (idx2    (opr::vec-ptr-index   last))
		  (buffer  (opr::vec-ptr-buffer first))
		  (pred-uf (functor_function (clone pred))))
	  (declare (type fixnum idx1 idx2))
	  (declare (type cl:vector buffer))
	  (declare (type cl:function pred-uf))
	  (let* ((top  idx1)
			 (dist (- idx2 top))
			 (mid  0)
			 (half 0))
		(declare (type fixnum top dist mid half))
		(for (nil (< 0 dist) nil :returns (__algo-make-vct-iterator first mid))
		  (setf half (the fixnum (ash dist -1)))
		  (setf mid  (the fixnum (+ top half)))
		  (if (not (funcall pred-uf (aref buffer mid)))
			  (setf dist half)
			  (progn
				(incf mid)
				(setf top mid)
				(setf dist (- dist half 1)))))))))





;;------------------------------------------------------------------------------
;; 25.3, sorting and related operations:
;;------------------------------------------------------------------------------
;; 25.3.1, sorting:


; first     : randomaccess_iterator
; last      : randomaccess_iterator
; comp      : binary_function ( default : #'operator_< )
; returns   : nil.
(locally (declare (optimize speed))

  ;;PTN; sort : 0 -   r 
  (labels ((__sort-imp-0 (first last comp)
			 (declare (type cl:function comp))
			 (if (<= (the fixnum (_- last first)) +SORT-MAX+)
				 (__insertion-sort-0 first last comp)
				 (with-operators
					 (let ((itr @~first))
					   (__recursive-sort-0 itr last (the fixnum (_- last itr)) comp)
					   (__insertion-sort-0 itr (_+ itr +SORT-MAX+) comp)
					   (_+= itr +SORT-MAX+)
					   (for (((tmp nil)) (_/= itr last) ++itr)
						 (_= tmp *itr)
						 (__unguarded-insert-0 itr tmp comp)))))))

	(defmethod-overload sort ((first randomaccess_iterator) (last randomaccess_iterator))
	  (__sort-imp-0 first last #'operator_<))

	(defmethod-overload sort ((first randomaccess_iterator) (last randomaccess_iterator) comp)
	  (__sort-imp-0 first last (functor_function (clone comp)))))

  
  ;;PTN; sort : 1 -   vp
  (labels ((__sort-imp-1 (idx1 idx2 buffer less-bf)
			 (declare (type fixnum idx1 idx2))
			 (declare (type cl:vector buffer))
			 (declare (type cl:function less-bf))
			 (if (<= (- idx2 idx1) +SORT-MAX+)
				 (__insertion-sort-1 idx1 idx2 buffer less-bf)
				 (let ((idx idx1))
				   (declare (type fixnum idx))
				   (__recursive-sort-1 idx idx2 buffer (the fixnum (- idx2 idx)) less-bf)
				   (__insertion-sort-1 idx (the fixnum (+ idx +SORT-MAX+)) buffer less-bf)
				   (incf idx +SORT-MAX+)
				   (for (((tmp nil)) (< idx idx2) (incf idx))
					 (_= tmp (aref buffer idx))
					 (__unguarded-insert-1 idx buffer tmp less-bf))))))

	(defmethod-overload sort ((first vector-pointer) (last vector-pointer))
	  ;;(format t "specialized sort for vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__sort-imp-1 (opr::vec-ptr-index  first)
					(opr::vec-ptr-index  last)
					(opr::vec-ptr-buffer first) #'operator_<))

	(defmethod-overload sort ((first vector-pointer) (last vector-pointer) comp)
	  ;;(format t "specialized sort for vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__sort-imp-1 (opr::vec-ptr-index  first)
					(opr::vec-ptr-index  last)
					(opr::vec-ptr-buffer first) (functor_function (clone comp))))))





; first     : randomaccess_iterator
; last      : randomaccess_iterator
; comp      : binary_function ( default : #'operator_< )
; returns   : nil.
(locally (declare (optimize speed))

  ;;PTN; stable_sort : 0 -   r 
  (labels ((__stable_sort-imp-0 (first last comp)
			 (declare (type cl:function comp))
			 (if (_== first last)
				 nil
				 (let ((tmpitr (new-tmpitr))
					   (n      (the fixnum (_- last first))))
				   (declare (type fixnum n))
				   (tmpitr-set-buf-len tmpitr n)
				   (__recursive-stable_sort-0 first last n tmpitr comp)))))

	(defmethod-overload stable_sort ((first randomaccess_iterator) (last randomaccess_iterator))
	  (__stable_sort-imp-0 first last #'operator_<))

	(defmethod-overload stable_sort ((first randomaccess_iterator) (last randomaccess_iterator) comp)
	  (__stable_sort-imp-0 first last (functor_function (clone comp)))))


  ;;PTN; stable_sort : 1 -   vp
  (labels ((__stable_sort-imp-1 (idx1 idx2 buffer less-bf)
			 (declare (type fixnum idx1 idx2))
			 (declare (type cl:vector buffer))
			 (declare (type cl:function less-bf))
			 (if (= idx1 idx2)
				 nil
				 (let ((tmpitr (new-tmpitr))
					   (n      (- idx2 idx1)))
				   (declare (type fixnum n))
				   (tmpitr-set-buf-len tmpitr n)
				   (__recursive-stable_sort-1 idx1 idx2 buffer n tmpitr less-bf)))))

	(defmethod-overload stable_sort ((first vector-pointer) (last vector-pointer))
	  ;;(format t "specialized stable_sort for vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__stable_sort-imp-1 (opr::vec-ptr-index  first)
						   (opr::vec-ptr-index  last)
						   (opr::vec-ptr-buffer first) #'operator_<))

	(defmethod-overload stable_sort ((first vector-pointer) (last vector-pointer) comp)
	  ;;(format t "specialized stable_sort for vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__stable_sort-imp-1 (opr::vec-ptr-index  first)
						   (opr::vec-ptr-index  last)
						   (opr::vec-ptr-buffer first) (functor_function (clone comp))))))



; first     : randomaccess_iterator
; middle    : randomaccess_iterator
; last      : randomaccess_iterator
; comp      : binary_function ( default : #'operator_< )
; returns   : nil.
(locally (declare (optimize speed))

  ;;PTN; partial_sort : 0 -   r 
  (labels ((__partial_sort-imp-0 (first middle last comp)
			 (declare (type cl:function comp))
			 (__make_heap-imp-0 first middle comp)
			 (with-operators
				 (for (((itr @~middle)) (_/= itr last) ++itr)
				   (when (funcall comp *itr *first)
					 (__pop_heap-imp-0 first middle itr *itr comp))))
			 (__sort_heap-imp-0 first middle comp)
			 nil))

	(defmethod-overload partial_sort ((first  randomaccess_iterator)
									  (middle randomaccess_iterator) (last randomaccess_iterator))
	  (__partial_sort-imp-0 first middle last #'operator_<))

	(defmethod-overload partial_sort ((first  randomaccess_iterator)
									  (middle randomaccess_iterator) (last randomaccess_iterator) comp)
	  (__partial_sort-imp-0 first middle last (functor_function (clone comp)))))


  ;;PTN; partial_sort : 1 -   vp
  (labels ((__partial_sort-imp-1 (idx1 idx2 idx3 buffer less-bf)
			 (declare (type fixnum idx1 idx2 idx3))
			 (declare (type cl:vector buffer))
			 (declare (type cl:function less-bf))
			 (__make_heap-imp-1 idx1 idx2 buffer less-bf)
			 (let ((idx idx2))
			   (declare (type fixnum idx))
			   (for (nil (< idx idx3) (incf idx))
				 (when (funcall less-bf (aref buffer idx) (aref buffer idx1))
				   (__pop_heap-imp-1 buffer idx1 idx2 idx (aref buffer idx) less-bf))))
			 (__sort_heap-imp-1 idx1 idx2 buffer less-bf)
			 nil))

	(defmethod-overload partial_sort ((first  vector-pointer)
									  (middle vector-pointer) (last vector-pointer))
	  ;;(format t "specialized partial_sort for vector-pointer is invoked.~%" ')
	  (__pointer-check-iterator-range first middle)
	  (__pointer-check-iterator-range middle last)
	  (__partial_sort-imp-1 (opr::vec-ptr-index  first)
							(opr::vec-ptr-index  middle)
							(opr::vec-ptr-index  last)
							(opr::vec-ptr-buffer first) #'operator_<))

	(defmethod-overload partial_sort ((first  vector-pointer)
									  (middle vector-pointer) (last vector-pointer) comp)
	  ;;(format t "specialized partial_sort for vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first middle)
	  (__pointer-check-iterator-range middle last)
	  (__partial_sort-imp-1 (opr::vec-ptr-index  first)
							(opr::vec-ptr-index  middle)
							(opr::vec-ptr-index  last)
							(opr::vec-ptr-buffer first) (functor_function (clone comp))))))




; first        : input_iterator
; last         : input_iterator
; result-first : randomaccess_iterator
; result-last  : randomaccess_iterator
; comp         : binary_function ( default : #'operator_< )
; returns      : copy of result-first ( point to end of sequence ).
(locally (declare (optimize speed))

  ;;PTN; partial_sort_copy : 0 -   i  x  r 
  (defmethod-overload partial_sort_copy ((first input_iterator) (last input_iterator)
										 (result-first randomaccess_iterator) (result-last randomaccess_iterator))
	(__partial_sort_copy-imp-0 first last result-first result-last #'operator_<))

  (defmethod-overload partial_sort_copy ((first input_iterator)
										 (last  input_iterator)
										 (result-first randomaccess_iterator)
										 (result-last  randomaccess_iterator) comp)
	(__partial_sort_copy-imp-0 first last result-first result-last (functor_function (clone comp))))


  ;;PTN; partial_sort_copy : 1 -  cci x  r 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload partial_sort_copy ((first cons_const_iterator) (last cons_const_iterator)
										 (result-first randomaccess_iterator) (result-last randomaccess_iterator))
	;;(format t "specialized partial_sort_copy for cons_const_iterator & randomaccess_iterator is invoked.~%")
	(__partial_sort_copy-imp-1 (__cons-itr-cons first)
							   (__cons-itr-cons  last) result-first result-last #'operator_<))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload partial_sort_copy ((first cons_const_iterator) (last cons_const_iterator)
										 (result-first randomaccess_iterator) (result-last randomaccess_iterator) comp)
	;;(format t "specialized partial_sort_copy for cons_const_iterator & randomaccess_iterator is invoked.~%")
	(__partial_sort_copy-imp-1 (__cons-itr-cons first)
							   (__cons-itr-cons  last) result-first result-last (functor_function (clone comp))))

  ;;PTN; partial_sort_copy : 2 -  cvp x  r 
  (defmethod-overload partial_sort_copy ((first const-vector-pointer) (last const-vector-pointer)
										 (result-first randomaccess_iterator) (result-last randomaccess_iterator))
	;;(format t "specialized partial_sort_copy for const-vector-pointer & randomaccess_iterator is invoked.~%")
	(__pointer-check-iterator-range first last)
	(__partial_sort_copy-imp-2 (opr::vec-ptr-index  first)
							   (opr::vec-ptr-index  last)
							   (opr::vec-ptr-buffer first) result-first result-last #'operator_<))

  (defmethod-overload partial_sort_copy ((first const-vector-pointer) (last const-vector-pointer)
										 (result-first randomaccess_iterator) (result-last randomaccess_iterator) comp)
	;;(format t "specialized partial_sort_copy for const-vector-pointer & randomaccess_iterator is invoked.~%")
	(__pointer-check-iterator-range first last)
	(__partial_sort_copy-imp-2 (opr::vec-ptr-index  first)
							   (opr::vec-ptr-index  last)
							   (opr::vec-ptr-buffer first) result-first result-last (functor_function (clone comp))))

  ;;PTN; partial_sort_copy : 3 -   i  x  vp
  (defmethod-overload partial_sort_copy ((first input_iterator) (last input_iterator)
										 (result-first vector-pointer) (result-last vector-pointer))
	;;(format t "specialized partial_sort_copy for input_iterator & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range result-first result-last)
	(__algo-make-vct-iterator result-first
							   (__partial_sort_copy-imp-3 first last
														  (opr::vec-ptr-index  result-first)
														  (opr::vec-ptr-index  result-last)
														  (opr::vec-ptr-buffer result-first) #'operator_<)))

  (defmethod-overload partial_sort_copy ((first input_iterator) (last input_iterator)
										 (result-first vector-pointer) (result-last vector-pointer) comp)
	;;(format t "specialized partial_sort_copy for input_iterator & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range result-first result-last)
	(__algo-make-vct-iterator result-first
							   (__partial_sort_copy-imp-3 first last
														  (opr::vec-ptr-index  result-first)
														  (opr::vec-ptr-index  result-last)
														  (opr::vec-ptr-buffer result-first) (functor_function (clone comp)))))

  ;;PTN; partial_sort_copy : 4 -  cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload partial_sort_copy ((first cons_const_iterator) (last cons_const_iterator)
										 (result-first vector-pointer) (result-last vector-pointer))
	;;(format t "specialized partial_sort_copy for cons_const_iterator & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range result-first result-last)
	(__algo-make-vct-iterator result-first
							   (__partial_sort_copy-imp-4 (__cons-itr-cons  first)
														  (__cons-itr-cons  last)
														  (opr::vec-ptr-index  result-first)
														  (opr::vec-ptr-index  result-last)
														  (opr::vec-ptr-buffer result-first) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload partial_sort_copy ((first cons_const_iterator) (last cons_const_iterator)
										 (result-first vector-pointer) (result-last vector-pointer) comp)
	;;(format t "specialized partial_sort_copy for cons_const_iterator & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range result-first result-last)
	(__algo-make-vct-iterator result-first
							   (__partial_sort_copy-imp-4 (__cons-itr-cons  first)
														  (__cons-itr-cons  last)
														  (opr::vec-ptr-index  result-first)
														  (opr::vec-ptr-index  result-last)
														  (opr::vec-ptr-buffer result-first) (functor_function (clone comp)))))

  ;;PTN; partial_sort_copy : 5 -  cvp x  vp
  (defmethod-overload partial_sort_copy ((first const-vector-pointer) (last const-vector-pointer)
										 (result-first vector-pointer) (result-last vector-pointer))
	;;(format t "specialized partial_sort_copy for const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(__pointer-check-iterator-range result-first result-last)
	(__algo-make-vct-iterator result-first
							   (__partial_sort_copy-imp-5 (opr::vec-ptr-index  first)
														  (opr::vec-ptr-index  last)
														  (opr::vec-ptr-buffer first)
														  (opr::vec-ptr-index  result-first)
														  (opr::vec-ptr-index  result-last)
														  (opr::vec-ptr-buffer result-first) #'operator_<)))

  (defmethod-overload partial_sort_copy ((first const-vector-pointer) (last const-vector-pointer)
										 (result-first vector-pointer) (result-last vector-pointer) comp)
	;;(format t "specialized partial_sort_copy for const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(__pointer-check-iterator-range result-first result-last)
	(__algo-make-vct-iterator result-first
							   (__partial_sort_copy-imp-5 (opr::vec-ptr-index  first)
														  (opr::vec-ptr-index  last)
														  (opr::vec-ptr-buffer first)
														  (opr::vec-ptr-index  result-first)
														  (opr::vec-ptr-index  result-last)
														  (opr::vec-ptr-buffer result-first) (functor_function (clone comp))))))





; first        : input_iterator
; last         : input_iterator
; comp         : binary_function ( default : #'operator_< )
; returns      : boolean value.
#-cl-stl-0x98    ; is_sorted
(locally (declare (optimize speed))

  ;;PTN; is_sorted : 0 -   i 
  (defmethod-overload is_sorted ((first input_iterator) (last input_iterator))
	(_== last (__is_sorted_until-imp-0 first last #'operator_<)))

  (defmethod-overload is_sorted ((first input_iterator) (last input_iterator) comp)
	(_== last (__is_sorted_until-imp-0 first last (functor_function (clone comp)))))


  ;;PTN; is_sorted : 1 -  cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload is_sorted ((first cons_const_iterator) (last cons_const_iterator))
	;;(format t "specialized is_sorted for cons_const_iterator is invoked.~%")
	(let ((cons2 (__cons-itr-cons last)))
	  (eq cons2 (__is_sorted_until-imp-1 (__cons-itr-cons first) cons2 #'operator_<))))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload is_sorted ((first cons_const_iterator) (last cons_const_iterator) comp)
	;;(format t "specialized is_sorted for cons_const_iterator is invoked.~%")
	(let ((cons2 (__cons-itr-cons last)))
	  (eq cons2 (__is_sorted_until-imp-1 (__cons-itr-cons first) cons2 (functor_function (clone comp))))))


  ;;PTN; is_sorted : 2 -  cvp
  (defmethod-overload is_sorted ((first const-vector-pointer) (last const-vector-pointer))
	;;(format t "specialized is_sorted for const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx2 (opr::vec-ptr-index  last)))
	  (declare (type fixnum idx2))
	  (= idx2 (the fixnum (__is_sorted_until-imp-2 (opr::vec-ptr-index  first) idx2
												   (opr::vec-ptr-buffer first) #'operator_<)))))

  (defmethod-overload is_sorted ((first const-vector-pointer) (last const-vector-pointer) comp)
	;;(format t "specialized is_sorted for const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx2 (opr::vec-ptr-index  last)))
	  (declare (type fixnum idx2))
	  (= idx2 (the fixnum (__is_sorted_until-imp-2 (opr::vec-ptr-index  first) idx2
												   (opr::vec-ptr-buffer first) (functor_function (clone comp))))))))




; first        : input_iterator
; last         : input_iterator
; comp         : binary_function ( default : #'operator_< )
; returns      : copy of iterator ( points result ).
#-cl-stl-0x98    ; is_sorted_until
(locally (declare (optimize speed))

  ;;PTN; is_sorted_until : 0 -   i 
  (defmethod-overload is_sorted_until ((first input_iterator) (last input_iterator))
	(__is_sorted_until-imp-0 first last #'operator_<))

  (defmethod-overload is_sorted_until ((first input_iterator) (last input_iterator) comp)
	(__is_sorted_until-imp-0 first last (functor_function (clone comp))))
  
  ;;PTN; is_sorted_until : 1 -  cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload is_sorted_until ((first cons_const_iterator) (last cons_const_iterator))
	;;(format t "specialized is_sorted_until for cons_const_iterator is invoked.~%")
	(__algo-make-cns-iterator first
							   (__is_sorted_until-imp-1 (__cons-itr-cons first)
														(__cons-itr-cons  last) #'operator_<)))
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload is_sorted_until ((first cons_const_iterator)
									   (last  cons_const_iterator) comp)
	;;(format t "specialized is_sorted_until for cons_const_iterator is invoked.~%")
	(__algo-make-cns-iterator first
							   (__is_sorted_until-imp-1 (__cons-itr-cons first)
														(__cons-itr-cons  last)
														(functor_function (clone comp)))))

  ;;PTN; is_sorted_until : 2 -  cvp
  (defmethod-overload is_sorted_until ((first const-vector-pointer) (last const-vector-pointer))
	;;(format t "specialized is_sorted_until for const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(__algo-make-vct-iterator first
							   (__is_sorted_until-imp-2 (opr::vec-ptr-index  first)
														(opr::vec-ptr-index  last)
														(opr::vec-ptr-buffer first) #'operator_<)))

  (defmethod-overload is_sorted_until ((first const-vector-pointer)
									   (last  const-vector-pointer) comp)
	;;(format t "specialized is_sorted_until for const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(__algo-make-vct-iterator first
							   (__is_sorted_until-imp-2 (opr::vec-ptr-index  first)
														(opr::vec-ptr-index  last)
														(opr::vec-ptr-buffer first)
														(functor_function (clone comp))))))



; first     : randomaccess_iterator
; nth       : randomaccess_iterator
; last      : randomaccess_iterator
; comp      : binary_function ( default : #'operator_< )
; returns   : nil.
(locally (declare (optimize speed))

  ;;PTN; nth_element : 0 -   r 
  (labels ((__nth_element-imp-0 (first nth last comp)
			 (declare (type cl:function comp))
			 (with-operators
				 (let ((first @~first)
					   (last  @~last))
				   (for (nil (< +SORT-MAX+ (the fixnum (_- last first))) nil)
					 (let* ((n   (ash (the fixnum (_- last first)) -1))
							(v   (__median *first first[n] last[-1] comp))
							(itr (__unguarded-partition-0 first last v comp)))
					   (declare (type fixnum n))
					   (if (<= 0 (the fixnum (_- nth itr)))
						   (_= first itr)
						   (_= last itr))))
				   (__insertion-sort-0 first last comp)))))
			 
	(defmethod-overload nth_element ((first randomaccess_iterator)
									 (nth randomaccess_iterator) (last randomaccess_iterator))
	  (__nth_element-imp-0 first nth last #'operator_<))
	
	(defmethod-overload nth_element ((first randomaccess_iterator)
									 (nth randomaccess_iterator) (last randomaccess_iterator) comp)
	  (__nth_element-imp-0 first nth last (functor_function (clone comp)))))
	  

  ;;PTN; nth_element : 1 -   vp
  (labels ((__nth_element-imp-1 (idx1 idx2 idx3 buffer less-bf)
			 (declare (type fixnum idx1 idx2 idx3))
			 (declare (type cl:vector buffer))
			 (declare (type cl:function less-bf))
			 (for (nil (< +SORT-MAX+ (- idx3 idx1)) nil)
			   (let* ((n   (ash (- idx3 idx1) -1))
					  (val nil)
					  (idx 0))
				 (declare (type fixnum n idx))
				 (_= val (__median (aref buffer idx1)
								   (aref buffer (the fixnum (+ idx1 n)))
								   (aref buffer (the fixnum (1- idx3))) less-bf))
				 (setf idx (__unguarded-partition-1 idx1 idx3 buffer val less-bf))
				 (if (>= (- idx2 idx) 0)
					 (setf idx1 idx)
					 (setf idx3 idx))))
			 (__insertion-sort-1 idx1 idx3 buffer less-bf)))

	(defmethod-overload nth_element ((first vector-pointer) (nth vector-pointer) (last vector-pointer))
	  ;;(format t "specialized nth_element for vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first nth)
	  (__pointer-check-iterator-range nth   last)
	  (__nth_element-imp-1 (opr::vec-ptr-index  first)
						   (opr::vec-ptr-index  nth)
						   (opr::vec-ptr-index  last)
						   (opr::vec-ptr-buffer first) #'operator_<))
	
	(defmethod-overload nth_element ((first vector-pointer) (nth vector-pointer) (last vector-pointer) comp)
	  ;;(format t "specialized nth_element for vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first nth)
	  (__pointer-check-iterator-range nth   last)
	  (__nth_element-imp-1 (opr::vec-ptr-index  first)
						   (opr::vec-ptr-index  nth)
						   (opr::vec-ptr-index  last)
						   (opr::vec-ptr-buffer first) (functor_function (clone comp))))))
	  




;; 25.3.3, binary search:


; first     : forward_iterator
; last      : forward_iterator
; comp      : binary_function ( default : #'operator_< )
; returns   : copy of first ( point to result ).
(locally (declare (optimize speed))

  ;;PTN; lower_bound : 0 -   f 
  (defmethod-overload lower_bound ((first forward_iterator) (last forward_iterator) val)
	(__lower_bound-imp-0 first last val #'operator_<))

  (defmethod-overload lower_bound ((first forward_iterator) (last forward_iterator) val comp)
	(__lower_bound-imp-0 first last val (functor_function (clone comp))))

  ;;PTN; lower_bound : 1 -  cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload lower_bound ((first cons_const_iterator) (last cons_const_iterator) val)
	;;(format t "specialized lower_bound for cons_const_iterator is invoked.~%")
	(__algo-make-cns-iterator first
							   (__lower_bound-imp-1 (__cons-itr-cons first)
													(__cons-itr-cons  last) val #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload lower_bound ((first cons_const_iterator) (last cons_const_iterator) val comp)
	;;(format t "specialized lower_bound for cons_const_iterator is invoked.~%")
	(__algo-make-cns-iterator first
							   (__lower_bound-imp-1 (__cons-itr-cons first)
													(__cons-itr-cons  last) val (functor_function (clone comp)))))

  ;;PTN; lower_bound : 2 -  cvp
  (defmethod-overload lower_bound ((first const-vector-pointer) (last const-vector-pointer) val)
	;;(format t "specialized lower_bound for const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(__algo-make-vct-iterator first
							   (__lower_bound-imp-2 (opr::vec-ptr-index  first)
													(opr::vec-ptr-index  last)
													(opr::vec-ptr-buffer first) val #'operator_<)))

  (defmethod-overload lower_bound ((first const-vector-pointer) (last const-vector-pointer) val comp)
	;;(format t "specialized lower_bound for const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(__algo-make-vct-iterator first
							   (__lower_bound-imp-2 (opr::vec-ptr-index  first)
													(opr::vec-ptr-index  last)
													(opr::vec-ptr-buffer first)
													val (functor_function (clone comp))))))




; first     : forward_iterator
; last      : forward_iterator
; comp      : binary_function ( default : #'operator_< )
; returns   : copy of first ( point to result ).
(locally (declare (optimize speed))

  ;;PTN; upper_bound : 0 -   f 
  (defmethod-overload upper_bound ((first forward_iterator) (last forward_iterator) val)
	(__upper_bound-imp-0 first last val #'operator_<))

  (defmethod-overload upper_bound ((first forward_iterator) (last forward_iterator) val comp)
	(__upper_bound-imp-0 first last val (functor_function (clone comp))))

  ;;PTN; upper_bound : 1 -  cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload upper_bound ((first cons_const_iterator) (last cons_const_iterator) val)
	;;(format t "specialized upper_bound for cons_const_iterator is invoked.~%")
	(__algo-make-cns-iterator first
							   (__upper_bound-imp-1 (__cons-itr-cons first)
													(__cons-itr-cons  last) val #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload upper_bound ((first cons_const_iterator) (last cons_const_iterator) val comp)
	;;(format t "specialized upper_bound for cons_const_iterator is invoked.~%")
	(__algo-make-cns-iterator first
							   (__upper_bound-imp-1 (__cons-itr-cons first)
													(__cons-itr-cons  last)
													val (functor_function (clone comp)))))

  ;;PTN; upper_bound : 2 -  cvp
  (defmethod-overload upper_bound ((first const-vector-pointer) (last const-vector-pointer) val)
	;;(format t "specialized upper_bound for const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(__algo-make-vct-iterator first
							   (__upper_bound-imp-2 (opr::vec-ptr-index  first)
													(opr::vec-ptr-index  last)
													(opr::vec-ptr-buffer first) val #'operator_<)))

  (defmethod-overload upper_bound ((first const-vector-pointer) (last const-vector-pointer) val comp)
	;;(format t "specialized upper_bound for const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(__algo-make-vct-iterator first
							   (__upper_bound-imp-2 (opr::vec-ptr-index  first)
													(opr::vec-ptr-index  last)
													(opr::vec-ptr-buffer first)
													val (functor_function (clone comp))))))



; first     : forward_iterator
; last      : forward_iterator
; comp      : binary_function ( default : #'operator_< )
; returns   : pair of iterators ( point to result ).
(locally (declare (optimize speed))

  ;;PTN; equal_range : 0 -   f 
  (defmethod-overload equal_range ((first forward_iterator) (last forward_iterator) val)
	(make_pair (__lower_bound-imp-0 first last val #'operator_<)
			   (__upper_bound-imp-0 first last val #'operator_<)))

  (defmethod-overload equal_range ((first forward_iterator) (last forward_iterator) val comp)
	(let ((comp (functor_function (clone comp))))
	  (make_pair (__lower_bound-imp-0 first last val comp)
				 (__upper_bound-imp-0 first last val comp))))


  ;;PTN; equal_range : 1 -  cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload equal_range ((first cons_const_iterator) (last cons_const_iterator) val)
	;;(format t "specialized equal_range for cons_const_iterator is invoked.~%")
	(let ((cons1 (__cons-itr-cons first))
		  (cons2 (__cons-itr-cons  last)))
	  (make_pair (__algo-make-cns-iterator first (__lower_bound-imp-1 cons1 cons2 val #'operator_<))
				 (__algo-make-cns-iterator first (__upper_bound-imp-1 cons1 cons2 val #'operator_<)))))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload equal_range ((first cons_const_iterator) (last cons_const_iterator) val comp)
	;;(format t "specialized equal_range for cons_const_iterator is invoked.~%")
	(let ((cons1 (__cons-itr-cons first))
		  (cons2 (__cons-itr-cons  last))
		  (comp  (functor_function (clone comp))))
	  (make_pair (__algo-make-cns-iterator first (__lower_bound-imp-1 cons1 cons2 val comp))
				 (__algo-make-cns-iterator first (__upper_bound-imp-1 cons1 cons2 val comp)))))


  ;;PTN; equal_range : 2 -  cvp
  (defmethod-overload equal_range ((first const-vector-pointer) (last const-vector-pointer) val)
	;;(format t "specialized equal_range for const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1   (opr::vec-ptr-index  first))
		  (idx2   (opr::vec-ptr-index  last))
		  (buffer (opr::vec-ptr-buffer first)))
	  (make_pair (__algo-make-vct-iterator first (__lower_bound-imp-2 idx1 idx2 buffer val #'operator_<))
				 (__algo-make-vct-iterator first (__upper_bound-imp-2 idx1 idx2 buffer val #'operator_<)))))

  (defmethod-overload equal_range ((first const-vector-pointer) (last const-vector-pointer) val comp)
	;;(format t "specialized equal_range for const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1   (opr::vec-ptr-index  first))
		  (idx2   (opr::vec-ptr-index  last))
		  (buffer (opr::vec-ptr-buffer first))
		  (comp   (functor_function (clone comp))))
	  (make_pair (__algo-make-vct-iterator first (__lower_bound-imp-2 idx1 idx2 buffer val comp))
				 (__algo-make-vct-iterator first (__upper_bound-imp-2 idx1 idx2 buffer val comp))))))



; first     : forward_iterator
; last      : forward_iterator
; comp      : binary_function ( default : #'operator_< )
; returns   : boolean value.
(locally (declare (optimize speed))

  ;;PTN; binary_search : 0 -   f 
  (defmethod-overload binary_search ((first forward_iterator) (last forward_iterator) val)
	(__binary_search-imp-0 first last val #'operator_<))

  (defmethod-overload binary_search ((first forward_iterator) (last forward_iterator) val comp)
	(__binary_search-imp-0 first last val (functor_function (clone comp))))


  ;;PTN; binary_search : 1 -  cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload binary_search ((first cons_const_iterator) (last cons_const_iterator) val)
	;;(format t "specialized binary_search for cons_const_iterator is invoked.~%")
	(__binary_search-imp-1 (__cons-itr-cons first)
						   (__cons-itr-cons  last) val #'operator_<))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload binary_search ((first cons_const_iterator) (last cons_const_iterator) val comp)
	;;(format t "specialized binary_search for cons_const_iterator is invoked.~%")
	(__binary_search-imp-1 (__cons-itr-cons first)
						   (__cons-itr-cons  last) val (functor_function (clone comp))))


  ;;PTN; binary_search : 2 -  cvp
  (defmethod-overload binary_search ((first const-vector-pointer) (last const-vector-pointer) val)
	;;(format t "specialized binary_search for const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(__binary_search-imp-2 (opr::vec-ptr-index  first)
						   (opr::vec-ptr-index   last)
						   (opr::vec-ptr-buffer first) val #'operator_<))

  (defmethod-overload binary_search ((first const-vector-pointer) (last const-vector-pointer) val comp)
	;;(format t "specialized binary_search for const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(__binary_search-imp-2 (opr::vec-ptr-index  first)
						   (opr::vec-ptr-index   last)
						   (opr::vec-ptr-buffer first) val (functor_function (clone comp)))))




;; 25.3.4, merge:

; first1    : input_iterator
; last1     : input_iterator
; first2    : input_iterator
; last2     : input_iterator
; result    : output_iterator
; comp      : binary_function ( default : #'operator_< )
; returns   : copy of reuslt ( point to end of sequence ).
(locally (declare (optimize speed))

  ;;PTN; merge : 00 -   i  x  i  x  o 
  (defmethod-overload merge ((first1 input_iterator) (last1 input_iterator)
							 (first2 input_iterator) (last2 input_iterator) (result output_iterator))
	(__merge-imp-00 first1 last1 first2 last2 result #'operator_<))

  (defmethod-overload merge ((first1 input_iterator) (last1 input_iterator)
							 (first2 input_iterator) (last2 input_iterator) (result output_iterator) comp)
	(__merge-imp-00 first1 last1 first2 last2 result (functor_function (clone comp))))


  ;;PTN; merge : 01 -   i  x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 input_iterator) (last1 input_iterator)
							 (first2 input_iterator) (last2 input_iterator) (result cons_iterator))
	;;(format t "specialized merge for input_iterator & input_iterator & cons_iterator is invoked.~%" ')
	(__algo-make-cns-iterator result
							   (__merge-imp-01 first1 last1 first2 last2
											   (__cons-itr-cons result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 input_iterator) (last1 input_iterator)
							 (first2 input_iterator) (last2 input_iterator) (result cons_iterator) comp)
	;;(format t "specialized merge for input_iterator & input_iterator & cons_iterator is invoked.~%" ')
	(__algo-make-cns-iterator result
							   (__merge-imp-01 first1 last1 first2 last2
											   (__cons-itr-cons result) (functor_function (clone comp)))))

  ;;PTN; merge : 02 -   i  x  i  x  vp
  (defmethod-overload merge ((first1 input_iterator) (last1 input_iterator)
							 (first2 input_iterator) (last2 input_iterator) (result vector-pointer))
	;;(format t "specialized merge for input_iterator & input_iterator & vector-pointer is invoked.~%" ')
	(__algo-make-vct-iterator result
							   (__merge-imp-02 first1 last1 first2 last2
											   (opr::vec-ptr-index  result)
											   (opr::vec-ptr-buffer result) #'operator_<)))

  (defmethod-overload merge ((first1 input_iterator) (last1 input_iterator)
							 (first2 input_iterator) (last2 input_iterator) (result vector-pointer) comp)
	;;(format t "specialized merge for input_iterator & input_iterator & vector-pointer is invoked.~%" ')
	(__algo-make-vct-iterator result
							   (__merge-imp-02 first1 last1 first2 last2
											   (opr::vec-ptr-index  result)
											   (opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; merge : 03 -   i  x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 input_iterator) (last1 input_iterator)
							 (first2 cons_const_iterator) (last2 cons_const_iterator) (result output_iterator))
	;;(format t "specialized merge for input_iterator & cons_const_iterator & output_iterator is invoked.~%" ')
	(__merge-imp-03 first1 last1
					(__cons-itr-cons first2)
					(__cons-itr-cons  last2) (clone result) #'operator_<))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 input_iterator) (last1 input_iterator)
							 (first2 cons_const_iterator) (last2 cons_const_iterator) (result output_iterator) comp)
	;;(format t "specialized merge for input_iterator & cons_const_iterator & output_iterator is invoked.~%" ')
	(__merge-imp-03 first1 last1
					(__cons-itr-cons first2)
					(__cons-itr-cons  last2) (clone result) (functor_function (clone comp))))

  ;;PTN; merge : 04 -   i  x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 input_iterator) (last1 input_iterator)
							 (first2 cons_const_iterator) (last2 cons_const_iterator) (result cons_iterator))
	;;(format t "specialized merge for input_iterator & cons_const_iterator & cons_iterator is invoked.~%" ')
	(__algo-make-cns-iterator result
							   (__merge-imp-04 first1 last1
											   (__cons-itr-cons first2)
											   (__cons-itr-cons  last2)
											   (__cons-itr-cons result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 input_iterator) (last1 input_iterator)
							 (first2 cons_const_iterator) (last2 cons_const_iterator) (result cons_iterator) comp)
	;;(format t "specialized merge for input_iterator & cons_const_iterator & cons_iterator is invoked.~%" ')
	(__algo-make-cns-iterator result
							   (__merge-imp-04 first1 last1
											   (__cons-itr-cons first2)
											   (__cons-itr-cons  last2)
											   (__cons-itr-cons result) (functor_function (clone comp)))))

  ;;PTN; merge : 05 -   i  x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 input_iterator) (last1 input_iterator)
							 (first2 cons_const_iterator) (last2 cons_const_iterator) (result vector-pointer))
	;;(format t "specialized merge for input_iterator & cons_const_iterator & vector-pointer is invoked.~%" ')
	(__algo-make-vct-iterator result
							   (__merge-imp-05 first1 last1
											   (__cons-itr-cons     first2)
											   (__cons-itr-cons      last2)
											   (opr::vec-ptr-index  result)
											   (opr::vec-ptr-buffer result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 input_iterator) (last1 input_iterator)
							 (first2 cons_const_iterator) (last2 cons_const_iterator) (result vector-pointer) comp)
	;;(format t "specialized merge for input_iterator & cons_const_iterator & vector-pointer is invoked.~%" ')
	(__algo-make-vct-iterator result
							   (__merge-imp-05 first1 last1
											   (__cons-itr-cons     first2)
											   (__cons-itr-cons      last2)
											   (opr::vec-ptr-index  result)
											   (opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; merge : 06 -   i  x cvp x  o 
  (defmethod-overload merge ((first1 input_iterator) (last1 input_iterator)
							 (first2 const-vector-pointer) (last2 const-vector-pointer) (result output_iterator))
	;;(format t "specialized merge for input_iterator & const-vector-pointer & output_iterator is invoked.~%" ')
	(__pointer-check-iterator-range first2 last2)
	(__merge-imp-06 first1 last1
					(opr::vec-ptr-index  first2)
					(opr::vec-ptr-index  last2)
					(opr::vec-ptr-buffer first2) (clone result) #'operator_<))

  (defmethod-overload merge ((first1 input_iterator) (last1 input_iterator)
							 (first2 const-vector-pointer) (last2 const-vector-pointer) (result output_iterator) comp)
	;;(format t "specialized merge for input_iterator & const-vector-pointer & output_iterator is invoked.~%" ')
	(__pointer-check-iterator-range first2 last2)
	(__merge-imp-06 first1 last1
					(opr::vec-ptr-index  first2)
					(opr::vec-ptr-index  last2)
					(opr::vec-ptr-buffer first2) (clone result) (functor_function (clone comp))))

  ;;PTN; merge : 07 -   i  x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 input_iterator) (last1 input_iterator)
							 (first2 const-vector-pointer) (last2 const-vector-pointer) (result cons_iterator))
	;;(format t "specialized merge for input_iterator & const-vector-pointer & cons_iterator is invoked.~%" ')
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-cns-iterator result
							   (__merge-imp-07 first1 last1
											   (opr::vec-ptr-index  first2)
											   (opr::vec-ptr-index  last2)
											   (opr::vec-ptr-buffer first2)
											   (__cons-itr-cons     result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 input_iterator) (last1 input_iterator)
							 (first2 const-vector-pointer) (last2 const-vector-pointer) (result cons_iterator) comp)
	;;(format t "specialized merge for input_iterator & const-vector-pointer & cons_iterator is invoked.~%" ')
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-cns-iterator result
							   (__merge-imp-07 first1 last1
											   (opr::vec-ptr-index  first2)
											   (opr::vec-ptr-index  last2)
											   (opr::vec-ptr-buffer first2)
											   (__cons-itr-cons     result) (functor_function (clone comp)))))

  ;;PTN; merge : 08 -   i  x cvp x  vp
  (defmethod-overload merge ((first1 input_iterator) (last1 input_iterator)
							 (first2 const-vector-pointer) (last2 const-vector-pointer) (result vector-pointer))
	;;(format t "specialized merge for input_iterator & const-vector-pointer & vector-pointer is invoked.~%" ')
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-vct-iterator result
							   (__merge-imp-08 first1 last1
											   (opr::vec-ptr-index  first2)
											   (opr::vec-ptr-index  last2)
											   (opr::vec-ptr-buffer first2)
											   (opr::vec-ptr-index  result)
											   (opr::vec-ptr-buffer result) #'operator_<)))

  (defmethod-overload merge ((first1 input_iterator) (last1 input_iterator)
							 (first2 const-vector-pointer) (last2 const-vector-pointer) (result vector-pointer) comp)
	;;(format t "specialized merge for input_iterator & const-vector-pointer & vector-pointer is invoked.~%" ')
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-vct-iterator result
							   (__merge-imp-08 first1 last1
											   (opr::vec-ptr-index  first2)
											   (opr::vec-ptr-index  last2)
											   (opr::vec-ptr-buffer first2)
											   (opr::vec-ptr-index  result)
											   (opr::vec-ptr-buffer result) (functor_function (clone comp)))))
  
  ;;PTN; merge : 09 -  cci x  i  x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 cons_const_iterator) (last1 cons_const_iterator)
							 (first2 input_iterator) (last2 input_iterator) (result output_iterator))
	;;(format t "specialized merge for const-vector-pointer & input_iterator & output_iterator is invoked.~%" ')
	(__merge-imp-09 (__cons-itr-cons first1)
					(__cons-itr-cons  last1) first2 last2 (clone result) #'operator_<))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 cons_const_iterator) (last1 cons_const_iterator)
							 (first2 input_iterator) (last2 input_iterator) (result output_iterator) comp)
	;;(format t "specialized merge for const-vector-pointer & input_iterator & output_iterator is invoked.~%" ')
	(__merge-imp-09 (__cons-itr-cons first1)
					(__cons-itr-cons  last1) first2 last2 (clone result) (functor_function (clone comp))))

  ;;PTN; merge : 10 -  cci x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 cons_const_iterator) (last1 cons_const_iterator)
							 (first2 input_iterator) (last2 input_iterator) (result cons_iterator))
	;;(format t "specialized merge for const-vector-pointer & input_iterator & cons_iterator is invoked.~%" ')
	(__algo-make-cns-iterator result
							   (__merge-imp-10 (__cons-itr-cons first1)
											   (__cons-itr-cons  last1)
											   first2 last2
											   (__cons-itr-cons     result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 cons_const_iterator) (last1 cons_const_iterator)
							 (first2 input_iterator) (last2 input_iterator) (result cons_iterator) comp)
	;;(format t "specialized merge for const-vector-pointer & input_iterator & cons_iterator is invoked.~%" ')
	(__algo-make-cns-iterator result
							   (__merge-imp-10 (__cons-itr-cons first1)
											   (__cons-itr-cons  last1)
											   first2 last2
											   (__cons-itr-cons result) (functor_function (clone comp)))))

  ;;PTN; merge : 11 -  cci x  i  x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 cons_const_iterator) (last1 cons_const_iterator)
							 (first2 input_iterator) (last2 input_iterator) (result vector-pointer))
	;;(format t "specialized merge for const-vector-pointer & input_iterator & vector-pointer is invoked.~%" ')
	(__algo-make-vct-iterator result
							   (__merge-imp-11 (__cons-itr-cons first1)
											   (__cons-itr-cons  last1)
											   first2 last2
											   (opr::vec-ptr-index  result)
											   (opr::vec-ptr-buffer result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 cons_const_iterator) (last1 cons_const_iterator)
							 (first2 input_iterator) (last2 input_iterator) (result vector-pointer) comp)
	;;(format t "specialized merge for const-vector-pointer & input_iterator & vector-pointer is invoked.~%" ')
	(__algo-make-vct-iterator result
							   (__merge-imp-11 (__cons-itr-cons first1)
											   (__cons-itr-cons  last1)
											   first2 last2
											   (opr::vec-ptr-index  result)
											   (opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; merge : 12 -  cci x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 cons_const_iterator) (last1 cons_const_iterator)
							 (first2 cons_const_iterator) (last2 cons_const_iterator) (result output_iterator))
	;;(format t "specialized merge for const-vector-pointer & cons_const_iterator & output_iterator is invoked.~%" ')
	(__merge-imp-12 (__cons-itr-cons first1)
					(__cons-itr-cons  last1)
					(__cons-itr-cons first2)
					(__cons-itr-cons  last2) (clone result) #'operator_<))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 cons_const_iterator) (last1 cons_const_iterator)
							 (first2 cons_const_iterator) (last2 cons_const_iterator) (result output_iterator) comp)
	;;(format t "specialized merge for const-vector-pointer & cons_const_iterator & output_iterator is invoked.~%" ')
	(__merge-imp-12 (__cons-itr-cons first1)
					(__cons-itr-cons  last1)
					(__cons-itr-cons first2)
					(__cons-itr-cons  last2) (clone result) (functor_function (clone comp))))

  ;;PTN; merge : 13 -  cci x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 cons_const_iterator) (last1 cons_const_iterator)
							 (first2 cons_const_iterator) (last2 cons_const_iterator) (result cons_iterator))
	;;(format t "specialized merge for const-vector-pointer & cons_const_iterator & cons_iterator is invoked.~%" ')
	(__algo-make-cns-iterator result
							   (__merge-imp-13 (__cons-itr-cons first1)
											   (__cons-itr-cons  last1)
											   (__cons-itr-cons first2)
											   (__cons-itr-cons  last2)
											   (__cons-itr-cons result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 cons_const_iterator) (last1 cons_const_iterator)
							 (first2 cons_const_iterator) (last2 cons_const_iterator) (result cons_iterator) comp)
	;;(format t "specialized merge for const-vector-pointer & cons_const_iterator & cons_iterator is invoked.~%" ')
	(__algo-make-cns-iterator result
							   (__merge-imp-13 (__cons-itr-cons first1)
											   (__cons-itr-cons  last1)
											   (__cons-itr-cons first2)
											   (__cons-itr-cons  last2)
											   (__cons-itr-cons result) (functor_function (clone comp)))))

  ;;PTN; merge : 14 -  cci x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 cons_const_iterator) (last1 cons_const_iterator)
							 (first2 cons_const_iterator) (last2 cons_const_iterator) (result vector-pointer))
	;;(format t "specialized merge for const-vector-pointer & cons_const_iterator & vector-pointer is invoked.~%" ')
	(__algo-make-vct-iterator result
							   (__merge-imp-14 (__cons-itr-cons     first1)
											   (__cons-itr-cons      last1)
											   (__cons-itr-cons     first2)
											   (__cons-itr-cons      last2)
											   (opr::vec-ptr-index  result)
											   (opr::vec-ptr-buffer result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 cons_const_iterator) (last1 cons_const_iterator)
							 (first2 cons_const_iterator) (last2 cons_const_iterator) (result vector-pointer) comp)
	;;(format t "specialized merge for const-vector-pointer & cons_const_iterator & vector-pointer is invoked.~%" ')
	(__algo-make-vct-iterator result
							   (__merge-imp-14 (__cons-itr-cons     first1)
											   (__cons-itr-cons      last1)
											   (__cons-itr-cons     first2)
											   (__cons-itr-cons      last2)
											   (opr::vec-ptr-index  result)
											   (opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; merge : 15 -  cci x cvp x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 cons_const_iterator) (last1 cons_const_iterator)
							 (first2 const-vector-pointer) (last2 const-vector-pointer) (result output_iterator))
	;;(format t "specialized merge for const-vector-pointer & const-vector-pointer & output_iterator is invoked.~%" ')
	(__pointer-check-iterator-range first2 last2)
	(__merge-imp-15 (__cons-itr-cons     first1)
					(__cons-itr-cons      last1)
					(opr::vec-ptr-index  first2)
					(opr::vec-ptr-index   last2)
					(opr::vec-ptr-buffer first2) (clone result) #'operator_<))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 cons_const_iterator) (last1 cons_const_iterator)
							 (first2 const-vector-pointer) (last2 const-vector-pointer) (result output_iterator) comp)
	;;(format t "specialized merge for const-vector-pointer & const-vector-pointer & output_iterator is invoked.~%" ')
	(__pointer-check-iterator-range first2 last2)
	(__merge-imp-15 (__cons-itr-cons     first1)
					(__cons-itr-cons      last1)
					(opr::vec-ptr-index  first2)
					(opr::vec-ptr-index   last2)
					(opr::vec-ptr-buffer first2) (clone result) (functor_function (clone comp))))

  ;;PTN; merge : 16 -  cci x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 cons_const_iterator) (last1 cons_const_iterator)
							 (first2 const-vector-pointer) (last2 const-vector-pointer) (result cons_iterator))
	;;(format t "specialized merge for const-vector-pointer & const-vector-pointer & cons_iterator is invoked.~%" ')
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-cns-iterator result
							   (__merge-imp-16 (__cons-itr-cons     first1)
											   (__cons-itr-cons      last1)
											   (opr::vec-ptr-index  first2)
											   (opr::vec-ptr-index   last2)
											   (opr::vec-ptr-buffer first2)
											   (__cons-itr-cons     result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 cons_const_iterator) (last1 cons_const_iterator)
							 (first2 const-vector-pointer) (last2 const-vector-pointer) (result cons_iterator) comp)
	;;(format t "specialized merge for const-vector-pointer & const-vector-pointer & cons_iterator is invoked.~%" ')
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-cns-iterator result
							   (__merge-imp-16 (__cons-itr-cons     first1)
											   (__cons-itr-cons      last1)
											   (opr::vec-ptr-index  first2)
											   (opr::vec-ptr-index   last2)
											   (opr::vec-ptr-buffer first2)
											   (__cons-itr-cons     result) (functor_function (clone comp)))))

  ;;PTN; merge : 17 -  cci x cvp x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 cons_const_iterator) (last1 cons_const_iterator)
							 (first2 const-vector-pointer) (last2 const-vector-pointer) (result vector-pointer))
	;;(format t "specialized merge for const-vector-pointer & const-vector-pointer & vector-pointer is invoked.~%" ')
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-vct-iterator result
							   (__merge-imp-17 (__cons-itr-cons     first1)
											   (__cons-itr-cons      last1)
											   (opr::vec-ptr-index  first2)
											   (opr::vec-ptr-index   last2)
											   (opr::vec-ptr-buffer first2)
											   (opr::vec-ptr-index  result)
											   (opr::vec-ptr-buffer result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 cons_const_iterator) (last1 cons_const_iterator)
							 (first2 const-vector-pointer) (last2 const-vector-pointer) (result vector-pointer) comp)
	;;(format t "specialized merge for const-vector-pointer & const-vector-pointer & vector-pointer is invoked.~%" ')
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-vct-iterator result
							   (__merge-imp-17 (__cons-itr-cons     first1)
											   (__cons-itr-cons      last1)
											   (opr::vec-ptr-index  first2)
											   (opr::vec-ptr-index   last2)
											   (opr::vec-ptr-buffer first2)
											   (opr::vec-ptr-index  result)
											   (opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; merge : 18 -  cvp x  i  x  o 
  (defmethod-overload merge ((first1 const-vector-pointer) (last1 const-vector-pointer)
							 (first2 input_iterator) (last2 input_iterator) (result output_iterator))
	;;(format t "specialized merge for const-vector-pointer & input_iterator & output_iterator is invoked.~%" ')
	(__pointer-check-iterator-range first1 last1)
	(__merge-imp-18 (opr::vec-ptr-index  first1)
					(opr::vec-ptr-index  last1)
					(opr::vec-ptr-buffer first1) first2 last2 (clone result) #'operator_<))

  (defmethod-overload merge ((first1 const-vector-pointer) (last1 const-vector-pointer)
							 (first2 input_iterator) (last2 input_iterator) (result output_iterator) comp)
	;;(format t "specialized merge for const-vector-pointer & input_iterator & output_iterator is invoked.~%" ')
	(__pointer-check-iterator-range first1 last1)
	(__merge-imp-18 (opr::vec-ptr-index  first1)
					(opr::vec-ptr-index  last1)
					(opr::vec-ptr-buffer first1) first2 last2 (clone result) (functor_function (clone comp))))

  ;;PTN; merge : 19 -  cvp x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 const-vector-pointer) (last1 const-vector-pointer)
							 (first2 input_iterator) (last2 input_iterator) (result cons_iterator))
	;;(format t "specialized merge for const-vector-pointer & input_iterator & cons_iterator is invoked.~%" ')
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-cns-iterator result
							   (__merge-imp-19 (opr::vec-ptr-index  first1)
											   (opr::vec-ptr-index  last1)
											   (opr::vec-ptr-buffer first1)
											   first2 last2
											   (__cons-itr-cons     result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 const-vector-pointer) (last1 const-vector-pointer)
							 (first2 input_iterator) (last2 input_iterator) (result cons_iterator) comp)
	;;(format t "specialized merge for const-vector-pointer & input_iterator & cons_iterator is invoked.~%" ')
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-cns-iterator result
							   (__merge-imp-19 (opr::vec-ptr-index  first1)
											   (opr::vec-ptr-index  last1)
											   (opr::vec-ptr-buffer first1)
											   first2 last2
											   (__cons-itr-cons     result) (functor_function (clone comp)))))

  ;;PTN; merge : 20 -  cvp x  i  x  vp
  (defmethod-overload merge ((first1 const-vector-pointer) (last1 const-vector-pointer)
							 (first2 input_iterator) (last2 input_iterator) (result vector-pointer))
	;;(format t "specialized merge for const-vector-pointer & input_iterator & vector-pointer is invoked.~%" ')
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-vct-iterator result
							   (__merge-imp-20 (opr::vec-ptr-index  first1)
											   (opr::vec-ptr-index  last1)
											   (opr::vec-ptr-buffer first1)
											   first2 last2
											   (opr::vec-ptr-index  result)
											   (opr::vec-ptr-buffer result) #'operator_<)))

  (defmethod-overload merge ((first1 const-vector-pointer) (last1 const-vector-pointer)
							 (first2 input_iterator) (last2 input_iterator) (result vector-pointer) comp)
	;;(format t "specialized merge for const-vector-pointer & input_iterator & vector-pointer is invoked.~%" ')
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-vct-iterator result
							   (__merge-imp-20 (opr::vec-ptr-index  first1)
											   (opr::vec-ptr-index  last1)
											   (opr::vec-ptr-buffer first1)
											   first2 last2
											   (opr::vec-ptr-index  result)
											   (opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; merge : 21 -  cvp x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 const-vector-pointer) (last1 const-vector-pointer)
							 (first2 cons_const_iterator) (last2 cons_const_iterator) (result output_iterator))
	;;(format t "specialized merge for const-vector-pointer & cons_const_iterator & output_iterator is invoked.~%" ')
	(__pointer-check-iterator-range first1 last1)
	(__merge-imp-21 (opr::vec-ptr-index  first1)
					(opr::vec-ptr-index   last1)
					(opr::vec-ptr-buffer first1)
					(__cons-itr-cons     first2)
					(__cons-itr-cons      last2) (clone result) #'operator_<))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 const-vector-pointer) (last1 const-vector-pointer)
							 (first2 cons_const_iterator) (last2 cons_const_iterator) (result output_iterator) comp)
	;;(format t "specialized merge for const-vector-pointer & cons_const_iterator & output_iterator is invoked.~%" ')
	(__pointer-check-iterator-range first1 last1)
	(__merge-imp-21 (opr::vec-ptr-index  first1)
					(opr::vec-ptr-index   last1)
					(opr::vec-ptr-buffer first1)
					(__cons-itr-cons     first2)
					(__cons-itr-cons      last2) (clone result) (functor_function (clone comp))))

  ;;PTN; merge : 22 -  cvp x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 const-vector-pointer) (last1 const-vector-pointer)
							 (first2 cons_const_iterator) (last2 cons_const_iterator) (result cons_iterator))
	;;(format t "specialized merge for const-vector-pointer & cons_const_iterator & cons_iterator is invoked.~%" ')
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-cns-iterator result
							   (__merge-imp-22 (opr::vec-ptr-index  first1)
											   (opr::vec-ptr-index  last1)
											   (opr::vec-ptr-buffer first1)
											   (__cons-itr-cons     first2)
											   (__cons-itr-cons     last2)
											   (__cons-itr-cons     result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 const-vector-pointer) (last1 const-vector-pointer)
							 (first2 cons_const_iterator) (last2 cons_const_iterator) (result cons_iterator) comp)
	;;(format t "specialized merge for const-vector-pointer & cons_const_iterator & cons_iterator is invoked.~%" ')
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-cns-iterator result
							   (__merge-imp-22 (opr::vec-ptr-index  first1)
											   (opr::vec-ptr-index  last1)
											   (opr::vec-ptr-buffer first1)
											   (__cons-itr-cons     first2)
											   (__cons-itr-cons     last2)
											   (__cons-itr-cons     result) (functor_function (clone comp)))))

  ;;PTN; merge : 23 -  cvp x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 const-vector-pointer) (last1 const-vector-pointer)
							 (first2 cons_const_iterator) (last2 cons_const_iterator) (result vector-pointer))
	;;(format t "specialized merge for const-vector-pointer & cons_const_iterator & vector-pointer is invoked.~%" ')
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-vct-iterator result
							   (__merge-imp-23 (opr::vec-ptr-index  first1)
											   (opr::vec-ptr-index   last1)
											   (opr::vec-ptr-buffer first1)
											   (__cons-itr-cons     first2)
											   (__cons-itr-cons      last2)
											   (opr::vec-ptr-index  result)
											   (opr::vec-ptr-buffer result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 const-vector-pointer) (last1 const-vector-pointer)
							 (first2 cons_const_iterator) (last2 cons_const_iterator) (result vector-pointer) comp)
	;;(format t "specialized merge for const-vector-pointer & cons_const_iterator & vector-pointer is invoked.~%" ')
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-vct-iterator result
							   (__merge-imp-23 (opr::vec-ptr-index  first1)
											   (opr::vec-ptr-index   last1)
											   (opr::vec-ptr-buffer first1)
											   (__cons-itr-cons     first2)
											   (__cons-itr-cons      last2)
											   (opr::vec-ptr-index  result)
											   (opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; merge : 24 -  cvp x cvp x  o 
  (defmethod-overload merge ((first1 const-vector-pointer) (last1 const-vector-pointer)
							 (first2 const-vector-pointer) (last2 const-vector-pointer) (result output_iterator))
	;;(format t "specialized merge for const-vector-pointer & const-vector-pointer & output_iterator is invoked.~%" ')
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__merge-imp-24 (opr::vec-ptr-index  first1)
					(opr::vec-ptr-index  last1)
					(opr::vec-ptr-buffer first1)
					(opr::vec-ptr-index  first2)
					(opr::vec-ptr-index  last2)
					(opr::vec-ptr-buffer first2) (clone result) #'operator_<))

  (defmethod-overload merge ((first1 const-vector-pointer) (last1 const-vector-pointer)
							 (first2 const-vector-pointer) (last2 const-vector-pointer) (result output_iterator) comp)
	;;(format t "specialized merge for const-vector-pointer & const-vector-pointer & output_iterator is invoked.~%" ')
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__merge-imp-24 (opr::vec-ptr-index  first1)
					(opr::vec-ptr-index  last1)
					(opr::vec-ptr-buffer first1)
					(opr::vec-ptr-index  first2)
					(opr::vec-ptr-index  last2)
					(opr::vec-ptr-buffer first2) (clone result) (functor_function (clone comp))))

  ;;PTN; merge : 25 -  cvp x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 const-vector-pointer) (last1 const-vector-pointer)
							 (first2 const-vector-pointer) (last2 const-vector-pointer) (result cons_iterator))
	;;(format t "specialized merge for const-vector-pointer & const-vector-pointer & cons_iterator is invoked.~%" ')
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-cns-iterator result
							   (__merge-imp-25 (opr::vec-ptr-index  first1)
											   (opr::vec-ptr-index  last1)
											   (opr::vec-ptr-buffer first1)
											   (opr::vec-ptr-index  first2)
											   (opr::vec-ptr-index  last2)
											   (opr::vec-ptr-buffer first2)
											   (__cons-itr-cons     result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload merge ((first1 const-vector-pointer) (last1 const-vector-pointer)
							 (first2 const-vector-pointer) (last2 const-vector-pointer) (result cons_iterator) comp)
	;;(format t "specialized merge for const-vector-pointer & const-vector-pointer & cons_iterator is invoked.~%" ')
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-cns-iterator result
							   (__merge-imp-25 (opr::vec-ptr-index  first1)
											   (opr::vec-ptr-index  last1)
											   (opr::vec-ptr-buffer first1)
											   (opr::vec-ptr-index  first2)
											   (opr::vec-ptr-index  last2)
											   (opr::vec-ptr-buffer first2)
											   (__cons-itr-cons     result) (functor_function (clone comp)))))

  ;;PTN; merge : 26 -  cvp x cvp x  vp
  (defmethod-overload merge ((first1 const-vector-pointer) (last1 const-vector-pointer)
							 (first2 const-vector-pointer) (last2 const-vector-pointer) (result vector-pointer))
	;;(format t "specialized merge for const-vector-pointer & const-vector-pointer & vector-pointer is invoked.~%" ')
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-vct-iterator result
							   (__merge-imp-26 (opr::vec-ptr-index  first1)
											   (opr::vec-ptr-index  last1)
											   (opr::vec-ptr-buffer first1)
											   (opr::vec-ptr-index  first2)
											   (opr::vec-ptr-index  last2)
											   (opr::vec-ptr-buffer first2)
											   (opr::vec-ptr-index  result)
											   (opr::vec-ptr-buffer result) #'operator_<)))

  (defmethod-overload merge ((first1 const-vector-pointer) (last1 const-vector-pointer)
							 (first2 const-vector-pointer) (last2 const-vector-pointer) (result vector-pointer) comp)
	;;(format t "specialized merge for const-vector-pointer & const-vector-pointer & vector-pointer is invoked.~%" ')
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-vct-iterator result
							   (__merge-imp-26 (opr::vec-ptr-index  first1)
											   (opr::vec-ptr-index  last1)
											   (opr::vec-ptr-buffer first1)
											   (opr::vec-ptr-index  first2)
											   (opr::vec-ptr-index  last2)
											   (opr::vec-ptr-buffer first2)
											   (opr::vec-ptr-index  result)
											   (opr::vec-ptr-buffer result) (functor_function (clone comp))))))




; first     : bidirectional_iterator
; middle    : bidirectional_iterator
; last      : bidirectional_iterator
; comp      : binary_function ( default : #'operator_< )
; returns   : nil.
(locally (declare (optimize speed))

  ;;PTN; inplace_merge : 0 -   b 
  (labels ((imp (first middle last comp)
			 (if (_== first last)
				 nil
				 (let ((d1 (the fixnum (distance first middle)))
					   (d2 (the fixnum (distance middle last)))
					   (tmpitr (new-tmpitr)))
				   (declare (type fixnum d1 d2))
				   (tmpitr-set-buf-len tmpitr (min d1 d2))
				   (__buffered-merge-0 first middle last d1 d2 tmpitr comp)
				   nil))))

	(defmethod-overload inplace_merge ((first  bidirectional_iterator)
									   (middle bidirectional_iterator) (last bidirectional_iterator))
	  (imp first middle last #'operator_<))

	(defmethod-overload inplace_merge ((first  bidirectional_iterator)
									   (middle bidirectional_iterator) (last bidirectional_iterator) comp)
	  (imp first middle last (functor_function (clone comp)))))


  ;;PTN; inplace_merge : 1 -   vp
  (defmethod-overload inplace_merge ((first vector-pointer) (middle vector-pointer) (last vector-pointer))
	;;(format t "specialized inplace_merge for vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first middle)
	(__pointer-check-iterator-range middle  last)
	(let ((idx1 (opr::vec-ptr-index first))
		  (idx2 (opr::vec-ptr-index middle))
		  (idx3 (opr::vec-ptr-index last)))
	  (declare (type fixnum idx1 idx2 idx3))
	  (if (= idx1 idx3)
		  nil
		  (let ((d1 (- idx2 idx1))
				(d2 (- idx3 idx2))
				(tmpitr (new-tmpitr)))
			(declare (type fixnum d1 d2))
			(tmpitr-set-buf-len tmpitr (min d1 d2))
			(__buffered-merge-1 idx1 idx2 idx3
								(opr::vec-ptr-buffer first) d1 d2 tmpitr #'operator_<)
			nil))))

  (defmethod-overload inplace_merge ((first vector-pointer) (middle vector-pointer) (last vector-pointer) comp)
	;;(format t "specialized inplace_merge for vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first middle)
	(__pointer-check-iterator-range middle  last)
	(let ((idx1 (opr::vec-ptr-index first))
		  (idx2 (opr::vec-ptr-index middle))
		  (idx3 (opr::vec-ptr-index last)))
	  (declare (type fixnum idx1 idx2 idx3))
	  (if (= idx1 idx3)
		  nil
		  (let ((d1 (- idx2 idx1))
				(d2 (- idx3 idx2))
				(tmpitr (new-tmpitr)))
			(declare (type fixnum d1 d2))
			(tmpitr-set-buf-len tmpitr (min d1 d2))
			(__buffered-merge-1 idx1 idx2 idx3
								(opr::vec-ptr-buffer first)
								d1 d2 tmpitr (functor_function (clone comp)))
			nil)))))




;; 25.3.5, set operations:

; first1    : input_iterator
; last1     : input_iterator
; first2    : input_iterator
; last2     : input_iterator
; comp      : binary_function ( default : #'operator_< )
; returns   : boolean value.
(locally (declare (optimize speed))

  ;;PTN; includes : 0 -   i  x  i 
  (labels ((__includes-imp-0 (first1 last1 first2 last2 comp)
			 (declare (type cl:function comp))
			 (if (_== first2 last2)
				 t
				 (if (_== first1 last1)
					 nil
					 (with-operators
						 (for (((itr1 @~first1) (itr2 @~first2)) (_/= itr2 last2) ++itr1 :returns t)
						   (if (_== itr1 last1)
							   (return-from __includes-imp-0 nil)
							   (let ((val-a *itr1)
									 (val-b *itr2))
								 (if (funcall comp val-b val-a)
									 (return-from __includes-imp-0 nil)
									 (unless (funcall comp val-a val-b)
									   ++itr2))))))))))

	(defmethod-overload includes ((first1 input_iterator) (last1 input_iterator)
								  (first2 input_iterator) (last2 input_iterator))
	  (__includes-imp-0 first1 last1 first2 last2 #'operator_<))

	(defmethod-overload includes ((first1 input_iterator) (last1 input_iterator)
								  (first2 input_iterator) (last2 input_iterator) comp)
	  (__includes-imp-0 first1 last1 first2 last2 (functor_function (clone comp)))))


  ;;PTN; includes : 1 -  cci x  i
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__includes-imp-1 (cons1 end1 first2 last2 less-bf)
			 (declare (type cl:list cons1 end1))
			 (declare (type cl:function less-bf))
			 (if (_== first2 last2)
				 t
				 (if (eq cons1 end1)
					 nil
					 (with-operators
						 (for (((itr2 @~first2)) (_/= itr2 last2) (setf cons1 (cdr cons1)) :returns t)
						   (if (eq cons1 end1)
							   (return-from __includes-imp-1 nil)
							   (let ((val1 (car cons1))
									 (val2 *itr2))
								 (if (funcall less-bf val2 val1)
									 (return-from __includes-imp-1 nil)
									 (unless (funcall less-bf val1 val2)
									   ++itr2))))))))))

	(defmethod-overload includes ((first1 cons_const_iterator)
								  (last1  cons_const_iterator)
								  (first2 input_iterator) (last2 input_iterator))
	  (format t "specialized includes for cons_const_iterator & input_iterator is invoked.~%" )
	  (__includes-imp-1 (__cons-itr-cons first1)
						(__cons-itr-cons  last1) first2 last2 #'operator_<))

	(defmethod-overload includes ((first1 cons_const_iterator)
								  (last1  cons_const_iterator)
								  (first2 input_iterator) (last2 input_iterator) comp)
	  (format t "specialized includes for cons_const_iterator & input_iterator is invoked.~%" )
	  (__includes-imp-1 (__cons-itr-cons first1)
						(__cons-itr-cons  last1) first2 last2 (functor_function (clone comp)))))


  ;;PTN; includes : 2 -  cvp x  i 
  (labels ((__includes-imp-2 (idx1 last1 buffer1 first2 last2 less-bf)
			 (declare (type fixnum idx1 last1))
			 (declare (type cl:vector buffer1))
			 (declare (type cl:function less-bf))
			 (if (_== first2 last2)
				 t
				 (if (= idx1 last1)
					 nil
					 (with-operators
						 (for (((itr2 @~first2)) (_/= itr2 last2) (incf idx1) :returns t)
						   (if (= idx1 last1)
							   (return-from __includes-imp-2 nil)
							   (let ((val1 (aref buffer1 idx1))
									 (val2 *itr2))
								 (if (funcall less-bf val2 val1)
									 (return-from __includes-imp-2 nil)
									 (unless (funcall less-bf val1 val2)
									   ++itr2))))))))))

	(defmethod-overload includes ((first1 const-vector-pointer)
								  (last1  const-vector-pointer)
								  (first2 input_iterator) (last2 input_iterator))
	  (format t "specialized includes for const-vector-pointer & input_iterator is invoked.~%" )
	  (__pointer-check-iterator-range first1 last1)
	  (__includes-imp-2 (opr::vec-ptr-index  first1)
						(opr::vec-ptr-index  last1)
						(opr::vec-ptr-buffer first1) first2 last2 #'operator_<))

	(defmethod-overload includes ((first1 const-vector-pointer)
								  (last1  const-vector-pointer)
								  (first2 input_iterator) (last2 input_iterator) comp)
	  (format t "specialized includes for const-vector-pointer & input_iterator is invoked.~%" )
	  (__pointer-check-iterator-range first1 last1)
	  (__includes-imp-2 (opr::vec-ptr-index  first1)
						(opr::vec-ptr-index  last1)
						(opr::vec-ptr-buffer first1) first2 last2 (functor_function (clone comp)))))

  ;;PTN; includes : 3 -   i  x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__includes-imp-3 (first1 last1 cons2 end2 less-bf)
			 (declare (type cl:list cons2 end2))
			 (declare (type cl:function less-bf))
			 (if (eq cons2 end2)
				 t
				 (if (_== first1 last1)
					 nil
					 (with-operators
						 (for (((itr1 @~first1)) (not (eq cons2 end2)) ++itr1 :returns t)
						   (if (_== itr1 last1)
							   (return-from __includes-imp-3 nil)
							   (let ((val1 *itr1)
									 (val2 (car cons2)))
								 (if (funcall less-bf val2 val1)
									 (return-from __includes-imp-3 nil)
									 (unless (funcall less-bf val1 val2)
									   (setf cons2 (cdr cons2))))))))))))

	(defmethod-overload includes ((first1 input_iterator) (last1 input_iterator)
								  (first2 cons_const_iterator) (last2 cons_const_iterator))
	  (format t "specialized includes for input_iterator & cons_const_iterator is invoked.~%" )
	  (__includes-imp-3 first1 last1
						(__cons-itr-cons first2)
						(__cons-itr-cons  last2) #'operator_<))

	(defmethod-overload includes ((first1 input_iterator) (last1 input_iterator)
								  (first2 cons_const_iterator) (last2 cons_const_iterator) comp)
	  (format t "specialized includes for input_iterator & cons_const_iterator is invoked.~%" )
	  (__includes-imp-3 first1 last1
						(__cons-itr-cons first2)
						(__cons-itr-cons  last2) (functor_function (clone comp)))))

  ;;PTN; includes : 4 -  cci x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__includes-imp-4 (cons1 end1 cons2 end2 less-bf)
			 (declare (type cl:list cons1 end1 cons2 end2))
			 (declare (type cl:function less-bf))
			 (for (nil (not (eq cons2 end2)) (setf cons1 (cdr cons1)) :returns t)
			   (when (eq cons1 end1)
				 (return-from __includes-imp-4 nil))
			   (let ((val1 (car cons1))
					 (val2 (car cons2)))
				 (if (funcall less-bf val2 val1)
					 (return-from __includes-imp-4 nil)
					 (unless (funcall less-bf val1 val2)
					   (setf cons2 (cdr cons2))))))))

	(defmethod-overload includes ((first1 cons_const_iterator) (last1 cons_const_iterator)
								  (first2 cons_const_iterator) (last2 cons_const_iterator))
	  (format t "specialized includes for cons_const_iterator & cons_const_iterator is invoked.~%" )
	  (__includes-imp-4 (__cons-itr-cons first1) (__cons-itr-cons last1)
						(__cons-itr-cons first2) (__cons-itr-cons last2) #'operator_<))

	(defmethod-overload includes ((first1 cons_const_iterator) (last1 cons_const_iterator)
								  (first2 cons_const_iterator) (last2 cons_const_iterator) comp)
	  (format t "specialized includes for cons_const_iterator & cons_const_iterator is invoked.~%" )
	  (__includes-imp-4 (__cons-itr-cons first1) (__cons-itr-cons last1)
						(__cons-itr-cons first2) (__cons-itr-cons last2) (functor_function (clone comp)))))


  ;;PTN; includes : 5 -  cvp x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__includes-imp-8 (idx1 last1 buffer1 cons2 end2 less-bf)
			 (declare (type fixnum idx1 last1))
			 (declare (type cl:vector buffer1))
			 (declare (type cl:list cons2 end2))
			 (declare (type cl:function less-bf))
			 (for (nil (not (eq cons2 end2)) (incf idx1) :returns t)
			   (when (= idx1 last1)
				 (return-from __includes-imp-8 nil))
			   (let ((val1 (aref buffer1 idx1))
					 (val2 (car cons2)))
				 (if (funcall less-bf val2 val1)
					 (return-from __includes-imp-8 nil)
					 (unless (funcall less-bf val1 val2)
					   (setf cons2 (cdr cons2))))))))

	(defmethod-overload includes ((first1 const-vector-pointer) (last1 const-vector-pointer)
								  (first2 cons_const_iterator) (last2 cons_const_iterator))
	  (format t "specialized includes for const-vector-pointer & cons_const_iterator is invoked.~%" )
	  (__pointer-check-iterator-range first1 last1)
	  (__includes-imp-8 (opr::vec-ptr-index  first1)
						(opr::vec-ptr-index   last1)
						(opr::vec-ptr-buffer first1)
						(__cons-itr-cons first2) (__cons-itr-cons last2) #'operator_<))

	(defmethod-overload includes ((first1 const-vector-pointer) (last1 const-vector-pointer)
								  (first2 cons_const_iterator) (last2 cons_const_iterator) comp)
	  (format t "specialized includes for const-vector-pointer & cons_const_iterator is invoked.~%" )
	  (__pointer-check-iterator-range first1 last1)
	  (__includes-imp-8 (opr::vec-ptr-index  first1)
						(opr::vec-ptr-index   last1)
						(opr::vec-ptr-buffer first1)
						(__cons-itr-cons first2) (__cons-itr-cons last2) (functor_function (clone comp)))))


  ;;PTN; includes : 6 -   i  x cvp
  (labels ((__includes-imp-6 (first1 last1 idx2 last2 buffer2 less-bf)
			 (declare (type fixnum idx2 last2))
			 (declare (type cl:vector buffer2))
			 (declare (type cl:function less-bf))
			 (if (= idx2 last2)
				 t
				 (if (_== first1 last1)
					 nil
					 (with-operators
						 (for (((itr1 @~first1)) (< idx2 last2) ++itr1 :returns t)
						   (if (_== itr1 last1)
							   (return-from __includes-imp-6 nil)
							   (let ((val1 *itr1)
									 (val2 (aref buffer2 idx2)))
								 (if (funcall less-bf val2 val1)
									 (return-from __includes-imp-6 nil)
									 (unless (funcall less-bf val1 val2)
									   (incf idx2)))))))))))

	(defmethod-overload includes ((first1 input_iterator)
								  (last1  input_iterator)
								  (first2 const-vector-pointer) (last2 const-vector-pointer))
	  (format t "specialized includes for input_iterator & const-vector-pointer is invoked.~%" )
	  (__pointer-check-iterator-range first2 last2)
	  (__includes-imp-6 first1 last1
						(opr::vec-ptr-index  first2)
						(opr::vec-ptr-index  last2)
						(opr::vec-ptr-buffer first2) #'operator_<))

	(defmethod-overload includes ((first1 input_iterator)
								  (last1  input_iterator)
								  (first2 const-vector-pointer) (last2 const-vector-pointer) comp)
	  (format t "specialized includes for input_iterator & const-vector-pointer is invoked.~%" )
	  (__pointer-check-iterator-range first2 last2)
	  (__includes-imp-6 first1 last1
						(opr::vec-ptr-index  first2)
						(opr::vec-ptr-index  last2)
						(opr::vec-ptr-buffer first2) (functor_function (clone comp)))))


  ;;PTN; includes : 7 -  cci x cvp
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__includes-imp-7 (cons1 end1 idx2 last2 buffer2 less-bf)
			 (declare (type cl:list cons1 end1))
			 (declare (type fixnum idx2 last2))
			 (declare (type cl:vector buffer2))
			 (declare (type cl:function less-bf))
			 (for (nil (< idx2 last2) (setf cons1 (cdr cons1)) :returns t)
			   (when (eq cons1 end1)
				 (return-from __includes-imp-7 nil))
			   (let ((val1 (car cons1))
					 (val2 (aref buffer2 idx2)))
				 (if (funcall less-bf val2 val1)
					 (return-from __includes-imp-7 nil)
					 (unless (funcall less-bf val1 val2)
					   (incf idx2)))))))

	(defmethod-overload includes ((first1 cons_const_iterator) (last1 cons_const_iterator)
								  (first2 const-vector-pointer) (last2 const-vector-pointer))
	  (format t "specialized includes for cons_const_iterator & const-vector-pointer is invoked.~%" )
	  (__pointer-check-iterator-range first2 last2)
	  (__includes-imp-7 (__cons-itr-cons  first1)
						(__cons-itr-cons  last1)
						(opr::vec-ptr-index  first2)
						(opr::vec-ptr-index  last2) (opr::vec-ptr-buffer first2) #'operator_<))

	(defmethod-overload includes ((first1 cons_const_iterator) (last1 cons_const_iterator)
								  (first2 const-vector-pointer) (last2 const-vector-pointer) comp)
	  (format t "specialized includes for cons_const_iterator & const-vector-pointer is invoked.~%" )
	  (__pointer-check-iterator-range first2 last2)
	  (__includes-imp-7 (__cons-itr-cons first1)
						(__cons-itr-cons  last1)
						(opr::vec-ptr-index first2)
						(opr::vec-ptr-index  last2)
						(opr::vec-ptr-buffer first2) (functor_function (clone comp)))))

  ;;PTN; includes : 8 -  cvp x cvp
  (labels ((__includes-imp-8 (idx1 last1 buffer1 idx2 last2 buffer2 less-bf)
			 (declare (type fixnum idx1 last1 idx2 last2))
			 (declare (type cl:vector buffer1 buffer2))
			 (declare (type cl:function less-bf))
			 (for (nil (< idx2 last2) nil :returns t)
			   (when (= idx1 last1)
				 (return-from __includes-imp-8 nil))
			   (let ((val1 (aref buffer1 idx1))
					 (val2 (aref buffer2 idx2)))
				 (if (funcall less-bf val2 val1)
					 (return-from __includes-imp-8 nil)
					 (unless (funcall less-bf val1 val2)
					   (incf idx2))))
			   (incf idx1))))

	(defmethod-overload includes ((first1 const-vector-pointer) (last1 const-vector-pointer)
								  (first2 const-vector-pointer) (last2 const-vector-pointer))
	  (format t "specialized includes for const-vector-pointer & const-vector-pointer is invoked.~%" )
	  (__pointer-check-iterator-range first1 last1)
	  (__pointer-check-iterator-range first2 last2)
	  (__includes-imp-8 (opr::vec-ptr-index  first1)
						(opr::vec-ptr-index  last1)
						(opr::vec-ptr-buffer first1)
						(opr::vec-ptr-index  first2)
						(opr::vec-ptr-index  last2)
						(opr::vec-ptr-buffer first2) #'operator_<))

	(defmethod-overload includes ((first1 const-vector-pointer) (last1 const-vector-pointer)
								  (first2 const-vector-pointer) (last2 const-vector-pointer) comp)
	  (format t "specialized includes for const-vector-pointer & const-vector-pointer is invoked.~%" )
	  (__pointer-check-iterator-range first1 last1)
	  (__pointer-check-iterator-range first2 last2)
	  (__includes-imp-8 (opr::vec-ptr-index  first1)
						(opr::vec-ptr-index  last1)
						(opr::vec-ptr-buffer first1)
						(opr::vec-ptr-index  first2)
						(opr::vec-ptr-index  last2)
						(opr::vec-ptr-buffer first2) (functor_function (clone comp))))))



; first1    : input_iterator
; last1     : input_iterator
; first2    : input_iterator
; last2     : input_iterator
; result    : output_iterator
; comp      : binary_function ( default : #'operator_< )
; returns   : copy of result ( point to end of sequence ).
(locally (declare (optimize speed))

  ;;PTN; set_union : 00 -   i  x  i  x  o 
  (defmethod-overload set_union ((first1 input_iterator) (last1 input_iterator)
								 (first2 input_iterator) (last2 input_iterator) (result output_iterator))
	(__set_union-imp-00 first1 last1 first2 last2 result #'operator_<))

  (defmethod-overload set_union ((first1 input_iterator) (last1 input_iterator)
								 (first2 input_iterator) (last2 input_iterator) (result output_iterator) comp)
	(__set_union-imp-00 first1 last1 first2 last2 result (functor_function (clone comp))))


  ;;PTN; set_union : 01 -   i  x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 input_iterator) (last1 input_iterator)
								 (first2 input_iterator) (last2 input_iterator) (result cons_iterator))
	;;(format t "specialized set_union for input_iterator & input_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__set_union-imp-01 first1 last1 first2 last2
												   (__cons-itr-cons result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 input_iterator) (last1 input_iterator)
								 (first2 input_iterator) (last2 input_iterator) (result cons_iterator) comp)
	;;(format t "specialized set_union for input_iterator & input_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__set_union-imp-01 first1 last1 first2 last2
												   (__cons-itr-cons result) (functor_function (clone comp)))))

  ;;PTN; set_union : 02 -   i  x  i  x  vp
  (defmethod-overload set_union ((first1 input_iterator) (last1 input_iterator)
								 (first2 input_iterator) (last2 input_iterator) (result vector-pointer))
	;;(format t "specialized set_union for input_iterator & input_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__set_union-imp-02 first1 last1 first2 last2
												   (opr::vec-ptr-index  result)
												   (opr::vec-ptr-buffer result) #'operator_<)))

  (defmethod-overload set_union ((first1 input_iterator) (last1 input_iterator)
								 (first2 input_iterator) (last2 input_iterator) (result vector-pointer) comp)
	;;(format t "specialized set_union for input_iterator & input_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__set_union-imp-02 first1 last1 first2 last2
												   (opr::vec-ptr-index  result)
												   (opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; set_union : 03 -   i  x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 input_iterator) (last1 input_iterator)
								 (first2 cons_const_iterator) (last2 cons_const_iterator) (result output_iterator))
	;;(format t "specialized set_union for input_iterator & cons_const_iterator & output_iterator is invoked.~%")
	(__set_union-imp-03 first1 last1
						(__cons-itr-cons first2)
						(__cons-itr-cons  last2) (clone result) #'operator_<))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 input_iterator) (last1 input_iterator)
								 (first2 cons_const_iterator) (last2 cons_const_iterator) (result output_iterator) comp)
	;;(format t "specialized set_union for input_iterator & cons_const_iterator & output_iterator is invoked.~%")
	(__set_union-imp-03 first1 last1
						(__cons-itr-cons first2)
						(__cons-itr-cons  last2) (clone result) (functor_function (clone comp))))

  ;;PTN; set_union : 04 -   i  x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 input_iterator) (last1 input_iterator)
								 (first2 cons_const_iterator) (last2 cons_const_iterator) (result cons_iterator))
	;;(format t "specialized set_union for input_iterator & cons_const_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__set_union-imp-04 first1 last1
												   (__cons-itr-cons first2)
												   (__cons-itr-cons  last2)
												   (__cons-itr-cons result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 input_iterator) (last1 input_iterator)
								 (first2 cons_const_iterator) (last2 cons_const_iterator) (result cons_iterator) comp)
	;;(format t "specialized set_union for input_iterator & cons_const_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__set_union-imp-04 first1 last1
												   (__cons-itr-cons first2)
												   (__cons-itr-cons  last2)
												   (__cons-itr-cons result) (functor_function (clone comp)))))

  ;;PTN; set_union : 05 -   i  x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 input_iterator) (last1 input_iterator)
								 (first2 cons_const_iterator) (last2 cons_const_iterator) (result vector-pointer))
	;;(format t "specialized set_union for input_iterator & cons_const_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__set_union-imp-05 first1 last1
												   (__cons-itr-cons     first2)
												   (__cons-itr-cons      last2)
												   (opr::vec-ptr-index  result)
												   (opr::vec-ptr-buffer result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 input_iterator) (last1 input_iterator)
								 (first2 cons_const_iterator) (last2 cons_const_iterator) (result vector-pointer) comp)
	;;(format t "specialized set_union for input_iterator & cons_const_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__set_union-imp-05 first1 last1
												   (__cons-itr-cons     first2)
												   (__cons-itr-cons      last2)
												   (opr::vec-ptr-index  result)
												   (opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; set_union : 06 -   i  x cvp x  o 
  (defmethod-overload set_union ((first1 input_iterator) (last1 input_iterator)
								 (first2 const-vector-pointer) (last2 const-vector-pointer) (result output_iterator))
	;;(format t "specialized set_union for input_iterator & const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__set_union-imp-06 first1 last1
						(opr::vec-ptr-index  first2)
						(opr::vec-ptr-index  last2)
						(opr::vec-ptr-buffer first2) (clone result) #'operator_<))

  (defmethod-overload set_union ((first1 input_iterator) (last1 input_iterator)
								 (first2 const-vector-pointer) (last2 const-vector-pointer) (result output_iterator) comp)
	;;(format t "specialized set_union for input_iterator & const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__set_union-imp-06 first1 last1
						(opr::vec-ptr-index  first2)
						(opr::vec-ptr-index  last2)
						(opr::vec-ptr-buffer first2) (clone result) (functor_function (clone comp))))

  ;;PTN; set_union : 07 -   i  x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 input_iterator) (last1 input_iterator)
								 (first2 const-vector-pointer) (last2 const-vector-pointer) (result cons_iterator))
	;;(format t "specialized set_union for input_iterator & const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-cns-iterator result
							   (__set_union-imp-07 first1 last1
												   (opr::vec-ptr-index  first2)
												   (opr::vec-ptr-index   last2)
												   (opr::vec-ptr-buffer first2)
												   (__cons-itr-cons     result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 input_iterator) (last1 input_iterator)
								 (first2 const-vector-pointer) (last2 const-vector-pointer) (result cons_iterator) comp)
	;;(format t "specialized set_union for input_iterator & const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-cns-iterator result
							   (__set_union-imp-07 first1 last1
												   (opr::vec-ptr-index  first2)
												   (opr::vec-ptr-index   last2)
												   (opr::vec-ptr-buffer first2)
												   (__cons-itr-cons     result) (functor_function (clone comp)))))

  ;;PTN; set_union : 08 -   i  x cvp x  vp
  (defmethod-overload set_union ((first1 input_iterator) (last1 input_iterator)
								 (first2 const-vector-pointer) (last2 const-vector-pointer) (result vector-pointer))
	;;(format t "specialized set_union for input_iterator & const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-vct-iterator result
							   (__set_union-imp-08 first1 last1
												   (opr::vec-ptr-index  first2)
												   (opr::vec-ptr-index  last2)
												   (opr::vec-ptr-buffer first2)
												   (opr::vec-ptr-index  result)
												   (opr::vec-ptr-buffer result) #'operator_<)))

  (defmethod-overload set_union ((first1 input_iterator) (last1 input_iterator)
								 (first2 const-vector-pointer) (last2 const-vector-pointer) (result vector-pointer) comp)
	;;(format t "specialized set_union for input_iterator & const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-vct-iterator result
							   (__set_union-imp-08 first1 last1
												   (opr::vec-ptr-index  first2)
												   (opr::vec-ptr-index  last2)
												   (opr::vec-ptr-buffer first2)
												   (opr::vec-ptr-index  result)
												   (opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; set_union : 09 -  cci x  i  x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 cons_const_iterator) (last1 cons_const_iterator)
								 (first2 input_iterator) (last2 input_iterator) (result output_iterator))
	;;(format t "specialized set_union for const-vector-pointer & input_iterator & output_iterator is invoked.~%")
	(__set_union-imp-09 (__cons-itr-cons first1)
						(__cons-itr-cons  last1) first2 last2 (clone result) #'operator_<))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 cons_const_iterator) (last1 cons_const_iterator)
								 (first2 input_iterator) (last2 input_iterator) (result output_iterator) comp)
	;;(format t "specialized set_union for const-vector-pointer & input_iterator & output_iterator is invoked.~%")
	(__set_union-imp-09 (__cons-itr-cons first1)
						(__cons-itr-cons  last1) first2 last2 (clone result) (functor_function (clone comp))))

  ;;PTN; set_union : 10 -  cci x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 cons_const_iterator) (last1 cons_const_iterator)
								 (first2 input_iterator) (last2 input_iterator) (result cons_iterator))
	;;(format t "specialized set_union for const-vector-pointer & input_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__set_union-imp-10 (__cons-itr-cons first1)
												   (__cons-itr-cons  last1)
												   first2 last2
												   (__cons-itr-cons result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 cons_const_iterator) (last1 cons_const_iterator)
								 (first2 input_iterator) (last2 input_iterator) (result cons_iterator) comp)
	;;(format t "specialized set_union for const-vector-pointer & input_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__set_union-imp-10 (__cons-itr-cons first1)
												   (__cons-itr-cons  last1)
												   first2 last2
												   (__cons-itr-cons result) (functor_function (clone comp)))))

  ;;PTN; set_union : 11 -  cci x  i  x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 cons_const_iterator) (last1 cons_const_iterator)
								 (first2 input_iterator) (last2 input_iterator) (result vector-pointer))
	;;(format t "specialized set_union for const-vector-pointer & input_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__set_union-imp-11 (__cons-itr-cons first1)
												   (__cons-itr-cons  last1)
												   first2 last2
												   (opr::vec-ptr-index  result)
												   (opr::vec-ptr-buffer result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 cons_const_iterator) (last1 cons_const_iterator)
								 (first2 input_iterator) (last2 input_iterator) (result vector-pointer) comp)
	;;(format t "specialized set_union for const-vector-pointer & input_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__set_union-imp-11 (__cons-itr-cons first1)
												   (__cons-itr-cons  last1)
												   first2 last2
												   (opr::vec-ptr-index  result)
												   (opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; set_union : 12 -  cci x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 cons_const_iterator) (last1 cons_const_iterator)
								 (first2 cons_const_iterator) (last2 cons_const_iterator) (result output_iterator))
	;;(format t "specialized set_union for const-vector-pointer & cons_const_iterator & output_iterator is invoked.~%")
	(__set_union-imp-12 (__cons-itr-cons first1)
						(__cons-itr-cons  last1)
						(__cons-itr-cons first2)
						(__cons-itr-cons  last2) (clone result) #'operator_<))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 cons_const_iterator) (last1 cons_const_iterator)
								 (first2 cons_const_iterator) (last2 cons_const_iterator) (result output_iterator) comp)
	;;(format t "specialized set_union for const-vector-pointer & cons_const_iterator & output_iterator is invoked.~%")
	(__set_union-imp-12 (__cons-itr-cons first1)
						(__cons-itr-cons  last1)
						(__cons-itr-cons first2)
						(__cons-itr-cons  last2) (clone result) (functor_function (clone comp))))

  ;;PTN; set_union : 13 -  cci x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 cons_const_iterator) (last1 cons_const_iterator)
								 (first2 cons_const_iterator) (last2 cons_const_iterator) (result cons_iterator))
	;;(format t "specialized set_union for const-vector-pointer & cons_const_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__set_union-imp-13 (__cons-itr-cons first1)
												   (__cons-itr-cons  last1)
												   (__cons-itr-cons first2)
												   (__cons-itr-cons  last2)
												   (__cons-itr-cons result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 cons_const_iterator) (last1 cons_const_iterator)
								 (first2 cons_const_iterator) (last2 cons_const_iterator) (result cons_iterator) comp)
	;;(format t "specialized set_union for const-vector-pointer & cons_const_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__set_union-imp-13 (__cons-itr-cons first1)
												   (__cons-itr-cons  last1)
												   (__cons-itr-cons first2)
												   (__cons-itr-cons  last2)
												   (__cons-itr-cons result) (functor_function (clone comp)))))

  ;;PTN; set_union : 14 -  cci x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 cons_const_iterator) (last1 cons_const_iterator)
								 (first2 cons_const_iterator) (last2 cons_const_iterator) (result vector-pointer))
	;;(format t "specialized set_union for const-vector-pointer & cons_const_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__set_union-imp-14 (__cons-itr-cons     first1)
												   (__cons-itr-cons      last1)
												   (__cons-itr-cons     first2)
												   (__cons-itr-cons      last2)
												   (opr::vec-ptr-index  result)
												   (opr::vec-ptr-buffer result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 cons_const_iterator) (last1 cons_const_iterator)
								 (first2 cons_const_iterator) (last2 cons_const_iterator) (result vector-pointer) comp)
	;;(format t "specialized set_union for const-vector-pointer & cons_const_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__set_union-imp-14 (__cons-itr-cons     first1)
												   (__cons-itr-cons      last1)
												   (__cons-itr-cons     first2)
												   (__cons-itr-cons      last2)
												   (opr::vec-ptr-index  result)
												   (opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; set_union : 15 -  cci x cvp x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 cons_const_iterator) (last1 cons_const_iterator)
								 (first2 const-vector-pointer) (last2 const-vector-pointer) (result output_iterator))
	;;(format t "specialized set_union for const-vector-pointer & const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__set_union-imp-15 (__cons-itr-cons     first1)
						(__cons-itr-cons      last1)
						(opr::vec-ptr-index  first2)
						(opr::vec-ptr-index   last2)
						(opr::vec-ptr-buffer first2) (clone result) #'operator_<))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 cons_const_iterator) (last1 cons_const_iterator)
								 (first2 const-vector-pointer) (last2 const-vector-pointer) (result output_iterator) comp)
	;;(format t "specialized set_union for const-vector-pointer & const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__set_union-imp-15 (__cons-itr-cons     first1)
						(__cons-itr-cons      last1)
						(opr::vec-ptr-index  first2)
						(opr::vec-ptr-index   last2)
						(opr::vec-ptr-buffer first2) (clone result) (functor_function (clone comp))))

  ;;PTN; set_union : 16 -  cci x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 cons_const_iterator) (last1 cons_const_iterator)
								 (first2 const-vector-pointer) (last2 const-vector-pointer) (result cons_iterator))
	;;(format t "specialized set_union for const-vector-pointer & const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-cns-iterator result
							   (__set_union-imp-16 (__cons-itr-cons     first1)
												   (__cons-itr-cons      last1)
												   (opr::vec-ptr-index  first2)
												   (opr::vec-ptr-index   last2)
												   (opr::vec-ptr-buffer first2)
												   (__cons-itr-cons     result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 cons_const_iterator) (last1 cons_const_iterator)
								 (first2 const-vector-pointer) (last2 const-vector-pointer) (result cons_iterator) comp)
	;;(format t "specialized set_union for const-vector-pointer & const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-cns-iterator result
							   (__set_union-imp-16 (__cons-itr-cons     first1)
												   (__cons-itr-cons      last1)
												   (opr::vec-ptr-index  first2)
												   (opr::vec-ptr-index   last2)
												   (opr::vec-ptr-buffer first2)
												   (__cons-itr-cons     result) (functor_function (clone comp)))))

  ;;PTN; set_union : 17 -  cci x cvp x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 cons_const_iterator) (last1 cons_const_iterator)
								 (first2 const-vector-pointer) (last2 const-vector-pointer) (result vector-pointer))
	;;(format t "specialized set_union for const-vector-pointer & const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-vct-iterator result
							   (__set_union-imp-17 (__cons-itr-cons     first1)
												   (__cons-itr-cons      last1)
												   (opr::vec-ptr-index  first2)
												   (opr::vec-ptr-index   last2)
												   (opr::vec-ptr-buffer first2)
												   (opr::vec-ptr-index  result)
												   (opr::vec-ptr-buffer result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 cons_const_iterator) (last1 cons_const_iterator)
								 (first2 const-vector-pointer) (last2 const-vector-pointer) (result vector-pointer) comp)
	;;(format t "specialized set_union for const-vector-pointer & const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-vct-iterator result
							   (__set_union-imp-17 (__cons-itr-cons     first1)
												   (__cons-itr-cons      last1)
												   (opr::vec-ptr-index  first2)
												   (opr::vec-ptr-index   last2)
												   (opr::vec-ptr-buffer first2)
												   (opr::vec-ptr-index  result)
												   (opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; set_union : 18 -  cvp x  i  x  o 
  (defmethod-overload set_union ((first1 const-vector-pointer) (last1 const-vector-pointer)
								 (first2 input_iterator) (last2 input_iterator) (result output_iterator))
	;;(format t "specialized set_union for const-vector-pointer & input_iterator & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__set_union-imp-18 (opr::vec-ptr-index  first1)
						(opr::vec-ptr-index  last1)
						(opr::vec-ptr-buffer first1) first2 last2 (clone result) #'operator_<))

  (defmethod-overload set_union ((first1 const-vector-pointer) (last1 const-vector-pointer)
								 (first2 input_iterator) (last2 input_iterator) (result output_iterator) comp)
	;;(format t "specialized set_union for const-vector-pointer & input_iterator & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__set_union-imp-18 (opr::vec-ptr-index  first1)
						(opr::vec-ptr-index  last1)
						(opr::vec-ptr-buffer first1) first2 last2 (clone result) (functor_function (clone comp))))

  ;;PTN; set_union : 19 -  cvp x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 const-vector-pointer) (last1 const-vector-pointer)
								 (first2 input_iterator) (last2 input_iterator) (result cons_iterator))
	;;(format t "specialized set_union for const-vector-pointer & input_iterator & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-cns-iterator result
							   (__set_union-imp-19 (opr::vec-ptr-index  first1)
												   (opr::vec-ptr-index   last1)
												   (opr::vec-ptr-buffer first1)
												   first2 last2
												   (__cons-itr-cons     result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 const-vector-pointer) (last1 const-vector-pointer)
								 (first2 input_iterator) (last2 input_iterator) (result cons_iterator) comp)
	;;(format t "specialized set_union for const-vector-pointer & input_iterator & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-cns-iterator result
							   (__set_union-imp-19 (opr::vec-ptr-index  first1)
												   (opr::vec-ptr-index   last1)
												   (opr::vec-ptr-buffer first1)
												   first2 last2
												   (__cons-itr-cons     result) (functor_function (clone comp)))))

  ;;PTN; set_union : 20 -  cvp x  i  x  vp
  (defmethod-overload set_union ((first1 const-vector-pointer) (last1 const-vector-pointer)
								 (first2 input_iterator) (last2 input_iterator) (result vector-pointer))
	;;(format t "specialized set_union for const-vector-pointer & input_iterator & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-vct-iterator result
							   (__set_union-imp-20 (opr::vec-ptr-index  first1)
												   (opr::vec-ptr-index  last1)
												   (opr::vec-ptr-buffer first1)
												   first2 last2
												   (opr::vec-ptr-index  result)
												   (opr::vec-ptr-buffer result) #'operator_<)))

  (defmethod-overload set_union ((first1 const-vector-pointer) (last1 const-vector-pointer)
								 (first2 input_iterator) (last2 input_iterator) (result vector-pointer) comp)
	;;(format t "specialized set_union for const-vector-pointer & input_iterator & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-vct-iterator result
							   (__set_union-imp-20 (opr::vec-ptr-index  first1)
												   (opr::vec-ptr-index  last1)
												   (opr::vec-ptr-buffer first1)
												   first2 last2
												   (opr::vec-ptr-index  result)
												   (opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; set_union : 21 -  cvp x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 const-vector-pointer) (last1 const-vector-pointer)
								 (first2 cons_const_iterator) (last2 cons_const_iterator) (result output_iterator))
	;;(format t "specialized set_union for const-vector-pointer & cons_const_iterator & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__set_union-imp-21 (opr::vec-ptr-index  first1)
						(opr::vec-ptr-index   last1)
						(opr::vec-ptr-buffer first1)
						(__cons-itr-cons     first2)
						(__cons-itr-cons      last2) (clone result) #'operator_<))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 const-vector-pointer) (last1 const-vector-pointer)
								 (first2 cons_const_iterator) (last2 cons_const_iterator) (result output_iterator) comp)
	;;(format t "specialized set_union for const-vector-pointer & cons_const_iterator & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__set_union-imp-21 (opr::vec-ptr-index  first1)
						(opr::vec-ptr-index   last1)
						(opr::vec-ptr-buffer first1)
						(__cons-itr-cons     first2)
						(__cons-itr-cons      last2) (clone result) (functor_function (clone comp))))

  ;;PTN; set_union : 22 -  cvp x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 const-vector-pointer) (last1 const-vector-pointer)
								 (first2 cons_const_iterator) (last2 cons_const_iterator) (result cons_iterator))
	;;(format t "specialized set_union for const-vector-pointer & cons_const_iterator & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-cns-iterator result
							   (__set_union-imp-22 (opr::vec-ptr-index  first1)
												   (opr::vec-ptr-index   last1)
												   (opr::vec-ptr-buffer first1)
												   (__cons-itr-cons     first2)
												   (__cons-itr-cons      last2)
												   (__cons-itr-cons     result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 const-vector-pointer) (last1 const-vector-pointer)
								 (first2 cons_const_iterator) (last2 cons_const_iterator) (result cons_iterator) comp)
	;;(format t "specialized set_union for const-vector-pointer & cons_const_iterator & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-cns-iterator result
							   (__set_union-imp-22 (opr::vec-ptr-index  first1)
												   (opr::vec-ptr-index   last1)
												   (opr::vec-ptr-buffer first1)
												   (__cons-itr-cons     first2)
												   (__cons-itr-cons      last2)
												   (__cons-itr-cons     result) (functor_function (clone comp)))))

  ;;PTN; set_union : 23 -  cvp x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 const-vector-pointer) (last1 const-vector-pointer)
								 (first2 cons_const_iterator) (last2 cons_const_iterator) (result vector-pointer))
	;;(format t "specialized set_union for const-vector-pointer & cons_const_iterator & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-vct-iterator result
							   (__set_union-imp-23 (opr::vec-ptr-index  first1)
												   (opr::vec-ptr-index  last1)
												   (opr::vec-ptr-buffer first1)
												   (__cons-itr-cons     first2)
												   (__cons-itr-cons      last2)
												   (opr::vec-ptr-index  result)
												   (opr::vec-ptr-buffer result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 const-vector-pointer) (last1 const-vector-pointer)
								 (first2 cons_const_iterator) (last2 cons_const_iterator) (result vector-pointer) comp)
	;;(format t "specialized set_union for const-vector-pointer & cons_const_iterator & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-vct-iterator result
							   (__set_union-imp-23 (opr::vec-ptr-index  first1)
												   (opr::vec-ptr-index   last1)
												   (opr::vec-ptr-buffer first1)
												   (__cons-itr-cons     first2)
												   (__cons-itr-cons      last2)
												   (opr::vec-ptr-index  result)
												   (opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; set_union : 24 -  cvp x cvp x  o 
  (defmethod-overload set_union ((first1 const-vector-pointer) (last1 const-vector-pointer)
								 (first2 const-vector-pointer) (last2 const-vector-pointer) (result output_iterator))
	;;(format t "specialized set_union for const-vector-pointer & const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__set_union-imp-24 (opr::vec-ptr-index  first1)
						(opr::vec-ptr-index  last1)
						(opr::vec-ptr-buffer first1)
						(opr::vec-ptr-index  first2)
						(opr::vec-ptr-index  last2)
						(opr::vec-ptr-buffer first2) (clone result) #'operator_<))

  (defmethod-overload set_union ((first1 const-vector-pointer) (last1 const-vector-pointer)
								 (first2 const-vector-pointer) (last2 const-vector-pointer) (result output_iterator) comp)
	;;(format t "specialized set_union for const-vector-pointer & const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__set_union-imp-24 (opr::vec-ptr-index  first1)
						(opr::vec-ptr-index  last1)
						(opr::vec-ptr-buffer first1)
						(opr::vec-ptr-index  first2)
						(opr::vec-ptr-index  last2)
						(opr::vec-ptr-buffer first2) (clone result) (functor_function (clone comp))))

  ;;PTN; set_union : 25 -  cvp x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 const-vector-pointer) (last1 const-vector-pointer)
								 (first2 const-vector-pointer) (last2 const-vector-pointer) (result cons_iterator))
	;;(format t "specialized set_union for const-vector-pointer & const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-cns-iterator result
							   (__set_union-imp-25 (opr::vec-ptr-index  first1)
												   (opr::vec-ptr-index   last1)
												   (opr::vec-ptr-buffer first1)
												   (opr::vec-ptr-index  first2)
												   (opr::vec-ptr-index   last2)
												   (opr::vec-ptr-buffer first2)
												   (__cons-itr-cons     result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_union ((first1 const-vector-pointer) (last1 const-vector-pointer)
								 (first2 const-vector-pointer) (last2 const-vector-pointer) (result cons_iterator) comp)
	;;(format t "specialized set_union for const-vector-pointer & const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-cns-iterator result
							   (__set_union-imp-25 (opr::vec-ptr-index  first1)
												   (opr::vec-ptr-index   last1)
												   (opr::vec-ptr-buffer first1)
												   (opr::vec-ptr-index  first2)
												   (opr::vec-ptr-index   last2)
												   (opr::vec-ptr-buffer first2)
												   (__cons-itr-cons     result) (functor_function (clone comp)))))

  ;;PTN; set_union : 26 -  cvp x cvp x  vp
  (defmethod-overload set_union ((first1 const-vector-pointer) (last1 const-vector-pointer)
								 (first2 const-vector-pointer) (last2 const-vector-pointer) (result vector-pointer))
	;;(format t "specialized set_union for const-vector-pointer & const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-vct-iterator result
							   (__set_union-imp-26 (opr::vec-ptr-index  first1)
												   (opr::vec-ptr-index  last1)
												   (opr::vec-ptr-buffer first1)
												   (opr::vec-ptr-index  first2)
												   (opr::vec-ptr-index  last2)
												   (opr::vec-ptr-buffer first2)
												   (opr::vec-ptr-index  result)
												   (opr::vec-ptr-buffer result) #'operator_<)))

  (defmethod-overload set_union ((first1 const-vector-pointer) (last1 const-vector-pointer)
								 (first2 const-vector-pointer) (last2 const-vector-pointer) (result vector-pointer) comp)
	;;(format t "specialized set_union for const-vector-pointer & const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-vct-iterator result
							   (__set_union-imp-26 (opr::vec-ptr-index  first1)
												   (opr::vec-ptr-index  last1)
												   (opr::vec-ptr-buffer first1)
												   (opr::vec-ptr-index  first2)
												   (opr::vec-ptr-index  last2)
												   (opr::vec-ptr-buffer first2)
												   (opr::vec-ptr-index  result)
												   (opr::vec-ptr-buffer result) (functor_function (clone comp))))))




; first1    : input_iterator
; last1     : input_iterator
; first2    : input_iterator
; last2     : input_iterator
; result    : output_iterator
; comp      : binary_function ( default : #'operator_< )
; returns   : copy of result ( point to end of sequence ).
(locally (declare (optimize speed))

  ;;PTN; set_intersection : 00 -   i  x  i  x  o 
  (defmethod-overload set_intersection ((first1 input_iterator) (last1 input_iterator)
										(first2 input_iterator) (last2 input_iterator) (result output_iterator))
	(__set_intersection-imp-00 first1 last1 first2 last2 result #'operator_<))

  (defmethod-overload set_intersection ((first1 input_iterator) (last1 input_iterator)
										(first2 input_iterator) (last2 input_iterator) (result output_iterator) comp)
	(__set_intersection-imp-00 first1 last1 first2 last2 result (functor_function (clone comp))))


  ;;PTN; set_intersection : 01 -   i  x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 input_iterator) (last1 input_iterator)
										(first2 input_iterator) (last2 input_iterator) (result cons_iterator))
	;;(format t "specialized sffet-intersection for input_iterator & input_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__set_intersection-imp-01 first1 last1 first2 last2
														  (__cons-itr-cons result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 input_iterator) (last1 input_iterator)
										(first2 input_iterator) (last2 input_iterator) (result cons_iterator) comp)
	;;(format t "specialized set_intersection for input_iterator & input_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__set_intersection-imp-01 first1 last1 first2 last2
														  (__cons-itr-cons result) (functor_function (clone comp)))))

  ;;PTN; set_intersection : 02 -   i  x  i  x  vp
  (defmethod-overload set_intersection ((first1 input_iterator) (last1 input_iterator)
										(first2 input_iterator) (last2 input_iterator) (result vector-pointer))
	;;(format t "specialized sffet-intersection for input_iterator & input_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__set_intersection-imp-02 first1 last1 first2 last2
														  (opr::vec-ptr-index  result)
														  (opr::vec-ptr-buffer result) #'operator_<)))

  (defmethod-overload set_intersection ((first1 input_iterator) (last1 input_iterator)
										(first2 input_iterator) (last2 input_iterator) (result vector-pointer) comp)
	;;(format t "specialized set_intersection for input_iterator & input_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__set_intersection-imp-02 first1 last1 first2 last2
														  (opr::vec-ptr-index  result)
														  (opr::vec-ptr-buffer result) (functor_function (clone comp)))))


  ;;PTN; set_intersection : 03 -   i  x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 input_iterator) (last1 input_iterator)
										(first2 cons_const_iterator) (last2 cons_const_iterator) (result output_iterator))
	;;(format t "specialized set_intersection for input_iterator & cons_const_iterator & output_iterator is invoked.~%")
	(__set_intersection-imp-03 first1 last1
							   (__cons-itr-cons    first2)
							   (__cons-itr-cons     last2) (clone result) #'operator_<))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 input_iterator) (last1 input_iterator)
										(first2 cons_const_iterator) (last2 cons_const_iterator) (result output_iterator) comp)
	;;(format t "specialized set_intersection for input_iterator & cons_const_iterator & output_iterator is invoked.~%")
	(__set_intersection-imp-03 first1 last1
							   (__cons-itr-cons    first2)
							   (__cons-itr-cons     last2) (clone result) (functor_function (clone comp))))


  ;;PTN; set_intersection : 04 -   i  x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 input_iterator) (last1 input_iterator)
										(first2 cons_const_iterator) (last2 cons_const_iterator) (result cons_iterator))
	;;(format t "specialized sffet-intersection for input_iterator & cons_const_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__set_intersection-imp-04 first1 last1
														  (__cons-itr-cons    first2)
														  (__cons-itr-cons     last2)
														  (__cons-itr-cons    result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 input_iterator) (last1 input_iterator)
										(first2 cons_const_iterator) (last2 cons_const_iterator) (result cons_iterator) comp)
	;;(format t "specialized set_intersection for input_iterator & cons_const_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__set_intersection-imp-04 first1 last1
														  (__cons-itr-cons    first2)
														  (__cons-itr-cons     last2)
														  (__cons-itr-cons    result) (functor_function (clone comp)))))

  ;;PTN; set_intersection : 05 -   i  x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 input_iterator) (last1 input_iterator)
										(first2 cons_const_iterator) (last2 cons_const_iterator) (result vector-pointer))
	;;(format t "specialized sffet-intersection for input_iterator & cons_const_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__set_intersection-imp-05 first1 last1
														  (__cons-itr-cons     first2)
														  (__cons-itr-cons      last2)
														  (opr::vec-ptr-index  result)
														  (opr::vec-ptr-buffer result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 input_iterator) (last1 input_iterator)
										(first2 cons_const_iterator) (last2 cons_const_iterator) (result vector-pointer) comp)
	;;(format t "specialized set_intersection for input_iterator & cons_const_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__set_intersection-imp-05 first1 last1
														  (__cons-itr-cons     first2)
														  (__cons-itr-cons      last2)
														  (opr::vec-ptr-index  result)
														  (opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; set_intersection : 06 -   i  x cvp x  o 
  (defmethod-overload set_intersection ((first1 input_iterator) (last1 input_iterator)
										(first2 const-vector-pointer) (last2 const-vector-pointer) (result output_iterator))
	;;(format t "specialized set_intersection for input_iterator & const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__set_intersection-imp-06 first1 last1
							   (opr::vec-ptr-index  first2)
							   (opr::vec-ptr-index  last2)
							   (opr::vec-ptr-buffer first2) (clone result) #'operator_<))

  (defmethod-overload set_intersection ((first1 input_iterator) (last1 input_iterator)
										(first2 const-vector-pointer) (last2 const-vector-pointer) (result output_iterator) comp)
	;;(format t "specialized set_intersection for input_iterator & const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__set_intersection-imp-06 first1 last1
							   (opr::vec-ptr-index  first2)
							   (opr::vec-ptr-index  last2)
							   (opr::vec-ptr-buffer first2) (clone result) (functor_function (clone comp))))


  ;;PTN; set_intersection : 07 -   i  x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 input_iterator) (last1 input_iterator)
										(first2 const-vector-pointer) (last2 const-vector-pointer) (result cons_iterator))
	;;(format t "specialized sffet-intersection for input_iterator & const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-cns-iterator result
							   (__set_intersection-imp-07 first1 last1
														  (opr::vec-ptr-index  first2)
														  (opr::vec-ptr-index   last2)
														  (opr::vec-ptr-buffer first2)
														  (__cons-itr-cons     result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 input_iterator) (last1 input_iterator)
										(first2 const-vector-pointer) (last2 const-vector-pointer) (result cons_iterator) comp)
	;;(format t "specialized set_intersection for input_iterator & const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-cns-iterator result
							   (__set_intersection-imp-07 first1 last1
														  (opr::vec-ptr-index  first2)
														  (opr::vec-ptr-index  last2)
														  (opr::vec-ptr-buffer first2)
														  (__cons-itr-cons     result) (functor_function (clone comp)))))

  ;;PTN; set_intersection : 08 -   i  x cvp x  vp
  (defmethod-overload set_intersection ((first1 input_iterator) (last1 input_iterator)
										(first2 const-vector-pointer) (last2 const-vector-pointer) (result vector-pointer))
	;;(format t "specialized sffet-intersection for input_iterator & const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-vct-iterator result
							   (__set_intersection-imp-08 first1 last1
														  (opr::vec-ptr-index  first2)
														  (opr::vec-ptr-index  last2)
														  (opr::vec-ptr-buffer first2)
														  (opr::vec-ptr-index  result)
														  (opr::vec-ptr-buffer result) #'operator_<)))

  (defmethod-overload set_intersection ((first1 input_iterator) (last1 input_iterator)
										(first2 const-vector-pointer) (last2 const-vector-pointer) (result vector-pointer) comp)
	;;(format t "specialized set_intersection for input_iterator & const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-vct-iterator result
							   (__set_intersection-imp-08 first1 last1
														  (opr::vec-ptr-index  first2)
														  (opr::vec-ptr-index  last2)
														  (opr::vec-ptr-buffer first2)
														  (opr::vec-ptr-index  result)
														  (opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; set_intersection : 09 -  cci x  i  x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 cons_const_iterator) (last1 cons_const_iterator)
										(first2 input_iterator) (last2 input_iterator) (result output_iterator))
	;;(format t "specialized set_intersection for const-vector-pointer & input_iterator & output_iterator is invoked.~%")
	(__set_intersection-imp-09 (__cons-itr-cons first1)
							   (__cons-itr-cons  last1) first2 last2 (clone result) #'operator_<))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 cons_const_iterator) (last1 cons_const_iterator)
										(first2 input_iterator) (last2 input_iterator) (result output_iterator) comp)
	;;(format t "specialized set_intersection for const-vector-pointer & input_iterator & output_iterator is invoked.~%")
	(__set_intersection-imp-09 (__cons-itr-cons first1)
							   (__cons-itr-cons  last1)
							   first2 last2 (clone result) (functor_function (clone comp))))

  ;;PTN; set_intersection : 10 -  cci x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 cons_const_iterator) (last1 cons_const_iterator)
										(first2 input_iterator) (last2 input_iterator) (result cons_iterator))
	;;(format t "specialized set_intersection for const-vector-pointer & input_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__set_intersection-imp-10 (__cons-itr-cons first1)
														  (__cons-itr-cons  last1)
														  first2 last2
														  (__cons-itr-cons result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 cons_const_iterator) (last1 cons_const_iterator)
										(first2 input_iterator) (last2 input_iterator) (result cons_iterator) comp)
	;;(format t "specialized set_intersection for const-vector-pointer & input_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__set_intersection-imp-10 (__cons-itr-cons first1)
														  (__cons-itr-cons  last1)
														  first2 last2
														  (__cons-itr-cons result) (functor_function (clone comp)))))

  ;;PTN; set_intersection : 11 -  cci x  i  x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 cons_const_iterator) (last1 cons_const_iterator)
										(first2 input_iterator) (last2 input_iterator) (result vector-pointer))
	;;(format t "specialized set_intersection for const-vector-pointer & input_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__set_intersection-imp-11 (__cons-itr-cons first1)
														  (__cons-itr-cons  last1)
														  first2 last2
														  (opr::vec-ptr-index  result)
														  (opr::vec-ptr-buffer result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 cons_const_iterator) (last1 cons_const_iterator)
										(first2 input_iterator) (last2 input_iterator) (result vector-pointer) comp)
	;;(format t "specialized set_intersection for const-vector-pointer & input_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__set_intersection-imp-11 (__cons-itr-cons first1)
														  (__cons-itr-cons  last1)
														  first2 last2
														  (opr::vec-ptr-index  result)
														  (opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; set_intersection : 12 -  cci x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 cons_const_iterator) (last1 cons_const_iterator)
										(first2 cons_const_iterator) (last2 cons_const_iterator) (result output_iterator))
	;;(format t "specialized set_intersection for const-vector-pointer & const-vector-pointer & output_iterator is invoked.~%")
	(__set_intersection-imp-12 (__cons-itr-cons first1)
							   (__cons-itr-cons  last1)
							   (__cons-itr-cons first2)
							   (__cons-itr-cons  last2) (clone result) #'operator_<))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 cons_const_iterator) (last1 cons_const_iterator)
										(first2 cons_const_iterator) (last2 cons_const_iterator) (result output_iterator) comp)
	;;(format t "specialized set_intersection for const-vector-pointer & const-vector-pointer & output_iterator is invoked.~%")
	(__set_intersection-imp-12 (__cons-itr-cons first1)
							   (__cons-itr-cons  last1)
							   (__cons-itr-cons first2)
							   (__cons-itr-cons  last2) (clone result) (functor_function (clone comp))))

  ;;PTN; set_intersection : 13 -  cci x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 cons_const_iterator) (last1 cons_const_iterator)
										(first2 cons_const_iterator) (last2 cons_const_iterator) (result cons_iterator))
	;;(format t "specialized set_intersection for const-vector-pointer & const-vector-pointer & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__set_intersection-imp-13 (__cons-itr-cons first1)
														  (__cons-itr-cons  last1)
														  (__cons-itr-cons first2)
														  (__cons-itr-cons  last2)
														  (__cons-itr-cons result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 cons_const_iterator) (last1 cons_const_iterator)
										(first2 cons_const_iterator) (last2 cons_const_iterator) (result cons_iterator) comp)
	;;(format t "specialized set_intersection for const-vector-pointer & const-vector-pointer & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__set_intersection-imp-13 (__cons-itr-cons first1)
														  (__cons-itr-cons  last1)
														  (__cons-itr-cons first2)
														  (__cons-itr-cons  last2)
														  (__cons-itr-cons result) (functor_function (clone comp)))))

  ;;PTN; set_intersection : 14 -  cci x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 cons_const_iterator) (last1 cons_const_iterator)
										(first2 cons_const_iterator) (last2 cons_const_iterator) (result vector-pointer))
	;;(format t "specialized set_intersection for const-vector-pointer & const-vector-pointer & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__set_intersection-imp-14 (__cons-itr-cons     first1)
														  (__cons-itr-cons      last1)
														  (__cons-itr-cons     first2)
														  (__cons-itr-cons      last2)
														  (opr::vec-ptr-index  result)
														  (opr::vec-ptr-buffer result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 cons_const_iterator) (last1 cons_const_iterator)
										(first2 cons_const_iterator) (last2 cons_const_iterator) (result vector-pointer) comp)
	;;(format t "specialized set_intersection for const-vector-pointer & const-vector-pointer & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__set_intersection-imp-14 (__cons-itr-cons     first1)
														  (__cons-itr-cons      last1)
														  (__cons-itr-cons     first2)
														  (__cons-itr-cons      last2)
														  (opr::vec-ptr-index  result)
														  (opr::vec-ptr-buffer result) (functor_function (clone comp)))))
  
  ;;PTN; set_intersection : 15 -  cci x cvp x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 cons_const_iterator) (last1 cons_const_iterator)
										(first2 const-vector-pointer) (last2 const-vector-pointer) (result output_iterator))
	;;(format t "specialized set_intersection for const-vector-pointer & const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__set_intersection-imp-15 (__cons-itr-cons     first1)
							   (__cons-itr-cons      last1)
							   (opr::vec-ptr-index  first2)
							   (opr::vec-ptr-index   last2)
							   (opr::vec-ptr-buffer first2) (clone result) #'operator_<))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 cons_const_iterator) (last1 cons_const_iterator)
										(first2 const-vector-pointer) (last2 const-vector-pointer) (result output_iterator) comp)
	;;(format t "specialized set_intersection for const-vector-pointer & const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__set_intersection-imp-15 (__cons-itr-cons     first1)
							   (__cons-itr-cons      last1)
							   (opr::vec-ptr-index  first2)
							   (opr::vec-ptr-index   last2)
							   (opr::vec-ptr-buffer first2) (clone result) (functor_function (clone comp))))

  ;;PTN; set_intersection : 16 -  cci x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 cons_const_iterator) (last1 cons_const_iterator)
										(first2 const-vector-pointer) (last2 const-vector-pointer) (result cons_iterator))
	;;(format t "specialized set_intersection for const-vector-pointer & const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-cns-iterator result
							   (__set_intersection-imp-16 (__cons-itr-cons     first1)
														  (__cons-itr-cons      last1)
														  (opr::vec-ptr-index  first2)
														  (opr::vec-ptr-index   last2)
														  (opr::vec-ptr-buffer first2)
														  (__cons-itr-cons     result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 cons_const_iterator) (last1 cons_const_iterator)
										(first2 const-vector-pointer) (last2 const-vector-pointer) (result cons_iterator) comp)
	;;(format t "specialized set_intersection for const-vector-pointer & const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-cns-iterator result
							   (__set_intersection-imp-16 (__cons-itr-cons     first1)
														  (__cons-itr-cons      last1)
														  (opr::vec-ptr-index  first2)
														  (opr::vec-ptr-index   last2)
														  (opr::vec-ptr-buffer first2)
														  (__cons-itr-cons     result) (functor_function (clone comp)))))

  ;;PTN; set_intersection : 17 -  cci x cvp x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 cons_const_iterator) (last1 cons_const_iterator)
										(first2 const-vector-pointer) (last2 const-vector-pointer) (result vector-pointer))
	;;(format t "specialized set_intersection for const-vector-pointer & const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-vct-iterator result
							   (__set_intersection-imp-17 (__cons-itr-cons     first1)
														  (__cons-itr-cons      last1)
														  (opr::vec-ptr-index  first2)
														  (opr::vec-ptr-index   last2)
														  (opr::vec-ptr-buffer first2)
														  (opr::vec-ptr-index  result)
														  (opr::vec-ptr-buffer result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 cons_const_iterator) (last1 cons_const_iterator)
										(first2 const-vector-pointer) (last2 const-vector-pointer) (result vector-pointer) comp)
	;;(format t "specialized set_intersection for const-vector-pointer & const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-vct-iterator result
							   (__set_intersection-imp-17 (__cons-itr-cons     first1)
														  (__cons-itr-cons      last1)
														  (opr::vec-ptr-index  first2)
														  (opr::vec-ptr-index   last2)
														  (opr::vec-ptr-buffer first2)
														  (opr::vec-ptr-index  result)
														  (opr::vec-ptr-buffer result) (functor_function (clone comp)))))
  
  ;;PTN; set_intersection : 18 -  cvp x  i  x  o 
  (defmethod-overload set_intersection ((first1 const-vector-pointer) (last1 const-vector-pointer)
										(first2 input_iterator) (last2 input_iterator) (result output_iterator))
	;;(format t "specialized set_intersection for const-vector-pointer & input_iterator & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__set_intersection-imp-18 (opr::vec-ptr-index  first1)
							   (opr::vec-ptr-index  last1)
							   (opr::vec-ptr-buffer first1) first2 last2 (clone result) #'operator_<))

  (defmethod-overload set_intersection ((first1 const-vector-pointer) (last1 const-vector-pointer)
										(first2 input_iterator) (last2 input_iterator) (result output_iterator) comp)
	;;(format t "specialized set_intersection for const-vector-pointer & input_iterator & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__set_intersection-imp-18 (opr::vec-ptr-index  first1)
							   (opr::vec-ptr-index  last1)
							   (opr::vec-ptr-buffer first1)
							   first2 last2 (clone result) (functor_function (clone comp))))

  ;;PTN; set_intersection : 19 -  cvp x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 const-vector-pointer) (last1 const-vector-pointer)
										(first2 input_iterator) (last2 input_iterator) (result cons_iterator))
	;;(format t "specialized set_intersection for const-vector-pointer & input_iterator & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-cns-iterator result
							   (__set_intersection-imp-19 (opr::vec-ptr-index  first1)
														  (opr::vec-ptr-index   last1)
														  (opr::vec-ptr-buffer first1)
														  first2 last2
														  (__cons-itr-cons     result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 const-vector-pointer) (last1 const-vector-pointer)
										(first2 input_iterator) (last2 input_iterator) (result cons_iterator) comp)
	;;(format t "specialized set_intersection for const-vector-pointer & input_iterator & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-cns-iterator result
							   (__set_intersection-imp-19 (opr::vec-ptr-index  first1)
														  (opr::vec-ptr-index   last1)
														  (opr::vec-ptr-buffer first1)
														  first2 last2
														  (__cons-itr-cons     result) (functor_function (clone comp)))))

  ;;PTN; set_intersection : 20 -  cvp x  i  x  vp
  (defmethod-overload set_intersection ((first1 const-vector-pointer) (last1 const-vector-pointer)
										(first2 input_iterator) (last2 input_iterator) (result vector-pointer))
	;;(format t "specialized set_intersection for const-vector-pointer & input_iterator & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-vct-iterator result
							   (__set_intersection-imp-20 (opr::vec-ptr-index  first1)
														  (opr::vec-ptr-index  last1)
														  (opr::vec-ptr-buffer first1)
														  first2 last2
														  (opr::vec-ptr-index  result)
														  (opr::vec-ptr-buffer result) #'operator_<)))

  (defmethod-overload set_intersection ((first1 const-vector-pointer) (last1 const-vector-pointer)
										(first2 input_iterator) (last2 input_iterator) (result vector-pointer) comp)
	;;(format t "specialized set_intersection for const-vector-pointer & input_iterator & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-vct-iterator result
							   (__set_intersection-imp-20 (opr::vec-ptr-index  first1)
														  (opr::vec-ptr-index  last1)
														  (opr::vec-ptr-buffer first1)
														  first2 last2
														  (opr::vec-ptr-index  result)
														  (opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; set_intersection : 21 -  cvp x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 const-vector-pointer) (last1 const-vector-pointer)
										(first2 cons_const_iterator) (last2 cons_const_iterator) (result output_iterator))
	;;(format t "specialized set_intersection for const-vector-pointer & const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__set_intersection-imp-21 (opr::vec-ptr-index  first1)
							   (opr::vec-ptr-index   last1)
							   (opr::vec-ptr-buffer first1)
							   (__cons-itr-cons     first2)
							   (__cons-itr-cons      last2) (clone result) #'operator_<))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 const-vector-pointer) (last1 const-vector-pointer)
										(first2 cons_const_iterator) (last2 cons_const_iterator) (result output_iterator) comp)
	;;(format t "specialized set_intersection for const-vector-pointer & const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__set_intersection-imp-21 (opr::vec-ptr-index  first1)
							   (opr::vec-ptr-index   last1)
							   (opr::vec-ptr-buffer first1)
							   (__cons-itr-cons     first2)
							   (__cons-itr-cons      last2) (clone result) (functor_function (clone comp))))

  ;;PTN; set_intersection : 22 -  cvp x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 const-vector-pointer) (last1 const-vector-pointer)
										(first2 cons_const_iterator) (last2 cons_const_iterator) (result cons_iterator))
	;;(format t "specialized set_intersection for const-vector-pointer & const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-cns-iterator result
							   (__set_intersection-imp-22 (opr::vec-ptr-index  first1)
														  (opr::vec-ptr-index   last1)
														  (opr::vec-ptr-buffer first1)
														  (__cons-itr-cons     first2)
														  (__cons-itr-cons      last2)
														  (__cons-itr-cons     result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 const-vector-pointer) (last1 const-vector-pointer)
										(first2 cons_const_iterator) (last2 cons_const_iterator) (result cons_iterator) comp)
	;;(format t "specialized set_intersection for const-vector-pointer & const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-cns-iterator result
							   (__set_intersection-imp-22 (opr::vec-ptr-index  first1)
														  (opr::vec-ptr-index   last1)
														  (opr::vec-ptr-buffer first1)
														  (__cons-itr-cons     first2)
														  (__cons-itr-cons      last2)
														  (__cons-itr-cons     result) (functor_function (clone comp)))))

  ;;PTN; set_intersection : 23 -  cvp x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 const-vector-pointer) (last1 const-vector-pointer)
										(first2 cons_const_iterator) (last2 cons_const_iterator) (result vector-pointer))
	;;(format t "specialized set_intersection for const-vector-pointer & const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-vct-iterator result
							   (__set_intersection-imp-23 (opr::vec-ptr-index  first1)
														  (opr::vec-ptr-index   last1)
														  (opr::vec-ptr-buffer first1)
														  (__cons-itr-cons     first2)
														  (__cons-itr-cons      last2)
														  (opr::vec-ptr-index  result)
														  (opr::vec-ptr-buffer result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 const-vector-pointer) (last1 const-vector-pointer)
										(first2 cons_const_iterator) (last2 cons_const_iterator) (result vector-pointer) comp)
	;;(format t "specialized set_intersection for const-vector-pointer & const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-vct-iterator result
							   (__set_intersection-imp-23 (opr::vec-ptr-index  first1)
														  (opr::vec-ptr-index   last1)
														  (opr::vec-ptr-buffer first1)
														  (__cons-itr-cons     first2)
														  (__cons-itr-cons      last2)
														  (opr::vec-ptr-index  result)
														  (opr::vec-ptr-buffer result) (functor_function (clone comp)))))
  
  ;;PTN; set_intersection : 24 -  cvp x cvp x  o 
  (defmethod-overload set_intersection ((first1 const-vector-pointer) (last1 const-vector-pointer)
										(first2 const-vector-pointer) (last2 const-vector-pointer) (result output_iterator))
	;;(format t "specialized set_intersection for const-vector-pointer & const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__set_intersection-imp-24 (opr::vec-ptr-index  first1)
							   (opr::vec-ptr-index  last1)
							   (opr::vec-ptr-buffer first1)
							   (opr::vec-ptr-index  first2)
							   (opr::vec-ptr-index  last2)
							   (opr::vec-ptr-buffer first2) (clone result) #'operator_<))

  (defmethod-overload set_intersection ((first1 const-vector-pointer) (last1 const-vector-pointer)
										(first2 const-vector-pointer) (last2 const-vector-pointer) (result output_iterator) comp)
	;;(format t "specialized set_intersection for const-vector-pointer & const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__set_intersection-imp-24 (opr::vec-ptr-index  first1)
							   (opr::vec-ptr-index  last1)
							   (opr::vec-ptr-buffer first1)
							   (opr::vec-ptr-index  first2)
							   (opr::vec-ptr-index  last2)
							   (opr::vec-ptr-buffer first2) (clone result) (functor_function (clone comp))))

  ;;PTN; set_intersection : 25 -  cvp x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 const-vector-pointer) (last1 const-vector-pointer)
										(first2 const-vector-pointer) (last2 const-vector-pointer) (result cons_iterator))
	;;(format t "specialized set_intersection for const-vector-pointer & const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-cns-iterator result
							   (__set_intersection-imp-25 (opr::vec-ptr-index  first1)
														  (opr::vec-ptr-index  last1)
														  (opr::vec-ptr-buffer first1)
														  (opr::vec-ptr-index  first2)
														  (opr::vec-ptr-index  last2)
														  (opr::vec-ptr-buffer first2)
														  (__cons-itr-cons     result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_intersection ((first1 const-vector-pointer) (last1 const-vector-pointer)
										(first2 const-vector-pointer) (last2 const-vector-pointer) (result cons_iterator) comp)
	;;(format t "specialized set_intersection for const-vector-pointer & const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-cns-iterator result
							   (__set_intersection-imp-25 (opr::vec-ptr-index  first1)
														  (opr::vec-ptr-index  last1)
														  (opr::vec-ptr-buffer first1)
														  (opr::vec-ptr-index  first2)
														  (opr::vec-ptr-index  last2)
														  (opr::vec-ptr-buffer first2)
														  (__cons-itr-cons     result) (functor_function (clone comp)))))

  ;;PTN; set_intersection : 26 -  cvp x cvp x  vp
  (defmethod-overload set_intersection ((first1 const-vector-pointer) (last1 const-vector-pointer)
										(first2 const-vector-pointer) (last2 const-vector-pointer) (result vector-pointer))
	;;(format t "specialized set_intersection for const-vector-pointer & const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-vct-iterator result
							   (__set_intersection-imp-26 (opr::vec-ptr-index  first1)
														  (opr::vec-ptr-index  last1)
														  (opr::vec-ptr-buffer first1)
														  (opr::vec-ptr-index  first2)
														  (opr::vec-ptr-index  last2)
														  (opr::vec-ptr-buffer first2)
														  (opr::vec-ptr-index  result)
														  (opr::vec-ptr-buffer result) #'operator_<)))

  (defmethod-overload set_intersection ((first1 const-vector-pointer) (last1 const-vector-pointer)
										(first2 const-vector-pointer) (last2 const-vector-pointer) (result vector-pointer) comp)
	;;(format t "specialized set_intersection for const-vector-pointer & const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-vct-iterator result
							   (__set_intersection-imp-26 (opr::vec-ptr-index  first1)
														  (opr::vec-ptr-index  last1)
														  (opr::vec-ptr-buffer first1)
														  (opr::vec-ptr-index  first2)
														  (opr::vec-ptr-index  last2)
														  (opr::vec-ptr-buffer first2)
														  (opr::vec-ptr-index  result)
														  (opr::vec-ptr-buffer result) (functor_function (clone comp))))))





; first1    : input_iterator
; last1     : input_iterator
; first2    : input_iterator
; last2     : input_iterator
; result    : output_iterator
; comp      : binary_function ( default : #'operator_< )
; returns   : copy of result ( point to end of sequence ).
(locally (declare (optimize speed))

  ;;PTN; set_difference : 00 -   i  x  i  x  o 
  (defmethod-overload set_difference ((first1 input_iterator) (last1 input_iterator)
									  (first2 input_iterator) (last2 input_iterator) (result output_iterator))
	(__set_difference-imp-00 first1 last1 first2 last2 result #'operator_<))

  (defmethod-overload set_difference ((first1 input_iterator) (last1 input_iterator)
									  (first2 input_iterator) (last2 input_iterator) (result output_iterator) comp)
	(__set_difference-imp-00 first1 last1 first2 last2 result (functor_function (clone comp))))

  ;;PTN; set_difference : 01 -   i  x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 input_iterator) (last1 input_iterator)
									  (first2 input_iterator) (last2 input_iterator) (result cons_iterator))
	;;(format t "specialized set_difference for input_iterator & input_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__set_difference-imp-01 first1 last1 first2 last2
														(__cons-itr-cons result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 input_iterator) (last1 input_iterator)
									  (first2 input_iterator) (last2 input_iterator) (result cons_iterator) comp)
	;;(format t "specialized set_difference for input_iterator & input_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__set_difference-imp-01 first1 last1 first2 last2
														(__cons-itr-cons result) (functor_function (clone comp)))))

  ;;PTN; set_difference : 02 -   i  x  i  x  vp
  (defmethod-overload set_difference ((first1 input_iterator) (last1 input_iterator)
									  (first2 input_iterator) (last2 input_iterator) (result vector-pointer))
	;;(format t "specialized set_difference for input_iterator & input_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__set_difference-imp-02 first1 last1 first2 last2
														(opr::vec-ptr-index  result)
														(opr::vec-ptr-buffer result) #'operator_<)))

  (defmethod-overload set_difference ((first1 input_iterator) (last1 input_iterator)
									  (first2 input_iterator) (last2 input_iterator) (result vector-pointer) comp)
	;;(format t "specialized set_difference for input_iterator & input_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__set_difference-imp-02 first1 last1 first2 last2
														(opr::vec-ptr-index  result)
														(opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; set_difference : 03 -   i  x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 input_iterator) (last1 input_iterator)
									  (first2 cons_const_iterator) (last2 cons_const_iterator) (result output_iterator))
	;;(format t "specialized set_difference for input_iterator & cons_const_iterator & output_iterator is invoked.~%")
	(__set_difference-imp-03 first1 last1
							 (__cons-itr-cons first2)
							 (__cons-itr-cons  last2) (clone result) #'operator_<))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 input_iterator) (last1 input_iterator)
									  (first2 cons_const_iterator) (last2 cons_const_iterator) (result output_iterator) comp)
	;;(format t "specialized set_difference for input_iterator & cons_const_iterator & output_iterator is invoked.~%")
	(__set_difference-imp-03 first1 last1
							 (__cons-itr-cons first2)
							 (__cons-itr-cons  last2) (clone result) (functor_function (clone comp))))

  ;;PTN; set_difference : 04 -   i  x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 input_iterator) (last1 input_iterator)
									  (first2 cons_const_iterator) (last2 cons_const_iterator) (result cons_iterator))
	;;(format t "specialized set_difference for input_iterator & cons_const_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__set_difference-imp-04 first1 last1
														(__cons-itr-cons first2)
														(__cons-itr-cons  last2)
														(__cons-itr-cons result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 input_iterator) (last1 input_iterator)
									  (first2 cons_const_iterator) (last2 cons_const_iterator) (result cons_iterator) comp)
	;;(format t "specialized set_difference for input_iterator & cons_const_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__set_difference-imp-04 first1 last1
														(__cons-itr-cons first2)
														(__cons-itr-cons  last2)
														(__cons-itr-cons result) (functor_function (clone comp)))))

  ;;PTN; set_difference : 05 -   i  x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 input_iterator) (last1 input_iterator)
									  (first2 cons_const_iterator) (last2 cons_const_iterator) (result vector-pointer))
	;;(format t "specialized set_difference for input_iterator & cons_const_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__set_difference-imp-05 first1 last1
														(__cons-itr-cons     first2)
														(__cons-itr-cons      last2)
														(opr::vec-ptr-index  result)
														(opr::vec-ptr-buffer result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 input_iterator) (last1 input_iterator)
									  (first2 cons_const_iterator) (last2 cons_const_iterator) (result vector-pointer) comp)
	;;(format t "specialized set_difference for input_iterator & cons_const_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__set_difference-imp-05 first1 last1
														(__cons-itr-cons     first2)
														(__cons-itr-cons      last2)
														(opr::vec-ptr-index  result)
														(opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; set_difference : 06 -   i  x cvp x  o 
  (defmethod-overload set_difference ((first1 input_iterator) (last1 input_iterator)
									  (first2 const-vector-pointer) (last2 const-vector-pointer) (result output_iterator))
	;;(format t "specialized set_difference for input_iterator & const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__set_difference-imp-06 first1 last1
							 (opr::vec-ptr-index  first2)
							 (opr::vec-ptr-index  last2)
							 (opr::vec-ptr-buffer first2) (clone result) #'operator_<))

  (defmethod-overload set_difference ((first1 input_iterator) (last1 input_iterator)
									  (first2 const-vector-pointer) (last2 const-vector-pointer) (result output_iterator) comp)
	;;(format t "specialized set_difference for input_iterator & const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__set_difference-imp-06 first1 last1
							 (opr::vec-ptr-index  first2)
							 (opr::vec-ptr-index  last2)
							 (opr::vec-ptr-buffer first2) (clone result) (functor_function (clone comp))))

  ;;PTN; set_difference : 07 -   i  x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 input_iterator) (last1 input_iterator)
									  (first2 const-vector-pointer) (last2 const-vector-pointer) (result cons_iterator))
	;;(format t "specialized set_difference for input_iterator & const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-cns-iterator result
							   (__set_difference-imp-07 first1 last1
														(opr::vec-ptr-index  first2)
														(opr::vec-ptr-index   last2)
														(opr::vec-ptr-buffer first2)
														(__cons-itr-cons     result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 input_iterator) (last1 input_iterator)
									  (first2 const-vector-pointer) (last2 const-vector-pointer) (result cons_iterator) comp)
	;;(format t "specialized set_difference for input_iterator & const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-cns-iterator result
							   (__set_difference-imp-07 first1 last1
														(opr::vec-ptr-index  first2)
														(opr::vec-ptr-index   last2)
														(opr::vec-ptr-buffer first2)
														(__cons-itr-cons     result) (functor_function (clone comp)))))

  ;;PTN; set_difference : 08 -   i  x cvp x  vp
  (defmethod-overload set_difference ((first1 input_iterator) (last1 input_iterator)
									  (first2 const-vector-pointer) (last2 const-vector-pointer) (result vector-pointer))
	;;(format t "specialized set_difference for input_iterator & const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-vct-iterator result
							   (__set_difference-imp-08 first1 last1
														(opr::vec-ptr-index  first2)
														(opr::vec-ptr-index  last2)
														(opr::vec-ptr-buffer first2)
														(opr::vec-ptr-index  result)
														(opr::vec-ptr-buffer result) #'operator_<)))

  (defmethod-overload set_difference ((first1 input_iterator) (last1 input_iterator)
									  (first2 const-vector-pointer) (last2 const-vector-pointer) (result vector-pointer) comp)
	;;(format t "specialized set_difference for input_iterator & const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-vct-iterator result
							   (__set_difference-imp-08 first1 last1
														(opr::vec-ptr-index  first2)
														(opr::vec-ptr-index  last2)
														(opr::vec-ptr-buffer first2)
														(opr::vec-ptr-index  result)
														(opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; set_difference : 09 -  cci x  i  x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
									  (first2 input_iterator) (last2 input_iterator) (result output_iterator))
	;;(format t "specialized set_difference for const-vector-pointer & input_iterator & output_iterator is invoked.~%")
	(__set_difference-imp-09 (__cons-itr-cons first1)
							 (__cons-itr-cons  last1) first2 last2 (clone result) #'operator_<))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
									  (first2 input_iterator) (last2 input_iterator) (result output_iterator) comp)
	;;(format t "specialized set_difference for const-vector-pointer & input_iterator & output_iterator is invoked.~%")
	(__set_difference-imp-09 (__cons-itr-cons first1)
							 (__cons-itr-cons  last1)
							 first2 last2 (clone result) (functor_function (clone comp))))

  ;;PTN; set_difference : 10 -  cci x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
									  (first2 input_iterator) (last2 input_iterator) (result cons_iterator))
	;;(format t "specialized set_difference for const-vector-pointer & input_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__set_difference-imp-10 (__cons-itr-cons first1)
														(__cons-itr-cons  last1)
														first2 last2
														(__cons-itr-cons     result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
									  (first2 input_iterator) (last2 input_iterator) (result cons_iterator) comp)
	;;(format t "specialized set_difference for const-vector-pointer & input_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__set_difference-imp-10 (__cons-itr-cons first1)
														(__cons-itr-cons  last1)
														first2 last2
														(__cons-itr-cons     result) (functor_function (clone comp)))))

  ;;PTN; set_difference : 11 -  cci x  i  x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
									  (first2 input_iterator) (last2 input_iterator) (result vector-pointer))
	;;(format t "specialized set_difference for const-vector-pointer & input_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__set_difference-imp-11 (__cons-itr-cons first1)
														(__cons-itr-cons  last1)
														first2 last2
														(opr::vec-ptr-index  result)
														(opr::vec-ptr-buffer result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
									  (first2 input_iterator) (last2 input_iterator) (result vector-pointer) comp)
	;;(format t "specialized set_difference for const-vector-pointer & input_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__set_difference-imp-11 (__cons-itr-cons first1)
														(__cons-itr-cons  last1)
														first2 last2
														(opr::vec-ptr-index  result)
														(opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; set_difference : 12 -  cci x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
									  (first2 cons_const_iterator) (last2 cons_const_iterator)
									  (result output_iterator))
	;;(format t "specialized set_difference for const-vector-pointer & cons_const_iterator & output_iterator is invoked.~%")
	(__set_difference-imp-12 (__cons-itr-cons first1)
							 (__cons-itr-cons  last1)
							 (__cons-itr-cons first2)
							 (__cons-itr-cons  last2) (clone result) #'operator_<))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
									  (first2 cons_const_iterator) (last2 cons_const_iterator)
									  (result output_iterator) comp)
	;;(format t "specialized set_difference for const-vector-pointer & cons_const_iterator & output_iterator is invoked.~%")
	(__set_difference-imp-12 (__cons-itr-cons first1)
							 (__cons-itr-cons  last1)
							 (__cons-itr-cons first2)
							 (__cons-itr-cons  last2) (clone result) (functor_function (clone comp))))

  ;;PTN; set_difference : 13 -  cci x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
									  (first2 cons_const_iterator) (last2 cons_const_iterator) (result cons_iterator))
	;;(format t "specialized set_difference for const-vector-pointer & cons_const_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__set_difference-imp-13 (__cons-itr-cons first1)
														(__cons-itr-cons  last1)
														(__cons-itr-cons first2)
														(__cons-itr-cons  last2)
														(__cons-itr-cons result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
									  (first2 cons_const_iterator) (last2 cons_const_iterator) (result cons_iterator) comp)
	;;(format t "specialized set_difference for const-vector-pointer & cons_const_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__set_difference-imp-13 (__cons-itr-cons first1)
														(__cons-itr-cons  last1)
														(__cons-itr-cons first2)
														(__cons-itr-cons  last2)
														(__cons-itr-cons result) (functor_function (clone comp)))))

  ;;PTN; set_difference : 14 -  cci x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
									  (first2 cons_const_iterator) (last2 cons_const_iterator) (result vector-pointer))
	;;(format t "specialized set_difference for const-vector-pointer & cons_const_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__set_difference-imp-14 (__cons-itr-cons     first1)
														(__cons-itr-cons      last1)
														(__cons-itr-cons     first2)
														(__cons-itr-cons      last2)
														(opr::vec-ptr-index  result)
														(opr::vec-ptr-buffer result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
									  (first2 cons_const_iterator) (last2 cons_const_iterator)
									  (result vector-pointer) comp)
	;;(format t "specialized set_difference for const-vector-pointer & cons_const_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__set_difference-imp-14 (__cons-itr-cons     first1)
														(__cons-itr-cons      last1)
														(__cons-itr-cons     first2)
														(__cons-itr-cons      last2)
														(opr::vec-ptr-index  result)
														(opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; set_difference : 15 -  cci x cvp x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
									  (first2 const-vector-pointer) (last2 const-vector-pointer)
									  (result output_iterator))
	;;(format t "specialized set_difference for const-vector-pointer & const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__set_difference-imp-15 (__cons-itr-cons     first1)
							 (__cons-itr-cons      last1)
							 (opr::vec-ptr-index  first2)
							 (opr::vec-ptr-index   last2)
							 (opr::vec-ptr-buffer first2) (clone result) #'operator_<))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
									  (first2 const-vector-pointer) (last2 const-vector-pointer)
									  (result output_iterator) comp)
	;;(format t "specialized set_difference for const-vector-pointer & const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__set_difference-imp-15 (__cons-itr-cons     first1)
							 (__cons-itr-cons      last1)
							 (opr::vec-ptr-index  first2)
							 (opr::vec-ptr-index   last2)
							 (opr::vec-ptr-buffer first2) (clone result) (functor_function (clone comp))))

  ;;PTN; set_difference : 16 -  cci x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
									  (first2 const-vector-pointer) (last2 const-vector-pointer) (result cons_iterator))
	;;(format t "specialized set_difference for const-vector-pointer & const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-cns-iterator result
							   (__set_difference-imp-16 (__cons-itr-cons     first1)
														(__cons-itr-cons      last1)
														(opr::vec-ptr-index  first2)
														(opr::vec-ptr-index   last2)
														(opr::vec-ptr-buffer first2)
														(__cons-itr-cons     result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
									  (first2 const-vector-pointer) (last2 const-vector-pointer) (result cons_iterator) comp)
	;;(format t "specialized set_difference for const-vector-pointer & const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-cns-iterator result
							   (__set_difference-imp-16 (__cons-itr-cons     first1)
														(__cons-itr-cons      last1)
														(opr::vec-ptr-index  first2)
														(opr::vec-ptr-index   last2)
														(opr::vec-ptr-buffer first2)
														(__cons-itr-cons     result) (functor_function (clone comp)))))

  ;;PTN; set_difference : 17 -  cci x cvp x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
									  (first2 const-vector-pointer) (last2 const-vector-pointer) (result vector-pointer))
	;;(format t "specialized set_difference for const-vector-pointer & const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-vct-iterator result
							   (__set_difference-imp-17 (__cons-itr-cons     first1)
														(__cons-itr-cons      last1)
														(opr::vec-ptr-index  first2)
														(opr::vec-ptr-index   last2)
														(opr::vec-ptr-buffer first2)
														(opr::vec-ptr-index  result)
														(opr::vec-ptr-buffer result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
									  (first2 const-vector-pointer) (last2 const-vector-pointer)
									  (result vector-pointer) comp)
	;;(format t "specialized set_difference for const-vector-pointer & const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-vct-iterator result
							   (__set_difference-imp-17 (__cons-itr-cons     first1)
														(__cons-itr-cons      last1)
														(opr::vec-ptr-index  first2)
														(opr::vec-ptr-index   last2)
														(opr::vec-ptr-buffer first2)
														(opr::vec-ptr-index  result)
														(opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; set_difference : 18 -  cvp x  i  x  o 
  (defmethod-overload set_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
									  (first2 input_iterator) (last2 input_iterator) (result output_iterator))
	;;(format t "specialized set_difference for const-vector-pointer & input_iterator & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__set_difference-imp-18 (opr::vec-ptr-index  first1)
							 (opr::vec-ptr-index  last1)
							 (opr::vec-ptr-buffer first1) first2 last2 (clone result) #'operator_<))

  (defmethod-overload set_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
									  (first2 input_iterator) (last2 input_iterator) (result output_iterator) comp)
	;;(format t "specialized set_difference for const-vector-pointer & input_iterator & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__set_difference-imp-18 (opr::vec-ptr-index  first1)
							 (opr::vec-ptr-index  last1)
							 (opr::vec-ptr-buffer first1)
							 first2 last2 (clone result) (functor_function (clone comp))))

  ;;PTN; set_difference : 19 -  cvp x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
									  (first2 input_iterator) (last2 input_iterator) (result cons_iterator))
	;;(format t "specialized set_difference for const-vector-pointer & input_iterator & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-cns-iterator result
							   (__set_difference-imp-19 (opr::vec-ptr-index  first1)
														(opr::vec-ptr-index   last1)
														(opr::vec-ptr-buffer first1)
														first2 last2
														(__cons-itr-cons     result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
									  (first2 input_iterator) (last2 input_iterator) (result cons_iterator) comp)
	;;(format t "specialized set_difference for const-vector-pointer & input_iterator & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-cns-iterator result
							   (__set_difference-imp-19 (opr::vec-ptr-index  first1)
														(opr::vec-ptr-index   last1)
														(opr::vec-ptr-buffer first1)
														first2 last2
														(__cons-itr-cons     result) (functor_function (clone comp)))))

  ;;PTN; set_difference : 20 -  cvp x  i  x  vp
  (defmethod-overload set_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
									  (first2 input_iterator) (last2 input_iterator) (result vector-pointer))
	;;(format t "specialized set_difference for const-vector-pointer & input_iterator & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-vct-iterator result
							   (__set_difference-imp-20 (opr::vec-ptr-index  first1)
														(opr::vec-ptr-index  last1)
														(opr::vec-ptr-buffer first1)
														first2 last2
														(opr::vec-ptr-index  result)
														(opr::vec-ptr-buffer result) #'operator_<)))

  (defmethod-overload set_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
									  (first2 input_iterator) (last2 input_iterator) (result vector-pointer) comp)
	;;(format t "specialized set_difference for const-vector-pointer & input_iterator & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-vct-iterator result
							   (__set_difference-imp-20 (opr::vec-ptr-index  first1)
														(opr::vec-ptr-index  last1)
														(opr::vec-ptr-buffer first1)
														first2 last2
														(opr::vec-ptr-index  result)
														(opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; set_difference : 21 -  cvp x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
									  (first2 cons_const_iterator) (last2 cons_const_iterator)
									  (result output_iterator))
	;;(format t "specialized set_difference for const-vector-pointer & cons_const_iterator & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__set_difference-imp-21 (opr::vec-ptr-index  first1)
							 (opr::vec-ptr-index   last1)
							 (opr::vec-ptr-buffer first1)
							 (__cons-itr-cons     first2)
							 (__cons-itr-cons      last2) (clone result) #'operator_<))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
									  (first2 cons_const_iterator) (last2 cons_const_iterator)
									  (result output_iterator) comp)
	;;(format t "specialized set_difference for const-vector-pointer & cons_const_iterator & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__set_difference-imp-21 (opr::vec-ptr-index  first1)
							 (opr::vec-ptr-index   last1)
							 (opr::vec-ptr-buffer first1)
							 (__cons-itr-cons     first2)
							 (__cons-itr-cons      last2) (clone result) (functor_function (clone comp))))

  ;;PTN; set_difference : 22 -  cvp x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
									  (first2 cons_const_iterator) (last2 cons_const_iterator) (result cons_iterator))
	;;(format t "specialized set_difference for const-vector-pointer & cons_const_iterator & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-cns-iterator result
							   (__set_difference-imp-22 (opr::vec-ptr-index  first1)
														(opr::vec-ptr-index   last1)
														(opr::vec-ptr-buffer first1)
														(__cons-itr-cons     first2)
														(__cons-itr-cons      last2)
														(__cons-itr-cons     result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
									  (first2 cons_const_iterator) (last2 cons_const_iterator) (result cons_iterator) comp)
	;;(format t "specialized set_difference for const-vector-pointer & cons_const_iterator & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-cns-iterator result
							   (__set_difference-imp-22 (opr::vec-ptr-index  first1)
														(opr::vec-ptr-index   last1)
														(opr::vec-ptr-buffer first1)
														(__cons-itr-cons     first2)
														(__cons-itr-cons      last2)
														(__cons-itr-cons     result) (functor_function (clone comp)))))

  ;;PTN; set_difference : 23 -  cvp x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
									  (first2 cons_const_iterator) (last2 cons_const_iterator) (result vector-pointer))
	;;(format t "specialized set_difference for const-vector-pointer & cons_const_iterator & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-vct-iterator result
							   (__set_difference-imp-23 (opr::vec-ptr-index  first1)
														(opr::vec-ptr-index   last1)
														(opr::vec-ptr-buffer first1)
														(__cons-itr-cons     first2)
														(__cons-itr-cons      last2)
														(opr::vec-ptr-index  result)
														(opr::vec-ptr-buffer result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
									  (first2 cons_const_iterator) (last2 cons_const_iterator)
									  (result vector-pointer) comp)
	;;(format t "specialized set_difference for const-vector-pointer & cons_const_iterator & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-vct-iterator result
							   (__set_difference-imp-23 (opr::vec-ptr-index  first1)
														(opr::vec-ptr-index   last1)
														(opr::vec-ptr-buffer first1)
														(__cons-itr-cons     first2)
														(__cons-itr-cons      last2)
														(opr::vec-ptr-index  result)
														(opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; set_difference : 24 -  cvp x cvp x  o 
  (defmethod-overload set_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
									  (first2 const-vector-pointer) (last2 const-vector-pointer)
									  (result output_iterator))
	;;(format t "specialized set_difference for const-vector-pointer & const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__set_difference-imp-24 (opr::vec-ptr-index  first1)
							 (opr::vec-ptr-index  last1)
							 (opr::vec-ptr-buffer first1)
							 (opr::vec-ptr-index  first2)
							 (opr::vec-ptr-index  last2)
							 (opr::vec-ptr-buffer first2) (clone result) #'operator_<))

  (defmethod-overload set_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
									  (first2 const-vector-pointer) (last2 const-vector-pointer)
									  (result output_iterator) comp)
	;;(format t "specialized set_difference for const-vector-pointer & const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__set_difference-imp-24 (opr::vec-ptr-index  first1)
							 (opr::vec-ptr-index  last1)
							 (opr::vec-ptr-buffer first1)
							 (opr::vec-ptr-index  first2)
							 (opr::vec-ptr-index  last2)
							 (opr::vec-ptr-buffer first2) (clone result) (functor_function (clone comp))))

  ;;PTN; set_difference : 25 -  cvp x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
									  (first2 const-vector-pointer) (last2 const-vector-pointer) (result cons_iterator))
	;;(format t "specialized set_difference for const-vector-pointer & const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-cns-iterator result
							   (__set_difference-imp-25 (opr::vec-ptr-index  first1)
														(opr::vec-ptr-index   last1)
														(opr::vec-ptr-buffer first1)
														(opr::vec-ptr-index  first2)
														(opr::vec-ptr-index   last2)
														(opr::vec-ptr-buffer first2)
														(__cons-itr-cons     result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
									  (first2 const-vector-pointer) (last2 const-vector-pointer) (result cons_iterator) comp)
	;;(format t "specialized set_difference for const-vector-pointer & const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-cns-iterator result
							   (__set_difference-imp-25 (opr::vec-ptr-index  first1)
														(opr::vec-ptr-index  last1)
														(opr::vec-ptr-buffer first1)
														(opr::vec-ptr-index  first2)
														(opr::vec-ptr-index  last2)
														(opr::vec-ptr-buffer first2)
														(__cons-itr-cons     result) (functor_function (clone comp)))))

  ;;PTN; set_difference : 26 -  cvp x cvp x  vp
  (defmethod-overload set_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
									  (first2 const-vector-pointer) (last2 const-vector-pointer) (result vector-pointer))
	;;(format t "specialized set_difference for const-vector-pointer & const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-vct-iterator result
							   (__set_difference-imp-26 (opr::vec-ptr-index  first1)
														(opr::vec-ptr-index  last1)
														(opr::vec-ptr-buffer first1)
														(opr::vec-ptr-index  first2)
														(opr::vec-ptr-index  last2)
														(opr::vec-ptr-buffer first2)
														(opr::vec-ptr-index  result)
														(opr::vec-ptr-buffer result) #'operator_<)))

  (defmethod-overload set_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
									  (first2 const-vector-pointer) (last2 const-vector-pointer)
									  (result vector-pointer) comp)
	;;(format t "specialized set_difference for const-vector-pointer & const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-vct-iterator result
							   (__set_difference-imp-26 (opr::vec-ptr-index  first1)
														(opr::vec-ptr-index  last1)
														(opr::vec-ptr-buffer first1)
														(opr::vec-ptr-index  first2)
														(opr::vec-ptr-index  last2)
														(opr::vec-ptr-buffer first2)
														(opr::vec-ptr-index  result)
														(opr::vec-ptr-buffer result) (functor_function (clone comp))))))



; first1    : input_iterator
; last1     : input_iterator
; first2    : input_iterator
; last2     : input_iterator
; result    : output_iterator
; comp      : binary_function ( default : #'operator_< )
; returns   : copy of result ( point to end of sequence ).
(locally (declare (optimize speed))

  ;;PTN; set_symmetric_difference : 00 -   i  x  i  x  o 
  (defmethod-overload set_symmetric_difference ((first1 input_iterator) (last1 input_iterator)
												(first2 input_iterator) (last2 input_iterator) (result output_iterator))
	(__set_symmetric_difference-imp-00 first1 last1 first2 last2 result #'operator_<))

  (defmethod-overload set_symmetric_difference ((first1 input_iterator) (last1 input_iterator)
												(first2 input_iterator) (last2 input_iterator) (result output_iterator) comp)
	(__set_symmetric_difference-imp-00 first1 last1 first2 last2 result (functor_function (clone comp))))

  ;;PTN; set_symmetric_difference : 01 -   i  x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 input_iterator) (last1 input_iterator)
												(first2 input_iterator) (last2 input_iterator) (result cons_iterator))
	;;(format t "specialized set_symmetric_difference for input_iterator & input_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__set_symmetric_difference-imp-01 first1 last1 first2 last2
																  (__cons-itr-cons result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 input_iterator) (last1 input_iterator)
												(first2 input_iterator) (last2 input_iterator) (result cons_iterator) comp)
	;;(format t "specialized set_symmetric_difference for input_iterator & input_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__set_symmetric_difference-imp-01 first1 last1 first2 last2
																  (__cons-itr-cons result) (functor_function (clone comp)))))

  ;;PTN; set_symmetric_difference : 02 -   i  x  i  x  vp
  (defmethod-overload set_symmetric_difference ((first1 input_iterator) (last1 input_iterator)
												(first2 input_iterator) (last2 input_iterator) (result vector-pointer))
	;;(format t "specialized set_symmetric_difference for input_iterator & input_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__set_symmetric_difference-imp-02 first1 last1 first2 last2
																  (opr::vec-ptr-index  result)
																  (opr::vec-ptr-buffer result) #'operator_<)))

  (defmethod-overload set_symmetric_difference ((first1 input_iterator) (last1 input_iterator)
												(first2 input_iterator) (last2 input_iterator) (result vector-pointer) comp)
	;;(format t "specialized set_symmetric_difference for input_iterator & input_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__set_symmetric_difference-imp-02 first1 last1 first2 last2
																  (opr::vec-ptr-index  result)
																  (opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; set_symmetric_difference : 03 -   i  x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 input_iterator) (last1 input_iterator)
												(first2 cons_const_iterator) (last2 cons_const_iterator) (result output_iterator))
	;;(format t "specialized set_symmetric_difference for input_iterator & cons_const_iterator & output_iterator is invoked.~%")
	(__set_symmetric_difference-imp-03 first1 last1
									   (__cons-itr-cons first2)
									   (__cons-itr-cons  last2) (clone result) #'operator_<))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 input_iterator) (last1 input_iterator)
												(first2 cons_const_iterator) (last2 cons_const_iterator) (result output_iterator) comp)
	;;(format t "specialized set_symmetric_difference for input_iterator & cons_const_iterator & output_iterator is invoked.~%")
	(__set_symmetric_difference-imp-03 first1 last1
									   (__cons-itr-cons first2)
									   (__cons-itr-cons  last2) (clone result) (functor_function (clone comp))))


  ;;PTN; set_symmetric_difference : 04 -   i  x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 input_iterator) (last1 input_iterator)
												(first2 cons_const_iterator) (last2 cons_const_iterator) (result cons_iterator))
	;;(format t "specialized set_symmetric_difference for input_iterator & cons_const_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__set_symmetric_difference-imp-04 first1 last1
																  (__cons-itr-cons first2)
																  (__cons-itr-cons  last2)
																  (__cons-itr-cons result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 input_iterator) (last1 input_iterator)
												(first2 cons_const_iterator) (last2 cons_const_iterator) (result cons_iterator) comp)
	;;(format t "specialized set_symmetric_difference for input_iterator & cons_const_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__set_symmetric_difference-imp-04 first1 last1
																  (__cons-itr-cons first2)
																  (__cons-itr-cons  last2)
																  (__cons-itr-cons result) (functor_function (clone comp)))))

  ;;PTN; set_symmetric_difference : 05 -   i  x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 input_iterator) (last1 input_iterator)
												(first2 cons_const_iterator) (last2 cons_const_iterator) (result vector-pointer))
	;;(format t "specialized set_symmetric_difference for input_iterator & cons_const_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__set_symmetric_difference-imp-05 first1 last1
																  (__cons-itr-cons     first2)
																  (__cons-itr-cons      last2)
																  (opr::vec-ptr-index  result)
																  (opr::vec-ptr-buffer result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 input_iterator) (last1 input_iterator)
												(first2 cons_const_iterator) (last2 cons_const_iterator) (result vector-pointer) comp)
	;;(format t "specialized set_symmetric_difference for input_iterator & cons_const_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__set_symmetric_difference-imp-05 first1 last1
																  (__cons-itr-cons     first2)
																  (__cons-itr-cons      last2)
																  (opr::vec-ptr-index  result)
																  (opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; set_symmetric_difference : 06 -   i  x cvp x  o 
  (defmethod-overload set_symmetric_difference ((first1 input_iterator) (last1 input_iterator)
												(first2 const-vector-pointer) (last2 const-vector-pointer) (result output_iterator))
	;;(format t "specialized set_symmetric_difference for input_iterator & const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__set_symmetric_difference-imp-06 first1 last1
									   (opr::vec-ptr-index  first2)
									   (opr::vec-ptr-index  last2)
									   (opr::vec-ptr-buffer first2) (clone result) #'operator_<))

  (defmethod-overload set_symmetric_difference ((first1 input_iterator) (last1 input_iterator)
												(first2 const-vector-pointer) (last2 const-vector-pointer) (result output_iterator) comp)
	;;(format t "specialized set_symmetric_difference for input_iterator & const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__set_symmetric_difference-imp-06 first1 last1
									   (opr::vec-ptr-index  first2)
									   (opr::vec-ptr-index  last2)
									   (opr::vec-ptr-buffer first2) (clone result) (functor_function (clone comp))))


  ;;PTN; set_symmetric_difference : 07 -   i  x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 input_iterator) (last1 input_iterator)
												(first2 const-vector-pointer) (last2 const-vector-pointer) (result cons_iterator))
	;;(format t "specialized set_symmetric_difference for input_iterator & const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-cns-iterator result
							   (__set_symmetric_difference-imp-07 first1 last1
																  (opr::vec-ptr-index  first2)
																  (opr::vec-ptr-index   last2)
																  (opr::vec-ptr-buffer first2)
																  (__cons-itr-cons     result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 input_iterator) (last1 input_iterator)
												(first2 const-vector-pointer) (last2 const-vector-pointer) (result cons_iterator) comp)
	;;(format t "specialized set_symmetric_difference for input_iterator & const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-cns-iterator result
							   (__set_symmetric_difference-imp-07 first1 last1
																  (opr::vec-ptr-index  first2)
																  (opr::vec-ptr-index   last2)
																  (opr::vec-ptr-buffer first2)
																  (__cons-itr-cons     result) (functor_function (clone comp)))))

  ;;PTN; set_symmetric_difference : 08 -   i  x cvp x  vp
  (defmethod-overload set_symmetric_difference ((first1 input_iterator) (last1 input_iterator)
												(first2 const-vector-pointer) (last2 const-vector-pointer) (result vector-pointer))
	;;(format t "specialized set_symmetric_difference for input_iterator & const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-vct-iterator result
							   (__set_symmetric_difference-imp-08 first1 last1
																  (opr::vec-ptr-index  first2)
																  (opr::vec-ptr-index  last2)
																  (opr::vec-ptr-buffer first2)
																  (opr::vec-ptr-index  result)
																  (opr::vec-ptr-buffer result) #'operator_<)))

  (defmethod-overload set_symmetric_difference ((first1 input_iterator) (last1 input_iterator)
												(first2 const-vector-pointer) (last2 const-vector-pointer) (result vector-pointer) comp)
	;;(format t "specialized set_symmetric_difference for input_iterator & const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-vct-iterator result
							   (__set_symmetric_difference-imp-08 first1 last1
																  (opr::vec-ptr-index  first2)
																  (opr::vec-ptr-index  last2)
																  (opr::vec-ptr-buffer first2)
																  (opr::vec-ptr-index  result)
																  (opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; set_symmetric_difference : 09 -  cci x  i  x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
												(first2 input_iterator) (last2 input_iterator) (result output_iterator))
	;;(format t "specialized set_symmetric_difference for cons_const_iterator & input_iterator & output_iterator is invoked.~%")
	(__set_symmetric_difference-imp-09 (__cons-itr-cons first1)
									   (__cons-itr-cons  last1) first2 last2 (clone result) #'operator_<))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
												(first2 input_iterator) (last2 input_iterator) (result output_iterator) comp)
	;;(format t "specialized set_symmetric_difference for cons_const_iterator & input_iterator & output_iterator is invoked.~%")
	(__set_symmetric_difference-imp-09 (__cons-itr-cons first1)
									   (__cons-itr-cons  last1)
									   first2 last2  (clone result) (functor_function (clone comp))))


  ;;PTN; set_symmetric_difference : 10 -  cci x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
												(first2 input_iterator) (last2 input_iterator) (result cons_iterator))
	;;(format t "specialized set_symmetric_difference for cons_const_iterator & input_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__set_symmetric_difference-imp-10 (__cons-itr-cons first1)
																  (__cons-itr-cons  last1)
																  first2 last2
																  (__cons-itr-cons result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
												(first2 input_iterator) (last2 input_iterator) (result cons_iterator) comp)
	;;(format t "specialized set_symmetric_difference for cons_const_iterator & input_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__set_symmetric_difference-imp-10 (__cons-itr-cons first1)
																  (__cons-itr-cons  last1)
																  first2 last2
																  (__cons-itr-cons result) (functor_function (clone comp)))))

  ;;PTN; set_symmetric_difference : 11 -  cci x  i  x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
												(first2 input_iterator) (last2 input_iterator) (result vector-pointer))
	;;(format t "specialized set_symmetric_difference for cons_const_iterator & input_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__set_symmetric_difference-imp-11 (__cons-itr-cons first1)
																  (__cons-itr-cons  last1)
																  first2 last2
																  (opr::vec-ptr-index  result)
																  (opr::vec-ptr-buffer result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
												(first2 input_iterator) (last2 input_iterator) (result vector-pointer) comp)
	;;(format t "specialized set_symmetric_difference for cons_const_iterator & input_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__set_symmetric_difference-imp-11 (__cons-itr-cons first1)
																  (__cons-itr-cons  last1)
																  first2 last2
																  (opr::vec-ptr-index  result)
																  (opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; set_symmetric_difference : 12 -  cci x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
												(first2 cons_const_iterator) (last2 cons_const_iterator) (result output_iterator))
	;;(format t "specialized set_symmetric_difference for cons_const_iterator & cons_const_iterator & output_iterator is invoked.~%")
	(__set_symmetric_difference-imp-12 (__cons-itr-cons first1)
									   (__cons-itr-cons  last1)
									   (__cons-itr-cons first2)
									   (__cons-itr-cons  last2) (clone result) #'operator_<))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
												(first2 cons_const_iterator) (last2 cons_const_iterator) (result output_iterator) comp)
	;;(format t "specialized set_symmetric_difference for cons_const_iterator & cons_const_iterator & output_iterator is invoked.~%")
	(__set_symmetric_difference-imp-12 (__cons-itr-cons first1)
									   (__cons-itr-cons  last1)
									   (__cons-itr-cons first2)
									   (__cons-itr-cons  last2) (clone result) (functor_function (clone comp))))

  ;;PTN; set_symmetric_difference : 13 -  cci x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
												(first2 cons_const_iterator) (last2 cons_const_iterator) (result cons_iterator))
	;;(format t "specialized set_symmetric_difference for cons_const_iterator & cons_const_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__set_symmetric_difference-imp-13 (__cons-itr-cons first1)
																  (__cons-itr-cons  last1)
																  (__cons-itr-cons first2)
																  (__cons-itr-cons  last2)
																  (__cons-itr-cons result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
												(first2 cons_const_iterator) (last2 cons_const_iterator) (result cons_iterator) comp)
	;;(format t "specialized set_symmetric_difference for cons_const_iterator & cons_const_iterator & cons_iterator is invoked.~%")
	(__algo-make-cns-iterator result
							   (__set_symmetric_difference-imp-13 (__cons-itr-cons first1)
																  (__cons-itr-cons  last1)
																  (__cons-itr-cons first2)
																  (__cons-itr-cons  last2)
																  (__cons-itr-cons result) (functor_function (clone comp)))))

  ;;PTN; set_symmetric_difference : 14 -  cci x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
												(first2 cons_const_iterator) (last2 cons_const_iterator) (result vector-pointer))
	;;(format t "specialized set_symmetric_difference for cons_const_iterator & cons_const_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__set_symmetric_difference-imp-14 (__cons-itr-cons     first1)
																  (__cons-itr-cons      last1)
																  (__cons-itr-cons     first2)
																  (__cons-itr-cons      last2)
																  (opr::vec-ptr-index  result)
																  (opr::vec-ptr-buffer result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
												(first2 cons_const_iterator) (last2 cons_const_iterator) (result vector-pointer) comp)
	;;(format t "specialized set_symmetric_difference for cons_const_iterator & cons_const_iterator & vector-pointer is invoked.~%")
	(__algo-make-vct-iterator result
							   (__set_symmetric_difference-imp-14 (__cons-itr-cons     first1)
																  (__cons-itr-cons      last1)
																  (__cons-itr-cons     first2)
																  (__cons-itr-cons      last2)
																  (opr::vec-ptr-index  result)
																  (opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; set_symmetric_difference : 15 -  cci x cvp x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
												(first2 const-vector-pointer) (last2 const-vector-pointer) (result output_iterator))
	;;(format t "specialized set_symmetric_difference for cons_const_iterator & const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__set_symmetric_difference-imp-15 (__cons-itr-cons first1)
									   (__cons-itr-cons  last1)
									   (opr::vec-ptr-index  first2)
									   (opr::vec-ptr-index  last2)
									   (opr::vec-ptr-buffer first2) (clone result) #'operator_<))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
												(first2 const-vector-pointer) (last2 const-vector-pointer)
												(result output_iterator) comp)
	;;(format t "specialized set_symmetric_difference for cons_const_iterator & const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__set_symmetric_difference-imp-15 (__cons-itr-cons first1)
									   (__cons-itr-cons  last1)
									   (opr::vec-ptr-index  first2)
									   (opr::vec-ptr-index  last2)
									   (opr::vec-ptr-buffer first2) (clone result) (functor_function (clone comp))))

  ;;PTN; set_symmetric_difference : 16 -  cci x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
												(first2 const-vector-pointer) (last2 const-vector-pointer) (result cons_iterator))
	;;(format t "specialized set_symmetric_difference for cons_const_iterator & const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-cns-iterator result
							   (__set_symmetric_difference-imp-16 (__cons-itr-cons first1)
																  (__cons-itr-cons  last1)
																  (opr::vec-ptr-index  first2)
																  (opr::vec-ptr-index   last2)
																  (opr::vec-ptr-buffer first2)
																  (__cons-itr-cons     result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
												(first2 const-vector-pointer) (last2 const-vector-pointer) (result cons_iterator) comp)
	;;(format t "specialized set_symmetric_difference for cons_const_iterator & const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-cns-iterator result
							   (__set_symmetric_difference-imp-16 (__cons-itr-cons first1)
																  (__cons-itr-cons  last1)
																  (opr::vec-ptr-index  first2)
																  (opr::vec-ptr-index   last2)
																  (opr::vec-ptr-buffer first2)
																  (__cons-itr-cons     result) (functor_function (clone comp)))))

  ;;PTN; set_symmetric_difference : 17 -  cci x cvp x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
												(first2 const-vector-pointer) (last2 const-vector-pointer) (result vector-pointer))
	;;(format t "specialized set_symmetric_difference for cons_const_iterator & const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-vct-iterator result
							   (__set_symmetric_difference-imp-17 (__cons-itr-cons first1)
																  (__cons-itr-cons  last1)
																  (opr::vec-ptr-index  first2)
																  (opr::vec-ptr-index   last2)
																  (opr::vec-ptr-buffer first2)
																  (opr::vec-ptr-index  result)
																  (opr::vec-ptr-buffer result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 cons_const_iterator) (last1 cons_const_iterator)
												(first2 const-vector-pointer) (last2 const-vector-pointer) (result vector-pointer) comp)
	;;(format t "specialized set_symmetric_difference for cons_const_iterator & const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-vct-iterator result
							   (__set_symmetric_difference-imp-17 (__cons-itr-cons first1)
																  (__cons-itr-cons  last1)
																  (opr::vec-ptr-index  first2)
																  (opr::vec-ptr-index   last2)
																  (opr::vec-ptr-buffer first2)
																  (opr::vec-ptr-index  result)
																  (opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; set_symmetric_difference : 18 -  cvp x  i  x  o 
  (defmethod-overload set_symmetric_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
												(first2 input_iterator) (last2 input_iterator) (result output_iterator))
	;;(format t "specialized set_symmetric_difference for const-vector-pointer & input_iterator & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__set_symmetric_difference-imp-18 (opr::vec-ptr-index  first1)
									   (opr::vec-ptr-index  last1)
									   (opr::vec-ptr-buffer first1) first2 last2 (clone result) #'operator_<))

  (defmethod-overload set_symmetric_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
												(first2 input_iterator) (last2 input_iterator) (result output_iterator) comp)
	;;(format t "specialized set_symmetric_difference for const-vector-pointer & input_iterator & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__set_symmetric_difference-imp-18 (opr::vec-ptr-index  first1)
									   (opr::vec-ptr-index  last1)
									   (opr::vec-ptr-buffer first1)
									   first2 last2  (clone result) (functor_function (clone comp))))


  ;;PTN; set_symmetric_difference : 19 -  cvp x  i  x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
												(first2 input_iterator) (last2 input_iterator) (result cons_iterator))
	;;(format t "specialized set_symmetric_difference for const-vector-pointer & input_iterator & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-cns-iterator result
							   (__set_symmetric_difference-imp-19 (opr::vec-ptr-index  first1)
																  (opr::vec-ptr-index   last1)
																  (opr::vec-ptr-buffer first1)
																  first2 last2
																  (__cons-itr-cons     result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
												(first2 input_iterator) (last2 input_iterator) (result cons_iterator) comp)
	;;(format t "specialized set_symmetric_difference for const-vector-pointer & input_iterator & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-cns-iterator result
							   (__set_symmetric_difference-imp-19 (opr::vec-ptr-index  first1)
																  (opr::vec-ptr-index   last1)
																  (opr::vec-ptr-buffer first1)
																  first2 last2
																  (__cons-itr-cons     result) (functor_function (clone comp)))))

  ;;PTN; set_symmetric_difference : 20 -  cvp x  i  x  vp
  (defmethod-overload set_symmetric_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
												(first2 input_iterator) (last2 input_iterator) (result vector-pointer))
	;;(format t "specialized set_symmetric_difference for const-vector-pointer & input_iterator & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-vct-iterator result
							   (__set_symmetric_difference-imp-20 (opr::vec-ptr-index  first1)
																  (opr::vec-ptr-index  last1)
																  (opr::vec-ptr-buffer first1)
																  first2 last2
																  (opr::vec-ptr-index  result)
																  (opr::vec-ptr-buffer result) #'operator_<)))

  (defmethod-overload set_symmetric_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
												(first2 input_iterator) (last2 input_iterator) (result vector-pointer) comp)
	;;(format t "specialized set_symmetric_difference for const-vector-pointer & input_iterator & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-vct-iterator result
							   (__set_symmetric_difference-imp-20 (opr::vec-ptr-index  first1)
																  (opr::vec-ptr-index  last1)
																  (opr::vec-ptr-buffer first1)
																  first2 last2
																  (opr::vec-ptr-index  result)
																  (opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; set_symmetric_difference : 21 -  cvp x cci x  o 
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
												(first2 cons_const_iterator) (last2 cons_const_iterator) (result output_iterator))
	;;(format t "specialized set_symmetric_difference for const-vector-pointer & cons_const_iterator & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__set_symmetric_difference-imp-21 (opr::vec-ptr-index  first1)
									   (opr::vec-ptr-index   last1)
									   (opr::vec-ptr-buffer first1)
									   (__cons-itr-cons     first2)
									   (__cons-itr-cons      last2) (clone result) #'operator_<))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
												(first2 cons_const_iterator) (last2 cons_const_iterator) (result output_iterator) comp)
	;;(format t "specialized set_symmetric_difference for const-vector-pointer & cons_const_iterator & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__set_symmetric_difference-imp-21 (opr::vec-ptr-index  first1)
									   (opr::vec-ptr-index  last1)
									   (opr::vec-ptr-buffer first1)
									   (__cons-itr-cons     first2)
									   (__cons-itr-cons      last2) (clone result) (functor_function (clone comp))))

  ;;PTN; set_symmetric_difference : 22 -  cvp x cci x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
												(first2 cons_const_iterator) (last2 cons_const_iterator) (result cons_iterator))
	;;(format t "specialized set_symmetric_difference for const-vector-pointer & cons_const_iterator & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-cns-iterator result
							   (__set_symmetric_difference-imp-22 (opr::vec-ptr-index  first1)
																  (opr::vec-ptr-index   last1)
																  (opr::vec-ptr-buffer first1)
																  (__cons-itr-cons     first2)
																  (__cons-itr-cons      last2)
																  (__cons-itr-cons     result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
												(first2 cons_const_iterator) (last2 cons_const_iterator) (result cons_iterator) comp)
	;;(format t "specialized set_symmetric_difference for const-vector-pointer & cons_const_iterator & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-cns-iterator result
							   (__set_symmetric_difference-imp-22 (opr::vec-ptr-index  first1)
																  (opr::vec-ptr-index   last1)
																  (opr::vec-ptr-buffer first1)
																  (__cons-itr-cons     first2)
																  (__cons-itr-cons      last2)
																  (__cons-itr-cons     result) (functor_function (clone comp)))))

  ;;PTN; set_symmetric_difference : 23 -  cvp x cci x  vp
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
												(first2 cons_const_iterator) (last2 cons_const_iterator) (result vector-pointer))
	;;(format t "specialized set_symmetric_difference for const-vector-pointer & cons_const_iterator & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-vct-iterator result
							   (__set_symmetric_difference-imp-23 (opr::vec-ptr-index  first1)
																  (opr::vec-ptr-index   last1)
																  (opr::vec-ptr-buffer first1)
																  (__cons-itr-cons     first2)
																  (__cons-itr-cons      last2)
																  (opr::vec-ptr-index  result)
																  (opr::vec-ptr-buffer result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
												(first2 cons_const_iterator) (last2 cons_const_iterator) (result vector-pointer) comp)
	;;(format t "specialized set_symmetric_difference for const-vector-pointer & cons_const_iterator & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__algo-make-vct-iterator result
							   (__set_symmetric_difference-imp-23 (opr::vec-ptr-index  first1)
																  (opr::vec-ptr-index   last1)
																  (opr::vec-ptr-buffer first1)
																  (__cons-itr-cons     first2)
																  (__cons-itr-cons      last2)
																  (opr::vec-ptr-index  result)
																  (opr::vec-ptr-buffer result) (functor_function (clone comp)))))

  ;;PTN; set_symmetric_difference : 24 -  cvp x cvp x  o 
  (defmethod-overload set_symmetric_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
												(first2 const-vector-pointer) (last2 const-vector-pointer)
												(result output_iterator))
	;;(format t "specialized set_symmetric_difference for const-vector-pointer & const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__set_symmetric_difference-imp-24 (opr::vec-ptr-index  first1)
									   (opr::vec-ptr-index  last1)
									   (opr::vec-ptr-buffer first1)
									   (opr::vec-ptr-index  first2)
									   (opr::vec-ptr-index  last2)
									   (opr::vec-ptr-buffer first2) (clone result) #'operator_<))

  (defmethod-overload set_symmetric_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
												(first2 const-vector-pointer) (last2 const-vector-pointer)
												(result output_iterator) comp)
	;;(format t "specialized set_symmetric_difference for const-vector-pointer & const-vector-pointer & output_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__set_symmetric_difference-imp-24 (opr::vec-ptr-index  first1)
									   (opr::vec-ptr-index  last1)
									   (opr::vec-ptr-buffer first1)
									   (opr::vec-ptr-index  first2)
									   (opr::vec-ptr-index  last2)
									   (opr::vec-ptr-buffer first2) (clone result) (functor_function (clone comp))))

  ;;PTN; set_symmetric_difference : 25 -  cvp x cvp x  ci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
												(first2 const-vector-pointer) (last2 const-vector-pointer) (result cons_iterator))
	;;(format t "specialized set_symmetric_difference for const-vector-pointer & const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-cns-iterator result
							   (__set_symmetric_difference-imp-25 (opr::vec-ptr-index  first1)
																  (opr::vec-ptr-index   last1)
																  (opr::vec-ptr-buffer first1)
																  (opr::vec-ptr-index  first2)
																  (opr::vec-ptr-index   last2)
																  (opr::vec-ptr-buffer first2)
																  (__cons-itr-cons     result) #'operator_<)))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload set_symmetric_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
												(first2 const-vector-pointer) (last2 const-vector-pointer) (result cons_iterator) comp)
	;;(format t "specialized set_symmetric_difference for const-vector-pointer & const-vector-pointer & cons_iterator is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-cns-iterator result
							   (__set_symmetric_difference-imp-25 (opr::vec-ptr-index  first1)
																  (opr::vec-ptr-index   last1)
																  (opr::vec-ptr-buffer first1)
																  (opr::vec-ptr-index  first2)
																  (opr::vec-ptr-index   last2)
																  (opr::vec-ptr-buffer first2)
																  (__cons-itr-cons     result) (functor_function (clone comp)))))

  ;;PTN; set_symmetric_difference : 26 -  cvp x cvp x  vp
  (defmethod-overload set_symmetric_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
												(first2 const-vector-pointer) (last2 const-vector-pointer)
												(result vector-pointer))
	;;(format t "specialized set_symmetric_difference for const-vector-pointer & const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-vct-iterator result
							   (__set_symmetric_difference-imp-26 (opr::vec-ptr-index  first1)
																  (opr::vec-ptr-index   last1)
																  (opr::vec-ptr-buffer first1)
																  (opr::vec-ptr-index  first2)
																  (opr::vec-ptr-index   last2)
																  (opr::vec-ptr-buffer first2)
																  (opr::vec-ptr-index  result)
																  (opr::vec-ptr-buffer result) #'operator_<)))

  (defmethod-overload set_symmetric_difference ((first1 const-vector-pointer) (last1 const-vector-pointer)
												(first2 const-vector-pointer) (last2 const-vector-pointer)
												(result vector-pointer) comp)
	;;(format t "specialized set_symmetric_difference for const-vector-pointer & const-vector-pointer & vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first1 last1)
	(__pointer-check-iterator-range first2 last2)
	(__algo-make-vct-iterator result
							   (__set_symmetric_difference-imp-26 (opr::vec-ptr-index  first1)
																  (opr::vec-ptr-index   last1)
																  (opr::vec-ptr-buffer first1)
																  (opr::vec-ptr-index  first2)
																  (opr::vec-ptr-index   last2)
																  (opr::vec-ptr-buffer first2)
																  (opr::vec-ptr-index  result)
																  (opr::vec-ptr-buffer result) (functor_function (clone comp))))))





;; 25.3.6, heap operations:

; first     : randomaccess_iterator
; last      : randomaccess_iterator
; comp      : binary_function ( default : #'operator_< )
; returns   : nil.
(locally (declare (optimize speed))

  ;;PTN; push_heap : 0 -  r
  (defmethod-overload push_heap ((first randomaccess_iterator) (last randomaccess_iterator))
	(__push_heap-imp-0 first last #'operator_<))

  (defmethod-overload push_heap ((first randomaccess_iterator) (last randomaccess_iterator) comp)
	(__push_heap-imp-0 first last (functor_function (clone comp))))


  ;;PTN; push_heap : 1 -  vp
  (defmethod-overload push_heap ((first vector-pointer) (last vector-pointer))
	;;(format t "specialized push_heap for vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1   (opr::vec-ptr-index  first))
		  (idx2   (opr::vec-ptr-index  last))
		  (buffer (opr::vec-ptr-buffer first)))
	  (declare (type fixnum idx1 idx2))
	  (declare (type cl:vector buffer))
	  (let ((tmp nil))
		(_= tmp (aref buffer (the fixnum (1- idx2))))
		(__push_heap-imp-1 buffer idx1
						   (the fixnum (1- (the fixnum (- idx2 idx1)))) 0 tmp #'operator_<)))
	nil)

  (defmethod-overload push_heap ((first vector-pointer) (last vector-pointer) comp)
	;;(format t "specialized push_heap for vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1   (opr::vec-ptr-index  first))
		  (idx2   (opr::vec-ptr-index  last))
		  (buffer (opr::vec-ptr-buffer first)))
	  (declare (type fixnum idx1 idx2))
	  (declare (type cl:vector buffer))
	  (let ((tmp nil))
		(_= tmp (aref buffer (the fixnum (1- idx2))))
		(__push_heap-imp-1 buffer idx1
						   (the fixnum (1- (the fixnum (- idx2 idx1))))
						   0 tmp (functor_function (clone comp)))))
	nil))




; first     : randomaccess_iterator
; last      : randomaccess_iterator
; comp      : binary_function ( default : #'operator_< )
; returns   : nil.
(locally (declare (optimize speed))

  ;;PTN; pop_heap : 0 -  r
  (defmethod-overload pop_heap ((first randomaccess_iterator) (last randomaccess_iterator))
	(with-operators
		(__pop_heap-imp-0 first (_- last 1) (_- last 1) last[-1] #'operator_<))
	nil)

  (defmethod-overload pop_heap ((first randomaccess_iterator) (last randomaccess_iterator) comp)
	(with-operators
		(__pop_heap-imp-0 first (_- last 1) (_- last 1) last[-1] (functor_function @~comp)))
	nil)

  ;;PTN; pop_heap : 1 -  vp
  (defmethod-overload pop_heap ((first vector-pointer) (last vector-pointer))
	;;(format t "specialized pop_heap for vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1   (opr::vec-ptr-index  first))
		  (idx2   (opr::vec-ptr-index  last))
		  (buffer (opr::vec-ptr-buffer first)))
	  (declare (type fixnum idx1 idx2))
	  (declare (type cl:vector buffer))
	  (let ((tmp nil))
		(_= tmp (aref buffer (the fixnum (1- idx2))))
		(__pop_heap-imp-1 buffer idx1
						  (the fixnum (1- idx2))
						  (the fixnum (1- idx2)) tmp #'operator_<)))
	nil)

  (defmethod-overload pop_heap ((first vector-pointer) (last vector-pointer) comp)
	;;(format t "specialized pop_heap for vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1   (opr::vec-ptr-index  first))
		  (idx2   (opr::vec-ptr-index  last))
		  (buffer (opr::vec-ptr-buffer first)))
	  (declare (type fixnum idx1 idx2))
	  (declare (type cl:vector buffer))
	  (let ((tmp nil))
		(_= tmp (aref buffer (the fixnum (1- idx2))))
		(__pop_heap-imp-1 buffer idx1
						  (the fixnum (1- idx2))
						  (the fixnum (1- idx2)) tmp (functor_function (clone comp)))))
	nil))




; first     : randomaccess_iterator
; last      : randomaccess_iterator
; comp      : binary_function ( default : #'operator_< )
; returns   : nil.
(locally (declare (optimize speed))

  ;;PTN; make_heap : 0 -  r
  (defmethod-overload make_heap ((first randomaccess_iterator) (last randomaccess_iterator))
	(__make_heap-imp-0 first last #'operator_<))
	
  (defmethod-overload make_heap ((first randomaccess_iterator) (last randomaccess_iterator) comp)
	(__make_heap-imp-0 first last (functor_function (clone comp))))


  ;;PTN; make_heap : 1 -  vp
  (defmethod-overload make_heap ((first vector-pointer) (last vector-pointer))
	;;(format t "specialized make_heap for vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(__make_heap-imp-1 (opr::vec-ptr-index  first)
					   (opr::vec-ptr-index  last)
					   (opr::vec-ptr-buffer first) #'operator_<))

  (defmethod-overload make_heap ((first vector-pointer) (last vector-pointer) comp)
	;;(format t "specialized make_heap for vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(__make_heap-imp-1 (opr::vec-ptr-index  first)
					   (opr::vec-ptr-index  last)
					   (opr::vec-ptr-buffer first) (functor_function (clone comp)))))




; first     : randomaccess_iterator
; last      : randomaccess_iterator
; comp      : binary_function ( default : #'operator_< )
; returns   : nil.
(locally (declare (optimize speed))

  ;;PTN; sort_heap : 0 -  r
  (defmethod-overload sort_heap ((first randomaccess_iterator) (last randomaccess_iterator))
	(__sort_heap-imp-0 first last #'operator_<))
  
  (defmethod-overload sort_heap ((first randomaccess_iterator) (last randomaccess_iterator) comp)
	(__sort_heap-imp-0 first last (functor_function (clone comp))))


  ;;PTN; sort_heap : 1 -  vp
  (defmethod-overload sort_heap ((first vector-pointer) (last vector-pointer))
	;;(format t "specialized sort_heap for vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(__sort_heap-imp-1 (opr::vec-ptr-index  first)
					   (opr::vec-ptr-index  last)
					   (opr::vec-ptr-buffer first) #'operator_<))

  (defmethod-overload sort_heap ((first vector-pointer) (last vector-pointer) comp)
	;;(format t "specialized sort_heap for vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(__sort_heap-imp-1 (opr::vec-ptr-index  first)
					   (opr::vec-ptr-index  last)
					   (opr::vec-ptr-buffer first) (functor_function (clone comp)))))




; first     : randomaccess_iterator
; last      : randomaccess_iterator
; comp      : binary_function ( default : #'operator_< )
; returns   : boolean value.
#-cl-stl-0x98    ; is_heap
(locally (declare (optimize speed))

  ;;PTN; is_heap : 0 -  r
  (defmethod-overload is_heap ((first randomaccess_iterator) (last randomaccess_iterator))
	(let ((len (_- last first)))
	  (declare (type fixnum len))
	  (= len (the fixnum (__is_heap_until-imp-0 first len #'operator_<)))))

  (defmethod-overload is_heap ((first randomaccess_iterator) (last randomaccess_iterator) comp)
	(let ((len (_- last first)))
	  (declare (type fixnum len))
	  (= len (the fixnum (__is_heap_until-imp-0 first len (functor_function (clone comp)))))))


  ;;PTN; is_heap : 1 - cvp
  (defmethod-overload is_heap ((first const-vector-pointer) (last const-vector-pointer))
	;;(format t "specialized is_heap for const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index first))
		  (idx2 (opr::vec-ptr-index last)))
	  (declare (type fixnum idx1 idx2))
	  (let ((len (- idx2 idx1)))
		(declare (type fixnum len))
		(= len (the fixnum (__is_heap_until-imp-1 idx1 len
												  (opr::vec-ptr-buffer first) #'operator_<))))))

  (defmethod-overload is_heap ((first const-vector-pointer) (last const-vector-pointer) comp)
	;;(format t "specialized is_heap for const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index first))
		  (idx2 (opr::vec-ptr-index last)))
	  (declare (type fixnum idx1 idx2))
	  (let ((len (- idx2 idx1)))
		(declare (type fixnum len))
		(= len (the fixnum (__is_heap_until-imp-1 idx1 len
												  (opr::vec-ptr-buffer first)
												  (functor_function (clone comp)))))))))




; first     : randomaccess_iterator
; last      : randomaccess_iterator
; comp      : binary_function ( default : #'operator_< )
; returns   : copy of iterator( points result ).
#-cl-stl-0x98    ; is_heap_until
(locally (declare (optimize speed))

  ;;PTN; is_heap_until : 0 -   r
  (defmethod-overload is_heap_until ((first randomaccess_iterator) (last randomaccess_iterator))
	(let* ((len (_- last first))
		   (idx (__is_heap_until-imp-0 first len #'operator_<)))
	  (_+ first idx)))

  (defmethod-overload is_heap_until ((first randomaccess_iterator) (last randomaccess_iterator) comp)
	(let* ((len (_- last first))
		   (idx (__is_heap_until-imp-0 first len (functor_function (clone comp)))))
	  (_+ first idx)))


  ;;PTN; is_heap_until : 1 -  cvp
  (defmethod-overload is_heap_until ((first const-vector-pointer) (last const-vector-pointer))
	;;(format t "specialized is_heap_until for const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index first))
		  (idx2 (opr::vec-ptr-index last)))
	  (declare (type fixnum idx1 idx2))
	  (_+ first (__is_heap_until-imp-1 idx1 (the fixnum (- idx2 idx1))
									   (opr::vec-ptr-buffer first) #'operator_<))))

  (defmethod-overload is_heap_until ((first const-vector-pointer) (last const-vector-pointer) comp)
	;;(format t "specialized is_heap_until for const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(let ((idx1 (opr::vec-ptr-index first))
		  (idx2 (opr::vec-ptr-index last)))
	  (declare (type fixnum idx1 idx2))
	  (_+ first (__is_heap_until-imp-1 idx1 (the fixnum (- idx2 idx1))
									   (opr::vec-ptr-buffer first)
									   (functor_function (clone comp)))))))





;; 25.3.7, minimum and maximum:

;; a        : value
;; b        : value
;; comp     : binary_function or function ( default #'operator_< )
;; returns  : value
(defmethod-overload min (a b)
  (if (_< b a) b a))

(defmethod-overload min (a b comp)
  (if (funcall (clone comp) b a) b a))

;; il       : initializer_list
;; comp     : binary_function or function ( default #'operator_< )
;; returns  : value
#-cl-stl-0x98 ; min ( initializer_list )
(locally (declare (optimize speed))
  (labels ((__min-imp (arr comp)
			 (declare (type simple-vector arr))
			 (declare (type cl:function   comp))
			 (let ((cnt (length arr)))
			   (declare (type fixnum cnt))
			   (if (zerop cnt)
				   (error 'undefined-behavior :what "Zero length initializer list.")
				   (let ((idx 1))
					 (declare (type fixnum idx))
					 (for (((min (aref arr 0))) (< idx cnt) (incf idx) :returns min)
					   (let ((val (aref arr idx)))
						 (when (funcall comp val min)
						   (setf min val)))))))))

	(defmethod-overload min ((il initializer_list))
	  (__min-imp (__initlist-data il) #'operator_<))

	(defmethod-overload min ((il initializer_list) comp)
	  (__min-imp (__initlist-data il) (functor_function (clone comp))))))


;; a        : value
;; b        : value
;; comp     : binary_function or function ( default #'operator_< )
;; returns  : value
(defmethod-overload max (a b)
  (if (_< a b) b a))

(defmethod-overload max (a b comp)
  (if (funcall (clone comp) a b) b a))

;; il       : initializer_list
;; comp     : binary_function or function ( default #'operator_< )
;; returns  : value
#-cl-stl-0x98 ; max ( initializer_list )
(locally (declare (optimize speed))
  (labels ((__max-imp (arr comp)
			 (declare (type simple-vector arr))
			 (declare (type cl:function   comp))
			 (let ((cnt (length arr)))
			   (declare (type fixnum cnt))
			   (if (zerop cnt)
				   (error 'undefined-behavior :what "Zero length initializer list.")
				   (let ((idx 1))
					 (declare (type fixnum idx))
					 (for (((max (aref arr 0))) (< idx cnt) (incf idx) :returns max)
					   (let ((val (aref arr idx)))
						 (when (funcall comp max val)
						   (setf max val)))))))))

	(defmethod-overload max ((il initializer_list))
	  (__max-imp (__initlist-data il) #'operator_<))

	(defmethod-overload max ((il initializer_list) comp)
	  (__max-imp (__initlist-data il) (functor_function (clone comp))))))


;; a        : value
;; b        : value
;; comp     : binary_function or function ( default #'operator_< )
;; returns  : pair
#-cl-stl-0x98 ; minmax
(defmethod-overload minmax (a b)
  (if (_< b a)
	  (make_pair b a)
	  (make_pair a b)))

#-cl-stl-0x98 ; minmax
(defmethod-overload minmax (a b comp)
  (if (funcall (clone comp) b a)
	  (make_pair b a)
	  (make_pair a b)))

;; il       : initializer_list
;; comp     : binary_function or function ( default #'operator_< )
;; returns  : pair
#-cl-stl-0x98
(locally (declare (optimize speed))

  (labels ((__minmax-imp (buf comp)
			 (declare (type simple-vector buf))
			 (declare (type cl:function   comp))
			 (multiple-value-bind (i1 i2) (__minmax_element-imp-2 0 (length buf) buf comp)
			   (make_pair (aref buf i1)
						  (aref buf i2)))))
	
	(defmethod-overload minmax ((il initializer_list))
	  (let ((buf (__initlist-data il)))
		(declare (type simple-vector buf))
		(if (zerop (length buf))
			(error 'undefined-behavior :what "Zero length initializer list.")
			(__minmax-imp buf #'operator_<))))

	(defmethod-overload minmax ((il initializer_list) comp)
	  (let ((buf (__initlist-data il)))
		(declare (type simple-vector buf))
		(if (zerop (length buf))
			(error 'undefined-behavior :what "Zero length initializer list.")
			(__minmax-imp buf (functor_function (clone comp))))))))
  



; first     : forward_iterator
; last      : forward_iterator
; comp      : binary_function ( default : #'operator_< )
; returns   : copy of first ( point to result ).
(locally (declare (optimize speed))

  ;;PTN; min_element : 0 -   f
  (labels ((__min_element-imp-0 (first last comp)
			 (declare (type cl:function comp))
			 (with-operators
				 (if (_== first last)
					 @~first
					 (for (((min @~first) (itr (next first))) (_/= itr last) ++itr :returns min)
					   (when (funcall comp *itr *min)
						 (_= min itr)))))))

	(defmethod-overload min_element ((first forward_iterator) (last forward_iterator))
	  (__min_element-imp-0 first last #'operator_<))

	(defmethod-overload min_element ((first forward_iterator) (last forward_iterator) comp)
	  (__min_element-imp-0 first last (functor_function (clone comp)))))


  ;;PTN; min_element : 1 -  cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__min_element-imp-1 (cons1 cons2 less-bf)
			 (declare (type cl:list cons1 cons2))
			 (declare (type cl:function less-bf))
			 (if (eq cons1 cons2)
				 cons2
				 (let ((min cons1)
					   (cns (cdr cons1)))
				   (declare (type cl:list min cns))
				   (for (((min-val (car cons1))) (not (eq cns cons2)) (setf cns (cdr cns)) :returns min)
					 (let ((cur-val (car cns)))
					   (when (funcall less-bf cur-val min-val)
						 (setf min cns)
						 (setf min-val cur-val))))))))

	(defmethod-overload min_element ((first cons_const_iterator) (last cons_const_iterator))
	  ;;(format t "specialized min_element for cons_const_iterator is invoked.~%")
	  (__algo-make-cns-iterator first
								 (__min_element-imp-1 (__cons-itr-cons first)
													  (__cons-itr-cons  last) #'operator_<)))

	(defmethod-overload min_element ((first cons_const_iterator) (last cons_const_iterator) comp)
	  ;;(format t "specialized min_element for cons_const_iterator is invoked.~%")
	  (__algo-make-cns-iterator first
								 (__min_element-imp-1 (__cons-itr-cons first)
													  (__cons-itr-cons  last) (functor_function (clone comp))))))


  ;;PTN; min_element : 2 -  cvp
  (labels ((__min_element-imp-2 (idx1 idx2 buffer less-bf)
			 (declare (type fixnum idx1 idx2))
			 (declare (type cl:vector buffer))
			 (declare (type cl:function less-bf))
			 (if (= idx1 idx2)
				 idx1
				 (let ((min idx1)
					   (idx (1+ idx1)))
				   (declare (type fixnum min idx))
				   (for (((min-val (aref buffer idx1))) (< idx idx2) (incf idx) :returns min)
					 (let ((cur-val (aref buffer idx)))
					   (when (funcall less-bf cur-val min-val)
						 (setf min idx)
						 (setf min-val cur-val))))))))

	(defmethod-overload min_element ((first const-vector-pointer) (last const-vector-pointer))
	  ;;(format t "specialized min_element for const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__algo-make-vct-iterator first
								 (__min_element-imp-2 (opr::vec-ptr-index  first)
													  (opr::vec-ptr-index  last)
													  (opr::vec-ptr-buffer first) #'operator_<)))

	(defmethod-overload min_element ((first const-vector-pointer) (last const-vector-pointer) comp)
	  ;;(format t "specialized min_element for const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__algo-make-vct-iterator first
								 (__min_element-imp-2 (opr::vec-ptr-index  first)
													  (opr::vec-ptr-index  last)
													  (opr::vec-ptr-buffer first) (functor_function (clone comp)))))))





; first     : forward_iterator
; last      : forward_iterator
; comp      : binary_function ( default : #'operator_< )
; returns   : copy of first ( point to result ).
(locally (declare (optimize speed))

  ;;PTN; max_element : 0 -   f
  (labels ((__max_element-imp-0 (first last comp)
			 (declare (type cl:function comp))
			 (with-operators
				 (if (_== first last)
					 @~first
					 (for (((max @~first) (itr (next first))) (_/= itr last) ++itr :returns max)
					   (when (funcall comp *max *itr)
						 (_= max itr)))))))

	(defmethod-overload max_element ((first forward_iterator) (last forward_iterator))
	  (__max_element-imp-0 first last #'operator_<))
	
	(defmethod-overload max_element ((first forward_iterator) (last forward_iterator) comp)
	  (__max_element-imp-0 first last (functor_function (clone comp)))))


  ;;PTN; max_element : 1 -  cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__max_element-imp-1 (cons1 cons2 less-bf)
			 (declare (type cl:list cons1 cons2))
			 (declare (type cl:function less-bf))
			 (if (eq cons1 cons2)
				 cons1
				 (let ((max cons1)
					   (cns (cdr cons1)))
				   (declare (type cl:list max cns))
				   (for (((max-val (car cons1))) (not (eq cns cons2)) (setf cns (cdr cns)) :returns max)
					 (let ((cur-val (car cns)))
					   (when (funcall less-bf max-val cur-val)
						 (setf max cns)
						 (setf max-val cur-val))))))))

	(defmethod-overload max_element ((first cons_const_iterator) (last cons_const_iterator))
	  ;;(format t "specialized max_element for cons_const_iterator is invoked.~%")
	  (__algo-make-cns-iterator first
								 (__max_element-imp-1 (__cons-itr-cons first)
													  (__cons-itr-cons  last) #'operator_<)))

	(defmethod-overload max_element ((first cons_const_iterator) (last cons_const_iterator) comp)
	  ;;(format t "specialized max_element for cons_const_iterator is invoked.~%")
	  (__algo-make-cns-iterator first
								 (__max_element-imp-1 (__cons-itr-cons first)
													  (__cons-itr-cons  last) (functor_function (clone comp))))))


  ;;PTN; max_element : 2 -  cvp
  (labels ((__max_element-imp-2 (idx1 idx2 buffer less-bf)
			 (declare (type fixnum idx1 idx2))
			 (declare (type cl:vector buffer))
			 (declare (type cl:function less-bf))
			 (if (= idx1 idx2)
				 idx1
				 (let ((max idx1)
					   (idx (1+ idx1)))
				   (declare (type fixnum max idx))
				   (for (((max-val (aref buffer idx1))) (< idx idx2) (incf idx) :returns max)
					 (let ((cur-val (aref buffer idx)))
					   (when (funcall less-bf max-val cur-val)
						 (setf max idx)
						 (setf max-val cur-val))))))))

	(defmethod-overload max_element ((first const-vector-pointer) (last const-vector-pointer))
	  ;;(format t "specialized max_element for const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__algo-make-vct-iterator first
								 (__max_element-imp-2 (opr::vec-ptr-index  first)
													  (opr::vec-ptr-index  last)
													  (opr::vec-ptr-buffer first) #'operator_<)))

	(defmethod-overload max_element ((first const-vector-pointer) (last const-vector-pointer) comp)
	  ;;(format t "specialized max_element for const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__algo-make-vct-iterator first
								 (__max_element-imp-2 (opr::vec-ptr-index  first)
													  (opr::vec-ptr-index  last)
													  (opr::vec-ptr-buffer first) (functor_function (clone comp)))))))




; first     : forward_iterator
; last      : forward_iterator
; comp      : binary_function ( default : #'operator_< )
; returns   : pair of iterator.
#-cl-stl-0x98    ; minmax_element
(locally (declare (optimize speed))

  ;;PTN; minmax_element : 0 -   f
  (defmethod-overload minmax_element ((first forward_iterator) (last forward_iterator))
	(__minmax_element-imp-0 first last #'operator_<))

  (defmethod-overload minmax_element ((first forward_iterator) (last forward_iterator) comp)
	(__minmax_element-imp-0 first last (functor_function (clone comp))))

  ;;PTN; minmax_element : 1 -  cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload minmax_element ((first cons_const_iterator) (last cons_const_iterator))
	;;(format t "specialized minmax_element for cons_const_iterator is invoked.~%")
	(multiple-value-bind (min max) (__minmax_element-imp-1 (__cons-itr-cons first)
														   (__cons-itr-cons  last) #'operator_<)
	  (make_pair (__algo-make-cns-iterator first min)
				 (__algo-make-cns-iterator first max))))

  #-(and cl-stl-noextra cl-stl-0x98)
  (defmethod-overload minmax_element ((first cons_const_iterator) (last cons_const_iterator) comp)
	;;(format t "specialized minmax_element for cons_const_iterator is invoked.~%")
	(multiple-value-bind (min max) (__minmax_element-imp-1 (__cons-itr-cons first)
														   (__cons-itr-cons  last)
														   (functor_function (clone comp)))
	  (make_pair (__algo-make-cns-iterator first min)
				 (__algo-make-cns-iterator first max))))

  ;;PTN; minmax_element : 2 -  cvp
  (defmethod-overload minmax_element ((first const-vector-pointer) (last const-vector-pointer))
	;;(format t "specialized minmax_element for const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(multiple-value-bind (min max) (__minmax_element-imp-2 (opr::vec-ptr-index  first)
														   (opr::vec-ptr-index  last)
														   (opr::vec-ptr-buffer first) #'operator_<)
	  (make_pair (__algo-make-vct-iterator first min)
				 (__algo-make-vct-iterator first max))))

  (defmethod-overload minmax_element ((first const-vector-pointer) (last const-vector-pointer) comp)
	;;(format t "specialized minmax_element for const-vector-pointer is invoked.~%")
	(__pointer-check-iterator-range first last)
	(multiple-value-bind (min max) (__minmax_element-imp-2 (opr::vec-ptr-index  first)
														   (opr::vec-ptr-index  last)
														   (opr::vec-ptr-buffer first)
														   (functor_function (clone comp)))
	  (make_pair (__algo-make-vct-iterator first min)
				 (__algo-make-vct-iterator first max)))))




; first1    : input_iterator
; last1     : input_iterator
; first2    : input_iterator
; last2     : input_iterator
; comp      : binary_function ( default : #'operator_< )
; returns   : boolean value.
(locally (declare (optimize speed))

  ;;PTN; lexicographical_compare : 0 -   i  x  i
  (labels ((__lexicographical_compare-imp-0 (first1 last1 first2 last2 comp)
			 (declare (type cl:function comp))
			 (with-operators
				 (for (((itr1 @~first1) (itr2 @~first2)) t (progn ++itr1 ++itr2))
				   (let ((last1 (_== itr1 last1))
						 (last2 (_== itr2 last2)))
					 (if (or last1 last2)
						 (return-from __lexicographical_compare-imp-0 (and last1 (not last2)))
						 (let ((val1 *itr1)
							   (val2 *itr2))
						   (if (funcall comp val1 val2)
							   (return-from __lexicographical_compare-imp-0 t)
							   (when (funcall comp val2 val1)
								 (return-from __lexicographical_compare-imp-0 nil))))))))))

	(defmethod-overload lexicographical_compare ((first1 input_iterator) (last1 input_iterator)
												 (first2 input_iterator) (last2 input_iterator))
	  (__lexicographical_compare-imp-0 first1 last1 first2 last2 #'operator_<))

	(defmethod-overload lexicographical_compare ((first1 input_iterator) (last1 input_iterator)
												 (first2 input_iterator) (last2 input_iterator) comp)
	  (__lexicographical_compare-imp-0 first1 last1 first2 last2 (functor_function (clone comp)))))

  ;;PTN; lexicographical_compare : 1 -  cci x  i 
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__lexicographical_compare-imp-1 (cons1 end1 first2 last2 less-bf)
			 (declare (type cl:list cons1 end1))
			 (declare (type cl:function less-bf))
			 (with-operators
				 (for (((itr2 @~first2)) t (progn (setf cons1 (cdr cons1)) ++itr2))
				   (let ((is-end1 (eq cons1 end1))
						 (is-end2 (_== itr2 last2)))
					 (if (or is-end1 is-end2)
						 (return-from __lexicographical_compare-imp-1 (and is-end1 (not is-end2)))
						 (let ((val1 (car cons1))
							   (val2 *itr2))
						   (if (funcall less-bf val1 val2)
							   (return-from __lexicographical_compare-imp-1 t)
							   (when (funcall less-bf val2 val1)
								 (return-from __lexicographical_compare-imp-1 nil))))))))))

	(defmethod-overload lexicographical_compare ((first1 cons_const_iterator)
												 (last1  cons_const_iterator)
												 (first2 input_iterator) (last2 input_iterator))
	  ;;(format t "specialized lexicographical_compare for cons_const_iterator & input_iterator is invoked.~%")
	  (__lexicographical_compare-imp-1 (__cons-itr-cons first1)
									   (__cons-itr-cons  last1) first2 last2 #'operator_<))

	(defmethod-overload lexicographical_compare ((first1 cons_const_iterator)
												 (last1  cons_const_iterator)
												 (first2 input_iterator) (last2 input_iterator) comp)
	  ;;(format t "specialized lexicographical_compare for cons_const_iterator & input_iterator is invoked.~%")
	  (__lexicographical_compare-imp-1 (__cons-itr-cons first1)
									   (__cons-itr-cons  last1) first2 last2 (functor_function (clone comp)))))

  ;;PTN; lexicographical_compare : 2 -  cvp x  i
  (labels ((__lexicographical_compare-imp-2 (idx1 last1 buffer1 first2 last2 less-bf)
			 (declare (type fixnum idx1 last1))
			 (declare (type cl:vector buffer1))
			 (declare (type cl:function less-bf))
			 (with-operators
				 (for (((itr2 @~first2)) t (progn (incf idx1) ++itr2))
				   (let ((end1 (= idx1 last1))
						 (end2 (_== itr2 last2)))
					 (if (or end1 end2)
						 (return-from __lexicographical_compare-imp-2 (and end1 (not end2)))
						 (let ((val1 (aref buffer1 idx1))
							   (val2 *itr2))
						   (if (funcall less-bf val1 val2)
							   (return-from __lexicographical_compare-imp-2 t)
							   (when (funcall less-bf val2 val1)
								 (return-from __lexicographical_compare-imp-2 nil))))))))))

	(defmethod-overload lexicographical_compare ((first1 const-vector-pointer)
												 (last1  const-vector-pointer)
												 (first2 input_iterator) (last2 input_iterator))
	  ;;(format t "specialized lexicographical_compare for const-vector-pointer & input_iterator is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__lexicographical_compare-imp-2 (opr::vec-ptr-index  first1)
									   (opr::vec-ptr-index  last1)
									   (opr::vec-ptr-buffer first1) first2 last2 #'operator_<))

	(defmethod-overload lexicographical_compare ((first1 const-vector-pointer)
												 (last1  const-vector-pointer)
												 (first2 input_iterator) (last2 input_iterator) comp)
	  ;;(format t "specialized lexicographical_compare for const-vector-pointer & input_iterator is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__lexicographical_compare-imp-2 (opr::vec-ptr-index  first1)
									   (opr::vec-ptr-index  last1)
									   (opr::vec-ptr-buffer first1)
									   first2 last2 (functor_function (clone comp)))))

  ;;PTN; lexicographical_compare : 3 -   i  x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__lexicographical_compare-imp-6 (first1 last1 cons2 end2 less-bf)
			 (declare (type cl:list cons2 end2))
			 (declare (type cl:function less-bf))
			 (with-operators
				 (for (((itr1 @~first1)) t (progn ++itr1 (setf cons2 (cdr cons2))))
				   (let ((is-end1 (_== itr1 last1))
						 (is-end2 (eq cons2 end2)))
					 (if (or is-end1 is-end2)
						 (return-from __lexicographical_compare-imp-6 (and is-end1 (not is-end2)))
						 (let ((val1 *itr1)
							   (val2 (car cons2)))
						   (if (funcall less-bf val1 val2)
							   (return-from __lexicographical_compare-imp-6 t)
							   (when (funcall less-bf val2 val1)
								 (return-from __lexicographical_compare-imp-6 nil))))))))))

	(defmethod-overload lexicographical_compare ((first1 input_iterator) (last1 input_iterator)
												 (first2 cons_const_iterator) (last2 cons_const_iterator))
	  ;;(format t "specialized lexicographical_compare for input_iterator & cons_const_iterator is invoked.~%")
	  (__lexicographical_compare-imp-6 first1 last1
									   (__cons-itr-cons first2) (__cons-itr-cons last2) #'operator_<))

	(defmethod-overload lexicographical_compare ((first1 input_iterator) (last1 input_iterator)
												 (first2 cons_const_iterator) (last2 cons_const_iterator) comp)
	  ;;(format t "specialized lexicographical_compare for input_iterator & cons_const_iterator is invoked.~%")
	  (__lexicographical_compare-imp-6 first1 last1
									   (__cons-itr-cons first2)
									   (__cons-itr-cons  last2) (functor_function (clone comp)))))


  ;;PTN; lexicographical_compare : 4 -  cci x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__lexicographical_compare-imp-7 (cons1 end1 cons2 end2 less-bf)
			 (declare (type cl:list cons1 end1 cons2 end2))
			 (declare (type cl:function less-bf))
			 (for (nil t (progn (setf cons1 (cdr cons1))
								(setf cons2 (cdr cons2))))
			   (let ((is-end1 (eq cons1 end1))
					 (is-end2 (eq cons2 end2)))
				 (if (or is-end1 is-end2)
					 (return-from __lexicographical_compare-imp-7 (and is-end1 (not is-end2)))
					 (let ((val1 (car cons1))
						   (val2 (car cons2)))
					   (if (funcall less-bf val1 val2)
						   (return-from __lexicographical_compare-imp-7 t)
						   (when (funcall less-bf val2 val1)
							 (return-from __lexicographical_compare-imp-7 nil)))))))))

	(defmethod-overload lexicographical_compare ((first1 cons_const_iterator) (last1 cons_const_iterator)
												 (first2 cons_const_iterator) (last2 cons_const_iterator))
	  ;;(format t "specialized lexicographical_compare for cons_const_iterator & cons_const_iterator is invoked.~%")
	  (__lexicographical_compare-imp-7 (__cons-itr-cons first1) (__cons-itr-cons last1)
									   (__cons-itr-cons first2) (__cons-itr-cons last2) #'operator_<))

	(defmethod-overload lexicographical_compare ((first1 cons_const_iterator) (last1 cons_const_iterator)
												 (first2 cons_const_iterator) (last2 cons_const_iterator) comp)
	  ;;(format t "specialized lexicographical_compare for cons_const_iterator & cons_const_iterator is invoked.~%")
	  (__lexicographical_compare-imp-7 (__cons-itr-cons first1) (__cons-itr-cons last1)
									   (__cons-itr-cons first2) (__cons-itr-cons last2) (functor_function (clone comp)))))


  ;;PTN; lexicographical_compare : 5 -  cvp x cci
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__lexicographical_compare-imp-5 (idx1 last1 buffer1 cons2 end2 less-bf)
			 (declare (type fixnum idx1 last1))
			 (declare (type cl:list cons2 end2))
			 (declare (type cl:function less-bf))
			 (for (nil t (progn (incf idx1) (setf cons2 (cdr cons2))))
			   (let ((is-end1 (= idx1 last1))
					 (is-end2 (eq cons2 end2)))
				 (if (or is-end1 is-end2)
					 (return-from __lexicographical_compare-imp-5 (and is-end1 (not is-end2)))
					 (let ((val1 (aref buffer1 idx1))
						   (val2 (car cons2)))
					   (if (funcall less-bf val1 val2)
						   (return-from __lexicographical_compare-imp-5 t)
						   (when (funcall less-bf val2 val1)
							 (return-from __lexicographical_compare-imp-5 nil)))))))))

	(defmethod-overload lexicographical_compare ((first1 const-vector-pointer) (last1 const-vector-pointer)
												 (first2 cons_const_iterator) (last2 cons_const_iterator))
	  ;;(format t "specialized lexicographical_compare for const-vector-pointer & cons_const_iterator is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__lexicographical_compare-imp-5 (opr::vec-ptr-index  first1)
									   (opr::vec-ptr-index   last1)
									   (opr::vec-ptr-buffer first1)
									   (__cons-itr-cons first2) (__cons-itr-cons last2) #'operator_<))

	(defmethod-overload lexicographical_compare ((first1 const-vector-pointer) (last1 const-vector-pointer)
												 (first2 cons_const_iterator) (last2 cons_const_iterator) comp)
	  ;;(format t "specialized lexicographical_compare for const-vector-pointer & cons_const_iterator is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__lexicographical_compare-imp-5 (opr::vec-ptr-index  first1)
									   (opr::vec-ptr-index   last1)
									   (opr::vec-ptr-buffer first1)
									   (__cons-itr-cons first2) (__cons-itr-cons last2) (functor_function (clone comp)))))
  
  ;;PTN; lexicographical_compare : 6 -   i  x cvp
  (labels ((__lexicographical_compare-imp-6 (first1 last1 idx2 last2 buffer2 less-bf)
			 (declare (type fixnum idx2 last2))
			 (declare (type cl:vector buffer2))
			 (declare (type cl:function less-bf))
			 (with-operators
				 (for (((itr1 @~first1)) t (progn ++itr1 (incf idx2)))
				   (let ((end1 (_== itr1 last1))
						 (end2 (= idx2 last2)))
					 (if (or end1 end2)
						 (return-from __lexicographical_compare-imp-6 (and end1 (not end2)))
						 (let ((val1 *itr1)
							   (val2 (aref buffer2 idx2)))
						   (if (funcall less-bf val1 val2)
							   (return-from __lexicographical_compare-imp-6 t)
							   (when (funcall less-bf val2 val1)
								 (return-from __lexicographical_compare-imp-6 nil))))))))))

	(defmethod-overload lexicographical_compare ((first1 input_iterator)
												 (last1  input_iterator)
												 (first2 const-vector-pointer) (last2 const-vector-pointer))
	  ;;(format t "specialized lexicographical_compare for input_iterator & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first2 last2)
	  (__lexicographical_compare-imp-6 first1 last1
									   (opr::vec-ptr-index  first2)
									   (opr::vec-ptr-index  last2)
									   (opr::vec-ptr-buffer first2) #'operator_<))

	(defmethod-overload lexicographical_compare ((first1 input_iterator)
												 (last1  input_iterator)
												 (first2 const-vector-pointer) (last2 const-vector-pointer) comp)
	  ;;(format t "specialized lexicographical_compare for input_iterator & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first2 last2)
	  (__lexicographical_compare-imp-6 first1 last1
									   (opr::vec-ptr-index  first2)
									   (opr::vec-ptr-index  last2)
									   (opr::vec-ptr-buffer first2) (functor_function (clone comp)))))

  ;;PTN; lexicographical_compare : 7 -  cci x cvp
  #-(and cl-stl-noextra cl-stl-0x98)
  (labels ((__lexicographical_compare-imp-7 (cons1 end1 idx2 last2 buffer2 less-bf)
			 (declare (type cl:list cons1 end1))
			 (declare (type fixnum idx2 last2))
			 (declare (type cl:vector buffer2))
			 (declare (type cl:function less-bf))
			 (for (nil t (progn (setf cons1 (cdr cons1)) (incf idx2)))
			   (let ((is-end1 (eq cons1 end1))
					 (is-end2 (= idx2 last2)))
				 (if (or is-end1 is-end2)
					 (return-from __lexicographical_compare-imp-7 (and is-end1 (not is-end2)))
					 (let ((val1 (car cons1))
						   (val2 (aref buffer2 idx2)))
					   (if (funcall less-bf val1 val2)
						   (return-from __lexicographical_compare-imp-7 t)
						   (when (funcall less-bf val2 val1)
							 (return-from __lexicographical_compare-imp-7 nil)))))))))

	(defmethod-overload lexicographical_compare ((first1 cons_const_iterator) (last1 cons_const_iterator)
												 (first2 const-vector-pointer) (last2 const-vector-pointer))
	  ;;(format t "specialized lexicographical_compare for cons_const_iterator & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first2 last2)
	  (__lexicographical_compare-imp-7 (__cons-itr-cons first1) (__cons-itr-cons last1)
									   (opr::vec-ptr-index first2) (opr::vec-ptr-index last2)
									   (opr::vec-ptr-buffer first2) #'operator_<))

	(defmethod-overload lexicographical_compare ((first1 cons_const_iterator) (last1 cons_const_iterator)
												 (first2 const-vector-pointer) (last2 const-vector-pointer) comp)
	  ;;(format t "specialized lexicographical_compare for cons_const_iterator & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first2 last2)
	  (__lexicographical_compare-imp-7 (__cons-itr-cons first1) (__cons-itr-cons last1)
									   (opr::vec-ptr-index first2) (opr::vec-ptr-index last2)
									   (opr::vec-ptr-buffer first2) (functor_function (clone comp)))))

  ;;PTN; lexicographical_compare : 8 -  cvp x cvp
  (labels ((__lexicographical_compare-imp-8 (idx1 last1 buffer1 idx2 last2 buffer2 less-bf)
			 (declare (type fixnum idx1 last1 idx2 last2))
			 (declare (type cl:vector buffer1 buffer2))
			 (declare (type cl:function less-bf))
			 (for (nil t (progn (incf idx1) (incf idx2)))
			   (let ((end1 (= idx1 last1))
					 (end2 (= idx2 last2)))
				 (if (or end1 end2)
					 (return-from __lexicographical_compare-imp-8 (and end1 (not end2)))
					 (let ((val1 (aref buffer1 idx1))
						   (val2 (aref buffer2 idx2)))
					   (if (funcall less-bf val1 val2)
						   (return-from __lexicographical_compare-imp-8 t)
						   (when (funcall less-bf val2 val1)
							 (return-from __lexicographical_compare-imp-8 nil)))))))))

	(defmethod-overload lexicographical_compare ((first1 const-vector-pointer) (last1 const-vector-pointer)
												 (first2 const-vector-pointer) (last2 const-vector-pointer))
	  ;;(format t "specialized lexicographical_compare for const-vector-pointer & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__pointer-check-iterator-range first2 last2)
	  (__lexicographical_compare-imp-8 (opr::vec-ptr-index  first1)
									   (opr::vec-ptr-index  last1)
									   (opr::vec-ptr-buffer first1)
									   (opr::vec-ptr-index  first2)
									   (opr::vec-ptr-index  last2)
									   (opr::vec-ptr-buffer first2) #'operator_<))

	(defmethod-overload lexicographical_compare ((first1 const-vector-pointer) (last1  const-vector-pointer)
												 (first2 const-vector-pointer) (last2  const-vector-pointer) comp)
	  ;;(format t "specialized lexicographical_compare for const-vector-pointer & const-vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first1 last1)
	  (__pointer-check-iterator-range first2 last2)
	  (__lexicographical_compare-imp-8 (opr::vec-ptr-index  first1)
									   (opr::vec-ptr-index  last1)
									   (opr::vec-ptr-buffer first1)
									   (opr::vec-ptr-index  first2)
									   (opr::vec-ptr-index  last2)
									   (opr::vec-ptr-buffer first2) (functor_function (clone comp))))))



;; 25.3.9, permutations

; first     : bidirectional_iterator
; last      : bidirectional_iterator
; comp      : binary_function ( default : #'operator_< )
; returns   : boolean value.
(locally (declare (optimize speed))

  ;;PTN; next_permutation : 0 -   b 
  (labels ((__next_permutation-imp-0 (first last comp)
			 (declare (type cl:function comp))
			 (if (_== first last)
				 nil
				 (with-operators
					 (let ((itr (prev last)))
					   (if (_== first itr)
						   nil
						   (for (((tmp1 @~itr) (tmp2 @~itr)) t nil)
							 (_= tmp1 itr)
							 --itr
							 (when (funcall comp *itr *tmp1)
							   (_= tmp2 last)
							   --tmp2
							   (for (((val *itr)) (not (funcall comp val *tmp2)) nil)
								 --tmp2)
							   (swap *itr *tmp2)
							   (__reverse-imp-0b tmp1 last)
							   (return-from __next_permutation-imp-0 t))
							 (when (_== itr first)
							   (__reverse-imp-0b first last)
							   (return-from __next_permutation-imp-0 nil)))))))))

	(defmethod-overload next_permutation ((first bidirectional_iterator) (last bidirectional_iterator))
	  (__next_permutation-imp-0 first last #'operator_<))

	(defmethod-overload next_permutation ((first bidirectional_iterator) (last bidirectional_iterator) comp)
	  (__next_permutation-imp-0 first last (functor_function (clone comp)))))


  ;;PTN; next_permutation : 1 -   vp
  (labels ((__next_permutation-imp-1 (idx1 idx2 buffer less-bf)
			 (declare (type fixnum idx1 idx2))
			 (declare (type cl:vector buffer))
			 (declare (type cl:function less-bf))
			 (when (/= idx1 idx2)
			   (let ((idx (1- idx2)))
				 (declare (type fixnum idx))
				 (when (/= idx1 idx)
				   (let ((tmp1 idx)
						 (tmp2 idx))
					 (declare (type fixnum tmp1 tmp2))
					 (for (nil t nil)
					   (setf tmp1 idx)
					   (decf idx)
					   (when (funcall less-bf (aref buffer idx) (aref buffer tmp1))
						 (setf tmp2 idx2)
						 (decf tmp2)
						 (for (((val (aref buffer idx))) (not (funcall less-bf val (aref buffer tmp2))) nil)
						   (decf tmp2))
						 (swap (aref buffer idx) (aref buffer tmp2))
						 (__reverse-imp-1 tmp1 idx2 buffer)
						 (return-from __next_permutation-imp-1 t))
					   (when (= idx idx1)
						 (__reverse-imp-1 idx1 idx2 buffer)
						 (return-from __next_permutation-imp-1 nil)))))))))

	(defmethod-overload next_permutation ((first vector-pointer) (last vector-pointer))
	  ;;(format t "specialized next_permutation for vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__next_permutation-imp-1 (opr::vec-ptr-index  first)
								(opr::vec-ptr-index  last)
								(opr::vec-ptr-buffer first) #'operator_<))

	(defmethod-overload next_permutation ((first vector-pointer) (last vector-pointer) comp)
	  ;;(format t "specialized next_permutation for vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__next_permutation-imp-1 (opr::vec-ptr-index  first)
								(opr::vec-ptr-index  last)
								(opr::vec-ptr-buffer first) (functor_function (clone comp))))))




; first     : bidirectional_iterator
; last      : bidirectional_iterator
; comp      : binary_function ( default : #'operator_< )
; returns   : boolean value.
(locally (declare (optimize speed))

  ;;PTN; prev_permutation : 0 -  b 
  (labels ((__prev_permutation-imp-0 (first last comp)
			 (declare (type cl:function comp))
			 (if (_== first last)
				 nil
				 (with-operators
					 (let ((itr (prev last)))
					   (if (_== first itr)
						   nil
						   (for (((tmp1 @~itr) (tmp2 @~itr)) t nil)
							 (_= tmp1 itr)
							 --itr
							 (unless (funcall comp *itr *tmp1)
							   (_= tmp2 last)
							   --tmp2
							   (for (((val *itr)) (funcall comp val *tmp2) nil)
								 --tmp2)
							   (swap *itr *tmp2)
							   (__reverse-imp-0b tmp1 last)
							   (return-from __prev_permutation-imp-0 t))
							 (when (_== itr first)
							   (__reverse-imp-0b first last)
							   (return-from __prev_permutation-imp-0 nil)))))))))

	(defmethod-overload prev_permutation ((first bidirectional_iterator) (last bidirectional_iterator))
	  (__prev_permutation-imp-0 first last #'operator_<))

	(defmethod-overload prev_permutation ((first bidirectional_iterator) (last bidirectional_iterator) comp)
	  (__prev_permutation-imp-0 first last (functor_function (clone comp)))))


  ;;PTN; prev_permutation : 1 -  vp
  (labels ((__prev_permutation-imp-1 (idx1 idx2 buffer less-bf)
			 (declare (type fixnum idx1 idx2))
			 (declare (type cl:vector buffer))
			 (declare (type cl:function less-bf))
			 (when (/= idx1 idx2)
			   (let ((idx (1- idx2)))
				 (declare (type fixnum idx))
				 (when (/= idx1 idx)
				   (let ((tmp1 idx)
						 (tmp2 idx))
					 (declare (type fixnum tmp1 tmp2))
					 (for (nil t nil)
					   (setf tmp1 idx)
					   (decf idx)
					   (unless (funcall less-bf (aref buffer idx) (aref buffer tmp1))
						 (setf tmp2 idx2)
						 (decf tmp2)
						 (for (((val (aref buffer idx))) (funcall less-bf val (aref buffer tmp2)) nil)
						   (decf tmp2))
						 (swap (aref buffer idx) (aref buffer tmp2))
						 (__reverse-imp-1 tmp1 idx2 buffer)
						 (return-from __prev_permutation-imp-1 t))
					   (when (= idx idx1)
						 (__reverse-imp-1 idx1 idx2 buffer)
						 (return-from __prev_permutation-imp-1 nil)))))))))

	(defmethod-overload prev_permutation ((first vector-pointer) (last vector-pointer))
	  ;;(format t "specialized prev_permutation for vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__prev_permutation-imp-1 (opr::vec-ptr-index  first)
								(opr::vec-ptr-index  last)
								(opr::vec-ptr-buffer first) #'operator_<))

	(defmethod-overload prev_permutation ((first vector-pointer) (last vector-pointer) comp)
	  ;;(format t "specialized prev_permutation for vector-pointer is invoked.~%")
	  (__pointer-check-iterator-range first last)
	  (__prev_permutation-imp-1 (opr::vec-ptr-index  first)
								(opr::vec-ptr-index  last)
								(opr::vec-ptr-buffer first) (functor_function (clone comp))))))



