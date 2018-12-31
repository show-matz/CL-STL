(in-package :cl-stl)

(defconstant +VECTOR-MINIMUM-BUFSIZE+ 8)

;;--------------------------------------------------------------------
;;
;; class difinition
;;
;;--------------------------------------------------------------------
(defstruct (vector-core (:conc-name vec-core-))
  (size       0 :type fixnum)
  (capacity   0 :type fixnum)
;  #+cl-stl-debug
;  (dbgkey   nil :type symbol)
  (buffer   nil :type simple-vector))

(defclass vector (randomaccess_container pushable_back_container)
  ((core :type     :vector-core
		 :initform nil
		 :initarg  :core
		 :accessor vector-core)))

(defclass vector_const_iterator (const-vector-pointer randomaccess_iterator) ())
;  (#+cl-stl-debug
;   (debug-key :type     :symbol
;			  :initarg  :dbgkey
;			  :accessor vec-itr-dbgkey)))
(defclass vector_iterator (vector-pointer vector_const_iterator) ())

(defclass vector_const_reverse_iterator (const-reverse-vector-pointer randomaccess_iterator) ())
;  (#+cl-stl-debug
;   (debug-key :type     :symbol
;			  :initarg  :dbgkey
;			  :accessor vec-rev-itr-dbgkey)))
(defclass vector_reverse_iterator (reverse-vector-pointer vector_const_reverse_iterator) ())


;;--------------------------------------------------------------------
;;
;; internal utilities
;;
;;--------------------------------------------------------------------
; バッファの確保量を要素数 cnt から計算する。+VECTOR-MINIMUM-BUFSIZE+ から始め、cnt より小さくない最小の２のべき乗
(locally (declare (optimize speed))
  (defun __vector-calc-capacity (cnt)
	(declare (type fixnum cnt))
	(do ((size +VECTOR-MINIMUM-BUFSIZE+))
		((<= cnt size) size)
	  (declare (type fixnum size))
	  (setf size (ash size 1)))))

(defun __vector-create-core (&optional (buf-count 0))
  (let ((capacity (__vector-calc-capacity buf-count)))
	(make-vector-core :size     0
					  :capacity capacity
;					  #+cl-stl-debug :dbgkey
;					  #+cl-stl-debug (gensym)
					  :buffer   (make-array capacity :initial-element nil))))

(locally (declare (optimize speed))
  (defun __vector-fill-buffer (buffer begin end val)
	(declare (type simple-vector buffer))
	(declare (type fixnum begin end))
	(do ((idx begin (1+ idx)))
		((<= end idx) nil)
	  (declare (type fixnum idx))
	  (_= (svref buffer idx) val))))

(locally (declare (optimize speed))
  (defun __vector-copy-buffer (from-buf dest-buf cnt)
	(declare (type fixnum cnt))
	(declare (type simple-vector from-buf dest-buf))
	(do ((idx 0 (1+ idx)))
		((<= cnt idx) dest-buf)
	  (declare (type fixnum idx))
	  (_= (svref dest-buf idx) (svref from-buf idx)))))

(locally (declare (optimize speed))
  (defun __vector-move-buffer (from-buf dest-buf cnt)
	(declare (type fixnum cnt))
	(declare (type simple-vector from-buf dest-buf))
	(do ((idx 0 (1+ idx)))
		((<= cnt idx) dest-buf)
	  (declare (type fixnum idx))
	  (setf (svref dest-buf idx) (svref from-buf idx))
	  (setf (svref from-buf idx) nil))))

(locally (declare (optimize speed))
  (defun __vector-move-items-rightward (buffer begin end count)
	(declare (type simple-vector buffer))
	(declare (type fixnum begin end count))
	(if (or (zerop count) (= begin end))
		nil
		(do ((idx (1- end) (1- idx)))
			((< idx begin) nil)
		  (declare (type fixnum idx))
		  (_= (svref buffer (+ idx count)) (svref buffer idx))))))

(locally (declare (optimize speed))
  (defun __vector-move-items-leftward (buffer begin end count)
	(declare (type simple-vector buffer))
	(declare (type fixnum begin end count))
	(if (or (zerop count) (= begin end))
		nil
		(do ((idx begin (1+ idx)))
			((<= end idx) nil)
		  (declare (type fixnum idx))
		  (_= (svref buffer (- idx count)) (svref buffer idx))))))

(defun __vector-expand-buf (old-buf old-size new-capacity)
  (let ((new-buf (make-array new-capacity :initial-element nil)))
	(__vector-move-buffer old-buf new-buf old-size)))

(defmacro __vector-ensure-core-exist (vec-sym)
  (check-type vec-sym symbol)
  `(unless (vector-core ,vec-sym)
	 (setf (vector-core ,vec-sym) (__vector-create-core))))


(defmacro __vector-error-when-empty (core-sym op)
  (check-type core-sym symbol)
  `(when (or (null ,core-sym)
			 (zerop (vec-core-size ,core-sym)))
	 (error 'undefined-behavior :what ,(format nil "~A for empty vector." op))))

(defmacro __vector-check-index (core-sym idx-sym)
  (check-type core-sym symbol)
  (check-type idx-sym  symbol)
  `(unless (and (<= 0 ,idx-sym)
				(<    ,idx-sym (if (null ,core-sym)
								   0
								   (vec-core-size ,core-sym))))
	 (error 'out_of_range :what ,(format nil "index ~A is out of range." idx-sym))))

(defmacro __vector-check-iterator-belong (itr cont)
  (declare (ignorable itr cont))
  #-cl-stl-debug nil
  #+cl-stl-debug
  `(unless (eq (vec-core-buffer (vector-core ,cont)) (opr::vec-ptr-buffer ,itr))
	 (error 'undefined-behavior :what ,(format nil "~A is not iterator of ~A." itr cont))))

(defun __vector-data (vec)
  (__vector-ensure-core-exist vec)
  (vec-core-buffer (vector-core vec)))


;;--------------------------------------------------------------------
;;
;; method implementation
;;
;;--------------------------------------------------------------------
(locally (declare (optimize speed))
  (defun __vector-push_back (core new-val need-copy)
	(let ((size     (vec-core-size     core))
		  (capacity (vec-core-capacity core)))
	  (declare (type fixnum size capacity))
	  (when (= size capacity)
		(let ((buffer  (vec-core-buffer core))
			  (new-cap (__vector-calc-capacity (the fixnum (1+ size)))))
		  (declare (type fixnum new-cap))
		  (declare (type simple-vector buffer))
		  (setf (vec-core-capacity core) new-cap)
		  (setf (vec-core-buffer   core) (__vector-expand-buf buffer size new-cap))))
	  (if need-copy
		  (_= (svref (vec-core-buffer core) size) new-val)
		  (setf (svref (vec-core-buffer core) size) new-val))
	  (incf (vec-core-size core)))))

(defun __vector-clear (core)
  (when core
	(let ((size (vec-core-size core)))
	  (unless (zerop size)
		(__vector-fill-buffer (vec-core-buffer core) 0 size nil)
		(setf (vec-core-size core) 0)))))

(defun __vector-erase (core index count)
  (let ((size   (vec-core-size   core))
		(buffer (vec-core-buffer core)))
	(__vector-move-items-leftward buffer (+ index count) size count)
	(__vector-fill-buffer buffer (- size count) size nil))
  (decf (vec-core-size core) count))
  
(locally (declare (optimize speed))
  (defun __vector-resize (core new-size init-elt)
	(declare (type fixnum new-size))
	(let ((size   (vec-core-size   core))
		  (buffer (vec-core-buffer core)))
	  (declare (type fixnum size))
	  (declare (type simple-vector buffer))
	  (if (< new-size size)
		  (__vector-fill-buffer buffer new-size size nil)
		  (when (< size new-size)
			(when (< (vec-core-capacity core) new-size)
			  (let ((new-cap (__vector-calc-capacity new-size)))
				(declare (type fixnum new-cap))
				(setf buffer (__vector-expand-buf buffer size new-cap))
				(setf (vec-core-buffer   core) buffer)
				(setf (vec-core-capacity core) new-cap)))
			(__vector-fill-buffer buffer size new-size init-elt)))
	  (setf (vec-core-size core) new-size))))

(defun __vector-reserve (core reserve-size)
  (let ((size     (vec-core-size     core))
		(capacity (vec-core-capacity core))
		(buffer   (vec-core-buffer   core))
		(new-cap  (__vector-calc-capacity reserve-size)))
	(when (< capacity new-cap)
	  (setf (vec-core-buffer   core) (__vector-expand-buf buffer size new-cap))
	  (setf (vec-core-capacity core) new-cap))))

(locally (declare (optimize speed))
  (defun __vector-counted-assign (core count get-next-value)
	(declare (type fixnum count))
	(declare (type cl:function get-next-value))
	(__vector-resize core count nil)
	(do ((i 0 (1+ i))
		 (buf (vec-core-buffer core)))
		((= i count) nil)
	  (declare (type fixnum i))
	  (declare (type simple-vector buf))
	  (_= (svref buf i) (funcall get-next-value)))))

(locally (declare (optimize speed))
  (defun __vector-uncounted-assign (core get-next-value eos-sym)
	(declare (type cl:function get-next-value))
	(let ((old-size (vec-core-size core))
		  (count 0)
		  (next-val (funcall get-next-value)))
	  (declare (type fixnum old-size count))
	  (do* ((idx 0 (1+ idx))
			(cap (vec-core-capacity core))
			(buf (vec-core-buffer   core)))
		   ((or (= idx cap)
				(eq next-val eos-sym)) nil)
		(declare (type fixnum idx cap))
		(declare (type simple-vector buf))
		(_= (svref buf idx) next-val)
		(setf next-val (funcall get-next-value))
		(incf count))
	  (if (< count old-size)
		  (__vector-resize core count nil)
		  (setf (vec-core-size core) count))
	  (do ()
		  ((eq next-val eos-sym) nil)
		(__vector-push_back core next-val t)
		(setf next-val (funcall get-next-value))))))

(locally (declare (optimize speed))
  (defun __vector-counted-insert (core itr count get-next-value need-copy)
	(declare (type fixnum count))
	(declare (type cl:function get-next-value))
	(let* ((old-size (vec-core-size core))
		   (new-size (+ count old-size)))
	  (declare (type fixnum old-size new-size))
	  (__vector-resize core new-size nil)
	  (let ((buf  (vec-core-buffer core))
			(base (opr::vec-ptr-index   itr)))
		(declare (type fixnum base))
		(__vector-move-items-rightward buf base old-size count)
		(do ((i base (1+ i))
			 (last (+ base count)))
			((= i last) nil)
		  (declare (type fixnum i last))
		  (declare (type simple-vector buf))
		  (if need-copy
			  (_= (svref buf i) (funcall get-next-value))
			  (setf (svref buf i) (funcall get-next-value))))))))

(defun __create-vector (size initial-element)
  (__error-unless-non-negative-fixnum vector size)
  (if (zerop size)
	  (make-instance 'stl:vector)
	  (let ((core (__vector-create-core size)))
		(when initial-element
		  (__vector-fill-buffer (vec-core-buffer core)
								0 size initial-element))
		(setf (vec-core-size core) size)
		(make-instance 'stl:vector :core core))))



;;------------------------------------------------------------------------------
;;
;; constructors
;;
;;------------------------------------------------------------------------------
(declare-constructor vector (0 1 2))

; default constructor
(define-constructor vector ()
  (__create-vector 0 nil))

; copy constructor
(define-constructor vector ((arg stl:vector))
  (clone arg))

; constructor with initializer list
#-cl-stl-0x98
(locally (declare (optimize speed))
  (define-constructor vector ((arg initializer_list))
	(declare (type initializer_list arg))
	(let* ((idx 0)
		   (arr (__initlist-data arg))
		   (cnt (length arr))
		   (obj (__create-vector cnt nil))
		   (dst (__vector-data obj)))
	  (declare (type simple-vector arr dst))
	  (declare (type fixnum idx cnt))
	  (for (nil (< idx cnt) (incf idx) :returns obj)
		(_= (svref dst idx) (svref arr idx))))))


; move constructor
#-cl-stl-0x98
(locally (declare (optimize speed))
  (define-constructor vector ((arg& remove-reference))
	(with-reference (arg)
	  (let ((cont arg))
		(__check-type-of-move-constructor cont stl:vector)
		(let ((core (vector-core cont)))
		  (setf (vector-core cont) nil)
		  (make-instance 'stl:vector :core core))))))

; fill constructor 1
(define-constructor vector ((arg integer))
  (__create-vector arg  nil))

; fill constructor 2
(define-constructor vector ((arg1 integer) arg2)
  (__create-vector arg1 arg2))


;; range constructor 2
(locally (declare (optimize speed))

  (define-constructor vector ((itr1 input_iterator) (itr2 input_iterator))
	(if (_== itr1 itr2)
		(make-instance 'stl:vector)
		;; get sequence length, only when randomaccess_iterator.
		;; otherwise, use auto-extend.
		(let ((core (if (not (typep itr1 'randomaccess_iterator))
						(__vector-create-core)
						(__vector-create-core (_- itr2 itr1)))))
		  (with-operators
			  (for (((itr @~itr1)) (_/= itr itr2) ++itr)
				(__vector-push_back core *itr t))
			(make-instance 'stl:vector :core core)))))

  (define-constructor vector ((itr1 const-vector-pointer) (itr2 const-vector-pointer))
	(__pointer-check-iterator-range itr1 itr2)
	(let ((idx1 (opr::vec-ptr-index  itr1))
		  (idx2 (opr::vec-ptr-index  itr2))
		  (buf  (opr::vec-ptr-buffer itr1)))
	  (declare (type fixnum idx1 idx2))
	  (declare (type cl:vector buf))
	  (if (= idx1 idx2)
		  (make-instance 'stl:vector)
		  (let ((core (__vector-create-core (the fixnum (- idx2 idx1)))))
			(for (nil (< idx1 idx2) (incf idx1))
			  (__vector-push_back core (aref buf idx1) t))
			(make-instance 'stl:vector :core core))))))


; copy constructor
(defmethod operator_clone ((container stl:vector))
  (let ((cnt (size container)))
	(if (zerop cnt)
		(make-instance 'stl:vector)
		(let* ((core     (__vector-create-core cnt))
			   (dest-buf (vec-core-buffer core)))
		  (__vector-copy-buffer (vec-core-buffer (vector-core container)) dest-buf cnt)
		  (setf (vec-core-size core) cnt)
		  (make-instance 'stl:vector :core core)))))


;;------------------------------------------------------------------------------
;;
;; methods
;;
;;------------------------------------------------------------------------------

;-----------------------------------------------------
; assignment
;-----------------------------------------------------
(locally (declare (optimize speed))
  (defmethod operator_= ((cont1 stl:vector) (cont2 stl:vector))
	(let ((new-size (size cont2)))
	  (declare (type fixnum new-size))
	  (if (zerop new-size)
		  (clear cont1)
		  (progn
			(__vector-ensure-core-exist cont1)
			(__vector-resize (vector-core cont1) new-size nil)
			(let ((idx 0)
				  (src-buf (vec-core-buffer (vector-core cont2)))
				  (dst-buf (vec-core-buffer (vector-core cont1))))
			  (declare (type fixnum idx))
			  (declare (type simple-vector src-buf dst-buf))
			  (for (nil (< idx new-size) (incf idx))
				(_= (svref dst-buf idx) (svref src-buf idx)))))))
	cont1))

#-cl-stl-0x98
(defmethod operator_= ((cont stl:vector) (il initializer_list))
  (assign cont il)
  cont)

#-cl-stl-0x98
(defmethod operator_move ((cont1 stl:vector) (cont2 stl:vector))
  (unless (eq cont1 cont2)
	(setf (vector-core cont1) (vector-core cont2))
	(setf (vector-core cont2) nil))
  (values cont1 cont2))


;MEMO : always returns nil.
(defmethod-overload assign ((cont stl:vector) (count integer) value)
  (__error-unless-non-negative-fixnum assign count)
  (__vector-ensure-core-exist cont)
  (__vector-counted-assign (vector-core cont) count (lambda () value))
  nil)

;; sequence assign.
;;MEMO : always returns nil.
(locally (declare (optimize speed))

  (defmethod-overload assign ((cont stl:vector) (itr1 input_iterator) (itr2 input_iterator))
	(if (_== itr1 itr2)
		(__vector-clear (vector-core cont))
		(let ((eos (gensym))
			  (itr (clone itr1)))
		  (__vector-ensure-core-exist cont)
		  (__vector-uncounted-assign (vector-core cont)
									 (lambda ()
									   (with-operators
										   (prog1 (if (_== itr itr2) eos *itr)
											 ++itr))) eos)))
	nil)

  (defmethod-overload assign ((cont stl:vector)
							  (itr1 randomaccess_iterator) (itr2 randomaccess_iterator))
	(if (_== itr1 itr2)
		(__vector-clear (vector-core cont))
		(let ((itr (clone itr1)))
		  (__vector-ensure-core-exist cont)
		  (__vector-counted-assign (vector-core cont)
								   (_- itr2 itr1)
								   (lambda ()
									 (with-operators
										 (prog1 *itr ++itr))))))
	nil)

  (defmethod-overload assign ((cont stl:vector)
							  (itr1 const-vector-pointer) (itr2 const-vector-pointer))
	(__pointer-check-iterator-range itr1 itr2)
	(let ((idx1 (opr::vec-ptr-index  itr1))
		  (idx2 (opr::vec-ptr-index  itr2))
		  (buf  (opr::vec-ptr-buffer itr1)))
	  (declare (type fixnum idx1 idx2))
	  (declare (type cl:vector buf))
	  (if (= idx1 idx2)
		  (__vector-clear (vector-core cont))
		  (progn
			(__vector-ensure-core-exist cont)
			(__vector-counted-assign (vector-core cont)
									 (the fixnum (- idx2 idx1))
									 (lambda ()
									   (prog1 (aref buf idx1) (incf idx1)))))))
	nil))


;;MEMO : always returns nil.
#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod-overload assign ((cont stl:vector) (arg initializer_list))
	(declare (type initializer_list arg))
	(let* ((arr (__initlist-data arg))
		   (cnt (length arr)))
	  (declare (type simple-vector arr))
	  (declare (type fixnum cnt))
	  (if (zerop cnt)
		  (__vector-clear (vector-core cont))
		  (progn
			(__vector-ensure-core-exist cont)
			(let ((core (vector-core cont)))
			  (__vector-resize core cnt nil)
			  (let ((idx 0)
					(dst (vec-core-buffer core)))
				(declare (type fixnum idx))
				(declare (type simple-vector dst))
				(for (nil (< idx cnt) (incf idx))
				  (_= (svref dst idx) (svref arr idx))))))))
	nil))
	  

;-----------------------------------------------------
; iterators
;-----------------------------------------------------
(defmethod begin ((cont stl:vector))
  (__vector-ensure-core-exist cont)
  (let ((core (vector-core cont)))
	(make-instance 'vector_iterator
;				   #+cl-stl-debug :dbgkey
;				   #+cl-stl-debug (vec-core-dbgkey core)
				   :buffer (vec-core-buffer core)
				   :index 0)))

(defmethod end ((cont stl:vector))
  (__vector-ensure-core-exist cont)
  (let ((core (vector-core cont)))
	(make-instance 'vector_iterator
				   :buffer (vec-core-buffer core)
				   :index (vec-core-size core))))

(defmethod rbegin ((cont stl:vector))
  (__vector-ensure-core-exist cont)
  (let ((core (vector-core cont)))
	(make-instance 'vector_reverse_iterator
				   :buffer (vec-core-buffer core)
				   :index (1- (vec-core-size core)))))

(defmethod rend ((cont stl:vector))
  (__vector-ensure-core-exist cont)
  (let ((core (vector-core cont)))
	(make-instance 'vector_reverse_iterator
				   :buffer (vec-core-buffer core) :index -1)))

#-cl-stl-0x98
(defmethod cbegin ((cont stl:vector))
  (__vector-ensure-core-exist cont)
  (make-instance 'vector_const_iterator
				 :buffer (vec-core-buffer (vector-core cont)) :index 0))

#-cl-stl-0x98
(defmethod cend ((cont stl:vector))
  (__vector-ensure-core-exist cont)
  (let ((core (vector-core cont)))
	(make-instance 'vector_const_iterator
				   :buffer (vec-core-buffer core)
				   :index (vec-core-size core))))

#-cl-stl-0x98
(defmethod crbegin ((cont stl:vector))
  (__vector-ensure-core-exist cont)
  (let ((core (vector-core cont)))
	(make-instance 'vector_const_reverse_iterator
				   :buffer (vec-core-buffer core)
				   :index (1- (vec-core-size core)))))

#-cl-stl-0x98
(defmethod crend ((cont stl:vector))
  (__vector-ensure-core-exist cont)
  (let ((core (vector-core cont)))
	(make-instance 'vector_const_reverse_iterator
				   :buffer (vec-core-buffer core) :index -1)))

;-----------------------------------------------------
; capacity
;-----------------------------------------------------
(defmethod empty ((cont stl:vector))
  (let ((core (vector-core cont)))
	(or (null core)
		(zerop (vec-core-size core)))))

(defmethod size ((cont stl:vector))
  (let ((core (vector-core cont)))
	(if (null core)
		0
		(vec-core-size core))))

(defmethod max_size ((cont stl:vector))
  most-positive-fixnum)

(labels ((imp (cont new-size initial-element)
		   (__error-unless-non-negative-fixnum resize new-size)
		   (if (zerop new-size)
			   (__vector-clear (vector-core cont))
			   (progn
				 (__vector-ensure-core-exist cont)
				 (__vector-resize (vector-core cont) new-size initial-element)))))

  (defmethod-overload resize ((cont stl:vector) (new-size integer))
	(imp cont new-size nil)
	nil)

  (defmethod-overload resize ((cont stl:vector) (new-size integer) initial-element)
	(imp cont new-size initial-element)
	nil))

(defmethod capacity ((cont stl:vector))
  (__vector-ensure-core-exist cont)
  (vec-core-capacity (vector-core cont)))

(defmethod reserve ((cont stl:vector) (size integer))
  (__vector-ensure-core-exist cont)
  (__vector-reserve (vector-core cont) size)
  nil)

;-----------------------------------------------------
; element access
;-----------------------------------------------------
(defmethod front ((cont stl:vector))
  (let ((core (vector-core cont)))
	(__vector-error-when-empty core "front")
	(svref (vec-core-buffer core) 0)))

(defmethod (setf front) (val (cont stl:vector))
  (let ((core (vector-core cont)))
	(__vector-error-when-empty core "front")
	(_= (svref (vec-core-buffer core) 0) val)))

(defmethod back ((cont stl:vector))
  (let ((core (vector-core cont)))
	(__vector-error-when-empty core "back")
	(svref (vec-core-buffer core) (1- (vec-core-size core)))))

(defmethod (setf back) (val (cont stl:vector))
  (let ((core (vector-core cont)))
	(__vector-error-when-empty core "back")
	(_= (svref (vec-core-buffer core) (1- (vec-core-size core))) val)))

(defmethod at ((cont stl:vector) (idx integer))
  (let ((core (vector-core cont)))
	(__vector-check-index core idx)
	(svref (vec-core-buffer core) idx)))

(defmethod (setf at) (val (cont stl:vector) (idx integer))
  (let ((core (vector-core cont)))
	(__vector-check-index core idx)
	(_= (svref (vec-core-buffer core) idx) val)))

(defmethod operator_[] ((cont stl:vector) (idx integer))
  (svref (vec-core-buffer (vector-core cont)) idx))

(defmethod (setf operator_[]) (val (cont stl:vector) (idx integer))
  (_= (svref (vec-core-buffer (vector-core cont)) idx) val))

(defmethod operator_& ((cont stl:vector) (idx integer))
  (let* ((core (vector-core cont))
		 (cnt  (if core (vec-core-size core) 0)))
	(if (zerop cnt)
		(error 'undefined-behavior :what "operator_& for empty vector.")
		(if (or (< idx 0) (< cnt idx))
			(error 'out_of_range :what (format nil "index ~A is out of range." idx))
			(make-instance 'vector-pointer :buffer (vec-core-buffer core) :index idx)))))
  
(defmethod operator_const& ((cont stl:vector) (idx integer))
  (let* ((core (vector-core cont))
		 (cnt  (if core (vec-core-size core) 0)))
	(if (zerop cnt)
		(error 'undefined-behavior :what "operator_const& for empty vector.")
		(if (or (< idx 0) (< cnt idx))
			(error 'out_of_range :what (format nil "index ~A is out of range." idx))
			(make-instance 'const-vector-pointer :buffer (vec-core-buffer core) :index idx)))))


#-cl-stl-0x98
(defmethod data ((container stl:vector))
  (__vector-ensure-core-exist container)
  (vec-core-buffer (vector-core container)))

;-----------------------------------------------------
; modifiers
;-----------------------------------------------------
(defmethod push_back ((container stl:vector) new-val)
  (__vector-ensure-core-exist container)
  (__vector-push_back (vector-core container) new-val t)
  nil)

(defmethod pop_back ((cont stl:vector))
  (let ((core (vector-core cont)))
	(__vector-error-when-empty core "pop_back")
	(decf (vec-core-size core))
	(setf (svref (vec-core-buffer core) (vec-core-size core)) nil)))

#-cl-stl-0x98    ; emplace_back
(defmethod-overload emplace_back ((container stl:vector) new-val)
  (__vector-ensure-core-exist container)
  (__vector-push_back (vector-core container) new-val nil)
  #+(or cl-stl-0x11 cl-stl-0x14) nil
  #-(or cl-stl-0x11 cl-stl-0x14) new-val)

#-cl-stl-0x98    ; shrink_to_fit
(defmethod shrink_to_fit ((cont stl:vector))
  (let ((core (vector-core cont)))
	(when core
	  (let* ((size    (vec-core-size core))
			 (new-buf (make-array size :initial-element nil)))
		(__vector-move-buffer (vec-core-buffer core) new-buf size)
		(setf (vec-core-buffer   core) new-buf)
		(setf (vec-core-capacity core) size))))
  nil)

;; insert ( single elemente ) - returns iterator
(defmethod-overload insert ((cont stl:vector)
							(itr #+cl-stl-0x98 vector_iterator
								 #-cl-stl-0x98 vector_const_iterator) value)
  (__vector-check-iterator-belong itr cont)
  (let ((idx (opr::vec-ptr-index itr))
		(core (vector-core cont)))
	(declare (type fixnum idx))
	(if (= idx (vec-core-size core))
		(__vector-push_back core value t)
		(__vector-counted-insert core itr 1 (lambda () value) t))
	(make-instance 'vector_iterator
				   :buffer (vec-core-buffer core) :index idx)))

;; insert ( fill )
(defmethod-overload insert ((cont stl:vector)
							(itr #+cl-stl-0x98 vector_iterator
								 #-cl-stl-0x98 vector_const_iterator) (count integer) value)
  ;; MEMO : in C++98, always returns nil. but C++11, returns iterator.
  (__error-unless-non-negative-fixnum insert count)
  (__vector-check-iterator-belong itr cont)
  (let ((core (vector-core cont)))
	(__vector-counted-insert core itr count (lambda () value) t)
	#+cl-stl-0x98 nil
	#-cl-stl-0x98 (make-instance 'vector_iterator 
								 :buffer (vec-core-buffer core) :index (opr::vec-ptr-index itr))))

;; range insert.
;; MEMO : in C++98, always returns nil. but C++11, returns iterator.
(locally (declare (optimize speed))

  (defmethod-overload insert ((cont stl:vector) (itr #+cl-stl-0x98 vector_iterator
													 #-cl-stl-0x98 vector_const_iterator)
							  (itr1 input_iterator) (itr2 input_iterator))
	(__vector-check-iterator-belong itr cont)
	(if (_== itr1 itr2)
		(progn
		  #+cl-stl-0x98 nil
		  #-cl-stl-0x98 (clone itr))
		(let ((idx (opr::vec-ptr-index itr))
			  (core (vector-core cont)))
		  (declare (type fixnum idx))
		  (cond
			((= idx (vec-core-size core))
			 (with-operators
				 (for (((itr1 @~itr1)) (_/= itr1 itr2) ++itr1)
				   (__vector-push_back core *itr1 t))))
			(t
			 (locally (declare (optimize speed))
			   (let* ((tmp (new stl:vector itr1 itr2))
					  (idx 0)
					  (buf (__vector-data tmp)))
				 (declare (type fixnum idx))
				 (declare (type simple-vector buf))
				 (__vector-counted-insert core itr (size tmp)
										  (lambda ()
											(prog1
												(svref buf idx)
											  (incf idx))) t)))))
		  #+cl-stl-0x98 nil
		  #-cl-stl-0x98 (make-instance 'vector_iterator 
									   :buffer (vec-core-buffer core)
									   :index  (opr::vec-ptr-index itr)))))

  (defmethod-overload insert ((cont stl:vector) (itr #+cl-stl-0x98 vector_iterator
													 #-cl-stl-0x98 vector_const_iterator)
							  (itr1 forward_iterator) (itr2 forward_iterator))
	(__vector-check-iterator-belong itr cont)
	(if (_== itr1 itr2)
		(progn
		  #+cl-stl-0x98 nil
		  #-cl-stl-0x98 (clone itr))
		(let ((core (vector-core cont))
			  (itr1 (clone itr1))
			  (cnt (distance itr1 itr2)))
		  (__vector-counted-insert core itr cnt
								   (lambda ()
									 (with-operators
										 (prog1 *itr1 ++itr1))) t)
		  #+cl-stl-0x98 nil
		  #-cl-stl-0x98 (make-instance 'vector_iterator 
									   :buffer (vec-core-buffer core)
									   :index  (opr::vec-ptr-index itr)))))

  (defmethod-overload insert ((cont stl:vector) (itr #+cl-stl-0x98 vector_iterator
													 #-cl-stl-0x98 vector_const_iterator)
							  (itr1 const-vector-pointer) (itr2 const-vector-pointer))
	(__vector-check-iterator-belong itr cont)
	(__pointer-check-iterator-range itr1 itr2)
	(let ((idx1 (opr::vec-ptr-index  itr1))
		  (idx2 (opr::vec-ptr-index  itr2))
		  (buf  (opr::vec-ptr-buffer itr1)))
	  (declare (type fixnum idx1 idx2))
	  (declare (type cl:vector buf))
	  (if (= idx1 idx2)
		  (progn
			#+cl-stl-0x98 nil
			#-cl-stl-0x98 (clone itr))
		  (let ((core (vector-core cont))
				(cnt  (- idx2 idx1)))
			(declare (type fixnum cnt))
			(__vector-counted-insert core itr cnt
									 (lambda ()
									   (prog1 (aref buf idx1) (incf idx1))) t)
			#+cl-stl-0x98 nil
			#-cl-stl-0x98 (make-instance 'vector_iterator 
										 :buffer (vec-core-buffer core)
										 :index  (opr::vec-ptr-index itr)))))))


;; insert ( move ) - returns iterator
#-cl-stl-0x98
(defmethod-overload insert ((cont stl:vector)
							(itr  vector_const_iterator) (rm& remove-reference))
  (__vector-check-iterator-belong itr cont)
  (with-reference (rm)
	(__vector-counted-insert (vector-core cont) itr 1 (lambda () rm) nil)
	(setf rm nil))
  (make-instance 'vector_iterator
				 :buffer (vec-core-buffer (vector-core cont))
				 :index  (opr::vec-ptr-index itr)))

;; insert ( initializer list ) - returns iterator
#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod-overload insert ((cont stl:vector)
							  (itr  vector_const_iterator) (il initializer_list))
	(declare (type initializer_list il))
	(__vector-check-iterator-belong itr cont)
	(let* ((arr (__initlist-data il))
		   (idx 0)
		   (cnt (length arr)))
	  (declare (type simple-vector arr))
	  (declare (type fixnum idx cnt))
	  (unless (zerop cnt)
		(__vector-counted-insert (vector-core cont) itr cnt
								 (lambda ()
								   (prog1
									   (svref arr idx)
									 (incf idx))) t)))
	(make-instance 'vector_iterator
				   :buffer (vec-core-buffer (vector-core cont))
				   :index (opr::vec-ptr-index itr))))

#-cl-stl-0x98    ; emplace
(defmethod-overload emplace ((cont stl:vector)
							 (itr vector_const_iterator) new-val)
  (__vector-check-iterator-belong itr cont)
  (let ((idx (opr::vec-ptr-index itr))
		(core (vector-core cont)))
	(if (= idx (vec-core-size core))
		(__vector-push_back core new-val nil)
		(__vector-counted-insert core itr 1 (lambda () new-val) nil))
	(make-instance 'vector_iterator
				   :buffer (vec-core-buffer core) :index idx)))

(defmethod-overload erase ((cont stl:vector)
						   (itr #+cl-stl-0x98 vector_iterator
								#-cl-stl-0x98 vector_const_iterator))
  (__vector-check-iterator-belong itr cont)
  (let ((idx (opr::vec-ptr-index itr))
		(core (vector-core cont)))
	(if (< idx (1- (vec-core-size core)))
		(__vector-erase core idx 1)
		(progn
		  (decf (vec-core-size core))
		  (setf (svref (vec-core-buffer core) (vec-core-size core)) nil)))
	(make-instance 'vector_iterator
				   :buffer (vec-core-buffer core) :index idx)))

(defmethod-overload erase ((cont stl:vector)
						   (first #+cl-stl-0x98 vector_iterator
								  #-cl-stl-0x98 vector_const_iterator)
						   (last  #+cl-stl-0x98 vector_iterator
								  #-cl-stl-0x98 vector_const_iterator))
  (__vector-check-iterator-belong first cont)
  (__pointer-check-iterator-range first last)
  (let ((idx1 (opr::vec-ptr-index first))
		(idx2 (opr::vec-ptr-index last))
		(core (vector-core cont)))
	(cond
	  ((and (zerop idx1) (= idx2 (vec-core-size core)))
	   (__vector-clear core))
	  ((= idx2 (vec-core-size core))
	   (__vector-resize core idx1 nil))
	  (t
	   (__vector-erase (vector-core cont) (opr::vec-ptr-index first) (_- last first))))
	(make-instance 'vector_iterator
				   :buffer (vec-core-buffer core) :index idx1)))

(defmethod-overload swap ((cont1 stl:vector) (cont2 stl:vector))
  (let ((core (vector-core cont1)))
	(setf (vector-core cont1) (vector-core cont2))
	(setf (vector-core cont2) core))
  (values cont1 cont2))

(defmethod clear ((cont stl:vector))
  (__vector-clear (vector-core cont)))


;-----------------------------------------------------
; compare
;-----------------------------------------------------
(locally (declare (optimize speed))
  (labels ((__container-equal (cont1 cont2)
			 (if (eq cont1 cont2)
				 t
				 (if (/= (the fixnum (size cont1)) (the fixnum (size cont2)))
					 nil
					 (let ((cnt (size cont1)))
					   (declare (type fixnum cnt))
					   (if (zerop cnt)
						   t
						   (let ((idx 0)
								 (buf1 (vec-core-buffer (vector-core cont1)))
								 (buf2 (vec-core-buffer (vector-core cont2))))
							 (declare (type fixnum idx))
							 (declare (type simple-vector buf1 buf2))
							 (for (nil (< idx cnt) (incf idx) :returns t)
							   (unless (_== (svref buf1 idx) (svref buf2 idx))
								 (return-from __container-equal nil))))))))))

	(defmethod operator_== ((cont1 stl:vector) (cont2 stl:vector))
	  (__container-equal cont1 cont2))

	(defmethod operator_/= ((cont1 stl:vector) (cont2 stl:vector))
	  (not (__container-equal cont1 cont2)))))


(locally (declare (optimize speed))
  (labels ((__container-compare (cont1 cont2)
			 (if (eq cont1 cont2)
				 0
				 (let* ((cnt1 (size cont1))
						(cnt2 (size cont2)))
				   (declare (type fixnum cnt1 cnt2))
				   (cond
					 ((and (zerop cnt1)
						   (zerop cnt2)) 0)
					 ((zerop cnt1)      -1)
					 ((zerop cnt2)       1)
					 (t (let ((idx1 0)
							  (idx2 0)
							  (buf1 (vec-core-buffer (vector-core cont1)))
							  (buf2 (vec-core-buffer (vector-core cont2))))
						  (declare (type fixnum idx1 idx2))
						  (declare (type simple-vector buf1 buf2))
						  (for (nil (or (< idx1 cnt1) (< idx2 cnt2)) (progn (incf idx1) (incf idx2)) :returns 0)
							(if (= idx1 cnt1)
								(return-from __container-compare -1)
								(if (= idx2 cnt2)
									(return-from __container-compare 1)
									(let ((val1 (svref buf1 idx1))
										  (val2 (svref buf2 idx2)))
									  (if (_< val1 val2)
										  (return-from __container-compare -1)
										  (if (_< val2 val1)
											  (return-from __container-compare 1))))))))))))))

	(defmethod operator_< ((cont1 stl:vector) (cont2 stl:vector))
	  (< (__container-compare cont1 cont2) 0))

	(defmethod operator_<= ((cont1 stl:vector) (cont2 stl:vector))
	  (<= (__container-compare cont1 cont2) 0))

	(defmethod operator_> ((cont1 stl:vector) (cont2 stl:vector))
	  (< 0 (__container-compare cont1 cont2)))

	(defmethod operator_>= ((cont1 stl:vector) (cont2 stl:vector))
	  (<= 0 (__container-compare cont1 cont2)))))


;-----------------------------------------------------
; enumeration
;-----------------------------------------------------
#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod-overload for ((cont stl:vector) func)
	(declare (type cl:function func))
	;MEMO : func is always lambda function ( see stl:for ). 
	(let ((core (vector-core cont)))
	  (when core
		(let ((idx 0)
			  (cnt (size cont))
			  (buf (vec-core-buffer core)))
		  (declare (type fixnum idx cnt))
		  (declare (type simple-vector buf))
		  (for (nil (< idx cnt) (incf idx))
			(funcall func (svref buf idx))))))))


;;------------------------------------------------------------------------------
;;
;; methods for vector_const_iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_= ((itr1 vector_const_iterator) (itr2 vector_const_iterator))
  (__error-when-const-removing-assign itr1 vector_iterator
									  itr2 vector_const_iterator)
  (setf (opr::vec-ptr-buffer itr1) (opr::vec-ptr-buffer itr2))
  (setf (opr::vec-ptr-index  itr1) (opr::vec-ptr-index  itr2))
  itr1)

(defmethod operator_clone ((itr vector_const_iterator))
  (make-instance 'vector_const_iterator
				 :buffer (opr::vec-ptr-buffer itr)
				 :index  (opr::vec-ptr-index  itr)))

(defmethod operator_+ ((itr vector_const_iterator) (n integer))
  (make-instance 'vector_const_iterator
				 :buffer (opr::vec-ptr-buffer itr)
				 :index  (+ n (opr::vec-ptr-index itr))))

(defmethod operator_- ((itr vector_const_iterator) (n integer))
  (make-instance 'vector_const_iterator
				 :buffer (opr::vec-ptr-buffer itr)
				 :index  (- (opr::vec-ptr-index itr) n)))

;; creating reverse iterator.
(define-constructor reverse_iterator ((itr vector_const_iterator))
  (make-instance 'vector_const_reverse_iterator
				 :buffer (opr::vec-ptr-buffer itr)
				 :index  (1- (opr::vec-ptr-index  itr))))


;;------------------------------------------------------------------------------
;;
;; methods for vector_iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_clone ((itr vector_iterator))
  (make-instance 'vector_iterator
				 :buffer (opr::vec-ptr-buffer itr)
				 :index  (opr::vec-ptr-index  itr)))

(defmethod operator_cast ((itr vector_iterator)
						  (typename (eql 'vector_const_iterator)))
  (__check-exact-type-of-cast itr 'vector_iterator 'vector_const_iterator)
  (make-instance 'vector_const_iterator
				 :buffer (opr::vec-ptr-buffer itr)
				 :index  (opr::vec-ptr-index  itr)))

(defmethod operator_+ ((itr vector_iterator) (n integer))
  (make-instance 'vector_iterator
				 :buffer (opr::vec-ptr-buffer itr)
				 :index  (+ n (opr::vec-ptr-index itr))))

(defmethod operator_- ((itr vector_iterator) (n integer))
  (make-instance 'vector_iterator
				 :buffer (opr::vec-ptr-buffer itr)
				 :index  (- (opr::vec-ptr-index itr) n)))

;; creating reverse iterator.
(define-constructor reverse_iterator ((itr vector_iterator))
  (make-instance 'vector_reverse_iterator
				 :buffer (opr::vec-ptr-buffer itr)
				 :index  (1- (opr::vec-ptr-index  itr))))


;;------------------------------------------------------------------------------
;;
;; methods for vector_const_reverse_iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_= ((itr1 vector_const_reverse_iterator)
					  (itr2 vector_const_reverse_iterator))
  (__error-when-const-removing-assign itr1 vector_reverse_iterator
									  itr2 vector_const_reverse_iterator)
  (setf (opr::rev-vec-ptr-buffer itr1) (opr::rev-vec-ptr-buffer itr2))
  (setf (opr::rev-vec-ptr-index  itr1) (opr::rev-vec-ptr-index  itr2))
  itr1)

(defmethod operator_clone ((itr vector_const_reverse_iterator))
  (make-instance 'vector_const_reverse_iterator
				 :buffer (opr::rev-vec-ptr-buffer itr)
				 :index  (opr::rev-vec-ptr-index  itr)))

(defmethod operator_+ ((itr vector_const_reverse_iterator) (n integer))
  (make-instance 'vector_const_reverse_iterator
				 :buffer (opr::rev-vec-ptr-buffer itr)
				 :index  (- (opr::rev-vec-ptr-index itr) n)))

(defmethod operator_- ((itr vector_const_reverse_iterator) (n integer))
  (make-instance 'vector_const_reverse_iterator
				 :buffer (opr::rev-vec-ptr-buffer itr)
				 :index  (+ (opr::rev-vec-ptr-index itr) n)))

(defmethod base ((rev-itr vector_const_reverse_iterator))
  (make-instance 'vector_const_iterator
				 :buffer (opr::rev-vec-ptr-buffer rev-itr)
				 :index  (1+ (opr::rev-vec-ptr-index rev-itr))))

;; creating reverse iterator.
(define-constructor reverse_iterator ((itr vector_const_reverse_iterator))
  (make-instance 'vector_const_iterator
				 :buffer (opr::rev-vec-ptr-buffer itr)
				 :index  (1+ (opr::rev-vec-ptr-index  itr))))


;;------------------------------------------------------------------------------
;;
;; methods for vector_reverse_iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_clone ((itr vector_reverse_iterator))
  (make-instance 'vector_reverse_iterator
				 :buffer (opr::rev-vec-ptr-buffer itr)
				 :index  (opr::rev-vec-ptr-index  itr)))

(defmethod operator_cast ((itr vector_reverse_iterator)
						  (typename (eql 'vector_const_reverse_iterator)))
  (__check-exact-type-of-cast itr 'vector_reverse_iterator
								  'vector_const_reverse_iterator)
  (make-instance 'vector_const_reverse_iterator
				 :buffer (opr::rev-vec-ptr-buffer itr)
				 :index  (opr::rev-vec-ptr-index  itr)))

(defmethod operator_+ ((itr vector_reverse_iterator) (n integer))
  (make-instance 'vector_reverse_iterator
				 :buffer (opr::rev-vec-ptr-buffer itr)
				 :index  (+ n (opr::rev-vec-ptr-index itr))))

(defmethod operator_- ((itr vector_reverse_iterator) (n integer))
  (make-instance 'vector_reverse_iterator
				 :buffer (opr::rev-vec-ptr-buffer itr)
				 :index  (- (opr::rev-vec-ptr-index itr) n)))

(defmethod base ((rev-itr vector_reverse_iterator))
  (make-instance 'vector_iterator
				 :buffer (opr::rev-vec-ptr-buffer rev-itr)
				 :index  (1+ (opr::rev-vec-ptr-index rev-itr))))

;; creating reverse iterator.
(define-constructor reverse_iterator ((itr vector_reverse_iterator))
  (make-instance 'vector_iterator
				 :buffer (opr::rev-vec-ptr-buffer itr)
				 :index  (1+ (opr::rev-vec-ptr-index  itr))))






;;------------------------------------------------------------------------------
;;
;; debug methods for stl:vector
;;
;;------------------------------------------------------------------------------

#+cl-stl-debug
(defmethod dump ((container stl:vector) &optional (stream t) (print-item-fnc nil))
  (format stream "begin dump ---------------------~%")
  (let ((core (vector-core container)))
	(when core
	  (setf print-item-fnc (if print-item-fnc
							   (functor_function (clone print-item-fnc))
							   (lambda (s x) (format s "~A" x))))
	  (let ((cnt (size container))
			(buf (vec-core-buffer core)))
		(for (((idx 0)) (< idx cnt) (incf idx))
		  (format stream "~A : " idx)
		  (funcall print-item-fnc stream (svref buf idx))
		  (format stream "~%")))))
  (format stream "end dump -----------------------~%")
  nil)

#+cl-stl-debug
(defmethod check_integrity ((container stl:vector) &optional (stream t))
  (let ((err-count 0))
	(format stream "begin integrity check -----------------------~%")
	(let ((core (vector-core container)))
	  (if (null core)
		  (format stream "core is null.~%")
		  (let ((size      (vec-core-size     core))
				(capacity  (vec-core-capacity core))
				(buffer    (vec-core-buffer   core)))
			; always (<= 0 size)
			(if (<= 0 size)
				(format stream "  (<= 0 size) ... OK.~%")
				(progn
				  (incf err-count)
				  (format stream "  ERROR : (<= 0 size) is nil. size is ~A.~%" size)))
			; always (<= size capacity)
			(if (<= size capacity)
				(format stream "  (<= size capacity) ... OK.~%")
				(progn
				  (incf err-count)
				  (format stream "  ERROR : (<= size capacity) is nil. size is ~A, capacity is ~A.~%" size capacity)))
			; always (null (svref m-buffer N)) ...  N is in [m-size, m-capacity).
			(if (for (((idx size)) (< idx capacity) (incf idx) :returns t)
				  (unless (null (svref buffer idx))
					(return nil)))
				(format stream "  (null (svref buffer N)) ... N is in [size, capacity) ... OK.~%")
				(progn
				  (incf err-count)
				  (format stream "  ERROR : (null (svref buffer N)) ... N is in [m-size, m-capacity) ... NG.~%"))))))
	(format stream "end integrity check -------------------------~%")
	(zerop err-count)))

