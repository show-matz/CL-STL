(in-package :cl-stl)

(declaim (inline __deq-external-to-chunk-index
				 __deq-external-to-item-index
				 __deq-ensure-core-exist
				 __deq-ensure-chunk-exist))

(defconstant +DEQUE-CHUNK-SIZE+      16)
(defconstant +DEQUE-INDEX-MAGIC+     -4)
(defconstant +DEQUE-INITIAL-MAP-LEN+  4)

;;--------------------------------------------------------------------
;;
;; class difinition
;;
;;--------------------------------------------------------------------
(defstruct (deque-core (:conc-name __deq-core-))
  (pivot       0 :type fixnum)    ; chunk index
  (top-offset  0 :type fixnum)    ; from pivot
  (size-cache  0 :type fixnum)
  (map         (make-array +DEQUE-INITIAL-MAP-LEN+ :initial-element nil) :type simple-vector))

(defclass deque (randomaccess_container pushable_back_container pushable_front_container)
  ((core :type     :deque-core
		 :initform nil
		 :initarg  :core
		 :accessor deque-core)))

(defclass deque_const_iterator (randomaccess_iterator)
  ((core   :type     :deque-core
		   :initform nil
		   :initarg  :core
		   :accessor __deq-itr-core)
   (offset :type     :fixnum    ; from pivot
		   :initform 0
		   :initarg  :offset
		   :accessor __deq-itr-offset)))

(defclass deque_iterator (deque_const_iterator) ())

(defclass deque_const_reverse_iterator (randomaccess_iterator)
  ((core   :type     :deque-core
		   :initform nil
		   :initarg  :core
		   :accessor __deq-rev-itr-core)
   (offset :type     :fixnum    ; from pivot
		   :initform 0
		   :initarg  :offset
		   :accessor __deq-rev-itr-offset)))

(defclass deque_reverse_iterator (deque_const_reverse_iterator) ())


;;--------------------------------------------------------------------
;;
;; internal utilities
;;
;;--------------------------------------------------------------------
(locally (declare (optimize speed))
  (defun __deq-chunk-clone (src)
	(let* ((dst (make-array +DEQUE-CHUNK-SIZE+ :initial-element nil)))
	  (declare (type simple-vector dst src))
	  (do ((idx 0 (1+ idx)))
		  ((= idx +DEQUE-CHUNK-SIZE+) dst)
		(declare (type fixnum idx))
		(_= (svref dst idx) (svref src idx))))))

(locally (declare (optimize speed))
  (defun __deq-chunk-fill (chunk val start end)
	(declare (type simple-vector chunk))
	(declare (type fixnum start end))
	(when chunk
	  (do ((idx start (1+ idx)))
		  ((= idx end) nil)
		(declare (type fixnum idx))
		(_= (svref chunk idx) val)))))

(locally (declare (optimize speed))
  (defun __deq-create-core ()
	(let ((core   (make-deque-core))
		  (offset 0)
		  (pivot  (ash +DEQUE-INITIAL-MAP-LEN+ -1)))
	  (declare (type fixnum pivot offset))
	  (setf (__deq-core-pivot      core) pivot)
	  (setf (__deq-core-top-offset core) offset)
	  core)))

;; get chunk index from external index.
(locally (declare (optimize speed))
  (defun __deq-external-to-chunk-index (core ex-index)
	(declare (type fixnum ex-index))
	(the fixnum
		 (+ (the fixnum (__deq-core-pivot core))
			(the fixnum (ash (the fixnum (+ ex-index
											(the fixnum (__deq-core-top-offset core))))
									+DEQUE-INDEX-MAGIC+))))))

;; get item index from external index.
(locally (declare (optimize speed))
  (defun __deq-external-to-item-index (core ex-index)
	(declare (type fixnum ex-index))
	(logand (+ ex-index (__deq-core-top-offset core)) (1- +DEQUE-CHUNK-SIZE+))))

(defmacro __deq-error-when-empty (core op)
  (let ((g-core (gensym "CORE")))
	`(let ((,g-core ,core))
	   (when (or (null ,g-core)
				 (zerop (__deq-core-size-cache ,g-core)))
		 (error 'undefined-behavior :what ,(format nil "~A for empty deque." op))))))

(defmacro __deq-check-iterator-belong (itr cont)
  (declare (ignorable itr cont))
  #-cl-stl-debug nil
  #+cl-stl-debug (check-type cont symbol)
  #+cl-stl-debug (check-type itr  symbol)
  #+cl-stl-debug
  `(unless (eq (deque-core ,cont) (__deq-itr-core ,itr))
	 (error 'undefined-behavior :what ,(format nil "~A is not iterator of ~A." itr cont))))

(defmacro __deq-check-iterator-range (itr1 itr2)
  (declare (ignorable itr1 itr2))
  #-cl-stl-debug nil
  #+cl-stl-debug (check-type itr1 symbol)
  #+cl-stl-debug (check-type itr2 symbol)
  #+cl-stl-debug
  `(unless (and (eq (__deq-itr-core   ,itr1) (__deq-itr-core   ,itr2))
				(<= (__deq-itr-offset ,itr1) (__deq-itr-offset ,itr2)))
	 (error 'undefined-behavior :what ,(format nil "[~A ~A) isn't legal sequence." itr1 itr2))))
  

(defun __deq-ensure-core-exist (cont)
  (unless (deque-core cont)
	(setf (deque-core cont) (__deq-create-core)))
  (deque-core cont))

(defun __deq-ensure-chunk-exist (core idx)
  (let ((map (__deq-core-map core)))
	(unless (svref map idx)
	  (setf (svref map idx)
			(make-array +DEQUE-CHUNK-SIZE+ :initial-element nil)))
	(svref map idx)))

(defun __deq-extend-map (core &optional (new-chunk-count 0))
  (let* ((old-map (__deq-core-map core))
		 (old-map-size (length old-map))
		 (new-map-size (max +DEQUE-INITIAL-MAP-LEN+
							(* 2 (if (<= new-chunk-count 0)
									 old-map-size new-chunk-count))))
		 (new-map (make-array new-map-size :initial-element nil))
		 (src (__deq-external-to-chunk-index core 0))
		 (dst (ash new-map-size -2))
		 (new-pivot (+ dst (- (__deq-core-pivot core) src))))
	(do ()
		((= src old-map-size) nil)
	  (setf (svref new-map dst) (svref old-map src))
	  (setf (svref old-map src) nil)
	  (incf src)
	  (incf dst))
	(setf (__deq-core-pivot core) new-pivot)
	(setf (__deq-core-map core) new-map)))

(defmacro __deq-get-at (core ex-idx)
  (let ((g-core   (gensym "CORE"))
		(g-ex-idx (gensym "EX-IDX"))
		(g-index1 (gensym "INDEX1"))
		(g-index2 (gensym "INDEX2")))
	`(locally (declare (optimize speed))
	   (let* ((,g-core   ,core)
			  (,g-ex-idx ,ex-idx)
			  (,g-index1 (__deq-external-to-chunk-index ,g-core ,g-ex-idx))
			  (,g-index2 (__deq-external-to-item-index  ,g-core ,g-ex-idx)))
		 (declare (type fixnum ,g-index1 ,g-index2))
		 (svref (svref (__deq-core-map ,g-core) ,g-index1) ,g-index2)))))

(defmacro __deq-set-at (core ex-idx new-val need-copy)
  (let ((g-core    (gensym "CORE"))
		(g-ex-idx  (gensym "EX-IDX"))
		(g-new-val (gensym "NEW-VAL"))
		(g-index1  (gensym "INDEX1"))
		(g-index2  (gensym "INDEX2")))
	`(locally (declare (optimize speed))
	   (let* ((,g-core    ,core)
			  (,g-ex-idx  ,ex-idx)
			  (,g-new-val ,new-val)
			  (,g-index1 (__deq-external-to-chunk-index ,g-core ,g-ex-idx))
			  (,g-index2 (__deq-external-to-item-index  ,g-core ,g-ex-idx)))
	   (declare (type fixnum ,g-index1 ,g-index2))
	   (if ,need-copy
		   (_=   (svref (svref (__deq-core-map ,g-core) ,g-index1) ,g-index2) ,g-new-val)
		   (setf (svref (svref (__deq-core-map ,g-core) ,g-index1) ,g-index2) ,g-new-val))))))

(defmacro __deq-iterate-chunk (core begin end (blk-sym idx1-sym idx2-sym) &body body)
  (let ((g-core  (gensym "CORE"))
		(g-begin (gensym "BEGIN"))
		(g-end   (gensym "END"))
		(g-map   (gensym "MAP"))
		(g-blk1  (gensym "BLK1"))
		(g-idx1  (gensym "IDX1"))
		(g-blk2  (gensym "BLK2"))
		(g-idx2  (gensym "IDX2"))
		(g-blk   (gensym "BLK")))
	`(locally (declare (optimize speed))
	   (let* ((,g-core  ,core)
			  (,g-begin ,begin)
			  (,g-end   ,end)
			  (,g-map  (__deq-core-map ,g-core))
			  (,g-blk1 (__deq-external-to-chunk-index ,g-core ,g-begin))
			  (,g-idx1 (__deq-external-to-item-index  ,g-core ,g-begin))
			  (,g-blk2 (__deq-external-to-chunk-index ,g-core ,g-end))
			  (,g-idx2 (__deq-external-to-item-index  ,g-core ,g-end)))
		 (declare (type simple-vector ,g-map))
		 (declare (type fixnum ,g-blk1 ,g-blk2 ,g-idx1 ,g-idx2))
		 (do ((,g-blk ,g-blk1 (1+ ,g-blk)))
			 ((< ,g-blk2 ,g-blk) nil)
		   (declare (type fixnum ,g-blk))
		   (let* ((,blk-sym  (svref ,g-map ,g-blk))
				  (,idx1-sym (if (= ,g-blk ,g-blk1) ,g-idx1 0))
				  (,idx2-sym (if (= ,g-blk ,g-blk2) ,g-idx2 +DEQUE-CHUNK-SIZE+)))
			 (declare (type fixnum ,idx1-sym ,idx2-sym))
			 ,@body))))))

(defmacro __deq-iterate-item (core begin end (arr-sym idx-sym) &body body)
  (let ((g-core  (gensym "CORE"))
		(g-begin (gensym "BEGIN"))
		(g-end   (gensym "END"))
		(g-map   (gensym "MAP"))
		(g-blk1  (gensym "BLK1"))
		(g-idx1  (gensym "IDX1"))
		(g-blk2  (gensym "BLK2"))
		(g-idx2  (gensym "IDX2"))
		(g-blk   (gensym "BLK"))
		(g-chunk (gensym "CHUNK"))
		(g-i1    (gensym "I1"))
		(g-i2    (gensym "I2")))
	`(locally (declare (optimize speed))
	   (let* ((,g-core  ,core)
			  (,g-begin ,begin)
			  (,g-end   ,end)
			  (,g-map  (__deq-core-map ,g-core))
			  (,g-blk1 (__deq-external-to-chunk-index ,g-core ,g-begin))
			  (,g-idx1 (__deq-external-to-item-index  ,g-core ,g-begin))
			  (,g-blk2 (__deq-external-to-chunk-index ,g-core ,g-end))
			  (,g-idx2 (__deq-external-to-item-index  ,g-core ,g-end)))
		 (declare (type simple-vector ,g-map))
		 (declare (type fixnum ,g-blk1 ,g-blk2 ,g-idx1 ,g-idx2))
		 (do ((,g-blk ,g-blk1 (1+ ,g-blk)))
			 ((< ,g-blk2 ,g-blk) nil)
		   (declare (type fixnum ,g-blk))
		   (let ((,g-chunk (svref ,g-map ,g-blk)))
			 (when ,g-chunk
			   (let ((,arr-sym ,g-chunk)
					 (,g-i1    (if (= ,g-blk ,g-blk1) ,g-idx1 0))
					 (,g-i2    (if (= ,g-blk ,g-blk2) ,g-idx2 +DEQUE-CHUNK-SIZE+)))
				 (declare (type simple-vector ,arr-sym))
				 (declare (type fixnum ,g-i1 ,g-i2))
				 (do ((,idx-sym ,g-i1 (1+ ,idx-sym)))
					 ((<= ,g-i2 ,idx-sym) nil)
				   (declare (type fixnum ,idx-sym))
				   ,@body)))))))))

(locally (declare (optimize speed))
  (defun __deq-move_backward (core begin end dest)
	(declare (type fixnum begin end dest))
	(let ((dst (+ dest (the fixnum (- end begin)))))
	  (declare (type fixnum dst))
	  (do ()
		  ((<= end begin) nil)
		(decf end)
		(decf dst)
		(__deq-set-at core dst (__deq-get-at core end) t)))))

(locally (declare (optimize speed))
  (defun __deq-move-foreward (core begin end dest)
	(declare (type fixnum begin end dest))
	(do ()
		((= begin end) nil)
	  (__deq-set-at core dest (__deq-get-at core begin) t)
	  (incf begin)
	  (incf dest))))


;;--------------------------------------------------------------------
;;
;; method implementation
;;
;;--------------------------------------------------------------------
(locally (declare (optimize speed))
  (defun __deq-push_back (core new-val need-copy)
	(let* ((cnt  (__deq-core-size-cache core))
		   (idx1 (__deq-external-to-chunk-index core cnt))
		   (idx2 (__deq-external-to-item-index  core cnt)))
	  (declare (type fixnum cnt idx1 idx2))
	  (when (<= (length (__deq-core-map core)) idx1)
		(__deq-extend-map core)
		(setf idx1 (__deq-external-to-chunk-index core cnt)))
	  (let ((chunk (__deq-ensure-chunk-exist core idx1)))
		(if need-copy
			(_=   (svref chunk idx2) new-val)
			(setf (svref chunk idx2) new-val))
		(incf (__deq-core-size-cache core))))))

(locally (declare (optimize speed))
  (defun __deq-push_front (core new-val need-copy)
	(let ((idx1 (__deq-external-to-chunk-index core -1))
		  (idx2 (__deq-external-to-item-index  core -1)))
	  (declare (type fixnum idx1 idx2))
	  (when (< idx1 0)
		(__deq-extend-map core)
		(setf idx1 (__deq-external-to-chunk-index core -1)))
	  (let ((chunk (__deq-ensure-chunk-exist core idx1)))
		(if need-copy
			(_=   (svref chunk idx2) new-val)
			(setf (svref chunk idx2) new-val))
		(decf (__deq-core-top-offset core))
		(incf (__deq-core-size-cache core))))))

(defun __deq-pop_back (core)
  (__deq-set-at core (decf (__deq-core-size-cache core)) nil t))

(defun __deq-pop_front (core)
  (__deq-set-at core 0 nil t)
  (incf (__deq-core-top-offset core))
  (decf (__deq-core-size-cache core)))

(locally (declare (optimize speed))
  (defun __deq-clear (core)
	(when core
	  (__deq-iterate-chunk core 0 (__deq-core-size-cache core) (chunk idx1 idx2)
		(__deq-chunk-fill chunk nil idx1 idx2))
	  (setf (__deq-core-size-cache core) 0))
	nil))

(locally (declare (optimize speed))
  (defun __deq-resize-downward (core old-size new-size)
	(declare (type fixnum old-size new-size))
	(__deq-iterate-chunk core new-size old-size (chunk idx1 idx2)
	  (__deq-chunk-fill chunk nil idx1 idx2))
	(setf (__deq-core-size-cache core) new-size)))

(locally (declare (optimize speed))
  (defun __deq-resize-rightward (core old-size new-size initial-element)
	(declare (type fixnum old-size new-size))
	(let ((map  (__deq-core-map core))
		  (blk1 (__deq-external-to-chunk-index core old-size))
		  (idx1 (__deq-external-to-item-index  core old-size))
		  (blk2 (__deq-external-to-chunk-index core new-size))
		  (idx2 (__deq-external-to-item-index  core new-size)))
	  (declare (type simple-vector map))
	  (declare (type fixnum blk1 blk2 idx1 idx2))
	  (when (<= (length map) blk2)
		(__deq-extend-map core (the fixnum (- blk2 blk1)))
		(setf map  (__deq-core-map core))
		(setf blk1 (__deq-external-to-chunk-index core old-size))
		(setf blk2 (__deq-external-to-chunk-index core new-size)))
	  (do ((blk blk1 (1+ blk)))
		  ((< blk2 blk) nil)
		(declare (type fixnum blk))
		(__deq-chunk-fill (__deq-ensure-chunk-exist core blk)
						  initial-element
						  (if (= blk blk1) idx1 0)
						  (if (= blk blk2) idx2 +DEQUE-CHUNK-SIZE+))))
	(setf (__deq-core-size-cache core) new-size)))

(locally (declare (optimize speed))
  ;; ToDo : __deq-resize-leftward : temporary implementation...
  (defun __deq-resize-leftward (core old-size new-size initial-element)
	(declare (type fixnum old-size new-size))
	(do ((idx old-size (1+ idx)))
		((= idx new-size) nil)
	  (declare (type fixnum idx))
	  (__deq-push_front core initial-element t))
	(setf (__deq-core-size-cache core) new-size)))

(locally (declare (optimize speed))
  (defun __deq-counted-assign (core count get-next-value)
	(declare (type fixnum count))
	(declare (type cl:function get-next-value))
	(let ((size (__deq-core-size-cache core)))
	  (cond
		((< count size) (__deq-resize-downward  core size count))
		((< size count) (__deq-resize-rightward core size count nil))))
	(__deq-iterate-item core 0 count (arr idx)
		(_= (svref arr idx) (funcall get-next-value)))))


(locally (declare (optimize speed))
  (defun __deq-counted-insert (core itr count get-next-value need-copy)
	(declare (type fixnum count))
	(declare (type cl:function get-next-value))
	(let* ((offset   (__deq-itr-offset itr))
		   (index    (- (the fixnum offset)
						(the fixnum (__deq-core-top-offset core))))
		   (old-size (__deq-core-size-cache core))
		   (new-size (+ count old-size)))
	  (declare (type fixnum offset index old-size new-size))
	  (if (< (- old-size index) index)
		  (progn
			(__deq-resize-rightward core old-size new-size nil)
			(let ((end (+ index count)))
			  (declare (type fixnum end))
			  (__deq-move_backward core index old-size end)
			  (do ((idx index (1+ idx)))
				  ((= idx end) offset)
				(__deq-set-at core idx (funcall get-next-value) need-copy))))
		  (progn
			(__deq-resize-leftward core old-size new-size nil)
			(let ((end (+ index count)))
			  (declare (type fixnum end))
			  (__deq-move-foreward core count end 0)
			  (do ((idx index (1+ idx)))
				  ((= idx end) (the fixnum (- offset count)))
				(__deq-set-at core idx (funcall get-next-value) need-copy))))))))

(defun __deq-insert (core itr value need-copy)
  (let* ((size   (__deq-core-size-cache core))
		 (offset (__deq-itr-offset itr))
		 (ex     (- offset (__deq-core-top-offset core))))
	(cond
	  ((zerop ex)
	   (__deq-push_front core value need-copy)
	   (let ((itr (clone itr)))
		 (advance itr -1)
		 itr))
	  ((= ex size)
	   (__deq-push_back core value need-copy)
	   (clone itr))
	  (t
	   (make-instance 'deque_iterator
					  :core   core
					  :offset (__deq-counted-insert core itr 1
													(lambda () value) need-copy))))))

(locally (declare (optimize speed))
  (defun __create-deque (size &optional (initial-element nil))
	(declare (type fixnum size))
	(__error-unless-non-negative-fixnum deque size)
	(if (zerop size)
		(make-instance 'deque)
		(let ((core (__deq-create-core)))
		  (do ((idx 0 (1+ idx)))
			  ((<= size idx) nil)
			(declare (type fixnum idx))
			(__deq-push_back core initial-element t))
		  (make-instance 'deque :core core)))))


;;------------------------------------------------------------------------------
;;
;; constructors
;;
;;------------------------------------------------------------------------------
(declare-constructor deque (0 1 2))

; default constructor
(define-constructor deque ()
  (__create-deque 0))

; copy constructor
(define-constructor deque ((arg deque))
  (clone arg))

; constructor with initializer list
#-cl-stl-0x98
(locally (declare (optimize speed))
  (define-constructor deque ((arg initializer_list))
	(let* ((arr (__initlist-data arg))
		   (cnt (length arr)))
	  (declare (type simple-vector arr))
	  (declare (type fixnum cnt))
	  (if (zerop cnt)
		  (make-instance 'deque)
		  (let ((core (__deq-create-core)))
			(do ((idx 0 (1+ idx)))
				((= idx cnt)
				 (make-instance 'deque :core core))
			  (declare (type fixnum idx))
			  (__deq-push_back core (svref arr idx) t)))))))

; move constructor
#-cl-stl-0x98
(locally (declare (optimize speed))
  (define-constructor deque ((arg& remove-reference))
	(with-reference (arg)
	  (let ((cont arg))
		(__check-type-of-move-constructor cont deque)
		(let ((core (deque-core cont)))
		  (setf (deque-core cont) nil)
		  (make-instance 'deque :core core))))))


; fill constructor 1
(define-constructor deque ((arg integer))
  (__create-deque arg))

; fill constructor 2
(define-constructor deque ((arg1 integer) arg2)
  (__create-deque arg1 arg2))

; range constructor 2
(locally (declare (optimize speed))

  (define-constructor deque ((itr1 input_iterator) (itr2 input_iterator))
	(if (_== itr1 itr2)
		(make-instance 'deque)
		(let ((core (__deq-create-core)))
		  (with-operators
			  (for (((itr @~itr1)) (_/= itr itr2) ++itr)
				(__deq-push_back core *itr t)))
		  (make-instance 'deque :core core))))

  (define-constructor deque ((itr1 const-vector-pointer) (itr2 const-vector-pointer))
	(__pointer-check-iterator-range itr1 itr2)
	(let ((idx1 (opr::vec-ptr-index  itr1))
		  (idx2 (opr::vec-ptr-index  itr2))
		  (buf  (opr::vec-ptr-buffer itr1)))
	  (declare (type fixnum idx1 idx2))
	  (declare (type cl:vector buf))
	  (if (= idx1 idx2)
		  (make-instance 'deque)
		  (let ((core (__deq-create-core)))
			(for (nil (< idx1 idx2) (incf idx1))
			  (__deq-push_back core (aref buf idx1) t))
			(make-instance 'deque :core core))))))


; copy constructor
(locally (declare (optimize speed))
  (defmethod operator_clone ((container deque))
	(let ((core (deque-core container)))
	  (if (null core)
		  (make-instance 'deque)
		  (let ((cnt (__deq-core-size-cache core)))
			(declare (type fixnum cnt))
			(if (zerop cnt)
				(make-instance 'deque)
				(let* ((old-map   (__deq-core-map core))
					   (src-blk1  (the fixnum (__deq-external-to-chunk-index core 0)))
					   (src-blk2  (the fixnum (__deq-external-to-chunk-index core cnt)))
					   (blk-cnt   (the fixnum
									   (cl:max +DEQUE-INITIAL-MAP-LEN+
											   (the fixnum
													(* 2 (the fixnum
															  (1+ (the fixnum
																	   (- src-blk2 src-blk1)))))))))
					   (new-map   (make-array blk-cnt :initial-element nil))
					   (dst-blk   (the fixnum (ash blk-cnt -2)))
					   (new-pivot (the fixnum (+ dst-blk
												 (the fixnum
													  (- (the fixnum (__deq-core-pivot core))
														 src-blk1))))))
				  (declare (type fixnum src-blk1 src-blk2 blk-cnt dst-blk new-pivot))
				  (do ((src-blk src-blk1 (1+ src-blk)))
					  ((< src-blk2 src-blk) nil)
					(setf (svref new-map dst-blk) (__deq-chunk-clone (svref old-map src-blk)))
					(incf dst-blk))
				  (make-instance 'deque
								 :core (make-deque-core :pivot      new-pivot
														:top-offset (__deq-core-top-offset core)
														:size-cache cnt
														:map        new-map)))))))))



;;------------------------------------------------------------------------------
;;
;; methods
;;
;;------------------------------------------------------------------------------

;-----------------------------------------------------
; assignment
;-----------------------------------------------------
(locally (declare (optimize speed))
  (defmethod operator_= ((cont1 deque) (cont2 deque))
	(let ((new-size (size cont2)))
	  (declare (type fixnum new-size))
	  (if (zerop new-size)
		  (clear cont1)
		  (progn
			(resize cont1 new-size nil)
			(let ((idx 0)
				  (core (deque-core cont1)))
			  (declare (type fixnum idx))
			  (__deq-iterate-item (deque-core cont2) 0 new-size (arr arr-idx)
				(__deq-set-at core idx (svref arr arr-idx) t)
				(incf idx))))))
	cont1))


#-cl-stl-0x98
(defmethod operator_= ((cont deque) (il initializer_list))
  (assign cont il)
  cont)

#-cl-stl-0x98
(defmethod operator_move ((cont1 deque) (cont2 deque))
  (unless (eq cont1 cont2)
	(setf (deque-core cont1) (deque-core cont2))
	(setf (deque-core cont2) nil))
  (values cont1 cont2))

;MEMO : always returns nil.
(defmethod-overload assign ((cont deque) (count integer) value)
  (__error-unless-non-negative-fixnum assign count)
  (__deq-counted-assign (__deq-ensure-core-exist cont)
						count (lambda () value))
  nil)

;; range assign.
;; MEMO : always returns nil.
;;; 新しい戦略 ： input_iterator のみが相手だし、assign なので、既存の要素までは反復しつつ代入する。
;;;            入力シーケンスが先に終われば、resize し、まだある場合は残りを push_back する。
(locally (declare (optimize speed))

  (defmethod-overload assign ((cont deque)
							  (itr1 input_iterator) (itr2 input_iterator))
	(if (_== itr1 itr2)
		(__deq-clear (deque-core cont))
		(with-operators
			(let ((idx  0)
				  (cnt  (size cont))
				  (itr  @~itr1)
				  (core (__deq-ensure-core-exist cont)))
			  (declare (type fixnum idx cnt))
			  (for (nil (and (< idx cnt)
							 (_/= itr itr2)) (progn ++itr (incf idx)))
				(__deq-set-at core idx *itr t))
			  (if (< idx cnt)
				  (__deq-resize-downward core cnt idx)
				  (for (nil (_/= itr itr2) ++itr)
					(__deq-push_back core *itr t))))))
	nil)

  (defmethod-overload assign ((cont deque)
							  (itr1 forward_iterator) (itr2 forward_iterator))
	(if (_== itr1 itr2)
		(__deq-clear (deque-core cont))
		(with-operators
			(let ((itr   @~itr1)
				  (count (distance itr1 itr2)))
			  (__deq-counted-assign (__deq-ensure-core-exist cont)
									count (lambda () (prog1 *itr ++itr))))))
	nil)

  (defmethod-overload assign ((cont deque)
							  (itr1 const-vector-pointer) (itr2 const-vector-pointer))
	(__pointer-check-iterator-range itr1 itr2)
	(let ((idx1 (opr::vec-ptr-index  itr1))
		  (idx2 (opr::vec-ptr-index  itr2))
		  (buf  (opr::vec-ptr-buffer itr1)))
	  (declare (type fixnum idx1 idx2))
	  (declare (type cl:vector buf))
	  (if (= idx1 idx2)
		  (__deq-clear (deque-core cont))
		  (__deq-counted-assign (__deq-ensure-core-exist cont)
								(the fixnum (- idx2 idx1))
								(lambda ()
								  (prog1 (aref buf idx1) (incf idx1))))))
	nil))

;;MEMO : always returns nil.
#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod-overload assign ((cont deque) (arg initializer_list))
	(let* ((arr (__initlist-data arg))
		   (cnt (length arr)))
	  (declare (type simple-vector arr))
	  (declare (type fixnum cnt))
	  (if (zerop cnt)
		  (__deq-clear (deque-core cont))
		  (let* ((core (__deq-ensure-core-exist cont))
				 (size (__deq-core-size-cache core)))
			(declare (type fixnum size))
			(cond
			  ((< cnt size) (__deq-resize-downward  core size cnt))
			  ((< size cnt) (__deq-resize-rightward core size cnt nil)))
			(let ((idx 0))
			  (declare (type fixnum idx))
			  (__deq-iterate-item core 0 cnt (dst dst-idx)
				(_= (svref dst dst-idx) (svref arr idx))
				(incf idx))))))
	nil))


;-----------------------------------------------------
; iterators
;-----------------------------------------------------
(defmethod begin ((cont deque))
  (let ((core (__deq-ensure-core-exist cont)))
	(make-instance 'deque_iterator
				   :core   core
				   :offset (__deq-core-top-offset core))))

(defmethod end ((cont deque))
  (let ((core (__deq-ensure-core-exist cont)))
	(make-instance 'deque_iterator
				   :core   core
				   :offset (+ (__deq-core-top-offset core)
							  (__deq-core-size-cache core)))))

(defmethod rbegin ((cont deque))
  (let ((core (__deq-ensure-core-exist cont)))
	(make-instance 'deque_reverse_iterator
				   :core   core
				   :offset (1- (+ (__deq-core-top-offset core)
								  (__deq-core-size-cache core))))))

(defmethod rend ((cont deque))
  (let ((core (__deq-ensure-core-exist cont)))
	(make-instance 'deque_reverse_iterator
				   :core   core
				   :offset (1- (__deq-core-top-offset core)))))

#-cl-stl-0x98
(defmethod cbegin ((cont deque))
  (let ((core (__deq-ensure-core-exist cont)))
	(make-instance 'deque_const_iterator
				   :core   core
				   :offset (__deq-core-top-offset core))))

#-cl-stl-0x98
(defmethod cend ((cont deque))
  (let ((core (__deq-ensure-core-exist cont)))
	(make-instance 'deque_const_iterator
				   :core   core
				   :offset (+ (__deq-core-top-offset core)
							  (__deq-core-size-cache core)))))

#-cl-stl-0x98
(defmethod crbegin ((cont deque))
  (let ((core (__deq-ensure-core-exist cont)))
	(make-instance 'deque_const_reverse_iterator
				   :core   core
				   :offset (1- (+ (__deq-core-top-offset core)
								  (__deq-core-size-cache core))))))

#-cl-stl-0x98
(defmethod crend ((cont deque))
  (let ((core (__deq-ensure-core-exist cont)))
	(make-instance 'deque_const_reverse_iterator
				   :core   core
				   :offset (1- (__deq-core-top-offset core)))))

;-----------------------------------------------------
; capacity
;-----------------------------------------------------
(defmethod empty ((cont deque))
  (let ((core (deque-core cont)))
	(or (null core)
		(zerop (__deq-core-size-cache core)))))

(defmethod size ((cont deque))
  (let ((core (deque-core cont)))
	(if (null core)
		0
		(__deq-core-size-cache core))))

(defmethod max_size ((cont deque))
  most-positive-fixnum)

(labels ((imp (cont new-size initial-element)
		   (__error-unless-non-negative-fixnum resize new-size)
		   (let* ((core (__deq-ensure-core-exist cont))
				  (cnt  (__deq-core-size-cache core)))
			 (cond
			   ((< new-size cnt) (__deq-resize-downward  core cnt new-size))
			   ((< cnt new-size) (__deq-resize-rightward core cnt new-size initial-element))))
		   nil))

  (defmethod-overload resize ((cont deque) (new-size integer))
	(imp cont new-size nil))

  (defmethod-overload resize ((cont deque) (new-size integer) initial-element)
	(imp cont new-size initial-element)))


;-----------------------------------------------------
; element access
;-----------------------------------------------------
(defmethod front ((cont deque))
  (let ((core (deque-core cont)))
	(__deq-error-when-empty core "front")
	(__deq-get-at core 0)))

(defmethod (setf front) (val (cont deque))
  (let ((core (deque-core cont)))
	(__deq-error-when-empty core "front")
	(__deq-set-at core 0 val t)
	val))

(defmethod back ((cont deque))
  (let ((core (deque-core cont)))
	(__deq-error-when-empty core "back")
	(__deq-get-at core (1- (__deq-core-size-cache core)))))

(defmethod (setf back) (val (cont deque))
  (let ((core (deque-core cont)))
	(__deq-error-when-empty core "back")
	(__deq-set-at core (1- (__deq-core-size-cache core)) val t)
	val))

(defmethod at ((cont deque) (idx integer))
  (let* ((core (deque-core cont))
		 (size (__deq-core-size-cache core)))
	(when (or (< idx 0) (<= size idx))
	  (error 'out_of_range :what (format nil "index ~A is out of range." idx)))
	(__deq-get-at core idx)))

(defmethod (setf at) (val (cont deque) (idx integer))
  (let* ((core (deque-core cont))
		 (size (__deq-core-size-cache core)))
	(when (or (< idx 0) (<= size idx))
	  (error 'out_of_range :what (format nil "index ~A is out of range." idx)))
	(__deq-set-at core idx val t)
	val))

(defmethod operator_[] ((cont deque) (idx integer))
  (__deq-get-at (deque-core cont) idx))

(defmethod (setf operator_[]) (val (cont deque) (idx integer))
  (__deq-set-at (deque-core cont) idx val t)
  val)

(defmethod operator_& ((cont deque) (idx integer))
  (_& (_[] cont idx)))

(defmethod operator_const& ((cont deque) (idx integer))
  (const_& (_[] cont idx)))


;-----------------------------------------------------
; modifiers
;-----------------------------------------------------
(defmethod push_back ((container deque) new-val)
  (__deq-push_back (__deq-ensure-core-exist container) new-val t)
  nil)

(defmethod push_front ((container deque) new-val)
  (__deq-push_front (__deq-ensure-core-exist container) new-val t)
  nil)

(defmethod pop_back ((cont deque))
  (let ((core (deque-core cont)))
	(__deq-error-when-empty core "pop_back")
	(__deq-pop_back core))
  nil)

(defmethod pop_front ((cont deque))
  (let ((core (deque-core cont)))
	(__deq-error-when-empty core "pop_front")
	(__deq-pop_front core))
  nil)

#-cl-stl-0x98    ; emplace_back
(defmethod-overload emplace_back ((container deque) new-val)
  (__deq-push_back (__deq-ensure-core-exist container) new-val nil)
  #+(or cl-stl-0x11 cl-stl-0x14) nil
  #-(or cl-stl-0x11 cl-stl-0x14) new-val)

#-cl-stl-0x98    ; emplace_front
(defmethod-overload emplace_front ((container deque) new-val)
  (__deq-push_front (__deq-ensure-core-exist container) new-val nil)
  #+(or cl-stl-0x11 cl-stl-0x14) nil
  #-(or cl-stl-0x11 cl-stl-0x14) new-val)

#-cl-stl-0x98    ; shrink_to_fit
(defmethod shrink_to_fit ((cont deque))
  (let* ((core (deque-core cont))
		 (cnt  (__deq-core-size-cache core)))
	(declare (type fixnum cnt))
	(unless (zerop cnt)
	  (let* ((old-map   (__deq-core-map core))
			 (src-blk1  (__deq-external-to-chunk-index core 0))
			 (src-blk2  (__deq-external-to-chunk-index core cnt))
			 (blk-cnt   (max +DEQUE-INITIAL-MAP-LEN+
							 (1+ (- src-blk2 src-blk1))))
			 (new-map   (make-array blk-cnt :initial-element nil))
			 (dst-blk   0)
			 (new-pivot (- (__deq-core-pivot core) src-blk1)))
		(do ((src-blk src-blk1 (1+ src-blk)))
			((< src-blk2 src-blk) nil)
		  (setf (svref new-map dst-blk) (svref old-map src-blk))
		  (setf (svref old-map src-blk) nil)
		  (incf dst-blk))
		(setf (__deq-core-pivot core) new-pivot)
		(setf (__deq-core-map   core) new-map)))
	nil))

;; insert ( single elemente ) - returns iterator
(defmethod-overload insert ((cont deque)
							(itr  #+cl-stl-0x98 deque_iterator
								  #-cl-stl-0x98 deque_const_iterator) value)
  (__deq-check-iterator-belong itr cont)
  (__deq-insert (deque-core cont) itr value t))

;; insert ( fill )
(locally (declare (optimize speed))
  (defmethod-overload insert ((cont deque)
							  (itr  #+cl-stl-0x98 deque_iterator
									#-cl-stl-0x98 deque_const_iterator) (count integer) value)
	;; MEMO : in C++98, always returns nil. but C++11, returns iterator.
	(declare (type fixnum count))
	(__deq-check-iterator-belong itr cont)
	(__error-unless-non-negative-fixnum insert count)
	(let* ((core (deque-core cont))
		   (size (__deq-core-size-cache core))
		   (ex   (- (the fixnum (__deq-itr-offset itr))
					(the fixnum (__deq-core-top-offset core)))))
	  (declare (type fixnum size ex))
	  (cond
		((zerop ex)
		 (do ((idx 0 (1+ idx)))
			 ((= idx count) #+cl-stl-0x98 nil
							#-cl-stl-0x98 (begin cont))
		   (declare (type fixnum idx))
		   (__deq-push_front core value t)))
		((= ex size)
		 (do ((idx 0 (1+ idx)))
			 ((= idx count) #+cl-stl-0x98 nil
							#-cl-stl-0x98 (let ((itr (end cont)))
											(advance itr (* -1 count))
											itr))
		   (declare (type fixnum idx))
		   (__deq-push_back core value t)))
		(t
		 (let ((offset (__deq-counted-insert core itr count
											 (lambda () value) t)))
		   (declare (ignorable offset))
		   #+cl-stl-0x98 nil
		   #-cl-stl-0x98 (make-instance 'deque_iterator
										:core core :offset offset)))))))

;; range insert.
;; MEMO : in C++98, always returns nil. but C++11, returns iterator.
(locally (declare (optimize speed))

  (defmethod-overload insert ((cont deque)
							  (itr  #+cl-stl-0x98 deque_iterator
									#-cl-stl-0x98 deque_const_iterator)
							  (itr1 input_iterator) (itr2 input_iterator))
	(__deq-check-iterator-belong itr cont)
	(let ((core (deque-core cont)))
	  (if (_== itr1 itr2)
		  (clone itr)
		  (let ((size (__deq-core-size-cache core))
				(ex   (- (the fixnum (__deq-itr-offset itr))
						 (the fixnum (__deq-core-top-offset core)))))
			(declare (type fixnum size ex))
			(if (/= ex size)
				(let ((tmp (new stl:deque itr1 itr2)))
				  (insert cont itr (begin tmp) (end tmp)))
				(with-operators
					(let ((cnt 0))
					  (declare (type fixnum cnt))
					  (for (((itr1 @~itr1)) (_/= itr1 itr2) (progn (incf cnt) ++itr1)
											:returns #+cl-stl-0x98 nil
													 #-cl-stl-0x98 (let ((itr (end cont)))
																	 (advance itr (the fixnum (- cnt)))
																	 itr))
						(__deq-push_back core *itr1 t)))))))))

  (defmethod-overload insert ((cont deque)
							  (itr  #+cl-stl-0x98 deque_iterator
									#-cl-stl-0x98 deque_const_iterator)
							  (itr1 forward_iterator) (itr2 forward_iterator))
	;; MEMO : in C++98, always returns nil. but C++11, returns iterator.
	(__deq-check-iterator-belong itr cont)
	(let ((core (deque-core cont)))
	  (if (_== itr1 itr2)
		  (clone itr)
		  (let* ((size (__deq-core-size-cache core))
				 (ex   (- (the fixnum (__deq-itr-offset itr))
						  (the fixnum (__deq-core-top-offset core)))))
			(declare (type fixnum size ex))
			(cond
			  ;; when top insertion
			  ((zerop ex)
			   (let ((tmp (new stl:deque itr1 itr2)))
				 (insert cont itr (begin tmp) (end tmp))))
			  ;; when tail insertion
			  ((= ex size)
			   (with-operators
				   (let ((cnt 0))
					 (declare (type fixnum cnt))
					 (for (((itr1 @~itr1)) (_/= itr1 itr2) (progn (incf cnt) ++itr1)
											:returns #+cl-stl-0x98 nil
													 #-cl-stl-0x98 (let ((itr (end cont)))
																	 (advance itr (the fixnum (- cnt)))
																	 itr))
					   (__deq-push_back core *itr1 t)))))
			  ;; otherwise
			  (t
			   (with-operators
				   (let ((itr1  @~itr1)
						 (count (distance itr1 itr2)))
					 (let ((offset (__deq-counted-insert core itr count
														 (lambda () (prog1 *itr1 ++itr1)) t)))
					   (declare (ignorable offset))
					   #+cl-stl-0x98 nil
					   #-cl-stl-0x98 (make-instance 'deque_iterator
													:core core :offset offset))))))))))

  (defmethod-overload insert ((cont deque)
							  (itr  #+cl-stl-0x98 deque_iterator
									#-cl-stl-0x98 deque_const_iterator)
							  (itr1 bidirectional_iterator) (itr2 bidirectional_iterator))
	(__deq-check-iterator-belong itr cont)
	(let ((core (deque-core cont)))
	  (if (_== itr1 itr2)
		  (clone itr)
		  (let* ((size (__deq-core-size-cache core))
				 (ex   (- (the fixnum (__deq-itr-offset itr))
						  (the fixnum (__deq-core-top-offset core)))))
			(declare (type fixnum size ex))
			(cond
			  ;; when top insertion
			  ((zerop ex)
			   (with-operators
				   (for (((itr2 @~itr2)) (_/= itr1 itr2) nil :returns #+cl-stl-0x98 nil
																	  #-cl-stl-0x98 (begin cont))
					 --itr2
					 (__deq-push_front core *itr2 t))))
			  ;; when tail insertion
			  ((= ex size)
			   (with-operators
				   (let ((cnt 0))
					 (declare (type fixnum cnt))
					 (for (((itr1 @~itr1)) (_/= itr1 itr2) (progn (incf cnt) ++itr1)
										   :returns #+cl-stl-0x98 nil
													#-cl-stl-0x98 (let ((itr (end cont)))
																	(advance itr (the fixnum (- cnt)))
																	itr))
					   (__deq-push_back core *itr1 t)))))
			  ;; otherwise
			  (t
			   (with-operators
				   (let ((itr1  @~itr1)
						 (count (distance itr1 itr2)))
					 (let ((offset (__deq-counted-insert core itr count
														 (lambda () (prog1 *itr1 ++itr1)) t)))
					   (declare (ignorable offset))
					   #+cl-stl-0x98 nil
					   #-cl-stl-0x98 (make-instance 'deque_iterator
													:core core :offset offset))))))))))

  (defmethod-overload insert ((cont deque)
							  (itr  #+cl-stl-0x98 deque_iterator
									#-cl-stl-0x98 deque_const_iterator)
							  (itr1 const-vector-pointer) (itr2 const-vector-pointer))
	(__deq-check-iterator-belong itr cont)
	(__pointer-check-iterator-range itr1 itr2)
	(let ((core (deque-core cont))
		  (idx1 (opr::vec-ptr-index  itr1))
		  (idx2 (opr::vec-ptr-index  itr2))
		  (buf  (opr::vec-ptr-buffer itr1)))
	  (declare (type fixnum idx1 idx2))
	  (declare (type cl:vector buf))
	  (if (<= idx2 idx1)
		  (clone itr)
		  (let* ((size (__deq-core-size-cache core))
				 (cnt  (- idx2 idx1))
				 (ex   (- (the fixnum (__deq-itr-offset itr))
						  (the fixnum (__deq-core-top-offset core)))))
			(declare (type fixnum size cnt ex))
			(cond
			  ;; when top insertion
			  ((zerop ex)
			   (for (nil (< idx1 idx2) nil :returns #+cl-stl-0x98 nil
													#-cl-stl-0x98 (begin cont))
				 (decf idx2)
				 (__deq-push_front core (aref buf idx2) t)))
			  ;; when tail insertion
			  ((= ex size)
			   (for (nil (< idx1 idx2) (incf idx1)
									   :returns #+cl-stl-0x98 nil
												#-cl-stl-0x98 (let ((itr (end cont)))
																(advance itr (the fixnum (- cnt)))
																itr))
				 (__deq-push_back core (aref buf idx1) t)))
			  ;; otherwise
			  (t
			   (let ((offset (__deq-counted-insert core itr cnt
												   (lambda ()
													 (prog1 (aref buf idx1) (incf idx1))) t)))
				 (declare (ignorable offset))
				 #+cl-stl-0x98 nil
				 #-cl-stl-0x98 (make-instance 'deque_iterator
											  :core core :offset offset)))))))))


;; insert ( move ) - returns iterator.
#-cl-stl-0x98
(defmethod-overload insert ((cont deque)
							(itr  deque_const_iterator) (rm& remove-reference))
  (__deq-check-iterator-belong itr cont)
  (let ((core (deque-core cont)))
	(with-reference (rm)
	  (prog1
		  (__deq-insert core itr rm nil)
		(setf rm nil)))))


;; insert ( initializer list ) - returns iterator.
#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod-overload insert ((cont deque)
							  (itr  deque_const_iterator) (il initializer_list))
	(__deq-check-iterator-belong itr cont)
	(let* ((core (deque-core cont))
		   (arr  (__initlist-data il))
		   (cnt  (length arr)))
	  (declare (type simple-vector arr))
	  (declare (type fixnum cnt))
	  (if (zerop cnt)
		  (clone itr)
		  (let ((size (__deq-core-size-cache core))
				(ex   (- (the fixnum (__deq-itr-offset itr))
						 (the fixnum (__deq-core-top-offset core)))))
			(declare (type fixnum size ex))
			(cond
			  ((zerop ex)
			   (do ((idx cnt))
				   ((zerop idx) (begin cont))
				 (declare (type fixnum idx))
				 (decf idx)
				 (__deq-push_front core (svref arr idx) t)))
			  ((= ex size)
			   (do ((idx 0 (1+ idx)))
				   ((= idx cnt) (let ((itr (end cont)))
								  (advance itr (* -1 cnt))
								  itr))
				 (declare (type fixnum idx))
				 (__deq-push_back core (svref arr idx) t)))
			  (t
			   (let ((idx 0))
				 (declare (type fixnum idx))
				 (make-instance 'deque_iterator
								:core core
								:offset (__deq-counted-insert core itr cnt
															  (lambda ()
																(prog1
																	(svref arr idx)
																  (incf idx))) t))))))))))
				   

#-cl-stl-0x98    ; emplace
(defmethod-overload emplace ((cont deque)
							 (itr  deque_const_iterator) new-val)
  (__deq-check-iterator-belong itr cont)
  (__deq-insert (deque-core cont) itr new-val nil))

(defmethod-overload erase ((cont deque)
						   (itr  #+cl-stl-0x98 deque_iterator
								 #-cl-stl-0x98 deque_const_iterator))
  (__deq-check-iterator-belong itr cont)
  (let ((core (deque-core cont)))
	(let* ((size   (__deq-core-size-cache core))
		   (offset (__deq-itr-offset itr))
		   (ex     (- offset (__deq-core-top-offset core))))
	  (declare (type fixnum size offset ex))
	  (cond
		((zerop ex)
		 (__deq-pop_front core)
		 (begin cont))
		((= ex (1- size))
		 (__deq-pop_back core)
		 (end cont))
		((< ex (- size ex))
		 (__deq-move_backward core 0 ex 1)
		 (__deq-pop_front core)
		 (make-instance 'deque_iterator :core core :offset (1+ offset)))
		(t
		 (__deq-move-foreward core (1+ ex) size ex)
		 (__deq-pop_back core)
		 (make-instance 'deque_iterator :core core :offset offset))))))
			

(defmethod-overload erase ((cont deque)
						   (first #+cl-stl-0x98 deque_iterator
								  #-cl-stl-0x98 deque_const_iterator)
						   (last  #+cl-stl-0x98 deque_iterator
								  #-cl-stl-0x98 deque_const_iterator))
  (__deq-check-iterator-belong first cont)
  (__deq-check-iterator-range  first last)
  (let ((core (deque-core cont)))
	(let* ((size (__deq-core-size-cache core))
		   (ex1  (- (__deq-itr-offset first) (__deq-core-top-offset core)))
		   (ex2  (- (__deq-itr-offset last)  (__deq-core-top-offset core)))
		   (cnt  (- ex2 ex1)))
	  (declare (type fixnum size ex1 ex2 cnt))
	  (cond
		((and (zerop ex1) (= ex2 size))
		 (__deq-clear core)
		 (end cont))
		((zerop ex1)
		 (__deq-iterate-item core 0 ex2 (arr idx)
		   (setf (svref arr idx) nil))
		 (incf (__deq-core-top-offset core) cnt)
		 (decf (__deq-core-size-cache core) cnt)
		 (begin cont))
		((= ex2 size)
		 (__deq-resize-downward core size ex1)
		 (end cont))
		((< ex1 (- size ex2))
		 (__deq-move_backward core 0 ex1 cnt)
		 (__deq-iterate-item core 0 cnt (arr idx)
		   (setf (svref arr idx) nil))
		 (incf (__deq-core-top-offset core) cnt)
		 (decf (__deq-core-size-cache core) cnt)
		 (make-instance 'deque_iterator :core core :offset (__deq-itr-offset last)))
		(t
		 (__deq-move-foreward core ex2 size ex1)
		 (__deq-iterate-item core (- size cnt) size (arr idx)
		   (setf (svref arr idx) nil))
		 (decf (__deq-core-size-cache core) cnt)
		 (make-instance 'deque_iterator :core core :offset (__deq-itr-offset first)))))))


(defmethod-overload swap ((cont1 deque) (cont2 deque))
  (let ((tmp (deque-core cont1)))
	(setf (deque-core cont1) (deque-core cont2))
	(setf (deque-core cont2) tmp))
  (values cont1 cont2))

(defmethod clear ((cont deque))
  (__deq-clear (deque-core cont)))


;-----------------------------------------------------
; compare
;-----------------------------------------------------
(locally (declare (optimize speed))
  (labels ((__container-equal (cont1 cont2)
			 (let ((cnt (size cont1)))
			   (declare (type fixnum cnt))
			   (cond
				 ((eq cont1 cont2) t)
				 ((/= cnt (the fixnum (size cont2))) nil)
				 ((zerop cnt) t)
				 (t (let ((core1 (deque-core cont1))
						  (core2 (deque-core cont2)))
					  (do ((idx 0 (1+ idx)))
						  ((= idx cnt) t)
						(declare (type fixnum idx))
						(unless (_== (__deq-get-at core1 idx)
									 (__deq-get-at core2 idx))
						  (return-from __container-equal nil)))))))))

	(defmethod operator_== ((cont1 deque) (cont2 deque))
	  (__container-equal cont1 cont2))

	(defmethod operator_/= ((cont1 deque) (cont2 deque))
	  (not (__container-equal cont1 cont2)))))


(locally (declare (optimize speed))
  (labels ((__container-compare (cont1 cont2)
			 (let ((cnt1 (size cont1))
				   (cnt2 (size cont2)))
			   (declare (type fixnum cnt1 cnt2))
			   (cond
				 ((eq cont1 cont2) 0)
				 ((and (zerop cnt1) (zerop cnt2))  0)
				 ((and (zerop cnt1) (<   0 cnt2)) -1)
				 ((and (<   0 cnt1) (zerop cnt2))  1)
				 (t    (let ((core1  (deque-core cont1))
							 (core2  (deque-core cont2)))
						 (do ((idx 0 (1+ idx))
							  (cnt (min cnt1 cnt2)))
							 ((= idx cnt) nil)
						   (declare (type fixnum idx cnt))
						   (let ((val1 (__deq-get-at core1 idx))
								 (val2 (__deq-get-at core2 idx)))
							 (when (_< val1 val2)
							   (return-from __container-compare -1))
							 (when (_< val2 val1)
							   (return-from __container-compare  1))))
						 (cond
						   ((= cnt1 cnt2)  0)
						   ((< cnt1 cnt2) -1)
						   ((> cnt1 cnt2)  1))))))))

	(defmethod operator_< ((cont1 deque) (cont2 deque))
	  (< (__container-compare cont1 cont2) 0))

	(defmethod operator_<= ((cont1 deque) (cont2 deque))
	  (<= (__container-compare cont1 cont2) 0))

	(defmethod operator_> ((cont1 deque) (cont2 deque))
	  (< 0 (__container-compare cont1 cont2)))

	(defmethod operator_>= ((cont1 deque) (cont2 deque))
	  (<= 0 (__container-compare cont1 cont2)))))


;-----------------------------------------------------
; enumeration
;-----------------------------------------------------
#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod-overload for ((cont deque) func)
	(declare (type cl:function func))
	;;MEMO : func is always lambda function ( see stl:for ).
	(let ((core (deque-core cont)))
	  (when core
		(__deq-iterate-item core 0 (size cont) (arr idx)
			(funcall func (svref arr idx)))))))


;;------------------------------------------------------------------------------
;;
;; methods for deque_const_iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_= ((itr1 deque_const_iterator) (itr2 deque_const_iterator))
  (__error-when-const-removing-assign itr1 deque_iterator
									  itr2 deque_const_iterator)
  (setf (__deq-itr-core   itr1) (__deq-itr-core   itr2))
  (setf (__deq-itr-offset itr1) (__deq-itr-offset itr2))
  itr1)

(defmethod operator_clone ((itr deque_const_iterator))
  (make-instance 'deque_const_iterator
				 :core   (__deq-itr-core   itr)
				 :offset (__deq-itr-offset itr)))

(defmethod operator_* ((itr deque_const_iterator))
  (let ((core (__deq-itr-core itr)))
	(__deq-get-at core (- (the fixnum (__deq-itr-offset itr))
						  (the fixnum (__deq-core-top-offset core))))))

(defmethod (setf operator_*) (new-val (itr deque_const_iterator))
  (error 'setf-to-const :what "setf to (_* deque_const_iterator)."))

(defmethod operator_++ ((itr deque_const_iterator))
  (incf (__deq-itr-offset itr))
  itr)

(defmethod operator_-- ((itr deque_const_iterator))
  (decf (__deq-itr-offset itr))
  itr)

(defmethod operator_== ((itr1 deque_const_iterator) (itr2 deque_const_iterator))
  (and (eq (__deq-itr-core   itr1) (__deq-itr-core   itr2))
	   (=  (__deq-itr-offset itr1) (__deq-itr-offset itr2))))

(defmethod operator_/= ((itr1 deque_const_iterator) (itr2 deque_const_iterator))
  (or (not (eq (__deq-itr-core   itr1) (__deq-itr-core   itr2)))
	  (/=      (__deq-itr-offset itr1) (__deq-itr-offset itr2))))

(defmethod operator_< ((itr1 deque_const_iterator) (itr2 deque_const_iterator))
  (and (eq (__deq-itr-core   itr1) (__deq-itr-core   itr2))
	   (<  (__deq-itr-offset itr1) (__deq-itr-offset itr2))))

(defmethod operator_<= ((itr1 deque_const_iterator) (itr2 deque_const_iterator))
  (and (eq (__deq-itr-core   itr1) (__deq-itr-core   itr2))
	   (<= (__deq-itr-offset itr1) (__deq-itr-offset itr2))))

(defmethod operator_> ((itr1 deque_const_iterator) (itr2 deque_const_iterator))
  (and (eq (__deq-itr-core   itr1) (__deq-itr-core   itr2))
	   (>  (__deq-itr-offset itr1) (__deq-itr-offset itr2))))

(defmethod operator_>= ((itr1 deque_const_iterator) (itr2 deque_const_iterator))
  (and (eq (__deq-itr-core   itr1) (__deq-itr-core   itr2))
	   (>= (__deq-itr-offset itr1) (__deq-itr-offset itr2))))

(defmethod operator_+ ((itr deque_const_iterator) (n integer))
  (make-instance 'deque_const_iterator
				 :core   (__deq-itr-core itr)
				 :offset (+ (__deq-itr-offset itr) n)))

(defmethod operator_+= ((itr deque_const_iterator) (n integer))
  (incf (__deq-itr-offset itr) n)
  itr)

(defmethod operator_- ((itr deque_const_iterator) (n integer))
  (make-instance 'deque_const_iterator
				 :core   (__deq-itr-core itr)
				 :offset (- (__deq-itr-offset itr) n)))

(defmethod operator_- ((itr1 deque_const_iterator) (itr2 deque_const_iterator))
  (unless (eq (__deq-itr-core itr1) (__deq-itr-core itr2))
	(error 'undefined-behavior :what "operator_- : invalid iterator pair."))
  (- (__deq-itr-offset itr1) (__deq-itr-offset itr2)))

(defmethod operator_-= ((itr deque_const_iterator) (n integer))
  (decf (__deq-itr-offset itr) n)
  itr)

(defmethod operator_[] ((itr deque_const_iterator) (idx integer))
  (declare (type fixnum idx))
  (let* ((core (__deq-itr-core itr))
		 (pt   (- (the fixnum (__deq-itr-offset itr))
				  (the fixnum (__deq-core-top-offset core)))))
	(declare (type fixnum pt))
	(__deq-get-at core (the fixnum (+ idx pt)))))

(defmethod (setf operator_[]) (new-val (itr deque_const_iterator) (idx integer))
  (error 'setf-to-const :what "setf to (_[] deque_const_iterator)."))

(defmethod advance ((itr deque_const_iterator) (n integer))
  (incf (__deq-itr-offset itr) n)
  nil)

(defmethod distance ((itr1 deque_const_iterator) (itr2 deque_const_iterator))
  (unless (eq (__deq-itr-core itr1) (__deq-itr-core itr2))
	(error 'undefined-behavior :what "distance : invalid iterator pair."))
  (- (__deq-itr-offset itr2) (__deq-itr-offset itr1)))

;; creating reverse iterator.
(define-constructor reverse_iterator ((itr deque_const_iterator))
  (make-instance 'deque_const_reverse_iterator
				 :core   (__deq-itr-core itr)
				 :offset (1- (__deq-itr-offset itr))))



;;------------------------------------------------------------------------------
;;
;; methods for deque_iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_clone ((itr deque_iterator))
  (make-instance 'deque_iterator
				 :core   (__deq-itr-core   itr)
				 :offset (__deq-itr-offset itr)))

(defmethod operator_cast ((itr deque_iterator)
						  (typename (eql 'deque_const_iterator)))
  (__check-exact-type-of-cast itr 'deque_iterator 'deque_const_iterator)
  (make-instance 'deque_const_iterator
				 :core   (__deq-itr-core   itr)
				 :offset (__deq-itr-offset itr)))

(defmethod (setf operator_*) (new-val (itr deque_iterator))
  (let ((core (__deq-itr-core itr)))
	(__deq-set-at core (- (the fixnum (__deq-itr-offset itr))
						  (the fixnum (__deq-core-top-offset core))) new-val t)))

(defmethod operator_+ ((itr deque_iterator) (n integer))
  (make-instance 'deque_iterator
				 :core   (__deq-itr-core itr)
				 :offset (+ (__deq-itr-offset itr) n)))

(defmethod operator_- ((itr deque_iterator) (n integer))
  (make-instance 'deque_iterator
				 :core   (__deq-itr-core itr)
				 :offset (- (__deq-itr-offset itr) n)))

(defmethod (setf operator_[]) (new-val (itr deque_iterator) (idx integer))
  (declare (type fixnum idx))
  (let* ((core (__deq-itr-core itr))
		 (pt   (- (the fixnum (__deq-itr-offset itr))
				  (the fixnum (__deq-core-top-offset core)))))
	(declare (type fixnum pt))
	(__deq-set-at core (the fixnum (+ idx pt)) new-val t)))

;; creating reverse iterator.
(define-constructor reverse_iterator ((itr deque_iterator))
  (make-instance 'deque_reverse_iterator
				 :core   (__deq-itr-core itr)
				 :offset (1- (__deq-itr-offset itr))))




;;------------------------------------------------------------------------------
;;
;; methods for deque_const_reverse_iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_= ((itr1 deque_const_reverse_iterator)
					  (itr2 deque_const_reverse_iterator))
  (__error-when-const-removing-assign itr1 deque_reverse_iterator
									  itr2 deque_const_reverse_iterator)
  (setf (__deq-rev-itr-core   itr1) (__deq-rev-itr-core   itr2))
  (setf (__deq-rev-itr-offset itr1) (__deq-rev-itr-offset itr2))
  itr1)

(defmethod operator_clone ((itr deque_const_reverse_iterator))
  (make-instance 'deque_const_reverse_iterator
				 :core   (__deq-rev-itr-core   itr)
				 :offset (__deq-rev-itr-offset itr)))

(defmethod operator_* ((itr deque_const_reverse_iterator))
  (let ((core (__deq-rev-itr-core itr)))
	(__deq-get-at core (the fixnum (- (the fixnum (__deq-rev-itr-offset itr))
									  (the fixnum (__deq-core-top-offset core)))))))

(defmethod (setf operator_*) (new-val (itr deque_const_reverse_iterator))
  (error 'setf-to-const :what "setf to (_* deque_const_reverse_iterator)."))

(defmethod operator_++ ((itr deque_const_reverse_iterator))
  (decf (__deq-rev-itr-offset itr))
  itr)

(defmethod operator_-- ((itr deque_const_reverse_iterator))
  (incf (__deq-rev-itr-offset itr))
  itr)

(defmethod operator_== ((itr1 deque_const_reverse_iterator)
				  (itr2 deque_const_reverse_iterator))
  (and (eq (__deq-rev-itr-core   itr1) (__deq-rev-itr-core   itr2))
	   (=  (__deq-rev-itr-offset itr1) (__deq-rev-itr-offset itr2))))

(defmethod operator_/= ((itr1 deque_const_reverse_iterator)
				  (itr2 deque_const_reverse_iterator))
  (or (not (eq (__deq-rev-itr-core   itr1) (__deq-rev-itr-core   itr2)))
	  (/=      (__deq-rev-itr-offset itr1) (__deq-rev-itr-offset itr2))))

(defmethod operator_< ((itr1 deque_const_reverse_iterator) (itr2 deque_const_reverse_iterator))
  (and (eq (__deq-rev-itr-core   itr1) (__deq-rev-itr-core   itr2))
	   (>  (__deq-rev-itr-offset itr1) (__deq-rev-itr-offset itr2))))

(defmethod operator_<= ((itr1 deque_const_reverse_iterator) (itr2 deque_const_reverse_iterator))
  (and (eq (__deq-rev-itr-core   itr1) (__deq-rev-itr-core   itr2))
	   (>= (__deq-rev-itr-offset itr1) (__deq-rev-itr-offset itr2))))

(defmethod operator_> ((itr1 deque_const_reverse_iterator) (itr2 deque_const_reverse_iterator))
  (and (eq (__deq-rev-itr-core   itr1) (__deq-rev-itr-core   itr2))
	   (<  (__deq-rev-itr-offset itr1) (__deq-rev-itr-offset itr2))))

(defmethod operator_>= ((itr1 deque_const_reverse_iterator) (itr2 deque_const_reverse_iterator))
  (and (eq (__deq-rev-itr-core   itr1) (__deq-rev-itr-core   itr2))
	   (<= (__deq-rev-itr-offset itr1) (__deq-rev-itr-offset itr2))))

(defmethod operator_+ ((itr deque_const_reverse_iterator) (n integer))
  (make-instance 'deque_const_reverse_iterator
				 :core   (__deq-rev-itr-core itr)
				 :offset (- (__deq-rev-itr-offset itr) n)))

(defmethod operator_+= ((itr deque_const_reverse_iterator) (n integer))
  (decf (__deq-rev-itr-offset itr) n)
  itr)

(defmethod operator_- ((itr deque_const_reverse_iterator) (n integer))
  (make-instance 'deque_const_reverse_iterator
				 :core   (__deq-rev-itr-core itr)
				 :offset (+ (__deq-rev-itr-offset itr) n)))

(defmethod operator_- ((itr1 deque_const_reverse_iterator)
				 (itr2 deque_const_reverse_iterator))
  (unless (eq (__deq-rev-itr-core itr1) (__deq-rev-itr-core itr2))
	(error 'undefined-behavior :what "operator_- : invalid iterator pair."))
  (- (__deq-rev-itr-offset itr2) (__deq-rev-itr-offset itr1)))

(defmethod operator_-= ((itr deque_const_reverse_iterator) (n integer))
  (incf (__deq-rev-itr-offset itr) n)
  itr)

(defmethod operator_[] ((itr deque_const_reverse_iterator) (idx integer))
  (declare (type fixnum idx))
  (let* ((core (__deq-rev-itr-core itr))
		 (pt   (- (the fixnum (__deq-rev-itr-offset itr))
				  (the fixnum (__deq-core-top-offset core)))))
	(declare (type fixnum pt))
	(__deq-get-at core (the fixnum (- (the fixnum (- pt idx)))))))

(defmethod (setf operator_[]) (new-val (itr deque_const_reverse_iterator) (idx integer))
  (error 'setf-to-const :what "setf to (_[] deque_const_reverse_iterator)."))

(defmethod advance ((itr deque_const_reverse_iterator) (n integer))
  (decf (__deq-rev-itr-offset itr) n)
  nil)

(defmethod distance ((itr1 deque_const_reverse_iterator) (itr2 deque_const_reverse_iterator))
  (unless (eq (__deq-rev-itr-core itr1) (__deq-rev-itr-core itr2))
	(error 'undefined-behavior :what "distance : invalid iterator pair."))
  (- (__deq-rev-itr-offset itr1) (__deq-rev-itr-offset itr2)))

(defmethod base ((rev-itr deque_const_reverse_iterator))
  (make-instance 'deque_const_iterator
				 :core   (__deq-rev-itr-core rev-itr)
				 :offset (1+ (__deq-rev-itr-offset rev-itr))))

;; creating reverse iterator.
(define-constructor reverse_iterator ((itr deque_const_reverse_iterator))
  (make-instance 'deque_const_iterator
				 :core   (__deq-rev-itr-core itr)
				 :offset (1+ (__deq-rev-itr-offset itr))))



;;------------------------------------------------------------------------------
;;
;; methods for deque_reverse_iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_clone ((itr deque_reverse_iterator))
  (make-instance 'deque_reverse_iterator
				 :core   (__deq-rev-itr-core  itr)
				 :offset (__deq-rev-itr-offset itr)))

(defmethod operator_cast ((itr deque_reverse_iterator)
						  (typename (eql 'deque_const_reverse_iterator)))
  (__check-exact-type-of-cast itr 'deque_reverse_iterator
								  'deque_const_reverse_iterator)
  (make-instance 'deque_const_reverse_iterator
				 :core   (__deq-rev-itr-core   itr)
				 :offset (__deq-rev-itr-offset itr)))

(defmethod (setf operator_*) (new-val (itr deque_reverse_iterator))
  (let ((core (__deq-rev-itr-core itr)))
	(__deq-set-at core (- (the fixnum (__deq-rev-itr-offset itr))
						  (the fixnum (__deq-core-top-offset core))) new-val t)))

(defmethod operator_+ ((itr deque_reverse_iterator) (n integer))
  (make-instance 'deque_reverse_iterator
				 :core   (__deq-rev-itr-core itr)
				 :offset (- (__deq-rev-itr-offset itr) n)))

(defmethod operator_- ((itr deque_reverse_iterator) (n integer))
  (make-instance 'deque_reverse_iterator
				 :core   (__deq-rev-itr-core itr)
				 :offset (+ (__deq-rev-itr-offset itr) n)))

(defmethod (setf operator_[]) (new-val (itr deque_reverse_iterator) (idx integer))
  (declare (type fixnum idx))
  (let ((core (__deq-rev-itr-core itr)))
	(__deq-set-at core (- (the fixnum
							   (- (the fixnum (__deq-rev-itr-offset itr))
								  (the fixnum (__deq-core-top-offset core)))) idx) new-val t)))

(defmethod base ((rev-itr deque_reverse_iterator))
  (make-instance 'deque_iterator
				 :core   (__deq-rev-itr-core rev-itr)
				 :offset (1+ (__deq-rev-itr-offset rev-itr))))

;; creating reverse iterator.
(define-constructor reverse_iterator ((itr deque_reverse_iterator))
  (make-instance 'deque_iterator
				 :core   (__deq-rev-itr-core itr)
				 :offset (1+ (__deq-rev-itr-offset itr))))




;;------------------------------------------------------------------------------
;;
;; debug methods for deque
;;
;;------------------------------------------------------------------------------
#+cl-stl-debug
(defmethod dump ((container deque) &optional (stream t) (print-item-fnc nil))
  (setf print-item-fnc (if print-item-fnc
						   (functor_function (clone print-item-fnc))
						   (lambda (s x) (format s "~A" x))))
  (format stream "begin dump ---------------------~%")
  (labels ((dump-chunk (index chunk)
			 (format stream "-----------------------~%")
			 (do ((idx 0 (incf idx)))
				 ((= idx +DEQUE-CHUNK-SIZE+) nil)
			   (format stream "~A    ~A " index idx)
			   (funcall print-item-fnc stream (svref chunk idx))
			   (format stream "~%"))
			 (format stream "-----------------------~%")))
	(let ((core (deque-core container)))
	  (when core
		(let* ((map (__deq-core-map core))
			   (len (length map)))
		  (do ((idx 0 (incf idx)))
			  ((= idx len) nil)
			(let ((chunk (svref map idx)))
			  (if chunk
				  (dump-chunk idx chunk)
				  (format stream "~A nil~%" idx))))))))
  (format stream "end dump -----------------------~%")
  nil)

#+cl-stl-debug
(defmethod check_integrity ((container deque) &optional (stream t))
  (declare (ignorable stream))
  (let ((core (deque-core container)))
	(when core
	  (error "not yet implemented.")))	  ;; ToDo : implement 'check_integrity'...
  nil)


