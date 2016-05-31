(in-package :cl-stl)

;;--------------------------------------------------------------------
;;
;; class difinition
;;
;;--------------------------------------------------------------------
(defstruct (list-node (:conc-name list-node-))
  (prev nil)
  (next nil)
  (item nil))

(defclass list (bidirectional_container pushable_back_container pushable_front_container)
  ((size-cache    :type     :fixnum
				  :initform 0
				  :initarg  :size
				  :accessor list-size-cache)
   (top-sentinel  :type     :list-node
				  :initform nil
				  :initarg  :top
				  :accessor list-top-sentinel)
   (last-sentinel :type     :list-node
				  :initform nil
				  :initarg  :last
				  :accessor list-last-sentinel)))

(defclass list_const_iterator (bidirectional_iterator)
  ((node  :type     :list-node
		  :initform nil
		  :initarg  :node
		  :accessor list-itr-node)))

(defclass list_iterator (list_const_iterator) ())

(defclass list_const_reverse_iterator (bidirectional_iterator)
  ((node  :type     :list-node
		  :initform nil
		  :initarg  :node
		  :accessor list-rev-itr-node)))

(defclass list_reverse_iterator (list_const_reverse_iterator) ())

;;--------------------------------------------------------------------
;;
;; internal utilities
;;
;;--------------------------------------------------------------------
(defmacro __list-new-node (val &optional (need-copy t))
  (let ((g-val      (gensym "VAL"))
		(g-new-node (gensym "NEW-NODE")))
	(if need-copy
		`(let ((,g-val ,val)
			   (,g-new-node (make-list-node :item nil)))
		   (_= (list-node-item ,g-new-node) ,g-val)
		   ,g-new-node)
		`(make-list-node :item ,val))))

(defmacro __list-connect-node (prev next)
  (let ((g-prev (gensym "PREV"))
		(g-next (gensym "NEXT")))
  `(let ((,g-prev ,prev)
		 (,g-next ,next))
	 (when ,g-prev
	   (setf (list-node-next ,g-prev) ,g-next))
	 (when ,g-next
	   (setf (list-node-prev ,g-next) ,g-prev)))))

(defmacro __list-pullout-node (node)
  (let ((g-node (gensym "NODE")))
	`(let ((,g-node ,node))
	   (__list-connect-node (list-node-prev ,g-node)
							(list-node-next ,g-node))
	   ,g-node)))

(defmacro __list-insert-node (dest node)
  (let ((g-dest (gensym "DEST"))
		(g-node (gensym "NODE")))
	`(let ((,g-dest ,dest)
		   (,g-node ,node))
	   (__list-connect-node (list-node-prev ,g-dest) ,g-node)
	   (__list-connect-node ,g-node ,g-dest)
	   ,g-node)))

(defmacro __list-error-when-empty (cont-sym op)
  (check-type cont-sym symbol)
  `(when (zerop (list-size-cache ,cont-sym))
	 (error 'undefined-behavior :what ,(format nil "~A for empty list." op))))

(defmacro __list-check-iterator-belong (itr cont)
  (declare (ignorable itr cont))
  #-cl-stl-debug nil
  #+cl-stl-debug
  `(unless (do ((target (list-itr-node ,itr))
				(node (list-top-sentinel  ,cont) (list-node-next node)))
			   ((null node) nil)
			 (when (eq node target) (return t)))
	 (error 'undefined-behavior :what ,(format nil "~A is not iterator of ~A." itr cont))))

(defmacro __list-check-iterator-range (itr1 itr2)
  (declare (ignorable itr1 itr2))
  #-cl-stl-debug nil
  #+cl-stl-debug
  `(unless (do ((node2 (list-itr-node ,itr2))
				(node1 (list-itr-node ,itr1) (list-node-next node1)))
			   ((null node1) nil)
			 (when (eq node1 node2) (return t)))
	 (error 'undefined-behavior :what ,(format nil "[~A ~A) is not valid range." itr1 itr2))))

(locally (declare (optimize speed))
  (defun __list-count-nodes (node1 node2)
	(do ((cnt 0 (1+ cnt)))
		((eq node1 node2) cnt)
	  (declare (type fixnum cnt))
	  (setf node1 (list-node-next node1)))))

;;--------------------------------------------------------------------
;;
;; method implementation
;;
;;--------------------------------------------------------------------
(defun __list-insert (node new-node)
  (let ((prev-node (list-node-prev node)))
	(__list-connect-node prev-node new-node)
	(__list-connect-node new-node node)))

(locally (declare (optimize speed))
  (defun __list-insert-values (prev-node next-node value count)
	(declare (type fixnum count))
	(do ()
		((zerop count) nil)
	  (let ((new-node (__list-new-node value)))
		(__list-connect-node prev-node new-node)
		(__list-connect-node new-node next-node)
		(setf prev-node new-node))
	  (decf count))))

(defun __list-insert-seq (prev-node next-node itr1 itr2)
  (if (_== itr1 itr2)
	  (progn (__list-connect-node prev-node next-node) 0)
	  (with-operators
		  (for (((cnt 0) (itr @~itr1)) (_/= itr itr2) (progn (incf cnt) ++itr) :returns cnt)
			(let ((new-node (__list-new-node *itr)))
			  (__list-connect-node prev-node new-node)
			  (__list-connect-node new-node next-node)
			  (setf prev-node new-node))))))

(defun __list-assign (sentinel1 sentinel2 size val)
  (let ((node (list-node-next sentinel1)))
	(do ()
		((or (zerop size)
			 (eq node sentinel2)) nil)
	  (_= (list-node-item node) val)
	  (decf size)
	  (setf node (list-node-next node)))
	(setf node (list-node-prev node))
	(if (zerop size)
		(__list-connect-node node sentinel2)
		(do ()
			((zerop size) nil)
		  (let ((new-node (__list-new-node val)))
			(__list-connect-node node new-node)
			(__list-connect-node new-node sentinel2)
			(setf node new-node))
		  (decf size))))
  nil)

(defun __list-assign-seq (sentinel1 sentinel2 itr1 itr2)
  (if (_== itr1 itr2)
	  (progn (__list-connect-node sentinel1 sentinel2) 0)
	  (with-operators
		  (let ((cnt  0)
				(itr  @~itr1)
				(node (list-node-next sentinel1)))
			(for (nil (and (_/= itr itr2) (not (eq node sentinel2))) nil)
			  (_= (list-node-item node) *itr)
			  ++itr
			  (incf cnt)
			  (setf node (list-node-next node)))
			(setf node (list-node-prev node))
			(if (_== itr itr2)
				(progn (__list-connect-node node sentinel2) cnt)
				(for (nil (_/= itr itr2) (progn ++itr (incf cnt)) :returns cnt)
				  (let ((new-node (__list-new-node *itr)))
					(__list-connect-node node new-node)
					(__list-connect-node new-node sentinel2)
					(setf node new-node))))))))

(defun __create-list (size &optional (initial-element nil))
  (__error-unless-non-negative-fixnum list size)
  (let ((s1 (make-list-node))
		(s2 (make-list-node)))
	(__list-connect-node s1 s2)
	(__list-assign s1 s2 size initial-element)
	(make-instance 'stl:list :size size :top s1 :last s2)))


;;------------------------------------------------------------------------------
;;
;; constructors
;;
;;------------------------------------------------------------------------------
(declare-constructor list (0 1 2))

; default constructor
(define-constructor list ()
  (__create-list 0))

; copy constructor
(define-constructor list ((arg stl:list))
  (clone arg))

; constructor with initializer list
#-cl-stl-0x98
(locally (declare (optimize speed))
  (define-constructor list ((arg initializer_list))
	(declare (type initializer_list arg))
	(let* ((arr (__initlist-data arg))
		   (cnt (length arr))
		   (s1  (make-list-node))
		   (s2  (make-list-node)))
	  (declare (type simple-vector arr))
	  (declare (type fixnum        cnt))
	  (__list-connect-node s1 s2)
	  (if (zerop cnt)
		  (make-instance 'stl:list :size 0 :top s1 :last s2)
		  (do ((idx 0 (1+ idx)))
			  ((= idx cnt) (make-instance 'stl:list
										  :size cnt :top s1 :last s2))
			(declare (type fixnum idx))
			(__list-insert-node s2 (__list-new-node (aref arr idx))))))))

; move constructor
#-cl-stl-0x98
(locally (declare (optimize speed))
  (define-constructor list ((arg& remove-reference))
	(with-reference (arg)
	  (let ((cont arg))
		(__check-type-of-move-constructor cont stl:list)
		(prog1
			(make-instance 'stl:list
						   :size (list-size-cache    cont)
						   :top  (list-top-sentinel  cont)
						   :last (list-last-sentinel cont))
		  (let ((s1 (make-list-node))
				(s2 (make-list-node)))
			(setf (list-size-cache    cont)  0)
			(setf (list-top-sentinel  cont) s1)
			(setf (list-last-sentinel cont) s2)
			(__list-connect-node s1 s2)))))))


; fill constructor 1
(define-constructor list ((arg integer))
  (__create-list arg))

; fill constructor 2
(define-constructor list ((arg1 integer) arg2)
  (__create-list arg1 arg2))

; range constructor 2
(labels ((__create-list-with-sequence (itr1 itr2)
		   (let ((s1 (make-list-node))
				 (s2 (make-list-node)))
			 (__list-connect-node s1 s2)
			 (let ((cnt (__list-assign-seq s1 s2 itr1 itr2)))
			   (make-instance 'stl:list :size cnt :top s1 :last s2)))))

  (define-constructor list ((arg1 input_iterator) (arg2 input_iterator))
	(__create-list-with-sequence arg1 arg2))

  (define-constructor list ((arg1 const-vector-pointer) (arg2 const-vector-pointer))
	(__pointer-check-iterator-range arg1 arg2)
	(__create-list-with-sequence arg1 arg2)))


; copy constructor
(defmethod operator_clone ((container stl:list))
  (let ((sentinel1 (make-list-node))
		(sentinel2 (make-list-node)))
	(__list-connect-node sentinel1 sentinel2)
	(let ((lst (make-instance 'stl:list :size 0 :top sentinel1 :last sentinel2)))
	  (if (empty container)
		  lst
		  (do ((dst sentinel1)
			   (src-node (list-node-next (list-top-sentinel container)))
			   (src-last (list-last-sentinel container)))
			  ((eq src-node src-last) lst)
			(let ((new-node (__list-new-node (list-node-item src-node))))
			  (__list-connect-node dst new-node)
			  (__list-connect-node new-node sentinel2)
			  (setf dst new-node))
			(incf (list-size-cache lst))
			(setf src-node (list-node-next src-node)))))))


;;------------------------------------------------------------------------------
;;
;; methods
;;
;;------------------------------------------------------------------------------

;-----------------------------------------------------
; assignment
;-----------------------------------------------------
(defmethod operator_= ((cont1 stl:list) (cont2 stl:list))
  (let ((src-node (list-node-next (list-top-sentinel cont2)))
		(src-last (list-last-sentinel cont2))
		(dst-node (list-node-next (list-top-sentinel cont1)))
		(dst-last (list-last-sentinel cont1)))
	(do ()
		((or (eq src-node src-last)
			 (eq dst-node dst-last)) nil)
	  (_= (list-node-item dst-node) (list-node-item src-node))
	  (setf src-node (list-node-next src-node))
	  (setf dst-node (list-node-next dst-node)))
	(unless (eq dst-node dst-last)
	  (__list-connect-node (list-node-prev dst-node) dst-last))
	(do ()
		((eq src-node src-last) nil)
	  (__list-insert-node dst-last
						  (__list-new-node (list-node-item src-node)))
	  (setf src-node (list-node-next src-node))))
  (setf (list-size-cache cont1) (list-size-cache cont2))
  cont1)

#-cl-stl-0x98
(defmethod operator_= ((cont stl:list) (il initializer_list))
  (assign cont il)
  cont)

#-cl-stl-0x98
(defmethod operator_move ((cont1 stl:list) (cont2 stl:list))
  (unless (eq cont1 cont2)
	;; clear cont1
	(__list-connect-node (list-top-sentinel  cont1)
						 (list-last-sentinel cont1))
	(setf (list-size-cache cont1) 0)
	(splice cont1 (begin cont1) cont2))
  (values cont1 cont2))

;MEMO : always returns nil.
(defmethod-overload assign ((cont stl:list) (count integer) value)
  (__error-unless-non-negative-fixnum assign count)
  (__list-assign (list-top-sentinel  cont)
				 (list-last-sentinel cont) count value)
  (setf (list-size-cache cont) count)
  nil)

;MEMO : always returns nil.
(defmethod-overload assign ((cont stl:list) (itr1 input_iterator) (itr2 input_iterator))
  (let ((cnt (__list-assign-seq (list-top-sentinel  cont)
								(list-last-sentinel cont) itr1 itr2)))
	(setf (list-size-cache cont) cnt))
  nil)

(defmethod-overload assign ((cont stl:list) (itr1 const-vector-pointer) (itr2 const-vector-pointer))
  (__pointer-check-iterator-range itr1 itr2)
  (let ((cnt (__list-assign-seq (list-top-sentinel  cont)
								(list-last-sentinel cont) itr1 itr2)))
	(setf (list-size-cache cont) cnt))
  nil)

;MEMO : always returns nil.
#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod-overload assign ((cont stl:list) (arg initializer_list))
	(declare (type initializer_list arg))
	(let* ((arr  (__initlist-data arg))
		   (idx  0)
		   (cnt  (length arr))
		   (node (list-top-sentinel  cont))
		   (last (list-last-sentinel cont)))
	  (declare (type simple-vector arr))
	  (declare (type fixnum        idx cnt))
	  (setf node (list-node-next node))
	  (do ()
		  ((or (= idx cnt)
			   (eq node last)) nil)
		(_= (list-node-item node) (aref arr idx))
		(incf idx)
		(setf node (list-node-next node)))
	  (setf node (list-node-prev node))
	  (if (= idx cnt)
		  (__list-connect-node node last)
		  (do ()
			  ((= idx cnt) nil)
			(let ((new-node (__list-new-node (aref arr idx))))
			  (incf idx)
			  (__list-connect-node node new-node)
			  (__list-connect-node new-node last)
			  (setf node new-node))))
	  (setf (list-size-cache cont) cnt))
	nil))



;-----------------------------------------------------
; iterators
;-----------------------------------------------------
(defmethod begin ((cont stl:list))
  (make-instance 'list_iterator :node (list-node-next (list-top-sentinel cont))))

(defmethod end ((cont stl:list))
  (make-instance 'list_iterator :node (list-last-sentinel cont)))

(defmethod rbegin ((cont stl:list))
  (make-instance 'list_reverse_iterator :node (list-node-prev (list-last-sentinel cont))))

(defmethod rend ((cont stl:list))
  (make-instance 'list_reverse_iterator :node (list-top-sentinel cont)))

#-cl-stl-0x98
(defmethod cbegin ((cont stl:list))
  (make-instance 'list_const_iterator
				 :node (list-node-next (list-top-sentinel cont))))

#-cl-stl-0x98
(defmethod cend ((cont stl:list))
  (make-instance 'list_const_iterator
				 :node (list-last-sentinel cont)))

#-cl-stl-0x98
(defmethod crbegin ((cont stl:list))
  (make-instance 'list_const_reverse_iterator
				 :node (list-node-prev (list-last-sentinel cont))))

#-cl-stl-0x98
(defmethod crend ((cont stl:list))
  (make-instance 'list_const_reverse_iterator
				 :node (list-top-sentinel cont)))

;-----------------------------------------------------
; capacity
;-----------------------------------------------------
(defmethod empty ((cont stl:list))
  (zerop (list-size-cache cont)))

(defmethod size ((cont stl:list))
  (list-size-cache cont))

(defmethod max_size ((cont stl:list))
  most-positive-fixnum)

(labels ((imp (cont new-size initial-element)
		   (let ((cur-size (size cont)))
			 (cond ((= new-size cur-size) nil)
				   ((< cur-size new-size)
					(do ()
						((= cur-size new-size) nil)
					  (push_back cont initial-element)
					  (incf cur-size)))
				   ((< new-size cur-size)
					(do ()
						((= cur-size new-size) nil)
					  (pop_back cont)
					  (decf cur-size))))
			 (setf (list-size-cache cont) new-size))))

  (defmethod-overload resize ((cont stl:list) (new-size integer))
	(imp cont new-size nil)
	nil)

  (defmethod-overload resize ((cont stl:list) (new-size integer) initial-element)
	(imp cont new-size initial-element)
	nil))

;-----------------------------------------------------
; element access
;-----------------------------------------------------
(defmethod front ((cont stl:list))
  (__list-error-when-empty cont "front")
  (list-node-item (list-node-next (list-top-sentinel cont))))

(defmethod (setf front) (val (cont stl:list))
  (__list-error-when-empty cont "front")
  (_= (list-node-item (list-node-next (list-top-sentinel cont))) val))

(defmethod back ((cont stl:list))
  (__list-error-when-empty cont "back")
  (list-node-item (list-node-prev (list-last-sentinel cont))))

(defmethod (setf back) (val (cont stl:list))
  (__list-error-when-empty cont "back")
  (_= (list-node-item (list-node-prev (list-last-sentinel cont))) val))

;-----------------------------------------------------
; modifiers
;-----------------------------------------------------
(defmethod push_back ((container stl:list) new-val)
  (__list-insert-node (list-last-sentinel container)
					  (__list-new-node new-val))
  (incf (list-size-cache container))
  nil)

(defmethod push_front ((container stl:list) new-val)
  (__list-insert-node (list-node-next (list-top-sentinel container))
					  (__list-new-node new-val))
  (incf (list-size-cache container))
  nil)

(defmethod pop_back ((cont stl:list))
  (__list-error-when-empty cont "pop_back")
  (__list-pullout-node (list-node-prev (list-last-sentinel cont)))
  (decf (list-size-cache cont))
  nil)

(defmethod pop_front ((cont stl:list))
  (__list-error-when-empty cont "pop_front")
  (__list-pullout-node (list-node-next (list-top-sentinel cont)))
  (decf (list-size-cache cont))
  nil)

#-cl-stl-0x98    ; emplace_back
(defmethod-overload emplace_back ((container stl:list) new-val)
  (__list-insert-node (list-last-sentinel container)
					  (__list-new-node new-val nil))
  (incf (list-size-cache container))
  nil)

#-cl-stl-0x98    ; emplace_front
(defmethod-overload emplace_front ((container stl:list) new-val)
  (__list-insert-node (list-node-next (list-top-sentinel container))
					  (__list-new-node new-val nil))
  (incf (list-size-cache container))
  nil)

;; insert ( single element ) - returns iterator
(defmethod-overload insert ((cont stl:list)
							(itr #+cl-stl-0x98 list_iterator
								 #-cl-stl-0x98 list_const_iterator) value)
  (__list-check-iterator-belong itr cont)
  (let ((node (list-itr-node itr)))
	(__list-insert node (__list-new-node value)))
  (incf (list-size-cache cont))
  (prev itr))

;; insert ( fill )
(defmethod-overload insert ((cont stl:list)
							(itr #+cl-stl-0x98 list_iterator
								 #-cl-stl-0x98 list_const_iterator) (count integer) value)
  ;; MEMO : in C++98, always returns nil. but C++11, returns iterator.
  (__error-unless-non-negative-fixnum insert count)
  (__list-check-iterator-belong itr cont)
  (let* ((node (list-itr-node itr))
		 #-cl-stl-0x98
		 (prev (list-node-prev node)))
	(__list-insert-values (list-node-prev node) node value count)
	(incf (list-size-cache cont) count)
	#+cl-stl-0x98 nil
	#-cl-stl-0x98 (make-instance 'list_iterator :node (list-node-next prev))))


;; insert ( range )
;; MEMO : in C++98, always returns nil. but C++11, returns iterator.
(labels ((__insert-imp (cont itr itr1 itr2)
		   (__list-check-iterator-belong itr cont)
		   (let* ((node (list-itr-node itr))
				  #-cl-stl-0x98
				  (prev (list-node-prev node)))
			 (let ((cnt (__list-insert-seq (list-node-prev node) node itr1 itr2)))
			   (incf (list-size-cache cont) cnt))
			 #+cl-stl-0x98 nil
			 #-cl-stl-0x98 (make-instance 'list_iterator :node (list-node-next prev)))))
  
  (defmethod-overload insert ((cont stl:list) (itr #+cl-stl-0x98 list_iterator
												   #-cl-stl-0x98 list_const_iterator)
							  (itr1 input_iterator) (itr2 input_iterator))
	(__insert-imp cont itr itr1 itr2))

  (defmethod-overload insert ((cont stl:list) (itr #+cl-stl-0x98 list_iterator
												   #-cl-stl-0x98 list_const_iterator)
							  (itr1 const-vector-pointer) (itr2 const-vector-pointer))
	(__pointer-check-iterator-range itr1 itr2)
	(__insert-imp cont itr itr1 itr2)))


;; insert ( move ) - returns iterator.
#-cl-stl-0x98
(defmethod-overload insert ((cont stl:list)
							(itr list_const_iterator) (rm& remove-reference))
  (__list-check-iterator-belong itr cont)
  (with-reference (rm)
	(let ((node (list-itr-node itr))
		  (val  rm))
	  (__list-insert node (__list-new-node val nil))
	  (setf rm nil)))
  (incf (list-size-cache cont))
  (prev itr))

;; insert ( initializer list ) - returns iterator.
#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod-overload insert ((cont stl:list)
							  (itr list_const_iterator) (il initializer_list))
	(declare (type initializer_list il))
	(__list-check-iterator-belong itr cont)
	(let* ((arr (__initlist-data il))
		   (cnt (length arr)))
	  (declare (type simple-vector arr))
	  (declare (type fixnum        cnt))
	  (if (zerop cnt)
		  (clone itr)
		  (let* ((node (list-itr-node itr))
				 (prev (list-node-prev node)))
			(do ((idx 0 (1+ idx)))
				((= idx cnt) nil)
			  (declare (type fixnum idx))
			  (__list-insert node (__list-new-node (aref arr idx))))
			(setf (list-size-cache cont)
				  (the fixnum (+ (the fixnum (list-size-cache cont)) cnt)))
			(make-instance 'list_iterator :node (list-node-next prev)))))))

#-cl-stl-0x98    ; emplace
(defmethod-overload emplace ((container stl:list)
							 (itr list_const_iterator) new-val)
  (__list-check-iterator-belong itr container)
  (let ((node (list-itr-node itr)))
	(__list-insert node (__list-new-node new-val nil)))
  (incf (list-size-cache container))
  (prev itr))

(defmethod-overload erase ((cont stl:list)
						   (itr #+cl-stl-0x98 list_iterator
								#-cl-stl-0x98 list_const_iterator))
  (__list-check-iterator-belong itr cont)
  (let* ((node1 (list-itr-node  itr))
		 (next  (list-node-next node1)))
	(__list-connect-node (list-node-prev node1) next)
	(decf (list-size-cache cont))
	(make-instance 'list_iterator :node next)))

(defmethod-overload erase ((cont stl:list)
						   (first #+cl-stl-0x98 list_iterator
								  #-cl-stl-0x98 list_const_iterator)
						   (last  #+cl-stl-0x98 list_iterator
								  #-cl-stl-0x98 list_const_iterator))
  (__list-check-iterator-belong first cont)
  (__list-check-iterator-range first last)
  (let ((node1 (list-itr-node first))
		(next  (list-itr-node last)))
	(__list-connect-node (list-node-prev node1) next)
	(decf (list-size-cache cont) (__list-count-nodes node1 next))
	(make-instance 'list_iterator :node next)))

(defmethod-overload swap ((cont1 stl:list) (cont2 stl:list))
  (let ((size1 (list-size-cache    cont1))
		(top1  (list-top-sentinel  cont1))
		(last1 (list-last-sentinel cont1)))
	(setf (list-size-cache    cont1) (list-size-cache    cont2))
	(setf (list-top-sentinel  cont1) (list-top-sentinel  cont2))
	(setf (list-last-sentinel cont1) (list-last-sentinel cont2))
	(setf (list-size-cache    cont2) size1)
	(setf (list-top-sentinel  cont2) top1)
	(setf (list-last-sentinel cont2) last1))
  (values cont1 cont2))

(defmethod clear ((cont stl:list))
  (__list-connect-node (list-top-sentinel  cont)
					   (list-last-sentinel cont))
  (setf (list-size-cache cont) 0)
  nil)

;-----------------------------------------------------
; specific operations
;-----------------------------------------------------
(labels ((__splice-imp (itr node1 node2)
		   (let* ((node (list-itr-node  itr))
				  (prev (list-node-prev node)))
			 (__list-connect-node prev node1)
			 (__list-connect-node node2 node))))

  (defmethod-overload splice ((lst1 stl:list)
							  (itr  #+cl-stl-0x98 list_iterator
									#-cl-stl-0x98 list_const_iterator)
							  (lst2 stl:list))
	(declare (ignorable lst1))
	(__list-check-iterator-belong itr lst1)
	(unless (empty lst2)
	  (let ((s1 (list-top-sentinel  lst2))
			(s2 (list-last-sentinel lst2)))
		(__splice-imp itr (list-node-next s1)
						  (list-node-prev s2))
		(__list-connect-node s1 s2))
	  (incf (list-size-cache lst1) (list-size-cache lst2))
	  (setf (list-size-cache lst2) 0))
	nil)

  (defmethod-overload splice ((lst1 stl:list)
							  (itr  #+cl-stl-0x98 list_iterator
									#-cl-stl-0x98 list_const_iterator)
							  (lst2 stl:list)
							  (itr1 #+cl-stl-0x98 list_iterator
									#-cl-stl-0x98 list_const_iterator))
	(declare (ignorable lst1 lst2))
	(__list-check-iterator-belong itr  lst1)
	(__list-check-iterator-belong itr1 lst2)
	(let* ((node (list-itr-node itr1))
		   (prev (list-node-prev node))
		   (next (list-node-next node)))
	  (__splice-imp itr node node)
	  (__list-connect-node prev next)
	  (incf (list-size-cache lst1))
	  (decf (list-size-cache lst2)))
	nil)

  (defmethod-overload splice ((lst1 stl:list)
							  (itr  #+cl-stl-0x98 list_iterator
									#-cl-stl-0x98 list_const_iterator)
							  (lst2 stl:list)
							  (itr1 #+cl-stl-0x98 list_iterator
									#-cl-stl-0x98 list_const_iterator)
							  (itr2 #+cl-stl-0x98 list_iterator
									#-cl-stl-0x98 list_const_iterator))
	(declare (ignorable lst1 lst2))
	(__list-check-iterator-belong itr  lst1)
	(__list-check-iterator-belong itr1 lst2)
	(__list-check-iterator-range  itr1 itr2)
	(let* ((node1 (list-itr-node itr1))
		   (prev  (list-node-prev node1))
		   (next  (list-itr-node itr2))
		   (node2 (list-node-prev next))
		   (cnt   (__list-count-nodes node1 next)))
	  (__splice-imp itr node1 node2)
	  (__list-connect-node prev next)
	  (incf (list-size-cache lst1) cnt)
	  (decf (list-size-cache lst2) cnt)
	  nil)))


(labels ((imp (lst val eql-bf)
		   ;; MEMO : eql-bf copied by caller. 
		   (do ((node (list-node-next (list-top-sentinel lst)))
				(last (list-last-sentinel lst)))
			   ((eq node last) nil)
			 (let ((tmp (list-node-next node)))
			   (when (funcall eql-bf val (list-node-item node))
				 (__list-pullout-node node)
				 (decf (list-size-cache lst)))
			   (setf node tmp)))))
  (defmethod-overload remove ((lst stl:list) val)
	(imp lst val #'operator_==))
  #-cl-stl-noextra
  (defmethod-overload remove ((lst stl:list) val eql-bf)
	(imp lst val (functor_function (clone eql-bf)))))

(defmethod-overload remove_if ((lst stl:list) pred-uf)
  (do ((pred-uf (functor_function (clone pred-uf)) )
	   (node (list-node-next (list-top-sentinel lst)))
	   (last (list-last-sentinel lst)))
	  ((eq node last) nil)
	(let ((tmp (list-node-next node)))
	  (when (funcall pred-uf (list-node-item node))
		(__list-pullout-node node)
		(decf (list-size-cache lst)))
	  (setf node tmp))))

(labels ((imp (node node-end eql-bf)
		   ;; MEMO : eql-bf copied by caller. 
		   (do ((cnt 0)
				(cur-node (list-node-next node)))
			   ((eq cur-node node-end) cnt)
			 (let ((tmp (list-node-next cur-node)))
			   (if (funcall eql-bf (list-node-item node)
								   (list-node-item cur-node))
				   (progn
					 (__list-pullout-node cur-node)
					 (incf cnt)
					 (setf cur-node tmp))
				   (progn
					 (setf node cur-node)
					 (setf cur-node tmp)))))))

  (defmethod-overload unique ((lst stl:list))
	(unless (< (list-size-cache lst) 2)
	  (let ((cnt (imp (list-node-next (list-top-sentinel lst))
					  (list-last-sentinel lst) #'operator_==)))
		(decf (list-size-cache lst) cnt)))
	nil)

  (defmethod-overload unique ((lst stl:list) eql-bf)
	(unless (< (list-size-cache lst) 2)
	  (let ((cnt (imp (list-node-next (list-top-sentinel lst))
					  (list-last-sentinel lst)
					  (functor_function (clone eql-bf)))))
		(decf (list-size-cache lst) cnt)))
	nil))
				   

(labels ((imp (node1 node-end1 node2 node-end2 less-bf)
		   ;; MEMO : less-bf copied by caller. 
		   (do ()
			   ((or (eq node1 node-end1)
					(eq node2 node-end2)) nil)
			 (if (funcall less-bf (list-node-item node2) (list-node-item node1))
				 (let ((tmp (list-node-next node2)))
				   (__list-insert-node node1 (__list-pullout-node node2))
				   (setf node2 tmp))
				 (setf node1 (list-node-next node1))))
		   (unless (eq node2 node-end2)
			 (let ((prev (list-node-prev node-end2)))
			   (__list-connect-node (list-node-prev node2) (list-node-next prev))
			   (__list-connect-node (list-node-prev node-end1) node2)
			   (__list-connect-node prev node-end1)))))

  (defmethod-overload merge ((lst1 stl:list)
							 (lst2 stl:list))
	(unless (or (eq lst1 lst2) (empty lst2))
	  (imp (list-node-next (list-top-sentinel lst1)) (list-last-sentinel lst1)
		   (list-node-next (list-top-sentinel lst2)) (list-last-sentinel lst2) #'operator_<)
	  (incf (list-size-cache lst1) (list-size-cache lst2))
	  (setf (list-size-cache lst2) 0))
	nil)

  (defmethod-overload merge ((lst1 stl:list)
							 (lst2 stl:list) less-bf)
	(unless (or (eq lst1 lst2) (empty lst2))
	  (imp (list-node-next (list-top-sentinel lst1)) (list-last-sentinel lst1)
		   (list-node-next (list-top-sentinel lst2)) (list-last-sentinel lst2)
		   (functor_function (clone less-bf)))
	  (incf (list-size-cache lst1) (list-size-cache lst2))
	  (setf (list-size-cache lst2) 0))
	nil))

(labels ((__sort-imp (lst less-bf)
		   ;; MEMO : less-bf copied by caller. 
		   (unless (< (list-size-cache lst) 2)
			 (let* ((+MAX-BINS+ 25)
					(bin-max    0)
					(bin-lists  (make-array +MAX-BINS+ :initial-element nil)))
			   (setf (aref bin-lists 0) (__create-list 0))
			   (do ((tmp-list (__create-list 0)))
				   ((empty lst) nil)
				 (splice tmp-list (begin tmp-list) lst (begin lst))
				 ;;// merge into ever larger bins
				 (let ((idx 0))
				   (do ()
					   ((<= bin-max idx) nil)
					 (let ((cur-lst (aref bin-lists idx)))
					   (when (zerop (size cur-lst))
						 (return nil))
					   (merge cur-lst tmp-list less-bf)
					   (swap cur-lst tmp-list))
					 (incf idx))
				   (if (= idx +MAX-BINS+)
					   (merge (aref bin-lists (1- idx)) tmp-list less-bf)
					   (progn
										;// spill to new bin, while they last
						 (swap (aref bin-lists idx) tmp-list)
						 (when (= idx bin-max)
						   (incf bin-max)
						   (setf (aref bin-lists bin-max) (__create-list 0))))))
;				 ;;// BEGIN DEBUG CODE
;				 (labels ((dump-sequence (itr1 itr2 tag)
;							(format t "~A" tag)
;							(with-operators
;								(for (nil (_/= itr1 ir2) ++itr1)
;								  (format t " ~A" *itr1)))
;							(format t "~%")))
;				   (dump-sequence (begin lst) (end lst) "lst =")
;				   (do ((idx 0))
;					   ((< bin-max idx) nil)
;					 (let ((cur-lst (aref bin-lists idx)))
;					   (dump-sequence (begin cur-lst)
;									  (end cur-lst) (format nil "bin-lists[~A] =" idx)))
;					 (incf idx)))
;				 ;;// END DEBUG CODE
				 )
			   (do ((idx 1))
				   ((<= bin-max idx) nil)
				 (merge (aref bin-lists idx) (aref bin-lists (1- idx)) less-bf)
				 (incf idx))
			   (splice lst (begin lst) (aref bin-lists (1- bin-max)))))))

  (defmethod-overload sort ((lst stl:list))
	(__sort-imp lst #'operator_<)
	nil)

  (defmethod-overload sort ((lst stl:list) less-bf)
	(__sort-imp lst (functor_function (clone less-bf)))
	nil))


(defmethod-overload reverse ((lst stl:list))
  (unless (< (list-size-cache lst) 2)
	(let* ((top  (list-top-sentinel lst))
		   (node (list-node-next top))
		   (ins-point (list-last-sentinel lst)))
	  (do ()
		  ((eq top (list-node-prev ins-point)) nil)
		(let ((next (list-node-next node)))
		  (__list-pullout-node node)
		  (setf ins-point (__list-insert-node ins-point node))
		  (setf node next))))))

;-----------------------------------------------------
; compare
;-----------------------------------------------------
(labels ((__container-equal (cont1 cont2)
		   (if (eq cont1 cont2)
			   t
			   (do ((node1 (list-node-next (list-top-sentinel cont1)))
					(node2 (list-node-next (list-top-sentinel cont2)))
					(last1 (list-last-sentinel cont1))
					(last2 (list-last-sentinel cont2)))
				   ((and (eq node1 last1) (eq node2 last2)) t)
				 (when (or (eq node1 last1) (eq node2 last2))
				   (return-from __container-equal nil))
				 (unless (_== (list-node-item node1) (list-node-item node2))
				   (return-from __container-equal nil))
				 (setf node1 (list-node-next node1))
				 (setf node2 (list-node-next node2))))))

  (defmethod operator_== ((cont1 stl:list) (cont2 stl:list))
	(__container-equal cont1 cont2))

  (defmethod operator_/= ((cont1 stl:list) (cont2 stl:list))
	(not (__container-equal cont1 cont2))))



(labels ((__container-compare (cont1 cont2)
		   (if (eq cont1 cont2)
			   0
			   (let ((last1 (list-last-sentinel cont1))
					 (last2 (list-last-sentinel cont2)))
				 (do ((node1 (list-node-next (list-top-sentinel cont1)))
					  (node2 (list-node-next (list-top-sentinel cont2))))
					 ((and (eq node1 last1) (eql node2 last2)) 0)
				   (if (eq node1 last1)
					   (return-from __container-compare -1)
					   (if (eql node2 last2)
						   (return-from __container-compare 1)
						   (let ((val1 (list-node-item node1))
								 (val2 (list-node-item node2)))
							 (if (_< val1 val2)
								 (return-from __container-compare -1)
								 (if (_< val2 val1)
									 (return-from __container-compare 1))))))
				   (setf node1 (list-node-next node1))
				   (setf node2 (list-node-next node2)))))))

  (defmethod operator_< ((cont1 stl:list) (cont2 stl:list))
	(< (__container-compare cont1 cont2) 0))

  (defmethod operator_<= ((cont1 stl:list) (cont2 stl:list))
	(<= (__container-compare cont1 cont2) 0))

  (defmethod operator_> ((cont1 stl:list) (cont2 stl:list))
	(< 0 (__container-compare cont1 cont2)))

  (defmethod operator_>= ((cont1 stl:list) (cont2 stl:list))
	(<= 0 (__container-compare cont1 cont2))))
			 

;-----------------------------------------------------
; enumeration
;-----------------------------------------------------
#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod-overload for ((cont stl:list) func)
	;MEMO : func is always lambda function ( see stl:for ). 
	(declare (type cl:function func))
	(let ((node (list-node-next (list-top-sentinel cont)))
		  (last (list-last-sentinel cont)))
	  (do ()
		  ((eq node last) nil)
		(funcall func (list-node-item node))
		(setf node (list-node-next node))))))


;;------------------------------------------------------------------------------
;;
;; methods for list_const_iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_= ((itr1 list_const_iterator) (itr2 list_const_iterator))
  (__error-when-const-removing-assign itr1 list_iterator
									  itr2 list_const_iterator)
  (setf (list-itr-node itr1) (list-itr-node itr2))
  itr1)

(defmethod operator_clone ((itr list_const_iterator))
  (make-instance 'list_const_iterator :node (list-itr-node itr)))

(defmethod operator_== ((itr1 list_const_iterator) (itr2 list_const_iterator))
  (eq (list-itr-node itr1) (list-itr-node itr2)))

(defmethod operator_/= ((itr1 list_const_iterator) (itr2 list_const_iterator))
  (not (eq (list-itr-node itr1) (list-itr-node itr2))))

(defmethod operator_* ((itr list_const_iterator))
  (list-node-item (list-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr list_const_iterator))
  (error 'setf-to-const :what "setf to (_* list_const_iterator)."))

(defmethod operator_++ ((itr list_const_iterator))
  (let ((node (list-itr-node itr)))
	(setf (list-itr-node itr) (list-node-next node)))
  itr)

(defmethod operator_-- ((itr list_const_iterator))
  (let ((node (list-itr-node itr)))
	(setf (list-itr-node itr) (list-node-prev node)))
  itr)

(locally (declare (optimize speed))
  (defmethod advance ((itr list_const_iterator) (n integer))
	(declare (type fixnum n))
	(let ((node (list-itr-node itr)))
	  (if (>= n 0)
		  (do ((i 0))
			  ((= i n) nil)
			(declare (type fixnum i))
			(setf node (list-node-next node))
			(incf i))
		  (do ((i 0))
			  ((= i n) nil)
			(declare (type fixnum i))
			(setf node (list-node-prev node))
			(decf i)))
	  (setf (list-itr-node itr) node))
	nil))

(defmethod distance ((itr1 list_const_iterator) (itr2 list_const_iterator))
  (__list-count-nodes (list-itr-node itr1) (list-itr-node itr2)))

;; creating reverse iterator.
(define-constructor reverse_iterator ((itr list_const_iterator))
  (make-instance 'list_const_reverse_iterator
				 :node (list-node-prev (list-itr-node itr))))


;;------------------------------------------------------------------------------
;;
;; methods for list_iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_clone ((itr list_iterator))
  (make-instance 'list_iterator :node (list-itr-node itr)))

(defmethod operator_cast ((itr list_iterator)
						  (typename (eql 'list_const_iterator)))
  (__check-exact-type-of-cast itr 'list_iterator 'list_const_iterator)
  (make-instance 'list_const_iterator :node (list-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr list_iterator))
  (_= (list-node-item (list-itr-node itr)) new-val))

;; creating reverse iterator.
(define-constructor reverse_iterator ((itr list_iterator))
  (make-instance 'list_reverse_iterator
				 :node (list-node-prev (list-itr-node itr))))



;;------------------------------------------------------------------------------
;;
;; methods for list_const_reverse_iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_= ((itr1 list_const_reverse_iterator)
					  (itr2 list_const_reverse_iterator))
  (__error-when-const-removing-assign itr1 list_reverse_iterator
									  itr2 list_const_reverse_iterator)
  (setf (list-rev-itr-node itr1) (list-rev-itr-node itr2))
  itr1)

(defmethod operator_clone ((itr list_const_reverse_iterator))
  (make-instance 'list_const_reverse_iterator :node (list-rev-itr-node itr)))

(defmethod operator_== ((itr1 list_const_reverse_iterator)
				  (itr2 list_const_reverse_iterator))
  (eq (list-rev-itr-node itr1) (list-rev-itr-node itr2)))

(defmethod operator_/= ((itr1 list_const_reverse_iterator)
				   (itr2 list_const_reverse_iterator))
  (not (eq (list-rev-itr-node itr1) (list-rev-itr-node itr2))))

(defmethod operator_* ((itr list_const_reverse_iterator))
  (list-node-item (list-rev-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr list_const_reverse_iterator))
  (error 'setf-to-const :what "setf to (_* list_const_reverse_iterator)."))

(defmethod operator_++ ((itr list_const_reverse_iterator))
  (let ((node (list-rev-itr-node itr)))
	(setf (list-rev-itr-node itr) (list-node-prev node)))
  itr)

(defmethod operator_-- ((itr list_const_reverse_iterator))
  (let ((node (list-rev-itr-node itr)))
	(setf (list-rev-itr-node itr) (list-node-next node)))
  itr)

(locally (declare (optimize speed))
  (defmethod advance ((itr list_const_reverse_iterator) (n integer))
	(declare (type fixnum n))
	(let ((node (list-rev-itr-node itr)))
	  (if (>= n 0)
		  (do ((i 0))
			  ((= i n) nil)
			(declare (type fixnum i))
			(setf node (list-node-prev node))
			(incf i))
		  (do ((i 0))
			  ((= i n) nil)
			(declare (type fixnum i))
			(setf node (list-node-next node))
			(decf i)))
	  (setf (list-rev-itr-node itr) node))
	nil))

(defmethod distance ((itr1 list_const_reverse_iterator)
					  (itr2 list_const_reverse_iterator))
   (__list-count-nodes (list-rev-itr-node itr2) (list-rev-itr-node itr1)))

(defmethod base ((rev-itr list_const_reverse_iterator))
  (make-instance 'list_const_iterator
				 :node  (list-node-next (list-rev-itr-node rev-itr))))

;; creating reverse iterator.
(define-constructor reverse_iterator ((itr list_const_reverse_iterator))
  (make-instance 'list_const_iterator
				 :node (list-node-next (list-rev-itr-node itr))))


;;------------------------------------------------------------------------------
;;
;; methods for list_reverse_iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_clone ((itr list_reverse_iterator))
  (make-instance 'list_reverse_iterator
				 :node (list-rev-itr-node itr)))

(defmethod operator_cast ((itr list_reverse_iterator)
						  (typename (eql 'list_const_reverse_iterator)))
  (__check-exact-type-of-cast itr 'list_reverse_iterator
								  'list_const_reverse_iterator)
  (make-instance 'list_const_reverse_iterator
				 :node (list-rev-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr list_reverse_iterator))
  (_= (list-node-item (list-rev-itr-node itr)) new-val))

(defmethod base ((rev-itr list_reverse_iterator))
  (make-instance 'list_iterator
				 :node  (list-node-next (list-rev-itr-node rev-itr))))

;; creating reverse iterator.
(define-constructor reverse_iterator ((itr list_reverse_iterator))
  (make-instance 'list_iterator
				 :node (list-node-next (list-rev-itr-node itr))))





;;------------------------------------------------------------------------------
;;
;; debug methods for stl:list
;;
;;------------------------------------------------------------------------------

#+cl-stl-debug
(defmethod dump ((container stl:list) &optional (stream t) (print-item-fnc nil))
  (setf print-item-fnc (if print-item-fnc
						   (functor_function (clone print-item-fnc))
						   (lambda (s x) (format s "~A" x))))
  (format stream "begin dump ---------------------~%")
  (with-operators
	  (for (((idx  0)
			 (itr1 (begin container))
			 (itr2 (end   container))) (_/= itr1 itr2) (progn (incf idx) ++itr1))
		(format stream "~A : " idx)
		(funcall print-item-fnc stream *itr1)
		(format stream "~%")))
  (format stream "end dump -----------------------~%")
  nil)

#+cl-stl-debug
(defmethod check_integrity ((container stl:list) &optional (stream t))
  (let ((result     nil)
		(err-count  0))
	; counting node count ( and check forward reachability )
	(setf result
		  (do ((cnt  0 (1+ cnt))
			   (node (list-top-sentinel  container))
			   (last (list-last-sentinel container)))
			  ((eq node last) (1- cnt))
			(when (null node)
			  (return nil))
			(setf node (list-node-next node))))
	(when (or (null result)
			  (/= result (list-size-cache container)))
	  (incf err-count)
	  (format stream "  ERROR : forward node chain has corrupted. cnt = ~A.~%" result))

	;check backward reachability
	(setf result
		  (do ((cnt  0 (1+ cnt))
			   (top  (list-top-sentinel  container))
			   (node (list-last-sentinel container)))
			  ((eq top node) (1- cnt))
			(when (null node)
			  (return nil))
			(setf node (list-node-prev node))))
	(when (or (null result)
			  (/= result (list-size-cache container)))
	  (incf err-count)
	  (format stream "  ERROR : backward node chain has corrupted. cnt = ~A.~%" result))

	;check next-prev connection
	(setf result
		  (do ((node (list-top-sentinel  container))
			   (last (list-last-sentinel container)))
			  ((eq node last) t)
			(unless (eq node (list-node-prev (list-node-next node)))
			  (return nil))
			(setf node (list-node-next node))))
	(unless result
	  (incf err-count)
	  (format stream "  ERROR : next - prev chain has corrupted.~%"))

	;check prev of top-sentinel is nil
	(unless (null (list-node-prev (list-top-sentinel container)))
	  (incf err-count)
	  (format stream "  ERROR : top-sentinel's prev is not nil.~%"))

	;check next of last-sentinel is nil
	(unless (null (list-node-next (list-last-sentinel container)))
	  (incf err-count)
	  (format stream "  ERROR : last-sentinel's next is not nil.~%"))

	;check value of top-sentinel is nil
	(unless (null (list-node-item (list-top-sentinel container)))
	  (incf err-count)
	  (format stream "  ERROR : top-sentinel's value is not nil.~%"))

	;check value of last-sentinel is nil
	(unless (null (list-node-item (list-last-sentinel container)))
	  (incf err-count)
	  (format stream "  ERROR : last-sentinel's value is not nil.~%"))

	(zerop err-count)))

