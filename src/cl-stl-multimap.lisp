(in-package :cl-stl)

;;------------------------------------------------------------------------------
;;
;; class difinition
;;
;;------------------------------------------------------------------------------
(defclass multimap (bidirectional-container)
  ((rbtree  :type     :rbtree
		    :initform nil
		    :initarg  :core
		    :accessor multimap-tree)))

(defclass multimap-const-iterator (bidirectional-iterator)
  ((rbnode :type     :rbnode
		   :initform nil
		   :initarg  :node
		   :accessor multimap-itr-node)))

(defclass multimap-iterator (multimap-const-iterator) ())

(defclass multimap-const-reverse-iterator (bidirectional-iterator)
  ((rbnode :type     :rbnode
		   :initform nil
		   :initarg  :node
		   :accessor multimap-rev-itr-node)))

(defclass multimap-reverse-iterator (multimap-const-reverse-iterator) ()) 

;;--------------------------------------------------------------------
;;
;; internal utilities
;;
;;--------------------------------------------------------------------
(defmacro __multimap-check-iterator-belong (itr cont)
  (declare (ignorable itr cont))
  #-cl-stl-debug nil
  #+cl-stl-debug
  `(unless (_== ,itr (stl:end ,cont))
	 (unless (do* ((tree  (multimap-tree ,cont))
				   (node  (multimap-itr-node ,itr))
				   (node1 (__rbtree-lower-bound tree (__rbnode-value node)))
				   (node2 (__rbtree-upper-bound tree (__rbnode-value node))))
				  ((eq node1 node2) nil)
			   (when (eq node1 node)
				 (return t))
			   (setf node1 (__rbnode-next node1)))
	   (error 'undefined-behavior :what ,(format nil "~A is not iterator of ~A." itr cont)))))

(defmacro __multimap-check-iterator-range (itr1 itr2)
  (declare (ignorable itr1 itr2))
  #-cl-stl-debug nil
  #+cl-stl-debug
  `(unless (__rbnode-check-reachable (multimap-itr-node ,itr1)
									 (multimap-itr-node ,itr2))
	 (error 'undefined-behavior :what ,(format nil "[~A ~A) is not valid range." itr1 itr2))))


;;--------------------------------------------------------------------
;;
;; method implementation
;;
;;--------------------------------------------------------------------
(defun __create-multimap (key-comp)
  (let ((rbtree (__rbtree-create (__create-map-val-cmp key-comp))))
	(make-instance 'multimap :core rbtree)))

#-cl-stl-0x98
(locally (declare (optimize speed))
  (defun __create-multimap-with-initlist (il key-comp)
	(declare (type initializer-list il))
	(let* ((arr (__initlist-data il))
		   (cnt (length arr))
		   (rbtree (__rbtree-create (__create-map-val-cmp key-comp))))
	  (declare (type cl:vector arr))
	  (declare (type fixnum    cnt))
	  (do ((idx 0 (1+ idx)))
		  ((= idx cnt)
		   (make-instance 'multimap :core rbtree))
		(declare (type fixnum idx))
		(let ((val (aref arr idx)))
		  (__map-check-item-pairness val)
		  (__rbtree-insert rbtree val t t))))))


;;------------------------------------------------------------------------------
;;
;; constructors
;;
;;------------------------------------------------------------------------------
(declare-constructor multimap (0 1 2 3))

; empty constructor 1
(define-constructor multimap ()
  (__create-multimap #'operator_<))

; empty constructor 2
(define-constructor multimap ((arg cl:function))
  (__create-multimap arg))

; empty constructor 3
(define-constructor multimap ((arg #-cl-stl-0x98 functor
								   #+cl-stl-0x98 binary-function))
  (__create-multimap arg))

; copy constructor
(define-constructor multimap ((arg multimap))
  (clone arg))

; constructor with initializer list 1
#-cl-stl-0x98
(define-constructor multimap ((arg initializer-list))
  (__create-multimap-with-initlist arg #'operator_<))

; constructor with initializer list 2
#-cl-stl-0x98
(define-constructor multimap ((arg1 initializer-list) (arg2 cl:function))
  (__create-multimap-with-initlist arg1 arg2))

; constructor with initializer list 3
#-cl-stl-0x98
(define-constructor multimap ((arg1 initializer-list)
							  (arg2 #-cl-stl-0x98 functor
									#+cl-stl-0x98 binary-function))
  (__create-multimap-with-initlist arg1 arg2))

; move constructor
#-cl-stl-0x98
(define-constructor multimap ((arg remove-reference))
  (let ((cont (funcall (__rm-ref-closure arg))))
	(__check-type-of-move-constructor cont multimap)
	(let ((new-map (new stl:multimap (clone (key-comp cont)))))
	  (swap new-map cont)
	  new-map)))

; range constructor
(labels ((__range-ctor-imp (itr1 itr2 key-comp)
		   (let ((rbtree (__rbtree-create (__create-map-val-cmp key-comp))))
			 (with-operators
				 (for (((itr @~itr1)) (_/= itr itr2) ++itr)
				   (let ((val *itr))
					 (__map-check-item-pairness val)
					 (__rbtree-insert rbtree val t t))))
			 (make-instance 'multimap :core rbtree))))

  (define-constructor multimap ((itr1 input-iterator) (itr2 input-iterator))
	(__range-ctor-imp itr1 itr2 #'operator_<))

  (define-constructor multimap ((itr1 input-iterator) (itr2 input-iterator) (comp cl:function))
	(__range-ctor-imp itr1 itr2 comp))

  (define-constructor multimap ((itr1 input-iterator) (itr2 input-iterator)
								(comp #-cl-stl-0x98 functor
									  #+cl-stl-0x98 binary-function))
	(__range-ctor-imp itr1 itr2 comp)))

;; range constructor for const-vector-pointer.
(locally (declare (optimize speed))
  (labels ((__range-ctor-imp (ptr1 ptr2 key-comp)
			 (__pointer-check-iterator-range ptr1 ptr2)
			 (let ((idx1   (opr::vec-ptr-index  ptr1))
				   (idx2   (opr::vec-ptr-index  ptr2))
				   (buf    (opr::vec-ptr-buffer ptr1))
				   (rbtree (__rbtree-create (__create-map-val-cmp key-comp))))
			   (declare (type fixnum idx1 idx2))
			   (declare (type cl:vector buf))
			   (for (nil (< idx1 idx2) (incf idx1))
				 (let ((val (aref buf idx1)))
				   (__map-check-item-pairness val)
				   (__rbtree-insert rbtree val t t)))
			   (make-instance 'multimap :core rbtree))))

	(define-constructor multimap ((itr1 const-vector-pointer) (itr2 const-vector-pointer))
	  (__range-ctor-imp itr1 itr2 #'operator_<))

	(define-constructor multimap ((itr1 const-vector-pointer) (itr2 const-vector-pointer) (comp cl:function))
	  (__range-ctor-imp itr1 itr2 comp))

	(define-constructor multimap ((itr1 const-vector-pointer) (itr2 const-vector-pointer)
								  (comp #-cl-stl-0x98 functor
										#+cl-stl-0x98 binary-function))
	  (__range-ctor-imp itr1 itr2 comp))))


(defmethod operator_clone ((container multimap))
  (let* ((src-tree (multimap-tree container))
		 (src-comp (__rbtree-cmp-func src-tree))
		 (rbtree   (__rbtree-create (clone src-comp))))
	(__rbtree-for-each src-tree
		(lambda (val)
		  (__rbtree-insert rbtree val t t)))
	(make-instance 'multimap :core rbtree)))


;;------------------------------------------------------------------------------
;;
;; methods
;;
;;------------------------------------------------------------------------------

;-----------------------------------------------------
; assignment
;-----------------------------------------------------
(defmethod operator_= ((cont1 multimap) (cont2 multimap))
  (let ((tree1 (multimap-tree cont1))
		(tree2 (multimap-tree cont2)))
	(__rbtree-clear tree1)
	(__rbtree-for-each tree2
		(lambda (val)
		  (__rbtree-insert tree1 val t t))))
  cont1)

#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod operator_= ((cont multimap) (il initializer-list))
	(declare (type initializer-list il))
	(let* ((arr  (__initlist-data il))
		   (cnt  (length arr))
		   (tree (multimap-tree cont)))
	  (declare (type cl:vector arr))
	  (declare (type fixnum    cnt))
	  (__rbtree-clear tree)
	  (do ((idx 0 (1+ idx)))
		  ((= idx cnt) cont)
		(declare (type fixnum idx))
		(let ((val (aref arr idx)))
		  (__map-check-item-pairness val)
		  (__rbtree-insert tree val t t))))))

#-cl-stl-0x98
(defmethod operator_move ((cont1 multimap) (cont2 multimap))
  (unless (eq cont1 cont2)
	(let ((tree1 (multimap-tree cont1))
		  (tree2 (multimap-tree cont2)))
	  (__rbtree-clear tree1)
	  (setf (multimap-tree cont1) tree2)
	  (setf (multimap-tree cont2) tree1)))
  (values cont1 cont2))


;-----------------------------------------------------
; iterators
;-----------------------------------------------------
(defmethod begin ((container multimap))
  (make-instance 'multimap-iterator
				 :node (__rbtree-begin (multimap-tree container))))

(defmethod end ((container multimap))
  (make-instance 'multimap-iterator
				 :node (__rbtree-end (multimap-tree container))))

(defmethod rbegin ((container multimap))
  (make-instance 'multimap-reverse-iterator 
				 :node (__rbtree-rbegin (multimap-tree container))))

(defmethod rend ((container multimap))
  (make-instance 'multimap-reverse-iterator 
				 :node (__rbtree-rend (multimap-tree container))))

#-cl-stl-0x98
(defmethod cbegin ((container multimap))
  (make-instance 'multimap-const-iterator
				 :node (__rbtree-begin (multimap-tree container))))

#-cl-stl-0x98
(defmethod cend ((container multimap))
  (make-instance 'multimap-const-iterator
				 :node (__rbtree-end (multimap-tree container))))

#-cl-stl-0x98
(defmethod crbegin ((container multimap))
  (make-instance 'multimap-const-reverse-iterator
				 :node (__rbtree-rbegin (multimap-tree container))))

#-cl-stl-0x98
(defmethod crend ((container multimap))
  (make-instance 'multimap-const-reverse-iterator
				 :node (__rbtree-rend (multimap-tree container))))


;-----------------------------------------------------
; capacity
;-----------------------------------------------------
(defmethod empty ((container multimap))
  (zerop (__rbtree-cur-size (multimap-tree container))))

(defmethod size ((container multimap))
  (__rbtree-cur-size (multimap-tree container)))

(defmethod max-size ((container multimap))
  (declare (ignorable container))
  most-positive-fixnum)

;-----------------------------------------------------
; element access
;-----------------------------------------------------
; NONE.

;-----------------------------------------------------
; modifiers
;-----------------------------------------------------
;; insert ( single elememt ) - returns iterator.
(defmethod-overload insert ((container multimap) value)
  (let ((tree (multimap-tree container)))
	(__map-check-item-pairness value)
	(make-instance 'multimap-iterator
				   :node (__rbtree-insert tree value t t))))

;; insert ( single elememt by remove reference ) - returns iterator.
#-cl-stl-0x98
(defmethod-overload insert ((container multimap) (rm remove-reference))
  (let ((tree (multimap-tree container))
		(val  (funcall (__rm-ref-closure rm))))
	(__map-check-item-pairness val)
	(funcall (__rm-ref-closure rm) nil)
	(let ((node (__rbtree-insert tree val t nil)))
	  (make-instance 'multimap-iterator :node node))))

;; insert ( single elememt with hint ) - returns iterator.
(defmethod-overload insert ((container multimap)
							(itr #+cl-stl-0x98 multimap-iterator
								 #-cl-stl-0x98 multimap-const-iterator) value)
  (__multimap-check-iterator-belong itr container)
  (__map-check-item-pairness value)
  (let* ((tree (multimap-tree container))
		 (node (__rbtree-insert-with-hint (multimap-itr-node itr) tree value t t)))
	(unless node
	  (setf node (__rbtree-insert tree value t t)))
	(make-instance 'multimap-iterator :node node)))

;; insert ( single elememt with hint by remove reference ) - returns iterator.
#-cl-stl-0x98
(defmethod-overload insert ((container multimap)
							(itr multimap-const-iterator) (rm remove-reference))
  (__multimap-check-iterator-belong itr container)
  (let ((tree (multimap-tree container))
		(val  (funcall (__rm-ref-closure rm))))
	(__map-check-item-pairness val)
	(let ((node (__rbtree-insert-with-hint (multimap-itr-node itr) tree val t nil)))
	  (unless node
		(setf node (__rbtree-insert tree val t nil)))
	  (funcall (__rm-ref-closure rm) nil)
	  (make-instance 'multimap-iterator :node node))))

;; range insert - returns nil.
(locally (declare (optimize speed))

  (defmethod-overload insert ((container multimap) (itr1 input-iterator) (itr2 input-iterator))
	(let ((tree (multimap-tree container)))
	  (with-operators
		  (for (((itr @~itr1)) (_/= itr itr2) ++itr)
			(let ((val *itr))
			  (__map-check-item-pairness val)
			  (__rbtree-insert tree val t t))))))
  
  (defmethod-overload insert ((container multimap)
							  (itr1 multimap-const-iterator) (itr2 multimap-const-iterator))
	(let ((tree (multimap-tree container)))
	  (with-operators
		  (for (((itr @~itr1)) (_/= itr itr2) ++itr)
			(__rbtree-insert tree *itr t t)))))
  
  (defmethod-overload insert ((container multimap) (itr1 const-vector-pointer) (itr2 const-vector-pointer))
	(__pointer-check-iterator-range itr1 itr2)
	(let ((tree (multimap-tree container))
		  (idx1 (opr::vec-ptr-index  itr1))
		  (idx2 (opr::vec-ptr-index  itr2))
		  (buf  (opr::vec-ptr-buffer itr1)))
	  (declare (type fixnum idx1 idx2))
	  (declare (type cl:vector buf))
	  (for (nil (< idx1 idx2) (incf idx1))
		(let ((val (aref buf idx1)))
		  (__map-check-item-pairness val)
		  (__rbtree-insert tree val t t))))))
  

;; insert ( initializer list ) - returns nil.
#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod-overload insert ((container multimap) (il initializer-list))
	(declare (type initializer-list il))
	(let* ((arr (__initlist-data il))
		   (cnt (length arr))
		   (tree (multimap-tree container)))
	  (declare (type cl:vector arr))
	  (declare (type fixnum    cnt))
	  (do ((idx 0 (1+ idx)))
		  ((= idx cnt) nil)
		(declare (type fixnum idx))
		(let ((val (aref arr idx)))
		  (__map-check-item-pairness val)
		  (__rbtree-insert tree val t t))))))

#-cl-stl-0x98    ; emplace
(defmethod-overload emplace ((container multimap) new-val)
  ;; MEMO : returns iterator.
  (let ((tree (multimap-tree container)))
	(__map-check-item-pairness new-val)
	(make-instance 'multimap-iterator
				   :node (__rbtree-insert tree new-val t nil))))

#-cl-stl-0x98    ; emplace-hint
(defmethod-overload emplace-hint ((container multimap)
								  (itr multimap-const-iterator) new-val)
  ;; MEMO : returns iterator.
  (__multimap-check-iterator-belong itr container)
  (__map-check-item-pairness new-val)
  (let* ((tree (multimap-tree container))
		 (node (__rbtree-insert-with-hint (multimap-itr-node itr)
										  tree new-val t nil)))
	(unless node
	  (setf node (__rbtree-insert tree new-val t nil)))
	(make-instance 'multimap-iterator :node node)))

; In 0x98, returns nil. In 0x11 returns iterator.
(defmethod-overload erase ((container multimap)
						   (itr #+cl-stl-0x98 multimap-iterator
								#-cl-stl-0x98 multimap-const-iterator))
  (__multimap-check-iterator-belong itr container)
  (let* ((node1 (multimap-itr-node itr))
		 #-cl-stl-0x98
		 (node2 (__rbnode-next node1)))
	(__rbtree-delete-node (multimap-tree container) node1)
	#+cl-stl-0x98 nil
	#-cl-stl-0x98 (make-instance 'multimap-iterator :node node2)))

; In 0x98, returns nil. In 0x11 returns iterator.
(defmethod-overload erase ((container multimap)
						   (first #+cl-stl-0x98 multimap-iterator
								  #-cl-stl-0x98 multimap-const-iterator)
						   (last  #+cl-stl-0x98 multimap-iterator
								  #-cl-stl-0x98 multimap-const-iterator))
  (__multimap-check-iterator-belong first container)
  (__multimap-check-iterator-range  first last)
  (let ((tree (multimap-tree container)))
	(with-operators
		(for (((itr @~first)) (_/= itr last) nil)
		  (let ((node (multimap-itr-node itr)))
			++itr
			(__rbtree-delete-node tree node)))))
  #+cl-stl-0x98 nil
  #-cl-stl-0x98 (make-instance 'multimap-iterator :node (multimap-itr-node last)))

; returns deleted node count.
(defmethod-overload erase ((container multimap) key)
  (let ((tree (multimap-tree container))
		(key  (make-pair key nil)))
	(let ((node1 (__rbtree-lower-bound tree key))
		  (node2 (__rbtree-upper-bound tree key)))
	  (do ((cnt 0 (incf cnt)))
		  ((eq node1 node2) cnt)
		(let ((node node1))
		  (setf node1 (__rbnode-next node1))
		  (__rbtree-delete-node tree node))))))

(defmethod-overload swap ((cont1 multimap) (cont2 multimap))
  (let ((tmp-tree (multimap-tree cont1)))
	(setf (multimap-tree cont1) (multimap-tree cont2))
	(setf (multimap-tree cont2) tmp-tree))
  (values cont1 cont2))

(defmethod clear ((container multimap))
  (__rbtree-clear (multimap-tree container))
  nil)

;-----------------------------------------------------
; specific operations
;-----------------------------------------------------
; returns iterator.
(defmethod-overload find ((container multimap) key)
  (let* ((rbtree (multimap-tree container))
		 (key    (make-pair key nil))
		 (node   (__rbtree-lower-bound rbtree key)))
	(if (__rbnode-is-external node)
		(end container)
		(let ((pred-fnc (__rbtree-cmp-func rbtree)))
		  (if (functor-call pred-fnc key (__rbnode-value node))
			  (end container)
			  (make-instance 'multimap-iterator :node node))))))

; returns fixnum.
(defmethod-overload count ((container multimap) key)
  (__rbtree-count (multimap-tree container) (make-pair key nil)))

; returns iterator.
(defmethod-overload lower-bound ((container multimap) key)
  (make-instance 'multimap-iterator
				 :node (__rbtree-lower-bound (multimap-tree container)
											 (make-pair key nil))))

; returns iterator.
(defmethod-overload upper-bound ((container multimap) key)
  (make-instance 'multimap-iterator
				 :node (__rbtree-upper-bound (multimap-tree container)
											 (make-pair key nil))))

; returns pair(itr,itr).
(defmethod-overload equal-range ((container multimap) key)
  (let ((rbtree (multimap-tree container))
		(key    (make-pair key nil)))
	(make-pair
	 (make-instance 'multimap-iterator
					:node (__rbtree-lower-bound rbtree key))
	 (make-instance 'multimap-iterator
					:node (__rbtree-upper-bound rbtree key)))))


;-----------------------------------------------------
; observers
;-----------------------------------------------------
(defmethod key-comp ((container multimap))
  (__map-valcmp-keycmp (__rbtree-cmp-func (multimap-tree container))))

(defmethod value-comp ((container multimap))
  (__rbtree-cmp-func (multimap-tree container)))

;-----------------------------------------------------
; compare
;-----------------------------------------------------
(labels ((__container-equal (cont1 cont2)
		   (if (eq cont1 cont2)
			   t
			   (let ((rbtree1 (multimap-tree cont1))
					 (rbtree2 (multimap-tree cont2)))
				 (if (/= (__rbtree-cur-size rbtree1)
						 (__rbtree-cur-size rbtree2))
					 nil
					 (do ((node1 (__rbtree-begin rbtree1))
						  (last1 (__rbtree-end   rbtree1))
						  (node2 (__rbtree-begin rbtree2)))
						 ((eq node1 last1) t)
					   (unless (_== (__rbnode-value node1) (__rbnode-value node2))
						 (return-from __container-equal nil))
					   (setf node1 (__rbnode-next node1))
					   (setf node2 (__rbnode-next node2))))))))

  (defmethod operator_== ((cont1 multimap) (cont2 multimap))
	(__container-equal cont1 cont2))

  (defmethod operator_/= ((cont1 multimap) (cont2 multimap))
	(not (__container-equal cont1 cont2))))



(labels ((__container-compare (rbtree1 rbtree2)
		   (let* ((node1 (__rbtree-begin rbtree1))
				  (node2 (__rbtree-begin rbtree2))
				  (last1 (__rbtree-end   rbtree1))
				  (last2 (__rbtree-end   rbtree2)))
			 (do ()
				 ((and (eq node1 last1) (eq node2 last2)) 0)
			   (if (eq node1 last1)
				   (return-from __container-compare -1)
				   (if (eq node2 last2)
					   (return-from __container-compare 1)
					   (let ((val1 (__rbnode-value node1))
							 (val2 (__rbnode-value node2)))
						 (if (_< val1 val2)
							 (return-from __container-compare -1)
							 (if (_< val2 val1)
								 (return-from __container-compare 1))))))
			   (setf node1 (__rbnode-next node1))
			   (setf node2 (__rbnode-next node2))))))

  (defmethod operator_< ((cont1 multimap) (cont2 multimap))
	(< (__container-compare (multimap-tree cont1) (multimap-tree cont2)) 0))

  (defmethod operator_<= ((cont1 multimap) (cont2 multimap))
	(<= (__container-compare (multimap-tree cont1) (multimap-tree cont2)) 0))

  (defmethod operator_> ((cont1 multimap) (cont2 multimap))
	(< 0 (__container-compare (multimap-tree cont1) (multimap-tree cont2))))

  (defmethod operator_>= ((cont1 multimap) (cont2 multimap))
	(<= 0 (__container-compare (multimap-tree cont1) (multimap-tree cont2)))))




;-----------------------------------------------------
; enumeration
;-----------------------------------------------------
#-cl-stl-0x98
(defmethod-overload for ((cont multimap) func)
  ;MEMO : func is always lambda function ( see stl:for ). 
  (__rbtree-for-each (multimap-tree cont) func))


;;------------------------------------------------------------------------------
;;
;; methods for multimap-const-iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_= ((itr1 multimap-const-iterator)
					  (itr2 multimap-const-iterator))
  (__error-when-const-removing-assign itr1 multimap-iterator
									  itr2 multimap-const-iterator)
  (setf (multimap-itr-node itr1) (multimap-itr-node itr2))
  itr1)

(defmethod operator_clone ((itr multimap-const-iterator))
  (make-instance 'multimap-const-iterator :node (multimap-itr-node itr)))

(defmethod operator_== ((itr1 multimap-const-iterator) (itr2 multimap-const-iterator))
  (eq (multimap-itr-node itr1) (multimap-itr-node itr2)))

(defmethod operator_/= ((itr1 multimap-const-iterator) (itr2 multimap-const-iterator))
  (not (eq (multimap-itr-node itr1) (multimap-itr-node itr2))))

(defmethod operator_* ((itr multimap-const-iterator))
  (__rbnode-value (multimap-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr multimap-const-iterator))
  (error 'setf-to-const :what "setf to (_* multimap-const-iterator)."))

(defmethod operator_++ ((itr multimap-const-iterator))
  (let ((node (multimap-itr-node itr)))
	(setf (multimap-itr-node itr) (__rbnode-next node)))
  itr)

(defmethod operator_-- ((itr multimap-const-iterator))
  (let ((node (multimap-itr-node itr)))
	(setf (multimap-itr-node itr) (__rbnode-prev node)))
  itr)

(locally (declare (optimize speed))
  (defmethod advance ((itr multimap-const-iterator) (n integer))
	(declare (type fixnum n))
	(let ((node (multimap-itr-node itr)))
	  (if (>= n 0)
		  (do ((i 0))
			  ((= i n) nil)
			(declare (type fixnum i))
			(setf node (__rbnode-next node))
			(incf i))
		  (do ((i 0))
			  ((= i n) nil)
			(declare (type fixnum i))
			(setf node (__rbnode-prev node))
			(decf i)))
	  (setf (multimap-itr-node itr) node))
	nil))

(locally (declare (optimize speed))
  (defmethod distance ((itr1 multimap-const-iterator) (itr2 multimap-const-iterator))
	(let ((cnt 0))
	  (declare (type fixnum cnt))
	  (do ((node1 (multimap-itr-node itr1))
		   (node2 (multimap-itr-node itr2)))
		  ((eq node1 node2) cnt)
		(incf cnt)
		(setf node1 (__rbnode-next node1))))))

;; creating reverse iterator.
(define-constructor reverse-iterator ((itr multimap-const-iterator))
  (make-instance 'multimap-const-reverse-iterator
				 :node (__rbnode-prev (multimap-itr-node itr))))


;;------------------------------------------------------------------------------
;;
;; methods for multimap-iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_clone ((itr multimap-iterator))
  (make-instance 'multimap-iterator :node (multimap-itr-node itr)))

(defmethod operator_cast ((itr multimap-iterator)
						  (typename (eql 'multimap-const-iterator)))
  (__check-exact-type-of-cast itr 'multimap-iterator 'multimap-const-iterator)
  (make-instance 'multimap-const-iterator :node (multimap-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr multimap-iterator))
  (__map-check-item-pairness new-val)
  (_= (__rbnode-value (multimap-itr-node itr)) new-val))

;; creating reverse iterator.
(define-constructor reverse-iterator ((itr multimap-iterator))
  (make-instance 'multimap-reverse-iterator
				 :node (__rbnode-prev (multimap-itr-node itr))))



;;------------------------------------------------------------------------------
;;
;; methods for multimap-const-reverse-iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_= ((itr1 multimap-const-reverse-iterator)
					  (itr2 multimap-const-reverse-iterator))
  (__error-when-const-removing-assign itr1 multimap-reverse-iterator
									  itr2 multimap-const-reverse-iterator)
  (setf (multimap-rev-itr-node itr1) (multimap-rev-itr-node itr2))
  itr1)

(defmethod operator_clone ((itr multimap-const-reverse-iterator))
  (make-instance 'multimap-const-reverse-iterator :node (multimap-rev-itr-node itr)))

(defmethod operator_== ((itr1 multimap-const-reverse-iterator)
				  (itr2 multimap-const-reverse-iterator))
  (eq (multimap-rev-itr-node itr1) (multimap-rev-itr-node itr2)))

(defmethod operator_/= ((itr1 multimap-const-reverse-iterator)
				   (itr2 multimap-const-reverse-iterator))
  (not (eq (multimap-rev-itr-node itr1) (multimap-rev-itr-node itr2))))

(defmethod operator_* ((itr multimap-const-reverse-iterator))
  (__rbnode-value (multimap-rev-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr multimap-const-reverse-iterator))
  (error 'setf-to-const :what "setf to (_* multimap-const-reverse-iterator)."))

(defmethod operator_++ ((itr multimap-const-reverse-iterator))
  (let ((node (multimap-rev-itr-node itr)))
	(setf (multimap-rev-itr-node itr) (__rbnode-prev node)))
  itr)

(defmethod operator_-- ((itr multimap-const-reverse-iterator))
  (let ((node (multimap-rev-itr-node itr)))
	(setf (multimap-rev-itr-node itr) (__rbnode-next node)))
  itr)

(locally (declare (optimize speed))
  (defmethod advance ((itr multimap-const-reverse-iterator) (n integer))
	(declare (type fixnum n))
	(let ((node (multimap-rev-itr-node itr)))
	  (if (>= n 0)
		  (do ((i 0))
			  ((= i n) nil)
			(declare (type fixnum i))
			(setf node (__rbnode-prev node))
			(incf i))
		  (do ((i 0))
			  ((= i n) nil)
			(declare (type fixnum i))
			(setf node (__rbnode-next node))
			(decf i)))
	  (setf (multimap-rev-itr-node itr) node))
	nil))

(locally (declare (optimize speed))
  (defmethod distance ((itr1 multimap-const-reverse-iterator)
					   (itr2 multimap-const-reverse-iterator))
	(let ((cnt 0))
	  (declare (type fixnum cnt))
	  (do ((node1 (multimap-rev-itr-node itr1))
		   (node2 (multimap-rev-itr-node itr2)))
		  ((eq node1 node2) cnt)
		(incf cnt)
		(setf node1 (__rbnode-prev node1))))))

(defmethod base ((rev-itr multimap-const-reverse-iterator))
  (make-instance 'multimap-const-iterator
				 :node  (__rbnode-next (multimap-rev-itr-node rev-itr))))

;; creating reverse iterator.
(define-constructor reverse-iterator ((itr multimap-const-reverse-iterator))
  (make-instance 'multimap-const-iterator
				 :node (__rbnode-next (multimap-rev-itr-node itr))))


;;------------------------------------------------------------------------------
;;
;; methods for multimap-reverse-iterator 
;;
;;------------------------------------------------------------------------------
(defmethod operator_clone ((itr multimap-reverse-iterator)) 
  (make-instance 'multimap-reverse-iterator :node (multimap-rev-itr-node itr))) 

(defmethod operator_cast ((itr multimap-reverse-iterator)
						  (typename (eql 'multimap-const-reverse-iterator)))
  (__check-exact-type-of-cast itr 'multimap-reverse-iterator
								  'multimap-const-reverse-iterator)
  (make-instance 'multimap-const-reverse-iterator
				 :node (multimap-rev-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr multimap-reverse-iterator)) 
  (__map-check-item-pairness new-val)
  (_= (__rbnode-value (multimap-rev-itr-node itr)) new-val))

(defmethod base ((rev-itr multimap-reverse-iterator))
  (make-instance 'multimap-iterator
				 :node  (__rbnode-next (multimap-rev-itr-node rev-itr))))

;; creating reverse iterator.
(define-constructor reverse-iterator ((itr multimap-reverse-iterator))
  (make-instance 'multimap-iterator
				 :node (__rbnode-next (multimap-rev-itr-node itr))))





;;------------------------------------------------------------------------------
;;
;; debug methods for multimap
;;
;;------------------------------------------------------------------------------
#+cl-stl-debug
(defmethod dump ((container multimap)
				 &optional (stream t) (print-item-fnc nil))
  (format stream "begin dump ---------------------~%")
  (let ((rbtree (multimap-tree container)))
	(__rbtree-dump rbtree stream print-item-fnc))
  (format stream "end dump -----------------------~%")
  nil)

#+cl-stl-debug
(defmethod check-integrity ((container multimap) &optional (stream t))
  (__rbtree-check-integrity (multimap-tree container) stream))

