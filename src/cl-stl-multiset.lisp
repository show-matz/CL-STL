(in-package :cl-stl)

;;------------------------------------------------------------------------------
;;
;; class difinition
;;
;;------------------------------------------------------------------------------
(defclass multiset (bidirectional-container)
  ((rbtree :type     :rbtree
		   :initform nil
		   :initarg  :core
		   :accessor multiset-tree)))

(defclass multiset-const-iterator (bidirectional-iterator)
  ((rbnode :type     :rbnode
		   :initform nil
		   :initarg  :node
		   :accessor multiset-itr-node)))

(defclass multiset-iterator (multiset-const-iterator) ())

(defclass multiset-const-reverse-iterator (bidirectional-iterator)
  ((rbnode :type     :rbnode
		   :initform nil
		   :initarg  :node
		   :accessor multiset-rev-itr-node)))

(defclass multiset-reverse-iterator (multiset-const-reverse-iterator) ()) 

;;--------------------------------------------------------------------
;;
;; internal utilities
;;
;;--------------------------------------------------------------------
(defmacro __multiset-check-iterator-belong (itr cont)
  (declare (ignorable itr cont))
  #-cl-stl-debug nil
  #+cl-stl-debug
  `(unless (_== ,itr (stl:end ,cont))
	 (unless (do* ((tree  (multiset-tree ,cont))
				   (node  (multiset-itr-node ,itr))
				   (node1 (__rbtree-lower-bound tree (__rbnode-value node)))
				   (node2 (__rbtree-upper-bound tree (__rbnode-value node))))
				  ((eq node1 node2) nil)
			   (when (eq node1 node)
				 (return t))
			   (setf node1 (__rbnode-next node1)))
	   (error 'undefined-behavior :what ,(format nil "~A is not iterator of ~A." itr cont)))))

(defmacro __multiset-check-iterator-range (itr1 itr2)
  (declare (ignorable itr1 itr2))
  #-cl-stl-debug nil
  #+cl-stl-debug
  `(unless (__rbnode-check-reachable (multiset-itr-node ,itr1)
									 (multiset-itr-node ,itr2))
	 (error 'undefined-behavior :what ,(format nil "[~A ~A) is not valid range." itr1 itr2))))


;;--------------------------------------------------------------------
;;
;; method implementation
;;
;;--------------------------------------------------------------------
(defun __create-multiset (comp)
  ;; MEMO : comp need copied here.
  (let ((rbtree (__rbtree-create (clone comp))))
	(make-instance 'multiset :core rbtree)))

#-cl-stl-0x98
(locally (declare (optimize speed))
  (defun __create-multiset-with-initlist (il comp)
	;; MEMO : comp need copied here.
	(declare (type initializer-list il))
	(let* ((arr (__initlist-data il))
		   (cnt (length arr))
		   (rbtree (__rbtree-create (clone comp))))
	  (declare (type cl:vector arr))
	  (declare (type fixnum    cnt))
	  (do ((idx 0 (1+ idx)))
		  ((= idx cnt)
		   (make-instance 'multiset :core rbtree))
		(declare (type fixnum idx))
		(__rbtree-insert rbtree (aref arr idx) t t)))))


;;------------------------------------------------------------------------------
;;
;; constructors
;;
;;------------------------------------------------------------------------------
(declare-constructor multiset (0 1 2 3))

; empty constructor 1
(define-constructor multiset ()
  (__create-multiset #'operator_<))

; empty constructor 2
(define-constructor multiset ((arg cl:function))
  (__create-multiset arg))

; empty constructor 3
(define-constructor multiset ((arg #-cl-stl-0x98 functor
								   #+cl-stl-0x98 binary-function))
  (__create-multiset arg))

; copy constructor
(define-constructor multiset ((arg multiset))
  (clone arg))

; constructor with initializer list 1
#-cl-stl-0x98
(define-constructor multiset ((arg initializer-list))
  (__create-multiset-with-initlist arg #'operator_<))

; constructor with initializer list 2
#-cl-stl-0x98
(define-constructor multiset ((arg1 initializer-list) (arg2 cl:function))
  (__create-multiset-with-initlist arg1 arg2))

; constructor with initializer list 3
#-cl-stl-0x98
(define-constructor multiset ((arg1 initializer-list)
							  (arg2 #-cl-stl-0x98 functor
									#+cl-stl-0x98 binary-function))
  (__create-multiset-with-initlist arg1 arg2))

; move constructor
#-cl-stl-0x98
(define-constructor multiset ((arg remove-reference))
  (let ((cont (funcall (__rm-ref-closure arg))))
	(__check-type-of-move-constructor cont multiset)
	(let ((new-set (new stl:multiset (clone (key-comp cont)))))
	  (swap new-set cont)
	  new-set)))

; range constructor
(labels ((__range-ctor-imp (itr1 itr2 comp)
		   (let ((rbtree (__rbtree-create comp)))
			 (with-operators
				 (for (((itr @~itr1)) (_/= itr itr2) ++itr)
				   (__rbtree-insert rbtree *itr t t)))
			 (make-instance 'multiset :core rbtree))))

  (define-constructor multiset ((itr1 input-iterator) (itr2 input-iterator))
	(__range-ctor-imp itr1 itr2 #'operator_<))

  (define-constructor multiset ((itr1 input-iterator) (itr2 input-iterator) (comp cl:function))
	(__range-ctor-imp itr1 itr2 comp))

  (define-constructor multiset ((itr1 input-iterator) (itr2 input-iterator)
								(comp #-cl-stl-0x98 functor
									  #+cl-stl-0x98 binary-function))
	(__range-ctor-imp itr1 itr2 (clone comp))))

; range constructor for const-vector-pointer.
(locally (declare (optimize speed))
  (labels ((__range-ctor-imp (ptr1 ptr2 comp)
			 (__pointer-check-iterator-range ptr1 ptr2)
			 (let ((idx1   (opr::vec-ptr-index  ptr1))
				   (idx2   (opr::vec-ptr-index  ptr2))
				   (buf    (opr::vec-ptr-buffer ptr1))
				   (rbtree (__rbtree-create comp)))
			   (declare (type fixnum idx1 idx2))
			   (declare (type cl:vector buf))
			   (for (nil (< idx1 idx2) (incf idx1))
				 (__rbtree-insert rbtree (aref buf idx1) t t))
			   (make-instance 'multiset :core rbtree))))

	(define-constructor multiset ((itr1 const-vector-pointer) (itr2 const-vector-pointer))
	  (__range-ctor-imp itr1 itr2 #'operator_<))

	(define-constructor multiset ((itr1 const-vector-pointer) (itr2 const-vector-pointer) (comp cl:function))
	  (__range-ctor-imp itr1 itr2 comp))

	(define-constructor multiset ((itr1 const-vector-pointer) (itr2 const-vector-pointer)
								  (comp #-cl-stl-0x98 functor
										#+cl-stl-0x98 binary-function))
	  (__range-ctor-imp itr1 itr2 (clone comp)))))


(defmethod operator_clone ((container multiset))
  ;; MEMO : src-comp need copied here.
  (let* ((src-tree (multiset-tree container))
		 (src-comp (__rbtree-cmp-func src-tree))
		 (rbtree (__rbtree-create (clone src-comp))))
	(__rbtree-for-each src-tree
		(lambda (val)
		  (__rbtree-insert rbtree val t t)))
	(make-instance 'multiset :core rbtree)))


;;------------------------------------------------------------------------------
;;
;; methods
;;
;;------------------------------------------------------------------------------

;-----------------------------------------------------
; assignment
;-----------------------------------------------------
(defmethod operator_= ((cont1 multiset) (cont2 multiset))
  (let ((tree1 (multiset-tree cont1))
		(tree2 (multiset-tree cont2)))
	(__rbtree-clear tree1)
	(__rbtree-for-each tree2
		(lambda (val)
		  (__rbtree-insert tree1 val t t))))
  cont1)

#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod operator_= ((cont multiset) (il initializer-list))
	(declare (type initializer-list il))
	(let* ((arr  (__initlist-data il))
		   (cnt  (length arr))
		   (tree (multiset-tree cont)))
	  (declare (type cl:vector arr))
	  (declare (type fixnum    cnt))
	  (__rbtree-clear tree)
	  (do ((idx 0 (1+ idx)))
		  ((= idx cnt) cont)
		(declare (type fixnum idx))
		(__rbtree-insert tree (aref arr idx) t t)))))

#-cl-stl-0x98
(defmethod operator_move ((cont1 multiset) (cont2 multiset))
  (unless (eq cont1 cont2)
	(let ((tree1 (multiset-tree cont1))
		  (tree2 (multiset-tree cont2)))
	  (__rbtree-clear tree1)
	  (setf (multiset-tree cont1) tree2)
	  (setf (multiset-tree cont2) tree1)))
  (values cont1 cont2))

;-----------------------------------------------------
; iterators
;-----------------------------------------------------
(defmethod begin ((container multiset))
  (make-instance 'multiset-iterator
				 :node (__rbtree-begin (multiset-tree container))))

(defmethod end ((container multiset))
  (make-instance 'multiset-iterator
				 :node (__rbtree-end (multiset-tree container))))

(defmethod rbegin ((container multiset))
  (make-instance 'multiset-reverse-iterator 
				 :node (__rbtree-rbegin (multiset-tree container))))

(defmethod rend ((container multiset))
  (make-instance 'multiset-reverse-iterator 
				 :node (__rbtree-rend (multiset-tree container))))

#-cl-stl-0x98
(defmethod cbegin ((container multiset))
  (make-instance 'multiset-const-iterator
				 :node (__rbtree-begin (multiset-tree container))))

#-cl-stl-0x98
(defmethod cend ((container multiset))
  (make-instance 'multiset-const-iterator
				 :node (__rbtree-end (multiset-tree container))))

#-cl-stl-0x98
(defmethod crbegin ((container multiset))
  (make-instance 'multiset-const-reverse-iterator
				 :node (__rbtree-rbegin (multiset-tree container))))

#-cl-stl-0x98
(defmethod crend ((container multiset))
  (make-instance 'multiset-const-reverse-iterator
				 :node (__rbtree-rend (multiset-tree container))))


;-----------------------------------------------------
; capacity
;-----------------------------------------------------
(defmethod empty ((container multiset))
  (zerop (__rbtree-cur-size (multiset-tree container))))

(defmethod size ((container multiset))
  (__rbtree-cur-size (multiset-tree container)))

(defmethod max-size ((container multiset))
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
(defmethod-overload insert ((container multiset) value)
  (let ((tree (multiset-tree container)))
	(make-instance 'multiset-iterator
				   :node (__rbtree-insert tree value t t))))

;; insert ( single elememt by remove reference ) - returns iterator.
#-cl-stl-0x98
(defmethod-overload insert ((container multiset) (rm remove-reference))
  (let* ((tree (multiset-tree container))
		 (val  (funcall (__rm-ref-closure rm)))
		 (node (__rbtree-insert tree val t nil)))
	(funcall (__rm-ref-closure rm) nil)
	(make-instance 'multiset-iterator :node node)))

;; insert ( single elememt with hint ) - returns iterator.
(defmethod-overload insert ((container multiset)
							(itr #+cl-stl-0x98 multiset-iterator
								 #-cl-stl-0x98 multiset-const-iterator) value)
  (__multiset-check-iterator-belong itr container)
  (let* ((tree (multiset-tree container))
		 (node (__rbtree-insert-with-hint (multiset-itr-node itr) tree value t t)))
	(unless node
	  (setf node (__rbtree-insert tree value t t)))
	(make-instance 'multiset-iterator :node node)))

;; insert ( single elememt with hint by remove reference ) - returns iterator.
#-cl-stl-0x98
(defmethod-overload insert ((container multiset)
							(itr #+cl-stl-0x98 multiset-iterator
								 #-cl-stl-0x98 multiset-const-iterator) (rm remove-reference))
  (__multiset-check-iterator-belong itr container)
  (let* ((tree (multiset-tree container))
		 (val  (funcall (__rm-ref-closure rm)))
		 (node (__rbtree-insert-with-hint (multiset-itr-node itr) tree val t nil)))
	(unless node
	  (setf node (__rbtree-insert tree val t nil)))
	(funcall (__rm-ref-closure rm) nil)
	(make-instance 'multiset-iterator :node node)))

;; range insert - returns nil.
(locally (declare (optimize speed))

  (defmethod-overload insert ((container multiset) (itr1 input-iterator) (itr2 input-iterator))
	(let ((tree (multiset-tree container)))
	  (with-operators
		  (for (((itr @~itr1)) (_/= itr itr2) ++itr)
			(__rbtree-insert tree *itr t t)))))
  
  (defmethod-overload insert ((container multiset)
							  (itr1 multiset-const-iterator) (itr2 multiset-const-iterator))
	(let ((tree (multiset-tree container)))
	  (with-operators
		  (for (((itr @~itr1)) (_/= itr itr2) ++itr)
			(__rbtree-insert tree *itr t t)))))
  
  (defmethod-overload insert ((container multiset)
							  (itr1 const-vector-pointer) (itr2 const-vector-pointer))
	(__pointer-check-iterator-range itr1 itr2)
	(let ((tree (multiset-tree container))
		  (idx1 (opr::vec-ptr-index  itr1))
		  (idx2 (opr::vec-ptr-index  itr2))
		  (buf  (opr::vec-ptr-buffer itr1)))
	  (declare (type fixnum idx1 idx2))
	  (declare (type cl:vector buf))
	  (for (nil (< idx1 idx2) (incf idx1))
		(__rbtree-insert tree (aref buf idx1) t t)))))


;; insert ( initializer list ) - returns nil.
#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod-overload insert ((container multiset) (il initializer-list))
	(declare (type initializer-list il))
	(let* ((arr (__initlist-data il))
		   (cnt (length arr))
		   (tree (multiset-tree container)))
	  (declare (type cl:vector arr))
	  (declare (type fixnum    cnt))
	  (do ((idx 0 (1+ idx)))
		  ((= idx cnt) nil)
		(declare (type fixnum idx))
		(__rbtree-insert tree (aref arr idx) t t)))))

#-cl-stl-0x98    ; emplace
(defmethod-overload emplace ((container multiset) new-val)
  ;;MEMO : returns iterator.
  (let ((tree (multiset-tree container)))
	(make-instance 'multiset-iterator
				   :node (__rbtree-insert tree new-val t nil))))

#-cl-stl-0x98    ; emplace-hint
(defmethod-overload emplace-hint ((container multiset)
								  (itr multiset-const-iterator) new-val)
  ;;MEMO : returns iterator.
  (__multiset-check-iterator-belong itr container)
  (let* ((tree (multiset-tree container))
		 (node (__rbtree-insert-with-hint (multiset-itr-node itr)
										  tree new-val t nil)))
	(unless node
	  (setf node (__rbtree-insert tree new-val t nil)))
	(make-instance 'multiset-iterator :node node)))

; In 0x98, returns nil. In 0x11 returns iterator.
(defmethod-overload erase ((container multiset)
						   (itr #+cl-stl-0x98 multiset-iterator
								#-cl-stl-0x98 multiset-const-iterator))
  (__multiset-check-iterator-belong itr container)
  (let* ((node1 (multiset-itr-node itr))
		 #-cl-stl-0x98
		 (node2 (__rbnode-next node1)))
	(__rbtree-delete-node (multiset-tree container) node1)
	#+cl-stl-0x98 nil
	#-cl-stl-0x98 (make-instance 'multiset-iterator :node node2)))

; In 0x98, returns nil. In 0x11 returns iterator.
(defmethod-overload erase ((container multiset) (first #+cl-stl-0x98 multiset-iterator
													   #-cl-stl-0x98 multiset-const-iterator)
												(last  #+cl-stl-0x98 multiset-iterator
													   #-cl-stl-0x98 multiset-const-iterator))
  (__multiset-check-iterator-belong first container)
  (__multiset-check-iterator-range  first last)
  (let ((tree (multiset-tree container)))
	(with-operators
		(for (((itr @~first)) (_/= itr last) nil)
		  (let ((node (multiset-itr-node itr)))
			++itr
			(__rbtree-delete-node tree node)))))
  #+cl-stl-0x98 nil
  #-cl-stl-0x98 (make-instance 'multiset-iterator :node (multiset-itr-node last)))

; returns deleted node count.
(defmethod-overload erase ((container multiset) key)
  (let ((tree (multiset-tree container)))
	(let ((node1 (__rbtree-lower-bound tree key))
		  (node2 (__rbtree-upper-bound tree key)))
	  (do ((cnt 0 (incf cnt)))
		  ((eq node1 node2) cnt)
		(let ((node node1))
		  (setf node1 (__rbnode-next node1))
		  (__rbtree-delete-node tree node))))))

(defmethod-overload swap ((cont1 multiset) (cont2 multiset))
  (let ((tmp (multiset-tree cont1)))
	(setf (multiset-tree cont1) (multiset-tree cont2))
	(setf (multiset-tree cont2) tmp))
  (values cont1 cont2))

(defmethod clear ((container multiset))
  (__rbtree-clear (multiset-tree container))
  nil)

;-----------------------------------------------------
; specific operations
;-----------------------------------------------------
; returns iterator.
(defmethod-overload find ((container multiset) key)
  (let* ((rbtree (multiset-tree container))
		 (node   (__rbtree-lower-bound rbtree key)))
	(if (__rbnode-is-external node)
		(end container)
		(let ((pred-fnc (__rbtree-cmp-func rbtree)))
		  (if (functor-call pred-fnc key (__rbnode-value node))
			  (end container)
			  (make-instance 'multiset-iterator :node node))))))

; returns fixnum.
(defmethod-overload count ((container multiset) key)
  (__rbtree-count (multiset-tree container) key))

; returns iterator.
(defmethod-overload lower-bound ((container multiset) key)
  (make-instance 'multiset-iterator
				 :node (__rbtree-lower-bound (multiset-tree container) key)))

; returns iterator.
(defmethod-overload upper-bound ((container multiset) key)
  (make-instance 'multiset-iterator
				 :node (__rbtree-upper-bound (multiset-tree container) key)))

; returns pair(itr,itr).
(defmethod-overload equal-range ((container multiset) key)
  (let ((rbtree (multiset-tree container)))
	(make-pair
	 (make-instance 'multiset-iterator
					:node (__rbtree-lower-bound rbtree key))
	 (make-instance 'multiset-iterator
					:node (__rbtree-upper-bound rbtree key)))))


;-----------------------------------------------------
; observers
;-----------------------------------------------------
(defmethod key-comp ((container multiset))
  (__rbtree-cmp-func (multiset-tree container)))

(defmethod value-comp ((container multiset))
  (__rbtree-cmp-func (multiset-tree container)))

;-----------------------------------------------------
; compare
;-----------------------------------------------------
(labels ((__container-equal (cont1 cont2)
		   (if (eq cont1 cont2)
			   t
			   (let ((rbtree1 (multiset-tree cont1))
					 (rbtree2 (multiset-tree cont2)))
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

  (defmethod operator_== ((cont1 multiset) (cont2 multiset))
	(__container-equal cont1 cont2))

  (defmethod operator_/= ((cont1 multiset) (cont2 multiset))
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

  (defmethod operator_< ((cont1 multiset) (cont2 multiset))
	(< (__container-compare (multiset-tree cont1) (multiset-tree cont2)) 0))

  (defmethod operator_<= ((cont1 multiset) (cont2 multiset))
	(<= (__container-compare (multiset-tree cont1) (multiset-tree cont2)) 0))

  (defmethod operator_> ((cont1 multiset) (cont2 multiset))
	(< 0 (__container-compare (multiset-tree cont1) (multiset-tree cont2))))

  (defmethod operator_>= ((cont1 multiset) (cont2 multiset))
	(<= 0 (__container-compare (multiset-tree cont1) (multiset-tree cont2)))))



;-----------------------------------------------------
; enumeration
;-----------------------------------------------------
#-cl-stl-0x98
(defmethod-overload for ((cont multiset) func)
  ;MEMO : func is always lambda function ( see stl:for ). 
  (__rbtree-for-each (multiset-tree cont) func))


;;------------------------------------------------------------------------------
;;
;; methods for multiset-const-iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_= ((itr1 multiset-const-iterator)
					  (itr2 multiset-const-iterator))
  (__error-when-const-removing-assign itr1 multiset-iterator
									  itr2 multiset-const-iterator)
  (setf (multiset-itr-node itr1) (multiset-itr-node itr2))
  itr1)

(defmethod operator_clone ((itr multiset-const-iterator))
  (make-instance 'multiset-const-iterator :node (multiset-itr-node itr)))

(defmethod operator_== ((itr1 multiset-const-iterator) (itr2 multiset-const-iterator))
  (eq (multiset-itr-node itr1) (multiset-itr-node itr2)))

(defmethod operator_/= ((itr1 multiset-const-iterator) (itr2 multiset-const-iterator))
  (not (eq (multiset-itr-node itr1) (multiset-itr-node itr2))))

(defmethod operator_* ((itr multiset-const-iterator))
  (__rbnode-value (multiset-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr multiset-const-iterator))
  (error 'setf-to-const :what "setf to (_* multiset-const-iterator)."))

(defmethod operator_++ ((itr multiset-const-iterator))
  (let ((node (multiset-itr-node itr)))
	(setf (multiset-itr-node itr) (__rbnode-next node)))
  itr)

(defmethod operator_-- ((itr multiset-const-iterator))
  (let ((node (multiset-itr-node itr)))
	(setf (multiset-itr-node itr) (__rbnode-prev node)))
  itr)

(locally (declare (optimize speed))
  (defmethod advance ((itr multiset-const-iterator) (n integer))
	(declare (type fixnum n))
	(let ((node (multiset-itr-node itr)))
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
	  (setf (multiset-itr-node itr) node))
	nil))

(locally (declare (optimize speed))
  (defmethod distance ((itr1 multiset-const-iterator) (itr2 multiset-const-iterator))
	(let ((cnt 0))
	  (declare (type fixnum cnt))
	  (do ((node1 (multiset-itr-node itr1))
		   (node2 (multiset-itr-node itr2)))
		  ((eq node1 node2) cnt)
		(incf cnt)
		(setf node1 (__rbnode-next node1))))))

;; creating reverse iterator.
(define-constructor reverse-iterator ((itr multiset-const-iterator))
  (make-instance 'multiset-const-reverse-iterator
				 :node (__rbnode-prev (multiset-itr-node itr))))


;;------------------------------------------------------------------------------
;;
;; methods for multiset-iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_clone ((itr multiset-iterator))
  (make-instance 'multiset-iterator :node (multiset-itr-node itr)))

(defmethod operator_cast ((itr multiset-iterator)
						  (typename (eql 'multiset-const-iterator)))
  (__check-exact-type-of-cast itr 'multiset-iterator 'multiset-const-iterator)
  (make-instance 'multiset-const-iterator :node (multiset-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr multiset-iterator))
  (_= (__rbnode-value (multiset-itr-node itr)) new-val))

;; creating reverse iterator.
(define-constructor reverse-iterator ((itr multiset-iterator))
  (make-instance 'multiset-reverse-iterator
				 :node (__rbnode-prev (multiset-itr-node itr))))



;;------------------------------------------------------------------------------
;;
;; methods for multiset-const-reverse-iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_= ((itr1 multiset-const-reverse-iterator)
					  (itr2 multiset-const-reverse-iterator))
  (__error-when-const-removing-assign itr1 multiset-reverse-iterator
									  itr2 multiset-const-reverse-iterator)
  (setf (multiset-rev-itr-node itr1) (multiset-rev-itr-node itr2))
  itr1)

(defmethod operator_clone ((itr multiset-const-reverse-iterator))
  (make-instance 'multiset-const-reverse-iterator :node (multiset-rev-itr-node itr)))

(defmethod operator_== ((itr1 multiset-const-reverse-iterator)
				  (itr2 multiset-const-reverse-iterator))
  (eq (multiset-rev-itr-node itr1) (multiset-rev-itr-node itr2)))

(defmethod operator_/= ((itr1 multiset-const-reverse-iterator)
				   (itr2 multiset-const-reverse-iterator))
  (not (eq (multiset-rev-itr-node itr1) (multiset-rev-itr-node itr2))))

(defmethod operator_* ((itr multiset-const-reverse-iterator))
  (__rbnode-value (multiset-rev-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr multiset-const-reverse-iterator))
  (error 'setf-to-const :what "setf to (_* multiset-const-reverse-iterator)."))

(defmethod operator_++ ((itr multiset-const-reverse-iterator))
  (let ((node (multiset-rev-itr-node itr)))
	(setf (multiset-rev-itr-node itr) (__rbnode-prev node)))
  itr)

(defmethod operator_-- ((itr multiset-const-reverse-iterator))
  (let ((node (multiset-rev-itr-node itr)))
	(setf (multiset-rev-itr-node itr) (__rbnode-next node)))
  itr)

(locally (declare (optimize speed))
  (defmethod advance ((itr multiset-const-reverse-iterator) (n integer))
	(declare (type fixnum n))
	(let ((node (multiset-rev-itr-node itr)))
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
	  (setf (multiset-rev-itr-node itr) node))
	nil))

(locally (declare (optimize speed))
  (defmethod distance ((itr1 multiset-const-reverse-iterator)
					   (itr2 multiset-const-reverse-iterator))
	(let ((cnt 0))
	  (declare (type fixnum cnt))
	  (do ((node1 (multiset-rev-itr-node itr1))
		   (node2 (multiset-rev-itr-node itr2)))
		  ((eq node1 node2) cnt)
		(incf cnt)
		(setf node1 (__rbnode-prev node1))))))

(defmethod base ((rev-itr multiset-const-reverse-iterator))
  (make-instance 'multiset-const-iterator
				 :node  (__rbnode-next (multiset-rev-itr-node rev-itr))))

;; creating reverse iterator.
(define-constructor reverse-iterator ((itr multiset-const-reverse-iterator))
  (make-instance 'multiset-const-iterator
				 :node (__rbnode-next (multiset-rev-itr-node itr))))


;;------------------------------------------------------------------------------
;;
;; methods for multiset-reverse-iterator 
;;
;;------------------------------------------------------------------------------
(defmethod operator_clone ((itr multiset-reverse-iterator)) 
  (make-instance 'multiset-reverse-iterator :node (multiset-rev-itr-node itr))) 

(defmethod operator_cast ((itr multiset-reverse-iterator)
						  (typename (eql 'multiset-const-reverse-iterator)))
  (__check-exact-type-of-cast itr 'multiset-reverse-iterator
								  'multiset-const-reverse-iterator)
  (make-instance 'multiset-const-reverse-iterator
				 :node (multiset-rev-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr multiset-reverse-iterator)) 
  (_= (__rbnode-value (multiset-rev-itr-node itr)) new-val))

(defmethod base ((rev-itr multiset-reverse-iterator))
  (make-instance 'multiset-iterator
				 :node  (__rbnode-next (multiset-rev-itr-node rev-itr))))

;; creating reverse iterator.
(define-constructor reverse-iterator ((itr multiset-reverse-iterator))
  (make-instance 'multiset-iterator
				 :node (__rbnode-next (multiset-rev-itr-node itr))))





;;------------------------------------------------------------------------------
;;
;; debug methods for multiset
;;
;;------------------------------------------------------------------------------
#+cl-stl-debug
(defmethod dump ((container multiset)
				 &optional (stream t) (print-item-fnc nil))
  (format stream "begin dump ---------------------~%")
  (let ((rbtree (multiset-tree container)))
	(__rbtree-dump rbtree stream print-item-fnc))
  (format stream "end dump -----------------------~%")
  nil)

#+cl-stl-debug
(defmethod check-integrity ((container multiset) &optional (stream t))
  (__rbtree-check-integrity (multiset-tree container) stream))

