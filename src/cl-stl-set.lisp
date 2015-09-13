(in-package :cl-stl)

;;------------------------------------------------------------------------------
;;
;; class difinition
;;
;;------------------------------------------------------------------------------
(defclass set (bidirectional-container)
  ((rbtree :type     :rbtree
		   :initform nil
		   :initarg  :core
		   :accessor set-tree)))

(defclass set-const-iterator (bidirectional-iterator)
  ((rbnode :type     :rbnode
		   :initform nil
		   :initarg  :node
		   :accessor set-itr-node)))

(defclass set-iterator (set-const-iterator) ())

(defclass set-const-reverse-iterator (bidirectional-iterator)
  ((rbnode :type     :rbnode
		   :initform nil
		   :initarg  :node
		   :accessor set-rev-itr-node)))

(defclass set-reverse-iterator (set-const-reverse-iterator) ())

;;--------------------------------------------------------------------
;;
;; internal utilities
;;
;;--------------------------------------------------------------------
(defmacro __set-check-iterator-belong (itr cont)
  (declare (ignorable itr cont))
  #-cl-stl-debug nil
  #+cl-stl-debug
  (let ((g-node (gensym "NODE")))
	`(let ((,g-node (set-itr-node ,itr)))
	   (unless (eq ,g-node (__rbtree-find (set-tree ,cont) (__rbnode-value ,g-node)))
		 (error 'undefined-behavior :what ,(format nil "~A is not iterator of ~A." itr cont))))))

(defmacro __set-check-iterator-range (itr1 itr2)
  (declare (ignorable itr1 itr2))
  #-cl-stl-debug nil
  #+cl-stl-debug
  `(unless (__rbnode-check-reachable (set-itr-node ,itr1) (set-itr-node ,itr2))
	 (error 'undefined-behavior :what ,(format nil "[~A ~A) is not valid range." itr1 itr2))))


;;--------------------------------------------------------------------
;;
;; method implementation
;;
;;--------------------------------------------------------------------
(defun __create-set (comp)
  ;; MEMO : comp need copied here.
  (let ((rbtree (__rbtree-create (clone comp))))
	(make-instance 'stl:set :core rbtree)))

#-cl-stl-0x98
(locally (declare (optimize speed))
  (defun __create-set-with-initlist (il comp)
	;; MEMO : comp need copied here.
	(declare (type initializer-list il))
	(let* ((arr (__initlist-data il))
		   (cnt (length arr))
		   (rbtree (__rbtree-create (clone comp))))
	  (declare (type cl:vector arr))
	  (declare (type fixnum cnt))
	  (do ((idx 0 (1+ idx)))
		  ((= idx cnt)
		   (make-instance 'stl:set :core rbtree))
		(declare (type fixnum idx))
		(__rbtree-insert rbtree (aref arr idx) nil t)))))


;;------------------------------------------------------------------------------
;;
;; constructors
;;
;;------------------------------------------------------------------------------
(declare-constructor set (0 1 2 3))

; empty constructor 1
(define-constructor set ()
  (__create-set #'operator_<))

; empty constructor 2
(define-constructor set ((arg cl:function))
  (__create-set arg))

; empty constructor 3
(define-constructor set ((arg #-cl-stl-0x98 functor
							  #+cl-stl-0x98 binary-function))
  (__create-set arg))

; copy constructor
(define-constructor set ((arg stl:set))
  (clone arg))

; constructor with initializer list 1
#-cl-stl-0x98
(define-constructor set ((arg initializer-list))
  (__create-set-with-initlist arg #'operator_<))

; constructor with initializer list 2
#-cl-stl-0x98
(define-constructor set ((arg1 initializer-list) (arg2 cl:function))
  (__create-set-with-initlist arg1 arg2))

; constructor with initializer list 3
#-cl-stl-0x98
(define-constructor set ((arg1 initializer-list)
						 (arg2 #-cl-stl-0x98 functor
							   #+cl-stl-0x98 binary-function))
  (__create-set-with-initlist arg1 arg2))

; move constructor
#-cl-stl-0x98
(define-constructor set ((arg remove-reference))
  (let ((cont (funcall (__rm-ref-closure arg))))
	(__check-type-of-move-constructor cont stl:set)
	(let ((new-set (new stl:set (clone (key-comp cont)))))
	  (swap new-set cont)
	  new-set)))

; range constructor
(labels ((__range-ctor-imp (itr1 itr2 comp)
		   (let ((rbtree (__rbtree-create comp)))
			 (with-operators
				 (for (((itr @~itr1)) (_/= itr itr2) ++itr)
				   (__rbtree-insert rbtree *itr nil t)))
			 (make-instance 'stl:set :core rbtree))))

  (define-constructor set ((itr1 input-iterator) (itr2 input-iterator))
	(__range-ctor-imp itr1 itr2 #'operator_<))

  (define-constructor set ((itr1 input-iterator) (itr2 input-iterator) (comp cl:function))
	(__range-ctor-imp itr1 itr2 comp))

  (define-constructor set ((itr1 input-iterator) (itr2 input-iterator)
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
				 (__rbtree-insert rbtree (aref buf idx1) nil t))
			   (make-instance 'stl:set :core rbtree))))

	(define-constructor set ((itr1 const-vector-pointer) (itr2 const-vector-pointer))
	  (__range-ctor-imp itr1 itr2 #'operator_<))

	(define-constructor set ((itr1 const-vector-pointer) (itr2 const-vector-pointer) (comp cl:function))
	  (__range-ctor-imp itr1 itr2 comp))

	(define-constructor set ((itr1 const-vector-pointer) (itr2 const-vector-pointer)
							 (comp #-cl-stl-0x98 functor
								   #+cl-stl-0x98 binary-function))
	  (__range-ctor-imp itr1 itr2 (clone comp)))))


(defmethod operator_clone ((container stl:set))
  ;; MEMO : src-comp need copied here.
  (let* ((src-tree (set-tree container))
		 (src-comp (__rbtree-cmp-func src-tree))
		 (rbtree (__rbtree-create (clone src-comp))))
	(__rbtree-for-each src-tree
		(lambda (val)
		  (__rbtree-insert rbtree val nil t)))
	(make-instance 'stl:set :core rbtree)))


;;------------------------------------------------------------------------------
;;
;; methods
;;
;;------------------------------------------------------------------------------

;-----------------------------------------------------
; assignment
;-----------------------------------------------------
(defmethod operator_= ((cont1 stl:set) (cont2 stl:set))
  (let ((tree1 (set-tree cont1))
		(tree2 (set-tree cont2)))
	(__rbtree-clear tree1)
	(__rbtree-for-each tree2
		(lambda (val)
		  (__rbtree-insert tree1 val nil t))))
  cont1)

#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod operator_= ((cont stl:set) (il initializer-list))
	(declare (type initializer-list il))
	(let* ((arr  (__initlist-data il))
		   (cnt  (length arr))
		   (tree (set-tree cont)))
	  (declare (type cl:vector arr))
	  (declare (type fixnum cnt))
	  (__rbtree-clear tree)
	  (do ((idx 0 (1+ idx)))
		  ((= idx cnt) cont)
		(declare (type fixnum idx))
		(__rbtree-insert tree (aref arr idx) nil t)))))

#-cl-stl-0x98
(defmethod operator_move ((cont1 stl:set) (cont2 stl:set))
  (unless (eq cont1 cont2)
	(let ((tree1 (set-tree cont1))
		  (tree2 (set-tree cont2)))
	  (__rbtree-clear tree1)
	  (setf (set-tree cont1) tree2)
	  (setf (set-tree cont2) tree1)))
  (values cont1 cont2))
  
  

;-----------------------------------------------------
; iterators
;-----------------------------------------------------
(defmethod begin ((container stl:set))
  (make-instance 'set-iterator
				 :node (__rbtree-get-first (set-tree container))))

(defmethod end ((container stl:set))
  (make-instance 'set-iterator
				 :node (__rbnode-next (__rbtree-get-end (set-tree container)))))

(defmethod rbegin ((container stl:set))
  (make-instance 'set-reverse-iterator
				 :node (__rbtree-get-end (set-tree container))))

(defmethod rend ((container stl:set))
  (make-instance 'set-reverse-iterator
				 :node (__rbnode-prev (__rbtree-get-first (set-tree container)))))

#-cl-stl-0x98
(defmethod cbegin ((container stl:set))
  (make-instance 'set-const-iterator
				 :node (__rbtree-get-first (set-tree container))))

#-cl-stl-0x98
(defmethod cend ((container stl:set))
  (make-instance 'set-const-iterator
				 :node (__rbnode-next (__rbtree-get-end (set-tree container)))))

#-cl-stl-0x98
(defmethod crbegin ((container stl:set))
  (make-instance 'set-const-reverse-iterator
				 :node (__rbtree-get-end (set-tree container))))

#-cl-stl-0x98
(defmethod crend ((container stl:set))
  (make-instance 'set-const-reverse-iterator
				 :node (__rbnode-prev (__rbtree-get-first (set-tree container)))))


;-----------------------------------------------------
; capacity
;-----------------------------------------------------
(defmethod empty ((container stl:set))
  (zerop (__rbtree-cur-size (set-tree container))))

(defmethod size ((container stl:set))
  (__rbtree-cur-size (set-tree container)))

(defmethod max-size ((container stl:set))
  (declare (ignorable container))
  most-positive-fixnum)

;-----------------------------------------------------
; element access
;-----------------------------------------------------
; NONE.

;-----------------------------------------------------
; modifiers
;-----------------------------------------------------
;; insert ( single elememt ) - returns pair<iterator,bool>.
(defmethod-overload insert ((container stl:set) value)
  (let ((tree (set-tree container)))
	(multiple-value-bind (node success)
						 (__rbtree-insert tree value nil t)
	  (make-pair (make-instance 'set-iterator :node node) success))))

;; insert ( single elememt by remove reference ) - returns pair<iterator,bool>.
#-cl-stl-0x98
(defmethod-overload insert ((container stl:set) (rm remove-reference))
  (let ((tree (set-tree container))
		(val  (funcall (__rm-ref-closure rm))))
	(funcall (__rm-ref-closure rm) nil)
	(multiple-value-bind (node success)
						 (__rbtree-insert tree val nil nil)
	  (make-pair (make-instance 'set-iterator :node node) success))))

;; insert ( single elememt with hint ) - returns iterator.
(defmethod-overload insert ((container stl:set)
							   (itr #+cl-stl-0x98 set-iterator
									#-cl-stl-0x98 set-const-iterator) value)
  (__set-check-iterator-belong itr container)
  (let* ((tree (set-tree container))
		 (node (__rbtree-insert-with-hint (set-itr-node itr) tree value nil t)))
	(unless node
	  (setf node (__rbtree-insert tree value nil t)))
	(make-instance 'set-iterator :node node)))

;; insert ( single elememt with hint by remove reference ) - returns iterator.
#-cl-stl-0x98
(defmethod-overload insert ((container stl:set)
							   (itr set-const-iterator) (rm remove-reference))
  (__set-check-iterator-belong itr container)
  (let* ((tree (set-tree container))
		 (val  (funcall (__rm-ref-closure rm)))
		 (node (__rbtree-insert-with-hint (set-itr-node itr) tree val nil nil)))
	(unless node
	  (setf node (__rbtree-insert tree val nil nil)))
	(funcall (__rm-ref-closure rm) nil)
	(make-instance 'set-iterator :node node)))


;; range insert - returns nil.
(locally (declare (optimize speed))

  (defmethod-overload insert ((container stl:set) (itr1 input-iterator) (itr2 input-iterator))
	(let ((tree (set-tree container)))
	  (with-operators
		  (for (((itr @~itr1)) (_/= itr itr2) ++itr)
			(__rbtree-insert tree *itr nil t)))))
  
  (defmethod-overload insert ((container stl:set) (itr1 set-const-iterator) (itr2 set-const-iterator))
	(let ((tree (set-tree container)))
	  (with-operators
		  (for (((itr @~itr1)) (_/= itr itr2) ++itr)
			(__rbtree-insert tree *itr nil t)))))
  
  (defmethod-overload insert ((container stl:set) (itr1 const-vector-pointer) (itr2 const-vector-pointer))
	(__pointer-check-iterator-range itr1 itr2)
	(let ((tree (set-tree container))
		  (idx1 (opr::vec-ptr-index  itr1))
		  (idx2 (opr::vec-ptr-index  itr2))
		  (buf  (opr::vec-ptr-buffer itr1)))
	  (declare (type fixnum idx1 idx2))
	  (declare (type cl:vector buf))
	  (for (nil (< idx1 idx2) (incf idx1))
		(__rbtree-insert tree (aref buf idx1) nil t)))))


;; insert ( initializer list ) - returns nil.
#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod-overload insert ((container stl:set) (il initializer-list))
	(declare (type initializer-list il))
	(let* ((arr (__initlist-data il))
		   (cnt (length arr))
		   (tree (set-tree container)))
	  (declare (type cl:vector arr))
	  (declare (type fixnum cnt))
	  (do ((idx 0 (1+ idx)))
		  ((= idx cnt) nil)
		(declare (type fixnum idx))
		(__rbtree-insert tree (aref arr idx) nil t)))))

#-cl-stl-0x98    ; emplace
(defmethod-overload emplace ((container stl:set) new-val)
  ;;MEMO : returns pair<iterator, bool>.
  (let ((tree (set-tree container)))
	(multiple-value-bind (node success)
						 (__rbtree-insert tree new-val nil nil)
	  (make-pair (make-instance 'set-iterator :node node) success))))

#-cl-stl-0x98    ; emplace-hint
(defmethod-overload emplace-hint ((container stl:set)
									 (itr set-const-iterator) new-val)
  ;;MEMO : returns iterator.
  (__set-check-iterator-belong itr container)
  (let* ((tree (set-tree container))
		 (node (__rbtree-insert-with-hint (set-itr-node itr)
										  tree new-val nil nil)))
	(unless node
	  (setf node (__rbtree-insert tree new-val nil nil)))
	(make-instance 'set-iterator :node node)))

; In 0x98, returns nil. In 0x11 returns iterator.
(defmethod-overload erase ((container stl:set)
							  (itr #+cl-stl-0x98 set-iterator
								   #-cl-stl-0x98 set-const-iterator))
  (__set-check-iterator-belong itr container)
  (let* ((node1 (set-itr-node itr))
		 #-cl-stl-0x98
		 (node2 (__rbnode-next node1)))
	(__rbtree-delete-node (set-tree container) node1)
	#+cl-stl-0x98 nil
	#-cl-stl-0x98 (make-instance 'set-iterator :node node2)))

; In 0x98, returns nil. In 0x11 returns iterator.
(defmethod-overload erase ((container stl:set) (first #+cl-stl-0x98 set-iterator
													  #-cl-stl-0x98 set-const-iterator)
											   (last  #+cl-stl-0x98 set-iterator
													  #-cl-stl-0x98 set-const-iterator))
  (__set-check-iterator-belong first container)
  (__set-check-iterator-range  first last)
  (let ((tree (set-tree container)))
	(with-operators
		(for (((itr @~first)) (_/= itr last) nil)
		  (let ((node (set-itr-node itr)))
			++itr
			(__rbtree-delete-node tree node)))))
  #+cl-stl-0x98 nil
  #-cl-stl-0x98 (make-instance 'set-iterator :node (set-itr-node last)))

; returns deleted node count.
(defmethod-overload erase ((container stl:set) key)
  (let* ((tree (set-tree container))
		 (node (__rbtree-find tree key)))
	(if (__rbnode-is-external node)
		0
		(progn
		  (__rbtree-delete-node tree node)
		  1))))

(defmethod-overload swap ((cont1 stl:set) (cont2 stl:set))
  (let ((tmp (set-tree cont1)))
	(setf (set-tree cont1) (set-tree cont2))
	(setf (set-tree cont2) tmp))
  (values cont1 cont2))

(defmethod clear ((container stl:set))
  (__rbtree-clear (set-tree container))
  nil)

;-----------------------------------------------------
; specific operations
;-----------------------------------------------------
; returns iterator.
(defmethod-overload find ((container stl:set) key)
  (let* ((rbtree (set-tree container))
		 (node   (__rbtree-lower-bound rbtree key)))
	(if (__rbnode-is-external node)
		(end container)
		(let ((pred-fnc (__rbtree-cmp-func rbtree)))
		  (if (functor-call pred-fnc key (__rbnode-value node))
			  (end container)
			  (make-instance 'set-iterator :node node))))))

; returns fixnum.
(defmethod-overload count ((container stl:set) key)
  (__rbtree-count (set-tree container) key))

; returns iterator.
(defmethod-overload lower-bound ((container stl:set) key)
  (make-instance 'set-iterator
				 :node (__rbtree-lower-bound (set-tree container) key)))

; returns iterator.
(defmethod-overload upper-bound ((container stl:set) key)
  (make-instance 'set-iterator
				 :node (__rbtree-upper-bound (set-tree container) key)))

; returns pair(itr,itr).
(defmethod-overload equal-range ((container stl:set) key)
  (let ((rbtree (set-tree container)))
	(make-pair
	 (make-instance 'set-iterator
					:node (__rbtree-lower-bound rbtree key))
	 (make-instance 'set-iterator
					:node (__rbtree-upper-bound rbtree key)))))


;-----------------------------------------------------
; observers
;-----------------------------------------------------
(defmethod key-comp ((container stl:set))
  (__rbtree-cmp-func (set-tree container)))

(defmethod value-comp ((container stl:set))
  (__rbtree-cmp-func (set-tree container)))

;-----------------------------------------------------
; compare
;-----------------------------------------------------
(labels ((__container-equal (cont1 cont2)
		   (if (eq cont1 cont2)
			   t
			   (let ((rbtree1 (set-tree cont1))
					 (rbtree2 (set-tree cont2)))
				 (if (/= (__rbtree-cur-size rbtree1)
						 (__rbtree-cur-size rbtree2))
					 nil
					 (do ((node1 (__rbtree-get-first rbtree1))
						  (last1 (__rbtree-get-end   rbtree1))
						  (node2 (__rbtree-get-first rbtree2)))
						 ((eq node1 last1) t)
					   (unless (_== (__rbnode-value node1) (__rbnode-value node2))
						 (return-from __container-equal nil))
					   (setf node1 (__rbnode-next node1))
					   (setf node2 (__rbnode-next node2))))))))

  (defmethod operator_== ((cont1 stl:set) (cont2 stl:set))
	(__container-equal cont1 cont2))

  (defmethod operator_/= ((cont1 stl:set) (cont2 stl:set))
	(not (__container-equal cont1 cont2))))



(labels ((__container-compare (rbtree1 rbtree2)
		   (let* ((node1 (__rbtree-get-first rbtree1))
				  (node2 (__rbtree-get-first rbtree2))
				  (last1 (__rbtree-get-end   rbtree1))
				  (last2 (__rbtree-get-end   rbtree2)))
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

  (defmethod operator_< ((cont1 stl:set) (cont2 stl:set))
	(< (__container-compare (set-tree cont1) (set-tree cont2)) 0))

  (defmethod operator_<= ((cont1 stl:set) (cont2 stl:set))
	(<= (__container-compare (set-tree cont1) (set-tree cont2)) 0))

  (defmethod operator_> ((cont1 stl:set) (cont2 stl:set))
	(< 0 (__container-compare (set-tree cont1) (set-tree cont2))))

  (defmethod operator_>= ((cont1 stl:set) (cont2 stl:set))
	(<= 0 (__container-compare (set-tree cont1) (set-tree cont2)))))



;-----------------------------------------------------
; enumeration
;-----------------------------------------------------
#-cl-stl-0x98
(defmethod-overload for ((cont stl:set) func)
  ;MEMO : func is always lambda function ( see stl:for ). 
  (__rbtree-for-each (set-tree cont) func))


;;------------------------------------------------------------------------------
;;
;; methods for set-const-iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_= ((itr1 set-const-iterator) (itr2 set-const-iterator))
  (__error-when-const-removing-assign itr1 set-iterator
									  itr2 set-const-iterator)
  (setf (set-itr-node itr1) (set-itr-node itr2))
  itr1)

(defmethod operator_clone ((itr set-const-iterator))
  (make-instance 'set-const-iterator :node (set-itr-node itr)))

(defmethod operator_== ((itr1 set-const-iterator) (itr2 set-const-iterator))
  (eq (set-itr-node itr1) (set-itr-node itr2)))

(defmethod operator_/= ((itr1 set-const-iterator) (itr2 set-const-iterator))
  (not (eq (set-itr-node itr1) (set-itr-node itr2))))

(defmethod operator_* ((itr set-const-iterator))
  (__rbnode-value (set-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr set-const-iterator))
  (error 'setf-to-const :what "setf to (_* set-const-iterator)."))

(defmethod operator_++ ((itr set-const-iterator))
  (let ((node (set-itr-node itr)))
	(setf (set-itr-node itr) (__rbnode-next node)))
  itr)

(defmethod operator_-- ((itr set-const-iterator))
  (let ((node (set-itr-node itr)))
	(setf (set-itr-node itr) (__rbnode-prev node)))
  itr)

(locally (declare (optimize speed))
  (defmethod advance ((itr set-const-iterator) (n integer))
	(declare (type fixnum n))
	(let ((node (set-itr-node itr)))
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
	  (setf (set-itr-node itr) node))
	nil))

(locally (declare (optimize speed))
  (defmethod distance ((itr1 set-const-iterator) (itr2 set-const-iterator))
	(let ((cnt 0))
	  (declare (type fixnum cnt))
	  (do ((node1 (set-itr-node itr1))
		   (node2 (set-itr-node itr2)))
		  ((eq node1 node2) cnt)
		(incf cnt)
		(setf node1 (__rbnode-next node1))))))

;; creating reverse iterator.
(define-constructor reverse-iterator ((itr set-const-iterator))
  (make-instance 'set-const-reverse-iterator
				 :node (__rbnode-prev (set-itr-node itr))))


;;------------------------------------------------------------------------------
;;
;; methods for set-iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_clone ((itr set-iterator))
  (make-instance 'set-iterator :node (set-itr-node itr)))

(defmethod operator_cast ((itr set-iterator)
						  (typename (eql 'set-const-iterator)))
  (__check-exact-type-of-cast itr 'set-iterator 'set-const-iterator)
  (make-instance 'set-const-iterator :node (set-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr set-iterator))
  (_= (__rbnode-value (set-itr-node itr)) new-val))

;; creating reverse iterator.
(define-constructor reverse-iterator ((itr set-iterator))
  (make-instance 'set-reverse-iterator
				 :node (__rbnode-prev (set-itr-node itr))))



;;------------------------------------------------------------------------------
;;
;; methods for set-const-reverse-iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_= ((itr1 set-const-reverse-iterator)
					  (itr2 set-const-reverse-iterator))
  (__error-when-const-removing-assign itr1 set-reverse-iterator
									  itr2 set-const-reverse-iterator)
  (setf (set-rev-itr-node itr1) (set-rev-itr-node itr2))
  itr1)

(defmethod operator_clone ((itr set-const-reverse-iterator))
  (make-instance 'set-const-reverse-iterator :node (set-rev-itr-node itr)))

(defmethod operator_== ((itr1 set-const-reverse-iterator)
				  (itr2 set-const-reverse-iterator))
  (eq (set-rev-itr-node itr1) (set-rev-itr-node itr2)))

(defmethod operator_/= ((itr1 set-const-reverse-iterator)
				   (itr2 set-const-reverse-iterator))
  (not (eq (set-rev-itr-node itr1) (set-rev-itr-node itr2))))

(defmethod operator_* ((itr set-const-reverse-iterator))
  (__rbnode-value (set-rev-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr set-const-reverse-iterator))
  (error 'setf-to-const :what "setf to (_* set-const-reverse-iterator)."))

(defmethod operator_++ ((itr set-const-reverse-iterator))
  (let ((node (set-rev-itr-node itr)))
	(setf (set-rev-itr-node itr) (__rbnode-prev node)))
  itr)

(defmethod operator_-- ((itr set-const-reverse-iterator))
  (let ((node (set-rev-itr-node itr)))
	(setf (set-rev-itr-node itr) (__rbnode-next node)))
  itr)

(locally (declare (optimize speed))
  (defmethod advance ((itr set-const-reverse-iterator) (n integer))
	(declare (type fixnum n))
	(let ((node (set-rev-itr-node itr)))
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
	  (setf (set-rev-itr-node itr) node))
	nil))

(locally (declare (optimize speed))
  (defmethod distance ((itr1 set-const-reverse-iterator)
					   (itr2 set-const-reverse-iterator))
	(let ((cnt 0))
	  (declare (type fixnum cnt))
	  (do ((node1 (set-rev-itr-node itr1))
		   (node2 (set-rev-itr-node itr2)))
		  ((eq node1 node2) cnt)
		(incf cnt)
		(setf node1 (__rbnode-prev node1))))))

(defmethod base ((rev-itr set-const-reverse-iterator))
  (make-instance 'set-const-iterator
				 :node  (__rbnode-next (set-rev-itr-node rev-itr))))

;; creating reverse iterator.
(define-constructor reverse-iterator ((itr set-const-reverse-iterator))
  (make-instance 'set-const-iterator
				 :node (__rbnode-next (set-rev-itr-node itr))))


;;------------------------------------------------------------------------------
;;
;; methods for set-reverse-iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_clone ((itr set-reverse-iterator))
  (make-instance 'set-reverse-iterator :node (set-rev-itr-node itr)))

(defmethod operator_cast ((itr set-reverse-iterator)
						  (typename (eql 'set-const-reverse-iterator)))
  (__check-exact-type-of-cast itr 'set-reverse-iterator
								  'set-const-reverse-iterator)
  (make-instance 'set-const-reverse-iterator :node (set-rev-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr set-reverse-iterator))
  (_= (__rbnode-value (set-rev-itr-node itr)) new-val))

(defmethod base ((rev-itr set-reverse-iterator))
  (make-instance 'set-iterator
				 :node  (__rbnode-next (set-rev-itr-node rev-itr))))

;; creating reverse iterator.
(define-constructor reverse-iterator ((itr set-reverse-iterator))
  (make-instance 'set-iterator
				 :node (__rbnode-next (set-rev-itr-node itr))))





;;------------------------------------------------------------------------------
;;
;; debug methods for stl:set
;;
;;------------------------------------------------------------------------------
#+cl-stl-debug
(defmethod dump ((container stl:set) &optional (stream t) (print-item-fnc nil))
  (format stream "begin dump ---------------------~%")
  (let ((rbtree (set-tree container)))
	(__rbtree-dump rbtree stream print-item-fnc))
  (format stream "end dump -----------------------~%")
  nil)

#+cl-stl-debug
(defmethod check-integrity ((container stl:set) &optional (stream t))
  (__rbtree-check-integrity (set-tree container) stream))

