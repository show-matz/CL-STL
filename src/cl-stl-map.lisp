(in-package :cl-stl)

;------------------------------------------------------------
; class __map-valcmp
;------------------------------------------------------------
(defclass __map-valcmp (#-cl-stl-0x98 functor
						#+cl-stl-0x98 binary-function)
  ((keycmp	:initform nil
			:initarg  :keycmp
			:accessor __map-valcmp-keycmp)))

(labels ((__map-valcmp-ctor (keycmp)
           (let* ((keycmp (clone keycmp))
				  (fnc    (functor-function keycmp)))
			 (make-instance '__map-valcmp
							:keycmp keycmp
							:closure (lambda (p1 p2)
									   (funcall fnc (stl:first p1) (stl:first p2)))))))
  (defun __create-map-val-cmp (functor)	
	(__map-valcmp-ctor functor))
  (defmethod operator_clone ((func __map-valcmp))
	(__map-valcmp-ctor (__map-valcmp-keycmp func))))


;;------------------------------------------------------------------------------
;;
;; class difinition
;;
;;------------------------------------------------------------------------------
(defclass map (bidirectional-container)
  ((rbtree :type     :rbtree
		   :initform nil
		   :initarg  :core
		   :accessor map-tree)))

(defclass map-const-iterator (bidirectional-iterator)
  ((rbnode :type     :rbnode
		   :initform nil
		   :initarg  :node
		   :accessor map-itr-node)))

(defclass map-iterator (map-const-iterator) ())

(defclass map-const-reverse-iterator (bidirectional-iterator)
  ((rbnode :type     :rbnode
		   :initform nil
		   :initarg  :node
		   :accessor map-rev-itr-node)))

(defclass map-reverse-iterator (map-const-reverse-iterator) ())

;;--------------------------------------------------------------------
;;
;; internal utilities
;;
;;--------------------------------------------------------------------
(defmacro __map-check-item-pairness (sym)
  `(unless (typep ,sym 'stl:pair)
	 (error 'type-mismatch :what "Item must be pair.")))

(defmacro __map-check-iterator-belong (itr cont)
  (declare (ignorable itr cont))
  #-cl-stl-debug nil
  #+cl-stl-debug
  (let ((g-node (gensym "NODE")))
	`(let ((,g-node (map-itr-node ,itr)))
	   (unless (eq ,g-node (__rbtree-find (map-tree ,cont) (__rbnode-value ,g-node)))
		 (error 'undefined-behavior :what ,(format nil "~A is not iterator of ~A." itr cont))))))

(defmacro __map-check-iterator-range (itr1 itr2)
  (declare (ignorable itr1 itr2))
  #-cl-stl-debug nil
  #+cl-stl-debug
  `(unless (__rbnode-check-reachable (map-itr-node ,itr1) (map-itr-node ,itr2))
	 (error 'undefined-behavior :what ,(format nil "[~A ~A) is not valid range." itr1 itr2))))



;;--------------------------------------------------------------------
;;
;; method implementation
;;
;;--------------------------------------------------------------------
(defun __create-map (key-comp)
  (let ((rbtree (__rbtree-create (__create-map-val-cmp key-comp))))
	(make-instance 'stl:map :core rbtree)))

#-cl-stl-0x98
(locally (declare (optimize speed))
  (defun __create-map-with-initlist (il key-comp)
	(declare (type initializer-list il))
	(let* ((arr (__initlist-data il))
		   (cnt (length arr))
		   (rbtree (__rbtree-create (__create-map-val-cmp key-comp))))
	  (declare (type cl:vector arr))
	  (declare (type fixnum    cnt))
	  (do ((idx 0 (1+ idx)))
		  ((= idx cnt)
		   (make-instance 'stl:map :core rbtree))
		(declare (type fixnum idx))
		(let ((val (aref arr idx)))
		  (__map-check-item-pairness val)
		  (__rbtree-insert rbtree val nil t))))))


;;------------------------------------------------------------------------------
;;
;; constructors
;;
;;------------------------------------------------------------------------------
(declare-constructor map (0 1 2 3))

; empty constructor 1
(define-constructor map ()
  (__create-map #'operator_<))

; empty constructor 2
(define-constructor map ((arg cl:function))
  (__create-map arg))

; empty constructor 3
(define-constructor map ((arg #-cl-stl-0x98 functor
							  #+cl-stl-0x98 binary-function))
  (__create-map arg))

; copy constructor
(define-constructor map ((arg stl:map))
  (clone arg))

; constructor with initializer list 1
#-cl-stl-0x98
(define-constructor map ((arg initializer-list))
  (__create-map-with-initlist arg #'operator_<))

; constructor with initializer list 2
#-cl-stl-0x98
(define-constructor map ((arg1 initializer-list) (arg2 cl:function))
  (__create-map-with-initlist arg1 arg2))

; constructor with initializer list 3
#-cl-stl-0x98
(define-constructor map ((arg1 initializer-list) (arg2 #-cl-stl-0x98 functor
													   #+cl-stl-0x98 binary-function))
  (__create-map-with-initlist arg1 arg2))

; move constructor
#-cl-stl-0x98
(define-constructor map ((arg remove-reference))
  (let ((cont (funcall (__rm-ref-closure arg))))
	(__check-type-of-move-constructor cont stl:map)
	(let ((new-map (new stl:map (clone (key-comp cont)))))
	  (swap new-map cont)
	  new-map)))

;; range constructor
(labels ((__range-ctor-imp (itr1 itr2 key-comp)
		   (let ((rbtree (__rbtree-create (__create-map-val-cmp key-comp))))
			 (with-operators
				 (for (((itr @~itr1)) (_/= itr itr2) ++itr)
				   (let ((val *itr))
					 (__map-check-item-pairness val)
					 (__rbtree-insert rbtree val nil t))))
			 (make-instance 'stl:map :core rbtree))))

  (define-constructor map ((itr1 input-iterator) (itr2 input-iterator))
	(__range-ctor-imp itr1 itr2 #'operator_<))

  (define-constructor map ((itr1 input-iterator) (itr2 input-iterator) (comp cl:function))
	(__range-ctor-imp itr1 itr2 comp))

  (define-constructor map ((itr1 input-iterator) (itr2 input-iterator)
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
				   (__rbtree-insert rbtree val nil t)))
			   (make-instance 'stl:map :core rbtree))))

	(define-constructor map ((itr1 const-vector-pointer) (itr2 const-vector-pointer))
	  (__range-ctor-imp itr1 itr2 #'operator_<))

	(define-constructor map ((itr1 const-vector-pointer) (itr2 const-vector-pointer) (comp cl:function))
	  (__range-ctor-imp itr1 itr2 comp))

	(define-constructor map ((itr1 const-vector-pointer) (itr2 const-vector-pointer)
							 (comp #-cl-stl-0x98 functor
								   #+cl-stl-0x98 binary-function))
	  (__range-ctor-imp itr1 itr2 comp))))


(defmethod operator_clone ((container stl:map))
  (let* ((src-tree (map-tree container))
		 (src-comp (__rbtree-cmp-func src-tree))
		 (rbtree   (__rbtree-create (clone src-comp))))
	(__rbtree-for-each src-tree
		(lambda (val)
		  (__rbtree-insert rbtree val nil t)))
	(make-instance 'stl:map :core rbtree)))


;;------------------------------------------------------------------------------
;;
;; methods
;;
;;------------------------------------------------------------------------------

;-----------------------------------------------------
; assignment
;-----------------------------------------------------
(defmethod operator_= ((cont1 stl:map) (cont2 stl:map))
  (let ((tree1 (map-tree cont1))
		(tree2 (map-tree cont2)))
	(__rbtree-clear tree1)
	(__rbtree-for-each tree2
		(lambda (val)
		  (__rbtree-insert tree1 val nil t))))
  cont1)

#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod operator_= ((cont stl:map) (il initializer-list))
	(declare (type initializer-list il))
	(let* ((arr  (__initlist-data il))
		   (cnt  (length arr))
		   (tree (map-tree cont)))
	  (declare (type cl:vector arr))
	  (declare (type fixnum    cnt))
	  (__rbtree-clear tree)
	  (do ((idx 0 (1+ idx)))
		  ((= idx cnt) cont)
		(declare (type fixnum idx))
		(let ((val (aref arr idx)))
		  (__map-check-item-pairness val)
		  (__rbtree-insert tree val nil t))))))

#-cl-stl-0x98
(defmethod operator_move ((cont1 stl:map) (cont2 stl:map))
  (unless (eq cont1 cont2)
	(let ((tree1 (map-tree cont1))
		  (tree2 (map-tree cont2)))
	  (__rbtree-clear tree1)
	  (setf (map-tree cont1) tree2)
	  (setf (map-tree cont2) tree1)))
  (values cont1 cont2))


;-----------------------------------------------------
; iterators
;-----------------------------------------------------
(defmethod begin ((container stl:map))
  (make-instance 'map-iterator
				 :node (__rbtree-get-first (map-tree container))))

(defmethod end ((container stl:map))
  (make-instance 'map-iterator
				 :node (__rbnode-next (__rbtree-get-end (map-tree container)))))

(defmethod rbegin ((container stl:map))
  (make-instance 'map-reverse-iterator
				 :node (__rbtree-get-end (map-tree container))))

(defmethod rend ((container stl:map))
  (make-instance 'map-reverse-iterator
				 :node (__rbnode-prev (__rbtree-get-first (map-tree container)))))

#-cl-stl-0x98
(defmethod cbegin ((container stl:map))
  (make-instance 'map-const-iterator
				 :node (__rbtree-get-first (map-tree container))))

#-cl-stl-0x98
(defmethod cend ((container stl:map))
  (make-instance 'map-const-iterator
				 :node (__rbnode-next (__rbtree-get-end (map-tree container)))))

#-cl-stl-0x98
(defmethod crbegin ((container stl:map))
  (make-instance 'map-const-reverse-iterator
				 :node (__rbtree-get-end (map-tree container))))

#-cl-stl-0x98
(defmethod crend ((container stl:map))
  (make-instance 'map-const-reverse-iterator
				 :node (__rbnode-prev (__rbtree-get-first (map-tree container)))))


;-----------------------------------------------------
; capacity
;-----------------------------------------------------
(defmethod empty ((container stl:map))
  (zerop (__rbtree-cur-size (map-tree container))))

(defmethod size ((container stl:map))
  (__rbtree-cur-size (map-tree container)))

(defmethod max-size ((container stl:map))
  (declare (ignorable container))
  most-positive-fixnum)

;-----------------------------------------------------
; element access
;-----------------------------------------------------
#-cl-stl-0x98
(defmethod at ((cont stl:map) key)
  (let* ((rbtree   (map-tree cont))
		 (key      (make-pair key nil))
		 (pred-fnc (__rbtree-cmp-func rbtree))
		 (node     (__rbtree-lower-bound rbtree key)))
	(if (or (__rbnode-is-external node)
			(functor-call pred-fnc key (__rbnode-value node)))
		(error 'out-of-range :what "(stl:at stl:map key)")
		(stl:second (__rbnode-value node)))))

#-cl-stl-0x98
(defmethod (setf at) (val (cont stl:map) key)
  (let* ((rbtree   (map-tree cont))
		 (key      (make-pair key nil))
		 (pred-fnc (__rbtree-cmp-func rbtree))
		 (node     (__rbtree-lower-bound rbtree key)))
	(if (or (__rbnode-is-external node)
			(functor-call pred-fnc key (__rbnode-value node)))
		(error 'out-of-range :what "(stl:at stl:map key)")
		(progn
		  (_= (stl:second (__rbnode-value node)) val)
		  val))))

(defmethod operator_[] ((cont stl:map) key)
  (let ((node (__rbtree-insert (map-tree cont)
							   (make-pair key nil) nil nil)))
	(stl:second (__rbnode-value node))))

(defmethod (setf operator_[]) (val (cont stl:map) key)
  (let ((node (__rbtree-insert (map-tree cont)
							   (make-pair key nil) nil nil)))
	(let ((pr (__rbnode-value node)))
	  (_= (stl:second pr) val)
	  (stl:second pr))))

;-----------------------------------------------------
; modifiers
;-----------------------------------------------------
;; insert ( single elememt ) - returns pair<iterator,bool>.
(defmethod-overload insert ((container stl:map) value)
  (let ((tree (map-tree container)))
	(__map-check-item-pairness value)
	(multiple-value-bind (node success)
						 (__rbtree-insert tree value nil t)
	  (make-pair (make-instance 'map-iterator :node node) success))))

;; insert ( single elememt by remove reference ) - returns pair<iterator,bool>.
#-cl-stl-0x98
(defmethod-overload insert ((container stl:map) (rm remove-reference))
  (let ((tree (map-tree container))
		(val  (funcall (__rm-ref-closure rm))))
	(__map-check-item-pairness val)
	(funcall (__rm-ref-closure rm) nil)
	(multiple-value-bind (node success)
						 (__rbtree-insert tree val nil nil)
	  (make-pair (make-instance 'map-iterator :node node) success))))

;; insert ( single elememt with hint ) - returns iterator.
(defmethod-overload insert ((container stl:map)
							(itr #+cl-stl-0x98 map-iterator
								 #-cl-stl-0x98 map-const-iterator) value)
  (__map-check-iterator-belong itr container)
  (__map-check-item-pairness value)
  (let* ((tree (map-tree container))
		 (node (__rbtree-insert-with-hint (map-itr-node itr) tree value nil t)))
	(unless node
	  (setf node (__rbtree-insert tree value nil t)))
	(make-instance 'map-iterator :node node)))

;; insert ( single elememt with hint by remove reference ) - returns iterator.
#-cl-stl-0x98
(defmethod-overload insert ((container stl:map)
							(itr map-const-iterator) (rm remove-reference))
  (__map-check-iterator-belong itr container)
  (let ((tree (map-tree container))
		(val  (funcall (__rm-ref-closure rm))))
	(__map-check-item-pairness val)
	(let ((node (__rbtree-insert-with-hint (map-itr-node itr) tree val nil nil)))
	  (unless node
		(setf node (__rbtree-insert tree val nil nil)))
	  (funcall (__rm-ref-closure rm) nil)
	  (make-instance 'map-iterator :node node))))

;; range insert - returns nil.
(locally (declare (optimize speed))

  (defmethod-overload insert ((container stl:map) (itr1 input-iterator) (itr2 input-iterator))
	(let ((tree (map-tree container)))
	  (with-operators
		  (for (((itr @~itr1)) (_/= itr itr2) ++itr)
			(let ((val *itr))
			  (__map-check-item-pairness val)
			  (__rbtree-insert tree val nil t))))))
  
  (defmethod-overload insert ((container stl:map) (itr1 map-const-iterator) (itr2 map-const-iterator))
	(let ((tree (map-tree container)))
	  (with-operators
		  (for (((itr @~itr1)) (_/= itr itr2) ++itr)
			(__rbtree-insert tree *itr nil t)))))
  
  (defmethod-overload insert ((container stl:map) (itr1 const-vector-pointer) (itr2 const-vector-pointer))
	(__pointer-check-iterator-range itr1 itr2)
	(let ((tree (map-tree container))
		  (idx1 (opr::vec-ptr-index  itr1))
		  (idx2 (opr::vec-ptr-index  itr2))
		  (buf  (opr::vec-ptr-buffer itr1)))
	  (declare (type fixnum idx1 idx2))
	  (declare (type cl:vector buf))
	  (for (nil (< idx1 idx2) (incf idx1))
		(let ((val (aref buf idx1)))
		  (__map-check-item-pairness val)
		  (__rbtree-insert tree val nil t))))))
  
;; insert ( initializer list ) - returns nil.
#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod-overload insert ((container stl:map) (il initializer-list))
	(declare (type initializer-list il))
	(let* ((arr (__initlist-data il))
		   (cnt (length arr))
		   (tree (map-tree container)))
	  (declare (type cl:vector arr))
	  (declare (type fixnum    cnt))
	  (do ((idx 0 (1+ idx)))
		  ((= idx cnt) nil)
		(declare (type fixnum idx))
		(let ((val (aref arr idx)))
		  (__map-check-item-pairness val)
		  (__rbtree-insert tree val nil t))))))

#-cl-stl-0x98    ; emplace
(defmethod-overload emplace ((container stl:map) new-val)
  ;; MEMO : returns pair<iterator,bool>.
  (let ((tree (map-tree container)))
	(__map-check-item-pairness new-val)
	(multiple-value-bind (node success)
						 (__rbtree-insert tree new-val nil nil)
	  (make-pair (make-instance 'map-iterator :node node) success))))

#-cl-stl-0x98    ; emplace-hint
(defmethod-overload emplace-hint ((container stl:map)
								  (itr map-const-iterator) new-val)
  ;; MEMO : returns iterator
  (__map-check-iterator-belong itr container)
  (__map-check-item-pairness new-val)
  (let* ((tree (map-tree container))
		 (node (__rbtree-insert-with-hint (map-itr-node itr)
										  tree new-val nil nil)))
	(unless node
	  (setf node (__rbtree-insert tree new-val nil nil)))
	(make-instance 'map-iterator :node node)))

; In 0x98, returns nil. In 0x11 returns iterator.
(defmethod-overload erase ((container stl:map)
						   (itr #+cl-stl-0x98 map-iterator
								#-cl-stl-0x98 map-const-iterator))
  (__map-check-iterator-belong itr container)
  (let* ((node1 (map-itr-node itr))
		 #-cl-stl-0x98
		 (node2 (__rbnode-next node1)))
	(__rbtree-delete-node (map-tree container) node1)
	#+cl-stl-0x98 nil
	#-cl-stl-0x98 (make-instance 'map-iterator :node node2)))

; In 0x98, returns nil. In 0x11 returns iterator.
(defmethod-overload erase ((container stl:map) (first #+cl-stl-0x98 map-iterator
													  #-cl-stl-0x98 map-const-iterator)
											   (last  #+cl-stl-0x98 map-iterator
													  #-cl-stl-0x98 map-const-iterator))
  (__map-check-iterator-belong first container)
  (__map-check-iterator-range  first last)
  (let ((tree (map-tree container)))
	(with-operators
		(for (((itr @~first)) (_/= itr last) nil)
		  (let ((node (map-itr-node itr)))
			++itr
			(__rbtree-delete-node tree node)))))
  #+cl-stl-0x98 nil
  #-cl-stl-0x98 (make-instance 'map-iterator :node (map-itr-node last)))

; returns deleted node count.
(defmethod-overload erase ((container stl:map) key)
  (let* ((tree (map-tree container))
		 (key  (make-pair key nil))
		 (node (__rbtree-find tree key)))
	(if (__rbnode-is-external node)
		0
		(progn
		  (__rbtree-delete-node tree node)
		  1))))

(defmethod-overload swap ((cont1 stl:map) (cont2 stl:map))
  (let ((tmp-tree (map-tree cont1)))
	(setf (map-tree cont1) (map-tree cont2))
	(setf (map-tree cont2) tmp-tree))
  (values cont1 cont2))

(defmethod clear ((container stl:map))
  (__rbtree-clear (map-tree container))
  nil)

;-----------------------------------------------------
; specific operations
;-----------------------------------------------------
; returns iterator.
(defmethod-overload find ((container stl:map) key)
  (let* ((rbtree (map-tree container))
		 (key    (make-pair key nil))
		 (node   (__rbtree-lower-bound rbtree key)))
	(if (__rbnode-is-external node)
		(end container)
		(let ((pred-fnc (__rbtree-cmp-func rbtree)))
		  (if (functor-call pred-fnc key (__rbnode-value node))
			  (end container)
			  (make-instance 'map-iterator :node node))))))

; returns fixnum.
(defmethod-overload count ((container stl:map) key)
  (__rbtree-count (map-tree container) (make-pair key nil)))

; returns iterator.
(defmethod-overload lower-bound ((container stl:map) key)
  (make-instance 'map-iterator
				 :node (__rbtree-lower-bound (map-tree container)
											 (make-pair key nil))))

; returns iterator.
(defmethod-overload upper-bound ((container stl:map) key)
  (make-instance 'map-iterator
				 :node (__rbtree-upper-bound (map-tree container)
											 (make-pair key nil))))

; returns pair(itr,itr).
(defmethod-overload equal-range ((container stl:map) key)
  (let ((rbtree (map-tree container))
		(key    (make-pair key nil)))
	(make-pair
	 (make-instance 'map-iterator
					:node (__rbtree-lower-bound rbtree key))
	 (make-instance 'map-iterator
					:node (__rbtree-upper-bound rbtree key)))))


;-----------------------------------------------------
; observers
;-----------------------------------------------------
(defmethod key-comp ((container stl:map))
  (__map-valcmp-keycmp (__rbtree-cmp-func (map-tree container))))

(defmethod value-comp ((container stl:map))
  (__rbtree-cmp-func (map-tree container)))

;-----------------------------------------------------
; compare
;-----------------------------------------------------
(labels ((__container-equal (cont1 cont2)
		   (if (eq cont1 cont2)
			   t
			   (let ((rbtree1 (map-tree cont1))
					 (rbtree2 (map-tree cont2)))
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

  (defmethod operator_== ((cont1 stl:map) (cont2 stl:map))
	(__container-equal cont1 cont2))

  (defmethod operator_/= ((cont1 stl:map) (cont2 stl:map))
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

  (defmethod operator_< ((cont1 stl:map) (cont2 stl:map))
	(< (__container-compare (map-tree cont1) (map-tree cont2)) 0))

  (defmethod operator_<= ((cont1 stl:map) (cont2 stl:map))
	(<= (__container-compare (map-tree cont1) (map-tree cont2)) 0))

  (defmethod operator_> ((cont1 stl:map) (cont2 stl:map))
	(< 0 (__container-compare (map-tree cont1) (map-tree cont2))))

  (defmethod operator_>= ((cont1 stl:map) (cont2 stl:map))
	(<= 0 (__container-compare (map-tree cont1) (map-tree cont2)))))


;-----------------------------------------------------
; enumeration
;-----------------------------------------------------
#-cl-stl-0x98
(defmethod-overload for ((cont stl:map) func)
  ;MEMO : func is always lambda function ( see stl:for ). 
  (__rbtree-for-each (map-tree cont) func))


;;------------------------------------------------------------------------------
;;
;; methods for map-const-iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_= ((itr1 map-const-iterator) (itr2 map-const-iterator))
  (__error-when-const-removing-assign itr1 map-iterator
									  itr2 map-const-iterator)
  (setf (map-itr-node itr1) (map-itr-node itr2))
  itr1)

(defmethod operator_clone ((itr map-const-iterator))
  (make-instance 'map-const-iterator :node (map-itr-node itr)))

(defmethod operator_== ((itr1 map-const-iterator) (itr2 map-const-iterator))
  (eq (map-itr-node itr1) (map-itr-node itr2)))

(defmethod operator_/= ((itr1 map-const-iterator) (itr2 map-const-iterator))
  (not (eq (map-itr-node itr1) (map-itr-node itr2))))

(defmethod operator_* ((itr map-const-iterator))
  (__rbnode-value (map-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr map-const-iterator))
  (error 'setf-to-const :what "setf to (_* map-const-iterator)."))

(defmethod operator_++ ((itr map-const-iterator))
  (let ((node (map-itr-node itr)))
	(setf (map-itr-node itr) (__rbnode-next node)))
  itr)

(defmethod operator_-- ((itr map-const-iterator))
  (let ((node (map-itr-node itr)))
	(setf (map-itr-node itr) (__rbnode-prev node)))
  itr)

(locally (declare (optimize speed))
  (defmethod advance ((itr map-const-iterator) (n integer))
	(declare (type fixnum n))
	(let ((node (map-itr-node itr)))
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
	  (setf (map-itr-node itr) node))
	nil))

(locally (declare (optimize speed))
  (defmethod distance ((itr1 map-const-iterator) (itr2 map-const-iterator))
	(let ((cnt 0))
	  (declare (type fixnum cnt))
	  (do ((node1 (map-itr-node itr1))
		   (node2 (map-itr-node itr2)))
		  ((eq node1 node2) cnt)
		(incf cnt)
		(setf node1 (__rbnode-next node1))))))

;; creating reverse iterator.
(define-constructor reverse-iterator ((itr map-const-iterator))
  (make-instance 'map-const-reverse-iterator
				 :node (__rbnode-prev (map-itr-node itr))))


;;------------------------------------------------------------------------------
;;
;; methods for map-iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_clone ((itr map-iterator))
  (make-instance 'map-iterator :node (map-itr-node itr)))

(defmethod operator_cast ((itr map-iterator)
						  (typename (eql 'map-const-iterator)))
  (__check-exact-type-of-cast itr 'map-iterator 'map-const-iterator)
  (make-instance 'map-const-iterator :node (map-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr map-iterator))
  (__map-check-item-pairness new-val)
  (_= (__rbnode-value (map-itr-node itr)) new-val))

;; creating reverse iterator.
(define-constructor reverse-iterator ((itr map-iterator))
  (make-instance 'map-reverse-iterator
				 :node (__rbnode-prev (map-itr-node itr))))



;;------------------------------------------------------------------------------
;;
;; methods for map-const-reverse-iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_= ((itr1 map-const-reverse-iterator)
					  (itr2 map-const-reverse-iterator))
  (__error-when-const-removing-assign itr1 map-reverse-iterator
									  itr2 map-const-reverse-iterator)
  (setf (map-rev-itr-node itr1) (map-rev-itr-node itr2))
  itr1)

(defmethod operator_clone ((itr map-const-reverse-iterator))
  (make-instance 'map-const-reverse-iterator :node (map-rev-itr-node itr)))

(defmethod operator_== ((itr1 map-const-reverse-iterator)
				  (itr2 map-const-reverse-iterator))
  (eq (map-rev-itr-node itr1) (map-rev-itr-node itr2)))

(defmethod operator_/= ((itr1 map-const-reverse-iterator)
				   (itr2 map-const-reverse-iterator))
  (not (eq (map-rev-itr-node itr1) (map-rev-itr-node itr2))))

(defmethod operator_* ((itr map-const-reverse-iterator))
  (__rbnode-value (map-rev-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr map-const-reverse-iterator))
  (error 'setf-to-const :what "setf to (_* map-const-reverse-iterator)."))

(defmethod operator_++ ((itr map-const-reverse-iterator))
  (let ((node (map-rev-itr-node itr)))
	(setf (map-rev-itr-node itr) (__rbnode-prev node)))
  itr)

(defmethod operator_-- ((itr map-const-reverse-iterator))
  (let ((node (map-rev-itr-node itr)))
	(setf (map-rev-itr-node itr) (__rbnode-next node)))
  itr)

(locally (declare (optimize speed))
  (defmethod advance ((itr map-const-reverse-iterator) (n integer))
	(declare (type fixnum n))
	(let ((node (map-rev-itr-node itr)))
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
	  (setf (map-rev-itr-node itr) node))
	nil))

(locally (declare (optimize speed))
  (defmethod distance ((itr1 map-const-reverse-iterator)
					   (itr2 map-const-reverse-iterator))
	(let ((cnt 0))
	  (declare (type fixnum cnt))
	  (do ((node1 (map-rev-itr-node itr1))
		   (node2 (map-rev-itr-node itr2)))
		  ((eq node1 node2) cnt)
		(incf cnt)
		(setf node1 (__rbnode-prev node1))))))

(defmethod base ((rev-itr map-const-reverse-iterator))
  (make-instance 'map-const-iterator
				 :node  (__rbnode-next (map-rev-itr-node rev-itr))))

;; creating reverse iterator.
(define-constructor reverse-iterator ((itr map-const-reverse-iterator))
  (make-instance 'map-const-iterator
				 :node (__rbnode-next (map-rev-itr-node itr))))

;;------------------------------------------------------------------------------
;;
;; methods for map-reverse-iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_clone ((itr map-reverse-iterator))
  (make-instance 'map-reverse-iterator :node (map-rev-itr-node itr)))

(defmethod operator_cast ((itr map-reverse-iterator)
						  (typename (eql 'map-const-reverse-iterator)))
  (__check-exact-type-of-cast itr 'map-reverse-iterator
								  'map-const-reverse-iterator)
  (make-instance 'map-const-reverse-iterator :node (map-rev-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr map-reverse-iterator))
  (__map-check-item-pairness new-val)
  (_= (__rbnode-value (map-rev-itr-node itr)) new-val))

(defmethod base ((rev-itr map-reverse-iterator))
  (make-instance 'map-iterator
				 :node  (__rbnode-next (map-rev-itr-node rev-itr))))

;; creating reverse iterator.
(define-constructor reverse-iterator ((itr map-reverse-iterator))
  (make-instance 'map-iterator
				 :node (__rbnode-next (map-rev-itr-node itr))))







;;------------------------------------------------------------------------------
;;
;; debug methods for stl:map
;;
;;------------------------------------------------------------------------------

#+cl-stl-debug
(defmethod dump ((container stl:map) &optional (stream t) (print-item-fnc nil))
  (format stream "begin dump ---------------------~%")
  (let ((rbtree (map-tree container)))
	(__rbtree-dump rbtree stream print-item-fnc))
  (format stream "end dump -----------------------~%")
  nil)

#+cl-stl-debug
(defmethod check-integrity ((container stl:map) &optional (stream t))
  (__rbtree-check-integrity (map-tree container) stream))

