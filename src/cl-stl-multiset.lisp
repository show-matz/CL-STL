(in-package :cl-stl)

;;------------------------------------------------------------------------------
;;
;; class difinition
;;
;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  (defclass multiset (bidirectional-container)
	((rbtree :type     :rbtree
			 :initform nil
			 :initarg  :core
			 :accessor __assoc-tree)
;TMP;	 #+cl-stl-debug
;TMP;	 (id     :type     :symbol
;TMP;			 :initform (gensym "MULTISET-")
;TMP;			 :initarg  :id
;TMP;			 :accessor __instance-id)
	))

  (defclass multiset-const-iterator (bidirectional-iterator)
	((node :type     :rbnode
		   :initform nil
		   :initarg  :node
		   :accessor __assoc-itr-node)))

  (defclass multiset-const-reverse-iterator (bidirectional-iterator)
	((node :type     :rbnode
		   :initform nil
		   :initarg  :node
		   :accessor __assoc-rev-itr-node)))

  (defclass multiset-iterator (multiset-const-iterator) ())
  (defclass multiset-reverse-iterator (multiset-const-reverse-iterator) ()))


;;--------------------------------------------------------------------
;;
;; internal utilities
;;
;;--------------------------------------------------------------------
(defmacro __multiset-check-iterator-belong (itr cont)
  (declare (ignore itr cont))
  nil)    ;ToDo : temporary...
;;  #-cl-stl-debug nil
;;  #+cl-stl-debug
;;  `(unless (_== ,itr (stl:end ,cont))
;;	 (unless (do* ((tree  (__assoc-tree ,cont))
;;				   (node  (multiset-itr-node ,itr))
;;				   (node1 (__rbtree-lower-bound tree (__rbnode-value node)))
;;				   (node2 (__rbtree-upper-bound tree (__rbnode-value node))))
;;				  ((eq node1 node2) nil)
;;			   (when (eq node1 node)
;;				 (return t))
;;			   (setf node1 (__rbnode-next node1)))
;;	   (error 'undefined-behavior :what ,(format nil "~A is not iterator of ~A." itr cont)))))

;;YET : (defmacro __multiset-check-iterator-range (itr1 itr2)
;;YET :   (declare (ignorable itr1 itr2))
;;YET :   #-cl-stl-debug nil
;;YET :   #+cl-stl-debug
;;YET :   `(unless (__rbnode-check-reachable (multiset-itr-node ,itr1)
;;YET : 									 (multiset-itr-node ,itr2))
;;YET : 	 (error 'undefined-behavior :what ,(format nil "[~A ~A) is not valid range." itr1 itr2))))


;;--------------------------------------------------------------------
;;
;; method implementation
;;
;;--------------------------------------------------------------------
(locally (declare (optimize speed))

  (defun __create-multiset (comp)
	;; MEMO : comp copy in __rbtree-ctor.
	(let ((tree (__rbtree-ctor comp #'identity)))
	  (make-instance 'multiset :core tree)))

  (defun __create-multiset-with-range (comp itr1 itr2)
	;; MEMO : comp copy in __rbtree-ctor.
	(let ((tree (__rbtree-ctor comp #'identity)))
	  (__rbtree-insert-range-equal tree itr1 itr2 t)
	  (make-instance 'multiset :core tree)))

  (defun __create-multiset-with-array (comp arr idx1 idx2)
	;; MEMO : comp copy in __rbtree-ctor.
	(let ((tree (__rbtree-ctor comp #'identity)))
	  (__rbtree-insert-array-equal tree arr idx1 idx2 t)
	  (make-instance 'multiset :core tree))))


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
(define-constructor multiset ((comp cl:function))
  (__create-multiset comp))

; empty constructor 3
(define-constructor multiset ((comp #-cl-stl-0x98 functor
									#+cl-stl-0x98 binary-function))
  (__create-multiset comp))

; copy constructor
(define-constructor multiset ((obj multiset))
  (clone obj))

; constructor with initializer list 1
#-cl-stl-0x98
(define-constructor multiset ((il initializer-list))
  (declare (type initializer-list il))
  (let ((arr (__initlist-data il)))
	(__create-multiset-with-array #'operator_< arr 0 (length arr))))

; constructor with initializer list 2
#-cl-stl-0x98
(define-constructor multiset ((il initializer-list) (comp cl:function))
  (declare (type initializer-list il))
  (let ((arr (__initlist-data il)))
	(__create-multiset-with-array comp arr 0 (length arr))))

; constructor with initializer list 3
#-cl-stl-0x98
(define-constructor multiset ((il   initializer-list)
							   (comp #-cl-stl-0x98 functor
									 #+cl-stl-0x98 binary-function))
  (declare (type initializer-list il))
  (let ((arr (__initlist-data il)))
	(__create-multiset-with-array comp arr 0 (length arr))))

; move constructor
#-cl-stl-0x98
(define-constructor multiset ((arg remove-reference))
  (let ((cont (funcall (__rm-ref-closure arg))))
	(__check-type-of-move-constructor cont multiset)
	(let ((obj (__create-multiset (key-comp cont))))
	  (__rbtree-swap (__assoc-tree obj) (__assoc-tree cont))
	  obj)))

; range constructor
(define-constructor multiset ((itr1 input-iterator) (itr2 input-iterator))
  (__create-multiset-with-range #'operator_< itr1 itr2))

(define-constructor multiset ((itr1 input-iterator)
							  (itr2 input-iterator) (comp cl:function))
  (__create-multiset-with-range comp itr1 itr2))

(define-constructor multiset ((itr1 input-iterator)
							  (itr2 input-iterator) (comp #-cl-stl-0x98 functor
														  #+cl-stl-0x98 binary-function))
  (__create-multiset-with-range comp itr1 itr2))

; range constructor for const-vector-pointer.
(define-constructor multiset ((ptr1 const-vector-pointer) (ptr2 const-vector-pointer))
  (__pointer-check-iterator-range ptr1 ptr2)
  (__create-multiset-with-array #'operator_<
								 (opr::vec-ptr-buffer ptr1)
								 (opr::vec-ptr-index  ptr1)
								 (opr::vec-ptr-index  ptr2)))

(define-constructor multiset ((ptr1 const-vector-pointer)
							  (ptr2 const-vector-pointer) (comp cl:function))
  (__pointer-check-iterator-range ptr1 ptr2)
  (__create-multiset-with-array comp
								 (opr::vec-ptr-buffer ptr1)
								 (opr::vec-ptr-index  ptr1)
								 (opr::vec-ptr-index  ptr2)))

(define-constructor multiset ((ptr1 const-vector-pointer)
							  (ptr2 const-vector-pointer) (comp #-cl-stl-0x98 functor
																#+cl-stl-0x98 binary-function))
  (__pointer-check-iterator-range ptr1 ptr2)
  (__create-multiset-with-array comp
								 (opr::vec-ptr-buffer ptr1)
								 (opr::vec-ptr-index  ptr1)
								 (opr::vec-ptr-index  ptr2)))


(defmethod operator_clone ((container multiset))
  (make-instance 'multiset
				 :core (__rbtree-copy-ctor (__assoc-tree container))))


;;------------------------------------------------------------------------------
;;
;; methods
;;
;;------------------------------------------------------------------------------

;;----------------------------------------------------------
;; assignment
;;----------------------------------------------------------
(locally (declare (optimize speed))
  
  (defmethod operator_= ((cont1 multiset) (cont2 multiset))
	(__rbtree-assign (__assoc-tree cont1) (__assoc-tree cont2))
	cont1)

  #-cl-stl-0x98
  (defmethod operator_move ((cont1 multiset) (cont2 multiset))
	(unless (eq cont1 cont2)
	  (let ((tree1 (__assoc-tree cont1))
			(tree2 (__assoc-tree cont2)))
		(__rbtree-clear tree1)
		(__rbtree-swap tree1 tree2)
		(setf (__rbtree-key-comp tree2) (clone (__rbtree-key-comp tree1)))))
	(values cont1 cont2))

  #-cl-stl-0x98
  (defmethod operator_= ((cont multiset) (il initializer-list))
	(declare (type initializer-list il))
	(let ((tree (__assoc-tree cont))
		  (arr  (__initlist-data il)))
	  (declare (type rbtree tree))
	  (declare (type cl:vector arr))
	  (__rbtree-clear tree)
	  (__rbtree-insert-array-equal tree arr 0 (length arr) t))
	cont))

;;----------------------------------------------------------
;; iterators
;;----------------------------------------------------------
(locally (declare (optimize speed))

  (defmethod begin ((container multiset))
	(make-instance 'multiset-iterator
				   :node (__rbtree-begin (__assoc-tree container))))

  (defmethod end ((container multiset))
	(make-instance 'multiset-iterator
				   :node (__rbtree-end (__assoc-tree container))))

  (defmethod rbegin ((container multiset))
	(make-instance 'multiset-reverse-iterator 
				   :node (__rbtree-rbegin (__assoc-tree container))))

  (defmethod rend ((container multiset))
	(make-instance 'multiset-reverse-iterator 
				   :node (__rbtree-rend (__assoc-tree container))))

  #-cl-stl-0x98
  (defmethod cbegin ((container multiset))
	(make-instance 'multiset-const-iterator
				   :node (__rbtree-begin (__assoc-tree container))))

  #-cl-stl-0x98
  (defmethod cend ((container multiset))
	(make-instance 'multiset-const-iterator
				   :node (__rbtree-end (__assoc-tree container))))

  #-cl-stl-0x98
  (defmethod crbegin ((container multiset))
	(make-instance 'multiset-const-reverse-iterator
				   :node (__rbtree-rbegin (__assoc-tree container))))

  #-cl-stl-0x98
  (defmethod crend ((container multiset))
	(make-instance 'multiset-const-reverse-iterator
				   :node (__rbtree-rend (__assoc-tree container)))))


;;----------------------------------------------------------
;; capacity
;;----------------------------------------------------------
(locally (declare (optimize speed))

  (defmethod empty ((container multiset))
	(zerop (__rbtree-size (__assoc-tree container))))

  (defmethod size ((container multiset))
	(__rbtree-size (__assoc-tree container)))

  (defmethod max-size ((container multiset))
	(__rbtree-max-size (__assoc-tree container))))


;;----------------------------------------------------------
;; element access
;;----------------------------------------------------------
; NONE.

;;----------------------------------------------------------
;; modifiers
;;----------------------------------------------------------
(locally (declare (optimize speed))

  ;; insert ( single element ) - returns iterator.
  (defmethod-overload insert ((container multiset) value)
	(make-instance 'multiset-iterator
				   :node (__rbtree-insert-equal (__assoc-tree container) value t)))

  ;; insert ( single element by remove reference ) - returns iterator.
  #-cl-stl-0x98
  (defmethod-overload insert ((container multiset) (rm remove-reference))
	(let ((val (funcall (__rm-ref-closure rm))))
	  (funcall (__rm-ref-closure rm) nil)
	  (make-instance 'multiset-iterator
					 :node (__rbtree-insert-equal (__assoc-tree container) val nil))))

  ;; insert ( single element with hint ) - returns iterator.
  (defmethod-overload insert ((container multiset)
							  (itr #+cl-stl-0x98 multiset-iterator
								   #-cl-stl-0x98 multiset-const-iterator) value)
	#+cl-stl-0x98  ;; HACK
	(when (and (typep itr   'multiset-const-iterator)
			   (typep value 'multiset-const-iterator))
	  (__rbtree-insert-range-equal (__assoc-tree container) itr value t)
	  (return-from __insert-3 nil))
	
	(__multiset-check-iterator-belong itr container)
	(make-instance 'multiset-iterator
				   :node (__rbtree-insert-hint-equal (__assoc-tree container)
													 (__assoc-itr-node itr) value t)))

  ;; insert ( single element with hint by remove reference ) - returns iterator.
  #-cl-stl-0x98
  (defmethod-overload insert ((container multiset)
							  (itr multiset-const-iterator) (rm remove-reference))
	(__multiset-check-iterator-belong itr container)
	(let ((val (funcall (__rm-ref-closure rm))))
	  (funcall (__rm-ref-closure rm) nil)
	  (make-instance 'multiset-iterator
					 :node (__rbtree-insert-hint-equal (__assoc-tree container)
													   (__assoc-itr-node itr) val nil))))

  ;; insert ( initializer list ) - returns nil.
  #-cl-stl-0x98
  (defmethod-overload insert ((container multiset) (il initializer-list))
	(declare (type initializer-list il))
	(let ((arr (__initlist-data il)))
	  (__rbtree-insert-array-equal (__assoc-tree container) arr 0 (length arr) t)
	  nil)))

;; range insert - returns nil.
(locally (declare (optimize speed))

  (defmethod-overload insert ((container multiset) (itr1 input-iterator) (itr2 input-iterator))
	(__rbtree-insert-range-equal (__assoc-tree container) itr1 itr2 t)
	nil)

  (defmethod-overload insert ((container multiset)
							  (itr1 multiset-const-iterator) (itr2 multiset-const-iterator))
	(__rbtree-insert-range-equal (__assoc-tree container) itr1 itr2 t)
	nil)

  (defmethod-overload insert ((container multiset)
							  (ptr1 const-vector-pointer) (ptr2 const-vector-pointer))
	(__pointer-check-iterator-range ptr1 ptr2)
	(let ((arr (opr::vec-ptr-buffer ptr1)))
	  (declare (type cl:vector arr))
	  (__rbtree-insert-array-equal (__assoc-tree container) arr 0 (length arr) t)
	nil)))

;; emplace
#-cl-stl-0x98
(locally (declare (optimize speed))

  ;;returns iterator.
  (defmethod-overload emplace ((container multiset) new-val)
	(make-instance 'multiset-iterator
				   :node (__rbtree-emplace-equal (__assoc-tree container) new-val)))

  ;;returns iterator.
  (defmethod-overload emplace-hint ((container multiset)
									(itr multiset-const-iterator) new-val)
	(__multiset-check-iterator-belong itr container)
	(make-instance 'multiset-iterator
				   :node (__rbtree-emplace-hint-equal (__assoc-tree container)
													  (__assoc-itr-node itr) new-val))))


;;erase
(locally (declare (optimize speed))

  ;; In 0x98, returns nil. In 0x11 returns iterator.
  (defmethod-overload erase ((container multiset)
							 (itr #+cl-stl-0x98 multiset-iterator
								  #-cl-stl-0x98 multiset-const-iterator))
	(__multiset-check-iterator-belong itr container)
	(let ((node (__rbtree-erase-node (__assoc-tree container) (__assoc-itr-node itr))))
	  (declare (ignorable node))
	  #+cl-stl-0x98 nil
	  #-cl-stl-0x98 (make-instance 'multiset-iterator :node node)))

  ;; In 0x98, returns nil. In 0x11 returns iterator.
  (defmethod-overload erase ((container multiset)
							 (first #+cl-stl-0x98 multiset-iterator #-cl-stl-0x98 multiset-const-iterator)
							 (last  #+cl-stl-0x98 multiset-iterator #-cl-stl-0x98 multiset-const-iterator))
	(__multiset-check-iterator-belong first container)
	;;(__multiset-check-iterator-range  first last)         ;ToDo :
	(let ((node (__rbtree-erase-range (__assoc-tree container)
									  (__assoc-itr-node first) (__assoc-itr-node last))))
	  (declare (ignorable node))
	  #+cl-stl-0x98 nil
	  #-cl-stl-0x98 (make-instance 'multiset-iterator :node node)))

  ;; returns deleted node count.
  (defmethod-overload erase ((container multiset) key)
	(__rbtree-erase-key (__assoc-tree container) key)))



(defmethod-overload swap ((cont1 multiset) (cont2 multiset))
  (__rbtree-swap (__assoc-tree cont1) (__assoc-tree cont2))
  (values cont1 cont2))

(defmethod clear ((container multiset))
  (__rbtree-clear (__assoc-tree container))
  nil)

;;----------------------------------------------------------
;; specific operations
;;----------------------------------------------------------
(locally (declare (optimize speed))

  ;; returns iterator.
  (defmethod-overload find ((container multiset) key)
	(make-instance 'multiset-iterator
				   :node (__rbtree-find (__assoc-tree container) key)))

  ;; returns fixnum.
  (defmethod-overload count ((container multiset) key)
	(__rbtree-count (__assoc-tree container) key))

  ;; returns iterator.
  (defmethod-overload lower-bound ((container multiset) key)
	(make-instance 'multiset-iterator
				   :node (__rbtree-lower-bound (__assoc-tree container) key)))

  ;; returns iterator.
  (defmethod-overload upper-bound ((container multiset) key)
	(make-instance 'multiset-iterator
				   :node (__rbtree-upper-bound (__assoc-tree container) key)))

  ;; returns pair(itr,itr).
  (defmethod-overload equal-range ((container multiset) key)
	(let ((tree (__assoc-tree container)))
	  (make-pair (make-instance 'multiset-iterator
								:node (__rbtree-lower-bound tree key))
				 (make-instance 'multiset-iterator
								:node (__rbtree-upper-bound tree key))))))


;;----------------------------------------------------------
;; observers
;;----------------------------------------------------------
(locally (declare (optimize speed))
  
  (defmethod key-comp ((container multiset))
	(clone (__rbtree-key-comp (__assoc-tree container))))

  (defmethod value-comp ((container multiset))
	(clone (__rbtree-key-comp (__assoc-tree container)))))


;;----------------------------------------------------------
;; compare
;;----------------------------------------------------------
(locally (declare (optimize speed))

  (defmethod operator_== ((cont1 multiset) (cont2 multiset))
	(__rbtree-equal (__assoc-tree cont1) (__assoc-tree cont2) #'operator_==))

  (defmethod operator_/= ((cont1 multiset) (cont2 multiset))
	(not (__rbtree-equal (__assoc-tree cont1) (__assoc-tree cont2) #'operator_==)))

  (defmethod operator_< ((cont1 multiset) (cont2 multiset))
	(__rbtree-less (__assoc-tree cont1) (__assoc-tree cont2) #'operator_<))

  (defmethod operator_<= ((cont1 multiset) (cont2 multiset))
	(not (__rbtree-less (__assoc-tree cont2) (__assoc-tree cont1) #'operator_<)))

  (defmethod operator_> ((cont1 multiset) (cont2 multiset))
	(__rbtree-less (__assoc-tree cont2) (__assoc-tree cont1) #'operator_<))

  (defmethod operator_>= ((cont1 multiset) (cont2 multiset))
	(not (__rbtree-less (__assoc-tree cont1) (__assoc-tree cont2) #'operator_<))))



;;----------------------------------------------------------
;; enumeration
;;----------------------------------------------------------
#-cl-stl-0x98
(defmethod-overload for ((cont multiset) func)
  ;MEMO : func is always lambda function ( see stl:for ). 
  (__rbtree-enumerate (__assoc-tree cont) func))


;;------------------------------------------------------------------------------
;;
;; methods for multiset-const-iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_= ((itr1 multiset-const-iterator) (itr2 multiset-const-iterator))
  (__error-when-const-removing-assign itr1 multiset-iterator
									  itr2 multiset-const-iterator)
  (setf (__assoc-itr-node itr1) (__assoc-itr-node itr2))
  itr1)

(defmethod operator_clone ((itr multiset-const-iterator))
  (make-instance 'multiset-const-iterator :node (__assoc-itr-node itr)))

(defmethod operator_== ((itr1 multiset-const-iterator) (itr2 multiset-const-iterator))
  (eq (__assoc-itr-node itr1) (__assoc-itr-node itr2)))

(defmethod operator_/= ((itr1 multiset-const-iterator) (itr2 multiset-const-iterator))
  (not (eq (__assoc-itr-node itr1) (__assoc-itr-node itr2))))

(defmethod operator_* ((itr multiset-const-iterator))
  (__rbnode-value (__assoc-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr multiset-const-iterator))
  (error 'setf-to-const :what "setf to (_* multiset-const-iterator)."))

(defmethod operator_++ ((itr multiset-const-iterator))
  (setf (__assoc-itr-node itr) (__rbnode-increment (__assoc-itr-node itr)))
  itr)

(defmethod operator_-- ((itr multiset-const-iterator))
  (setf (__assoc-itr-node itr) (__rbnode-decrement (__assoc-itr-node itr)))
  itr)

(locally (declare (optimize speed))
  (defmethod advance ((itr multiset-const-iterator) (n integer))
	(declare (type fixnum n))
	(let ((node (__assoc-itr-node itr)))
	  (if (>= n 0)
		  (do ((i 0))
			  ((= i n) nil)
			(declare (type fixnum i))
			(setf node (__rbnode-increment node))
			(incf i))
		  (do ((i 0))
			  ((= i n) nil)
			(declare (type fixnum i))
			(setf node (__rbnode-decrement node))
			(decf i)))
	  (setf (__assoc-itr-node itr) node))
	nil))

(locally (declare (optimize speed))
  (defmethod distance ((itr1 multiset-const-iterator) (itr2 multiset-const-iterator))
	(let ((cnt 0))
	  (declare (type fixnum cnt))
	  (do ((node1 (__assoc-itr-node itr1))
		   (node2 (__assoc-itr-node itr2)))
		  ((eq node1 node2) cnt)
		(incf cnt)
		(setf node1 (__rbnode-increment node1))))))

;; creating reverse iterator.
(define-constructor reverse-iterator ((itr multiset-const-iterator))
  (make-instance 'multiset-const-reverse-iterator
				 :node (__rbnode-decrement (__assoc-itr-node itr))))


;;------------------------------------------------------------------------------
;;
;; methods for multiset-iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_clone ((itr multiset-iterator))
  (make-instance 'multiset-iterator :node (__assoc-itr-node itr)))

(defmethod operator_cast ((itr multiset-iterator)
						  (typename (eql 'multiset-const-iterator)))
  (__check-exact-type-of-cast itr 'multiset-iterator 'multiset-const-iterator)
  (make-instance 'multiset-const-iterator :node (__assoc-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr multiset-iterator))
  (_= (__rbnode-value (__assoc-itr-node itr)) new-val))

;; creating reverse iterator.
(define-constructor reverse-iterator ((itr multiset-iterator))
  (make-instance 'multiset-reverse-iterator
				 :node (__rbnode-decrement (__assoc-itr-node itr))))



;;------------------------------------------------------------------------------
;;
;; methods for multiset-const-reverse-iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_= ((itr1 multiset-const-reverse-iterator)
					   (itr2 multiset-const-reverse-iterator))
  (__error-when-const-removing-assign itr1 multiset-reverse-iterator
									  itr2 multiset-const-reverse-iterator)
  (setf (__assoc-rev-itr-node itr1) (__assoc-rev-itr-node itr2))
  itr1)

(defmethod operator_clone ((itr multiset-const-reverse-iterator))
  (make-instance 'multiset-const-reverse-iterator :node (__assoc-rev-itr-node itr)))

(defmethod operator_== ((itr1 multiset-const-reverse-iterator)
						(itr2 multiset-const-reverse-iterator))
  (eq (__assoc-rev-itr-node itr1) (__assoc-rev-itr-node itr2)))

(defmethod operator_/= ((itr1 multiset-const-reverse-iterator)
						(itr2 multiset-const-reverse-iterator))
  (not (eq (__assoc-rev-itr-node itr1) (__assoc-rev-itr-node itr2))))

(defmethod operator_* ((itr multiset-const-reverse-iterator))
  (__rbnode-value (__assoc-rev-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr multiset-const-reverse-iterator))
  (error 'setf-to-const :what "setf to (_* multiset-const-reverse-iterator)."))

(defmethod operator_++ ((itr multiset-const-reverse-iterator))
  (setf (__assoc-rev-itr-node itr) (__rbnode-decrement (__assoc-rev-itr-node itr)))
  itr)

(defmethod operator_-- ((itr multiset-const-reverse-iterator))
  (setf (__assoc-rev-itr-node itr) (__rbnode-increment (__assoc-rev-itr-node itr)))
  itr)

(locally (declare (optimize speed))
  (defmethod advance ((itr multiset-const-reverse-iterator) (n integer))
	(declare (type fixnum n))
	(let ((node (__assoc-rev-itr-node itr)))
	  (if (>= n 0)
		  (do ((i 0))
			  ((= i n) nil)
			(declare (type fixnum i))
			(setf node (__rbnode-decrement node))
			(incf i))
		  (do ((i 0))
			  ((= i n) nil)
			(declare (type fixnum i))
			(setf node (__rbnode-increment node))
			(decf i)))
	  (setf (__assoc-rev-itr-node itr) node))
	nil))

(locally (declare (optimize speed))
  (defmethod distance ((itr1 multiset-const-reverse-iterator)
					   (itr2 multiset-const-reverse-iterator))
	(let ((cnt 0))
	  (declare (type fixnum cnt))
	  (do ((node1 (__assoc-rev-itr-node itr1))
		   (node2 (__assoc-rev-itr-node itr2)))
		  ((eq node1 node2) cnt)
		(incf cnt)
		(setf node1 (__rbnode-decrement node1))))))

(defmethod base ((rev-itr multiset-const-reverse-iterator))
  (make-instance 'multiset-const-iterator
				 :node  (__rbnode-increment (__assoc-rev-itr-node rev-itr))))

;; creating reverse iterator.
(define-constructor reverse-iterator ((itr multiset-const-reverse-iterator))
  (make-instance 'multiset-const-iterator
				 :node (__rbnode-increment (__assoc-rev-itr-node itr))))


;;------------------------------------------------------------------------------
;;
;; methods for multiset-reverse-iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_clone ((itr multiset-reverse-iterator))
  (make-instance 'multiset-reverse-iterator :node (__assoc-rev-itr-node itr)))

(defmethod operator_cast ((itr multiset-reverse-iterator)
						  (typename (eql 'multiset-const-reverse-iterator)))
  (__check-exact-type-of-cast itr 'multiset-reverse-iterator 'multiset-const-reverse-iterator)
  (make-instance 'multiset-const-reverse-iterator :node (__assoc-rev-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr multiset-reverse-iterator))
  (_= (__rbnode-value (__assoc-rev-itr-node itr)) new-val))

(defmethod base ((rev-itr multiset-reverse-iterator))
  (make-instance 'multiset-iterator
				 :node  (__rbnode-increment (__assoc-rev-itr-node rev-itr))))

;; creating reverse iterator.
(define-constructor reverse-iterator ((itr multiset-reverse-iterator))
  (make-instance 'multiset-iterator
				 :node (__rbnode-increment (__assoc-rev-itr-node itr))))


;;------------------------------------------------------------------------------
;;
;; debug methods for multiset
;;
;;------------------------------------------------------------------------------
#+cl-stl-debug
(defmethod dump ((container multiset) &optional (stream t) (value-printer nil))
  (format stream "begin dump ---------------------~%")
  (__rbtree-dump (__assoc-tree container) stream value-printer)
  (format stream "end dump -----------------------~%")
  nil)

#+cl-stl-debug
(defmethod check-integrity ((container multiset) &optional (stream t))
  (declare (ignorable stream))
  (__rbtree-verify (__assoc-tree container)))

