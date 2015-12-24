(in-package :cl-stl)

;;------------------------------------------------------------------------------
;;
;; class difinition
;;
;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  (defclass multimap (bidirectional-container)
	((rbtree  :type     :rbtree
			  :initform nil
			  :initarg  :core
			  :accessor __assoc-tree)
;TMP;	 #+cl-stl-debug
;TMP;	 (id     :type     :symbol
;TMP;			 :initform (gensym "MULTIMAP-")
;TMP;			 :initarg  :id
;TMP;			 :accessor __instance-id)
	))

  (defclass multimap-const-iterator (bidirectional-iterator)
	((node :type     :rbnode
		   :initform nil
		   :initarg  :node
		   :accessor __assoc-itr-node)))

  (defclass multimap-const-reverse-iterator (bidirectional-iterator)
	((node :type     :rbnode
		   :initform nil
		   :initarg  :node
		   :accessor __assoc-rev-itr-node)))

  (defclass multimap-iterator (multimap-const-iterator) ())
  (defclass multimap-reverse-iterator (multimap-const-reverse-iterator) ()))


;;--------------------------------------------------------------------
;;
;; internal utilities
;;
;;--------------------------------------------------------------------
(defmacro __multimap-check-iterator-belong (itr cont)
  (declare (ignore itr cont))
  nil)    ;;ToDo : temporary...
;;  #-cl-stl-debug nil
;;  #+cl-stl-debug
;;  `(unless (_== ,itr (stl:end ,cont))
;;	 (unless (do* ((tree  (__assoc-tree ,cont))
;;				   (node  (multimap-itr-node ,itr))
;;				   (node1 (__rbtree-lower-bound tree (__rbnode-value node)))
;;				   (node2 (__rbtree-upper-bound tree (__rbnode-value node))))
;;				  ((eq node1 node2) nil)
;;			   (when (eq node1 node)
;;				 (return t))
;;			   (setf node1 (__rbnode-next node1)))
;;	   (error 'undefined-behavior :what ,(format nil "~A is not iterator of ~A." itr cont)))))

;;YET : (defmacro __multimap-check-iterator-range (itr1 itr2)
;;YET :   (declare (ignorable itr1 itr2))
;;YET :   #-cl-stl-debug nil
;;YET :   #+cl-stl-debug
;;YET :   `(unless (__rbnode-check-reachable (multimap-itr-node ,itr1)
;;YET : 									 (multimap-itr-node ,itr2))
;;YET : 	 (error 'undefined-behavior :what ,(format nil "[~A ~A) is not valid range." itr1 itr2))))


;;--------------------------------------------------------------------
;;
;; method implementation
;;
;;--------------------------------------------------------------------
(locally (declare (optimize speed))

  (defun __create-multimap (key-comp)
	;; MEMO : key-comp copy in __rbtree-ctor.
	(let ((tree (__rbtree-ctor key-comp #'stl:first)))
	  (make-instance 'multimap :core tree)))

  (defun __create-multimap-with-range (key-comp itr1 itr2)
	;; MEMO : key-comp copy in __rbtree-ctor.
	(let ((tree (__rbtree-ctor key-comp #'stl:first)))
	  ;;ToDo : check pair-ness of values in sequence...
	  (__rbtree-insert-range-equal tree itr1 itr2 t)
	  (make-instance 'multimap :core tree)))

  (defun __create-multimap-with-array (key-comp arr idx1 idx2)
	;; MEMO : key-comp copy in __rbtree-ctor.
	(let ((tree (__rbtree-ctor key-comp #'stl:first)))
	  ;;ToDo : check pair-ness of values in sequence...
	  (__rbtree-insert-array-equal tree arr idx1 idx2 t)
	  (make-instance 'multimap :core tree))))


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
(define-constructor multimap ((il initializer-list))
  (declare (type initializer-list il))
  (let ((arr (__initlist-data il)))
	(declare (type simple-vector arr))
	(__create-multimap-with-array #'operator_< arr 0 (length arr))))

; constructor with initializer list 2
#-cl-stl-0x98
(define-constructor multimap ((il initializer-list) (comp cl:function))
  (declare (type initializer-list il))
  (let ((arr (__initlist-data il)))
	(declare (type simple-vector arr))
	(__create-multimap-with-array comp arr 0 (length arr))))

; constructor with initializer list 3
#-cl-stl-0x98
(define-constructor multimap ((il initializer-list)
							  (comp #-cl-stl-0x98 functor
									#+cl-stl-0x98 binary-function))
  (declare (type initializer-list il))
  (let ((arr (__initlist-data il)))
	(declare (type simple-vector arr))
	(__create-multimap-with-array comp arr 0 (length arr))))

; move constructor
#-cl-stl-0x98
(define-constructor multimap ((arg remove-reference))
  (let ((cont (funcall (the cl:function (__rm-ref-closure arg)))))
	(__check-type-of-move-constructor cont multimap)
	(let ((obj (__create-multimap (key-comp cont))))
	  (__rbtree-swap (__assoc-tree obj) (__assoc-tree cont))
	  obj)))

; range constructor
(define-constructor multimap ((itr1 input-iterator) (itr2 input-iterator))
  (__create-multimap-with-range #'operator_< itr1 itr2))

(define-constructor multimap ((itr1 input-iterator)
							  (itr2 input-iterator) (comp cl:function))
  (__create-multimap-with-range comp itr1 itr2))

(define-constructor multimap ((itr1 input-iterator)
							  (itr2 input-iterator) (comp #-cl-stl-0x98 functor
														  #+cl-stl-0x98 binary-function))
  (__create-multimap-with-range comp itr1 itr2))

;; range constructor for const-vector-pointer.
(define-constructor multimap ((ptr1 const-vector-pointer) (ptr2 const-vector-pointer))
  (__pointer-check-iterator-range ptr1 ptr2)
  (__create-multimap-with-array #'operator_<
								 (opr::vec-ptr-buffer ptr1)
								 (opr::vec-ptr-index  ptr1)
								 (opr::vec-ptr-index  ptr2)))

(define-constructor multimap ((ptr1 const-vector-pointer)
							  (ptr2 const-vector-pointer) (comp cl:function))
  (__pointer-check-iterator-range ptr1 ptr2)
  (__create-multimap-with-array comp
								 (opr::vec-ptr-buffer ptr1)
								 (opr::vec-ptr-index  ptr1)
								 (opr::vec-ptr-index  ptr2)))

(define-constructor multimap ((ptr1 const-vector-pointer)
							  (ptr2 const-vector-pointer) (comp #-cl-stl-0x98 functor
																#+cl-stl-0x98 binary-function))
  (__pointer-check-iterator-range ptr1 ptr2)
  (__create-multimap-with-array comp
								 (opr::vec-ptr-buffer ptr1)
								 (opr::vec-ptr-index  ptr1)
								 (opr::vec-ptr-index  ptr2)))


(defmethod operator_clone ((container stl::multimap))
  (make-instance 'stl::multimap
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
  
  (defmethod operator_= ((cont1 multimap) (cont2 multimap))
	(__rbtree-assign (__assoc-tree cont1) (__assoc-tree cont2))
	cont1)

  #-cl-stl-0x98
  (defmethod operator_move ((cont1 multimap) (cont2 multimap))
	(unless (eq cont1 cont2)
	  (let ((tree1 (__assoc-tree cont1))
			(tree2 (__assoc-tree cont2)))
		(__rbtree-clear tree1)
		(__rbtree-swap tree1 tree2)
		(setf (__rbtree-key-comp tree2) (clone (__rbtree-key-comp tree1)))))
	(values cont1 cont2))

  #-cl-stl-0x98
  (defmethod operator_= ((cont multimap) (il initializer-list))
	(declare (type initializer-list il))
	(let ((tree (__assoc-tree cont))
		  (arr  (__initlist-data il)))
	  (declare (type rbtree tree))
	  (declare (type simple-vector arr))
	  (__rbtree-clear tree)
	  (__rbtree-insert-array-equal tree arr 0 (length arr) t))
	cont))


;;----------------------------------------------------------
;; iterators
;;----------------------------------------------------------
(locally (declare (optimize speed))

  (defmethod begin ((container multimap))
	(make-instance 'multimap-iterator
				   :node (__rbtree-begin (__assoc-tree container))))

  (defmethod end ((container multimap))
	(make-instance 'multimap-iterator
				   :node (__rbtree-end (__assoc-tree container))))

  (defmethod rbegin ((container multimap))
	(make-instance 'multimap-reverse-iterator 
				   :node (__rbtree-rbegin (__assoc-tree container))))

  (defmethod rend ((container multimap))
	(make-instance 'multimap-reverse-iterator 
				   :node (__rbtree-rend (__assoc-tree container))))

  #-cl-stl-0x98
  (defmethod cbegin ((container multimap))
	(make-instance 'multimap-const-iterator
				   :node (__rbtree-begin (__assoc-tree container))))

  #-cl-stl-0x98
  (defmethod cend ((container multimap))
	(make-instance 'multimap-const-iterator
				   :node (__rbtree-end (__assoc-tree container))))

  #-cl-stl-0x98
  (defmethod crbegin ((container multimap))
	(make-instance 'multimap-const-reverse-iterator
				   :node (__rbtree-rbegin (__assoc-tree container))))

  #-cl-stl-0x98
  (defmethod crend ((container multimap))
	(make-instance 'multimap-const-reverse-iterator
				   :node (__rbtree-rend (__assoc-tree container)))))


;;----------------------------------------------------------
;; capacity
;;----------------------------------------------------------
(locally (declare (optimize speed))

  (defmethod empty ((container multimap))
	(zerop (__rbtree-size (__assoc-tree container))))

  (defmethod size ((container multimap))
	(__rbtree-size (__assoc-tree container)))

  (defmethod max-size ((container multimap))
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
  (defmethod-overload insert ((container multimap) value)
	(__map-check-item-pairness value)
	(make-instance 'multimap-iterator
				   :node (__rbtree-insert-equal (__assoc-tree container) value t)))

  ;; insert ( single element by remove reference ) - returns iterator.
  #-cl-stl-0x98
  (defmethod-overload insert ((container multimap) (rm remove-reference))
	(let ((val (funcall (the cl:function (__rm-ref-closure rm)))))
	  (__map-check-item-pairness val)
	  (funcall (the cl:function (__rm-ref-closure rm)) nil)
	  (make-instance 'multimap-iterator
					 :node (__rbtree-insert-equal (__assoc-tree container) val nil))))

  ;; insert ( single element with hint ) - returns iterator.
  (defmethod-overload insert ((container multimap)
							  (itr #+cl-stl-0x98 multimap-iterator
								   #-cl-stl-0x98 multimap-const-iterator) value)
	#+cl-stl-0x98  ;; HACK
	(when (and (typep itr   'multimap-const-iterator)
			   (typep value 'multimap-const-iterator))
	  (__rbtree-insert-range-equal (__assoc-tree container) itr value t)
	  (return-from __insert-3 nil))
	
	(__multimap-check-iterator-belong itr container)
	(__map-check-item-pairness value)
	(make-instance 'multimap-iterator
				   :node (__rbtree-insert-hint-equal (__assoc-tree container)
													 (__assoc-itr-node itr) value t)))

  ;; insert ( single element with hint by remove reference ) - returns iterator.
  #-cl-stl-0x98
  (defmethod-overload insert ((container multimap)
							  (itr multimap-const-iterator) (rm remove-reference))
	(__multimap-check-iterator-belong itr container)
	(let ((val (funcall (the cl:function (__rm-ref-closure rm)))))
	  (__map-check-item-pairness val)
	  (funcall (the cl:function (__rm-ref-closure rm)) nil)
	  (make-instance 'multimap-iterator
					 :node (__rbtree-insert-hint-equal (__assoc-tree container)
													   (__assoc-itr-node itr) val nil))))

  ;; insert ( initializer list ) - returns nil.
  #-cl-stl-0x98
  (defmethod-overload insert ((container multimap) (il initializer-list))
	(declare (type initializer-list il))
	(let ((arr (__initlist-data il)))
	  ;;ToDo : check pair-ness of values in sequence...
	  (declare (type simple-vector arr))
	  (__rbtree-insert-array-equal (__assoc-tree container) arr 0 (length arr) t)
	  nil)))

;; range insert - returns nil.
(locally (declare (optimize speed))

  (defmethod-overload insert ((container multimap) (itr1 input-iterator) (itr2 input-iterator))
	;;ToDo : check pair-ness of values in sequence...
	(__rbtree-insert-range-equal (__assoc-tree container) itr1 itr2 t)
	nil)

  (defmethod-overload insert ((container multimap)
							  (itr1 multimap-const-iterator) (itr2 multimap-const-iterator))
	;;ToDo : check pair-ness of values in sequence...
	(__rbtree-insert-range-equal (__assoc-tree container) itr1 itr2 t)
	nil)

  (defmethod-overload insert ((container multimap) (ptr1 const-vector-pointer) (ptr2 const-vector-pointer))
	(__pointer-check-iterator-range ptr1 ptr2)
	;;ToDo : check pair-ness of values in sequence...
	(__rbtree-insert-array-equal (__assoc-tree container)
								 (opr::vec-ptr-buffer ptr1)
								 (opr::vec-ptr-index  ptr1)
								 (opr::vec-ptr-index  ptr2) t)
	nil))


;; emplace
#-cl-stl-0x98
(locally (declare (optimize speed))

  ;;returns iterator.
  (defmethod-overload emplace ((container multimap) new-val)
	(__map-check-item-pairness new-val)
	(make-instance 'multimap-iterator
				   :node (__rbtree-emplace-equal (__assoc-tree container) new-val)))

  ;;returns iterator.
  (defmethod-overload emplace-hint ((container multimap)
									(itr multimap-const-iterator) new-val)
	(__multimap-check-iterator-belong itr container)
	(__map-check-item-pairness new-val)
	(make-instance 'multimap-iterator
				   :node (__rbtree-emplace-hint-equal (__assoc-tree container)
													  (__assoc-itr-node itr) new-val))))

;;erase
(locally (declare (optimize speed))

  ;; In 0x98, returns nil. In 0x11 returns iterator.
  (defmethod-overload erase ((container multimap)
							 (itr #+cl-stl-0x98 multimap-iterator
								  #-cl-stl-0x98 multimap-const-iterator))
	(__multimap-check-iterator-belong itr container)
	(let ((node (__rbtree-erase-node (__assoc-tree container) (__assoc-itr-node itr))))
	  (declare (ignorable node))
	  #+cl-stl-0x98 nil
	  #-cl-stl-0x98 (make-instance 'multimap-iterator :node node)))

  ;; In 0x98, returns nil. In 0x11 returns iterator.
  (defmethod-overload erase ((container multimap)
							 (first #+cl-stl-0x98 multimap-iterator #-cl-stl-0x98 multimap-const-iterator)
							 (last  #+cl-stl-0x98 multimap-iterator #-cl-stl-0x98 multimap-const-iterator))
	(__multimap-check-iterator-belong first container)
	;;(__multimap-check-iterator-range  first last)         ;ToDo : 
	(let ((node (__rbtree-erase-range (__assoc-tree container)
									  (__assoc-itr-node first) (__assoc-itr-node last))))
	  (declare (ignorable node))
	  #+cl-stl-0x98 nil
	  #-cl-stl-0x98 (make-instance 'multimap-iterator :node node)))

  ;; returns deleted node count.
  (defmethod-overload erase ((container multimap) key)
	(__rbtree-erase-key (__assoc-tree container) key)))



(defmethod-overload swap ((cont1 multimap) (cont2 multimap))
  (__rbtree-swap (__assoc-tree cont1) (__assoc-tree cont2))
  (values cont1 cont2))

(defmethod clear ((container multimap))
  (__rbtree-clear (__assoc-tree container))
  nil)

;;----------------------------------------------------------
;; specific operations
;;----------------------------------------------------------
(locally (declare (optimize speed))

  ;; returns iterator.
  (defmethod-overload find ((container multimap) key)
	(make-instance 'multimap-iterator
				   :node (__rbtree-find (__assoc-tree container) key)))

  ;; returns fixnum.
  (defmethod-overload count ((container multimap) key)
	(__rbtree-count (__assoc-tree container) key))

  ;; returns iterator.
  (defmethod-overload lower-bound ((container multimap) key)
	(make-instance 'multimap-iterator
				   :node (__rbtree-lower-bound (__assoc-tree container) key)))

  ;; returns iterator.
  (defmethod-overload upper-bound ((container multimap) key)
	(make-instance 'multimap-iterator
				   :node (__rbtree-upper-bound (__assoc-tree container) key)))

  ;; returns pair(itr,itr).
  (defmethod-overload equal-range ((container multimap) key)
	(let ((tree (__assoc-tree container)))
	  (make-pair (make-instance 'multimap-iterator
								:node (__rbtree-lower-bound tree key))
				 (make-instance 'multimap-iterator
								:node (__rbtree-upper-bound tree key))))))


;;----------------------------------------------------------
;; observers
;;----------------------------------------------------------
(locally (declare (optimize speed))

  (defmethod key-comp ((container multimap))
	(clone (__rbtree-key-comp (__assoc-tree container))))

  (defmethod value-comp ((container multimap))
	(let ((fnc (functor-function (clone (__rbtree-key-comp (__assoc-tree container))))))
	  (declare (type cl:function fnc))
	  (lambda (pr1 pr2)
		(funcall fnc (stl:first pr1) (stl:first pr2))))))


;;----------------------------------------------------------
;; compare
;;----------------------------------------------------------
(locally (declare (optimize speed))

  (defmethod operator_== ((cont1 multimap) (cont2 multimap))
	(__rbtree-equal (__assoc-tree cont1) (__assoc-tree cont2) #'operator_==))

  (defmethod operator_/= ((cont1 multimap) (cont2 multimap))
	(not (__rbtree-equal (__assoc-tree cont1) (__assoc-tree cont2) #'operator_==)))

  (defmethod operator_< ((cont1 multimap) (cont2 multimap))
	(__rbtree-less (__assoc-tree cont1) (__assoc-tree cont2) #'operator_<))

  (defmethod operator_<= ((cont1 multimap) (cont2 multimap))
	(not (__rbtree-less (__assoc-tree cont2) (__assoc-tree cont1) #'operator_<)))

  (defmethod operator_> ((cont1 multimap) (cont2 multimap))
	(__rbtree-less (__assoc-tree cont2) (__assoc-tree cont1) #'operator_<))

  (defmethod operator_>= ((cont1 multimap) (cont2 multimap))
	(not (__rbtree-less (__assoc-tree cont1) (__assoc-tree cont2) #'operator_<))))



;;----------------------------------------------------------
;; enumeration
;;----------------------------------------------------------
#-cl-stl-0x98
(defmethod-overload for ((cont multimap) func)
  ;MEMO : func is always lambda function ( see stl:for ). 
  (__rbtree-enumerate (__assoc-tree cont) func))


;;------------------------------------------------------------------------------
;;
;; methods for multimap-const-iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_= ((itr1 multimap-const-iterator) (itr2 multimap-const-iterator))
  (__error-when-const-removing-assign itr1 multimap-iterator
									  itr2 multimap-const-iterator)
  (setf (__assoc-itr-node itr1) (__assoc-itr-node itr2))
  itr1)

(defmethod operator_clone ((itr multimap-const-iterator))
  (make-instance 'multimap-const-iterator :node (__assoc-itr-node itr)))

(defmethod operator_== ((itr1 multimap-const-iterator) (itr2 multimap-const-iterator))
  (eq (__assoc-itr-node itr1) (__assoc-itr-node itr2)))

(defmethod operator_/= ((itr1 multimap-const-iterator) (itr2 multimap-const-iterator))
  (not (eq (__assoc-itr-node itr1) (__assoc-itr-node itr2))))

(defmethod operator_* ((itr multimap-const-iterator))
  (__rbnode-value (__assoc-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr multimap-const-iterator))
  (error 'setf-to-const :what "setf to (_* multimap-const-iterator)."))

(defmethod operator_++ ((itr multimap-const-iterator))
  (setf (__assoc-itr-node itr) (__rbnode-increment (__assoc-itr-node itr)))
  itr)

(defmethod operator_-- ((itr multimap-const-iterator))
  (setf (__assoc-itr-node itr) (__rbnode-decrement (__assoc-itr-node itr)))
  itr)

(locally (declare (optimize speed))
  (defmethod advance ((itr multimap-const-iterator) (n integer))
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
  (defmethod distance ((itr1 multimap-const-iterator) (itr2 multimap-const-iterator))
	(let ((cnt 0))
	  (declare (type fixnum cnt))
	  (do ((node1 (__assoc-itr-node itr1))
		   (node2 (__assoc-itr-node itr2)))
		  ((eq node1 node2) cnt)
		(incf cnt)
		(setf node1 (__rbnode-increment node1))))))

;; creating reverse iterator.
(define-constructor reverse-iterator ((itr multimap-const-iterator))
  (make-instance 'multimap-const-reverse-iterator
				 :node (__rbnode-decrement (__assoc-itr-node itr))))


;;------------------------------------------------------------------------------
;;
;; methods for multimap-iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_clone ((itr multimap-iterator))
  (make-instance 'multimap-iterator :node (__assoc-itr-node itr)))

(defmethod operator_cast ((itr multimap-iterator)
						  (typename (eql 'multimap-const-iterator)))
  (__check-exact-type-of-cast itr 'multimap-iterator 'multimap-const-iterator)
  (make-instance 'multimap-const-iterator :node (__assoc-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr multimap-iterator))
  (_= (__rbnode-value (__assoc-itr-node itr)) new-val))

;; creating reverse iterator.
(define-constructor reverse-iterator ((itr multimap-iterator))
  (make-instance 'multimap-reverse-iterator
				 :node (__rbnode-decrement (__assoc-itr-node itr))))



;;------------------------------------------------------------------------------
;;
;; methods for multimap-const-reverse-iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_= ((itr1 multimap-const-reverse-iterator)
					   (itr2 multimap-const-reverse-iterator))
  (__error-when-const-removing-assign itr1 multimap-reverse-iterator
									  itr2 multimap-const-reverse-iterator)
  (setf (__assoc-rev-itr-node itr1) (__assoc-rev-itr-node itr2))
  itr1)

(defmethod operator_clone ((itr multimap-const-reverse-iterator))
  (make-instance 'multimap-const-reverse-iterator :node (__assoc-rev-itr-node itr)))

(defmethod operator_== ((itr1 multimap-const-reverse-iterator)
						(itr2 multimap-const-reverse-iterator))
  (eq (__assoc-rev-itr-node itr1) (__assoc-rev-itr-node itr2)))

(defmethod operator_/= ((itr1 multimap-const-reverse-iterator)
						(itr2 multimap-const-reverse-iterator))
  (not (eq (__assoc-rev-itr-node itr1) (__assoc-rev-itr-node itr2))))

(defmethod operator_* ((itr multimap-const-reverse-iterator))
  (__rbnode-value (__assoc-rev-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr multimap-const-reverse-iterator))
  (error 'setf-to-const :what "setf to (_* multimap-const-reverse-iterator)."))

(defmethod operator_++ ((itr multimap-const-reverse-iterator))
  (setf (__assoc-rev-itr-node itr) (__rbnode-decrement (__assoc-rev-itr-node itr)))
  itr)

(defmethod operator_-- ((itr multimap-const-reverse-iterator))
  (setf (__assoc-rev-itr-node itr) (__rbnode-increment (__assoc-rev-itr-node itr)))
  itr)

(locally (declare (optimize speed))
  (defmethod advance ((itr multimap-const-reverse-iterator) (n integer))
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
  (defmethod distance ((itr1 multimap-const-reverse-iterator)
					   (itr2 multimap-const-reverse-iterator))
	(let ((cnt 0))
	  (declare (type fixnum cnt))
	  (do ((node1 (__assoc-rev-itr-node itr1))
		   (node2 (__assoc-rev-itr-node itr2)))
		  ((eq node1 node2) cnt)
		(incf cnt)
		(setf node1 (__rbnode-decrement node1))))))

(defmethod base ((rev-itr multimap-const-reverse-iterator))
  (make-instance 'multimap-const-iterator
				 :node  (__rbnode-increment (__assoc-rev-itr-node rev-itr))))

;; creating reverse iterator.
(define-constructor reverse-iterator ((itr multimap-const-reverse-iterator))
  (make-instance 'multimap-const-iterator
				 :node (__rbnode-increment (__assoc-rev-itr-node itr))))


;;------------------------------------------------------------------------------
;;
;; methods for multimap-reverse-iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_clone ((itr multimap-reverse-iterator))
  (make-instance 'multimap-reverse-iterator :node (__assoc-rev-itr-node itr)))

(defmethod operator_cast ((itr multimap-reverse-iterator)
						  (typename (eql 'multimap-const-reverse-iterator)))
  (__check-exact-type-of-cast itr 'multimap-reverse-iterator 'multimap-const-reverse-iterator)
  (make-instance 'multimap-const-reverse-iterator :node (__assoc-rev-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr multimap-reverse-iterator))
  (_= (__rbnode-value (__assoc-rev-itr-node itr)) new-val))

(defmethod base ((rev-itr multimap-reverse-iterator))
  (make-instance 'multimap-iterator
				 :node  (__rbnode-increment (__assoc-rev-itr-node rev-itr))))

;; creating reverse iterator.
(define-constructor reverse-iterator ((itr multimap-reverse-iterator))
  (make-instance 'multimap-iterator
				 :node (__rbnode-increment (__assoc-rev-itr-node itr))))


;;------------------------------------------------------------------------------
;;
;; debug methods for multimap
;;
;;------------------------------------------------------------------------------
#+cl-stl-debug
(defmethod dump ((container multimap) &optional (stream t) (value-printer nil))
  (format stream "begin dump ---------------------~%")
  (__rbtree-dump (__assoc-tree container) stream value-printer)
  (format stream "end dump -----------------------~%")
  nil)

#+cl-stl-debug
(defmethod check-integrity ((container multimap) &optional (stream t))
  (declare (ignorable stream))
  (__rbtree-verify (__assoc-tree container)))

