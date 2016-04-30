(in-package :cl-stl)

;;------------------------------------------------------------------------------
;;
;; class difinition
;;
;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  (defclass multimap (bidirectional_container)
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

  (defclass multimap_const_iterator (bidirectional_iterator)
	((node :type     :rbnode
		   :initform nil
		   :initarg  :node
		   :accessor __assoc-itr-node)))

  (defclass multimap_const_reverse_iterator (bidirectional_iterator)
	((node :type     :rbnode
		   :initform nil
		   :initarg  :node
		   :accessor __assoc-rev-itr-node)))

  (defclass multimap_iterator (multimap_const_iterator) ())
  (defclass multimap_reverse_iterator (multimap_const_reverse_iterator) ()))


;;--------------------------------------------------------------------
;;
;; internal utilities
;;
;;--------------------------------------------------------------------
#+cl-stl-debug
(labels ((__multimap-check-iterator-belong-imp (itr cont)
		   (let* ((tree (__assoc-tree cont))
				  (node (__assoc-itr-node itr)))
			 (if (eq node (__rbtree-end tree))
				 t
				 (let ((val (__rbnode-value node)))
				   (if (not (typep val 'pair))
					   nil
					   (let ((key (stl:first val)))
						 (handler-case
							 (do ((node1 (__rbtree-lower_bound tree key) (__rbnode-increment node1))
								  (node2 (__rbtree-upper_bound tree key)))
								 ((eq node1 node2) nil)
							   (when (eq node node1)
								 (return t)))
						   (error () nil)))))))))
		   
  (defun __multimap-check-iterator-belong (itr cont)
	(unless (__multimap-check-iterator-belong-imp itr cont)
	  (error 'undefined-behavior :what "Not a iterator of container.")))

  (defun __multimap-check-iterator-range (cont itr1 itr2)
	(let* ((tree  (__assoc-tree cont))
		   (nodeZ (__rbtree-end tree))
		   (node1 (__assoc-itr-node itr1))
		   (node2 (__assoc-itr-node itr2)))
	  (if (eq node2 nodeZ)
		  (if (or (eq node1 nodeZ)
				  (__multimap-check-iterator-belong-imp itr1 cont))
			  t
			  (error 'undefined-behavior :what "Invalid iterator range."))
		  (if (eq node1 nodeZ)
			  (error 'undefined-behavior :what "Invalid iterator range.")
			  (if (handler-case
					  (let ((key1 (stl:first (__rbnode-value node1)))
							(key2 (stl:first (__rbnode-value node2)))
							(comp (functor_function (key_comp cont))))
						(if (funcall comp key2 key1)
							nil
							(do ((nodeA (__rbtree-lower_bound tree key1) (__rbnode-increment nodeA))
								 (nodeB (__rbtree-upper_bound tree key2)))
								((eq nodeA nodeB) nil)
							  (if (eq node1 nodeA)
								  (return (__multimap-check-iterator-belong-imp itr2 cont))
								  (when (eq node2 nodeA)
									(return nil))))))
					(error () nil))
				  t
				  (error 'undefined-behavior :what "Invalid iterator range.")))))))


;;--------------------------------------------------------------------
;;
;; method implementation
;;
;;--------------------------------------------------------------------
(locally (declare (optimize speed))

  (defun __create-multimap (key_comp)
	;; MEMO : key_comp copy in __rbtree-ctor.
	(let ((tree (__rbtree-ctor key_comp #'stl:first)))
	  #+cl-stl-debug (setf (__rbtree-checker tree) #'__map-item-checker)
	  (make-instance 'multimap :core tree)))

  (defun __create-multimap-with-range (key_comp itr1 itr2)
	;; MEMO : key_comp copy in __rbtree-ctor.
	;; MEMO : [itr1, itr2) is 'input_iterator'...
	(let ((tree (__rbtree-ctor key_comp #'stl:first)))
	  #+cl-stl-debug (setf (__rbtree-checker tree) #'__map-item-checker)
	  (__rbtree-insert-range-equal tree itr1 itr2 t)
	  (make-instance 'multimap :core tree)))

  (defun __create-multimap-with-array (key_comp arr idx1 idx2)
	(declare (type cl:vector arr))
	(declare (type fixnum idx1 idx2))
	;; MEMO : key_comp copy in __rbtree-ctor.
	(let ((tree (__rbtree-ctor key_comp #'stl:first)))
	  #+cl-stl-debug (setf (__rbtree-checker tree) #'__map-item-checker)
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
								   #+cl-stl-0x98 binary_function))
  (__create-multimap arg))

; copy constructor
(define-constructor multimap ((arg multimap))
  (clone arg))

; constructor with initializer list 1
#-cl-stl-0x98
(define-constructor multimap ((il initializer_list))
  (declare (type initializer_list il))
  (let ((arr (__initlist-data il)))
	(declare (type simple-vector arr))
	(__create-multimap-with-array #'operator_< arr 0 (length arr))))

; constructor with initializer list 2
#-cl-stl-0x98
(define-constructor multimap ((il initializer_list) (comp cl:function))
  (declare (type initializer_list il))
  (let ((arr (__initlist-data il)))
	(declare (type simple-vector arr))
	(__create-multimap-with-array comp arr 0 (length arr))))

; constructor with initializer list 3
#-cl-stl-0x98
(define-constructor multimap ((il initializer_list)
							  (comp #-cl-stl-0x98 functor
									#+cl-stl-0x98 binary_function))
  (declare (type initializer_list il))
  (let ((arr (__initlist-data il)))
	(declare (type simple-vector arr))
	(__create-multimap-with-array comp arr 0 (length arr))))

; move constructor
#-cl-stl-0x98
(define-constructor multimap ((arg remove-reference))
  (let ((cont (funcall (the cl:function (__rm-ref-closure arg)))))
	(__check-type-of-move-constructor cont multimap)
	(let ((obj (__create-multimap (key_comp cont))))
	  (__rbtree-swap (__assoc-tree obj) (__assoc-tree cont))
	  obj)))

; range constructor
(define-constructor multimap ((itr1 input_iterator) (itr2 input_iterator))
  (__create-multimap-with-range #'operator_< itr1 itr2))

(define-constructor multimap ((itr1 input_iterator)
							  (itr2 input_iterator) (comp cl:function))
  (__create-multimap-with-range comp itr1 itr2))

(define-constructor multimap ((itr1 input_iterator)
							  (itr2 input_iterator) (comp #-cl-stl-0x98 functor
														  #+cl-stl-0x98 binary_function))
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
																#+cl-stl-0x98 binary_function))
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
		(setf (__rbtree-key_comp tree2) (clone (__rbtree-key_comp tree1)))))
	(values cont1 cont2))

  #-cl-stl-0x98
  (defmethod operator_= ((cont multimap) (il initializer_list))
	(declare (type initializer_list il))
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
	(make-instance 'multimap_iterator
				   :node (__rbtree-begin (__assoc-tree container))))

  (defmethod end ((container multimap))
	(make-instance 'multimap_iterator
				   :node (__rbtree-end (__assoc-tree container))))

  (defmethod rbegin ((container multimap))
	(make-instance 'multimap_reverse_iterator 
				   :node (__rbtree-rbegin (__assoc-tree container))))

  (defmethod rend ((container multimap))
	(make-instance 'multimap_reverse_iterator 
				   :node (__rbtree-rend (__assoc-tree container))))

  #-cl-stl-0x98
  (defmethod cbegin ((container multimap))
	(make-instance 'multimap_const_iterator
				   :node (__rbtree-begin (__assoc-tree container))))

  #-cl-stl-0x98
  (defmethod cend ((container multimap))
	(make-instance 'multimap_const_iterator
				   :node (__rbtree-end (__assoc-tree container))))

  #-cl-stl-0x98
  (defmethod crbegin ((container multimap))
	(make-instance 'multimap_const_reverse_iterator
				   :node (__rbtree-rbegin (__assoc-tree container))))

  #-cl-stl-0x98
  (defmethod crend ((container multimap))
	(make-instance 'multimap_const_reverse_iterator
				   :node (__rbtree-rend (__assoc-tree container)))))


;;----------------------------------------------------------
;; capacity
;;----------------------------------------------------------
(locally (declare (optimize speed))

  (defmethod empty ((container multimap))
	(zerop (__rbtree-size (__assoc-tree container))))

  (defmethod size ((container multimap))
	(__rbtree-size (__assoc-tree container)))

  (defmethod max_size ((container multimap))
	(__rbtree-max_size (__assoc-tree container))))


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
	(make-instance 'multimap_iterator
				   :node (__rbtree-insert-equal (__assoc-tree container) value t)))

  ;; insert ( single element by remove reference ) - returns iterator.
  #-cl-stl-0x98
  (defmethod-overload insert ((container multimap) (rm remove-reference))
	(let ((val (funcall (the cl:function (__rm-ref-closure rm)))))
	  (funcall (the cl:function (__rm-ref-closure rm)) nil)
	  (make-instance 'multimap_iterator
					 :node (__rbtree-insert-equal (__assoc-tree container) val nil))))

  ;; insert ( single element with hint ) - returns iterator.
  (defmethod-overload insert ((container multimap)
							  (itr #+cl-stl-0x98 multimap_iterator
								   #-cl-stl-0x98 multimap_const_iterator) value)
	#+cl-stl-0x98  ;; HACK
	(when (and (typep itr   'multimap_const_iterator)
			   (typep value 'multimap_const_iterator))
	  (__rbtree-insert-range-equal (__assoc-tree container) itr value t)
	  (return-from __insert-3 nil))
	
	#+cl-stl-debug (__multimap-check-iterator-belong itr container)
	(make-instance 'multimap_iterator
				   :node (__rbtree-insert-hint-equal (__assoc-tree container)
													 (__assoc-itr-node itr) value t)))

  ;; insert ( single element with hint by remove reference ) - returns iterator.
  #-cl-stl-0x98
  (defmethod-overload insert ((container multimap)
							  (itr multimap_const_iterator) (rm remove-reference))
	#+cl-stl-debug (__multimap-check-iterator-belong itr container)
	(let ((val (funcall (the cl:function (__rm-ref-closure rm)))))
	  (funcall (the cl:function (__rm-ref-closure rm)) nil)
	  (make-instance 'multimap_iterator
					 :node (__rbtree-insert-hint-equal (__assoc-tree container)
													   (__assoc-itr-node itr) val nil))))

  ;; insert ( initializer list ) - returns nil.
  #-cl-stl-0x98
  (defmethod-overload insert ((container multimap) (il initializer_list))
	(declare (type initializer_list il))
	(let* ((arr (__initlist-data il))
		   (cnt (length arr)))
	  (declare (type simple-vector arr))
	  (declare (type fixnum cnt))
	  (__rbtree-insert-array-equal (__assoc-tree container) arr 0 cnt t)
	  nil)))

;; range insert - returns nil.
(locally (declare (optimize speed))

  (defmethod-overload insert ((container multimap) (itr1 input_iterator) (itr2 input_iterator))
	(__rbtree-insert-range-equal (__assoc-tree container) itr1 itr2 t)
	nil)

  (defmethod-overload insert ((container multimap)
							  (itr1 multimap_const_iterator) (itr2 multimap_const_iterator))
	(__rbtree-insert-range-equal (__assoc-tree container) itr1 itr2 t)
	nil)

  (defmethod-overload insert ((container multimap) (ptr1 const-vector-pointer) (ptr2 const-vector-pointer))
	(__pointer-check-iterator-range ptr1 ptr2)
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
	(make-instance 'multimap_iterator
				   :node (__rbtree-emplace-equal (__assoc-tree container) new-val)))

  ;;returns iterator.
  (defmethod-overload emplace_hint ((container multimap)
									(itr multimap_const_iterator) new-val)
	#+cl-stl-debug (__multimap-check-iterator-belong itr container)
	(make-instance 'multimap_iterator
				   :node (__rbtree-emplace_hint-equal (__assoc-tree container)
													  (__assoc-itr-node itr) new-val))))

;;erase
(locally (declare (optimize speed))

  ;; In 0x98, returns nil. In 0x11 returns iterator.
  (defmethod-overload erase ((container multimap)
							 (itr #+cl-stl-0x98 multimap_iterator
								  #-cl-stl-0x98 multimap_const_iterator))
	#+cl-stl-debug (__multimap-check-iterator-belong itr container)
	(let ((node (__rbtree-erase-node (__assoc-tree container) (__assoc-itr-node itr))))
	  (declare (ignorable node))
	  #+cl-stl-0x98 nil
	  #-cl-stl-0x98 (make-instance 'multimap_iterator :node node)))

  ;; In 0x98, returns nil. In 0x11 returns iterator.
  (defmethod-overload erase ((container multimap)
							 (first #+cl-stl-0x98 multimap_iterator #-cl-stl-0x98 multimap_const_iterator)
							 (last  #+cl-stl-0x98 multimap_iterator #-cl-stl-0x98 multimap_const_iterator))
	#+cl-stl-debug (__multimap-check-iterator-range container first last)
	(let ((node (__rbtree-erase-range (__assoc-tree container)
									  (__assoc-itr-node first) (__assoc-itr-node last))))
	  (declare (ignorable node))
	  #+cl-stl-0x98 nil
	  #-cl-stl-0x98 (make-instance 'multimap_iterator :node node)))

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
	(make-instance 'multimap_iterator
				   :node (__rbtree-find (__assoc-tree container) key)))

  ;; returns fixnum.
  (defmethod-overload count ((container multimap) key)
	(__rbtree-count (__assoc-tree container) key))

  ;; returns iterator.
  (defmethod-overload lower_bound ((container multimap) key)
	(make-instance 'multimap_iterator
				   :node (__rbtree-lower_bound (__assoc-tree container) key)))

  ;; returns iterator.
  (defmethod-overload upper_bound ((container multimap) key)
	(make-instance 'multimap_iterator
				   :node (__rbtree-upper_bound (__assoc-tree container) key)))

  ;; returns pair(itr,itr).
  (defmethod-overload equal_range ((container multimap) key)
	(let ((tree (__assoc-tree container)))
	  (make_pair (make-instance 'multimap_iterator
								:node (__rbtree-lower_bound tree key))
				 (make-instance 'multimap_iterator
								:node (__rbtree-upper_bound tree key))))))


;;----------------------------------------------------------
;; observers
;;----------------------------------------------------------
(locally (declare (optimize speed))

  (defmethod key_comp ((container multimap))
	(clone (__rbtree-key_comp (__assoc-tree container))))

  (defmethod value_comp ((container multimap))
	(let ((fnc (functor_function (clone (__rbtree-key_comp (__assoc-tree container))))))
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
;; methods for multimap_const_iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_= ((itr1 multimap_const_iterator) (itr2 multimap_const_iterator))
  (__error-when-const-removing-assign itr1 multimap_iterator
									  itr2 multimap_const_iterator)
  (setf (__assoc-itr-node itr1) (__assoc-itr-node itr2))
  itr1)

(defmethod operator_clone ((itr multimap_const_iterator))
  (make-instance 'multimap_const_iterator :node (__assoc-itr-node itr)))

(defmethod operator_== ((itr1 multimap_const_iterator) (itr2 multimap_const_iterator))
  (eq (__assoc-itr-node itr1) (__assoc-itr-node itr2)))

(defmethod operator_/= ((itr1 multimap_const_iterator) (itr2 multimap_const_iterator))
  (not (eq (__assoc-itr-node itr1) (__assoc-itr-node itr2))))

(defmethod operator_* ((itr multimap_const_iterator))
  (__rbnode-value (__assoc-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr multimap_const_iterator))
  (error 'setf-to-const :what "setf to (_* multimap_const_iterator)."))

(defmethod operator_++ ((itr multimap_const_iterator))
  (setf (__assoc-itr-node itr) (__rbnode-increment (__assoc-itr-node itr)))
  itr)

(defmethod operator_-- ((itr multimap_const_iterator))
  (setf (__assoc-itr-node itr) (__rbnode-decrement (__assoc-itr-node itr)))
  itr)

(locally (declare (optimize speed))
  (defmethod advance ((itr multimap_const_iterator) (n integer))
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
  (defmethod distance ((itr1 multimap_const_iterator) (itr2 multimap_const_iterator))
	(let ((cnt 0))
	  (declare (type fixnum cnt))
	  (do ((node1 (__assoc-itr-node itr1))
		   (node2 (__assoc-itr-node itr2)))
		  ((eq node1 node2) cnt)
		(incf cnt)
		(setf node1 (__rbnode-increment node1))))))

;; creating reverse iterator.
(define-constructor reverse_iterator ((itr multimap_const_iterator))
  (make-instance 'multimap_const_reverse_iterator
				 :node (__rbnode-decrement (__assoc-itr-node itr))))


;;------------------------------------------------------------------------------
;;
;; methods for multimap_iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_clone ((itr multimap_iterator))
  (make-instance 'multimap_iterator :node (__assoc-itr-node itr)))

(defmethod operator_cast ((itr multimap_iterator)
						  (typename (eql 'multimap_const_iterator)))
  (__check-exact-type-of-cast itr 'multimap_iterator 'multimap_const_iterator)
  (make-instance 'multimap_const_iterator :node (__assoc-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr multimap_iterator))
  (_= (__rbnode-value (__assoc-itr-node itr)) new-val))

;; creating reverse iterator.
(define-constructor reverse_iterator ((itr multimap_iterator))
  (make-instance 'multimap_reverse_iterator
				 :node (__rbnode-decrement (__assoc-itr-node itr))))



;;------------------------------------------------------------------------------
;;
;; methods for multimap_const_reverse_iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_= ((itr1 multimap_const_reverse_iterator)
					   (itr2 multimap_const_reverse_iterator))
  (__error-when-const-removing-assign itr1 multimap_reverse_iterator
									  itr2 multimap_const_reverse_iterator)
  (setf (__assoc-rev-itr-node itr1) (__assoc-rev-itr-node itr2))
  itr1)

(defmethod operator_clone ((itr multimap_const_reverse_iterator))
  (make-instance 'multimap_const_reverse_iterator :node (__assoc-rev-itr-node itr)))

(defmethod operator_== ((itr1 multimap_const_reverse_iterator)
						(itr2 multimap_const_reverse_iterator))
  (eq (__assoc-rev-itr-node itr1) (__assoc-rev-itr-node itr2)))

(defmethod operator_/= ((itr1 multimap_const_reverse_iterator)
						(itr2 multimap_const_reverse_iterator))
  (not (eq (__assoc-rev-itr-node itr1) (__assoc-rev-itr-node itr2))))

(defmethod operator_* ((itr multimap_const_reverse_iterator))
  (__rbnode-value (__assoc-rev-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr multimap_const_reverse_iterator))
  (error 'setf-to-const :what "setf to (_* multimap_const_reverse_iterator)."))

(defmethod operator_++ ((itr multimap_const_reverse_iterator))
  (setf (__assoc-rev-itr-node itr) (__rbnode-decrement (__assoc-rev-itr-node itr)))
  itr)

(defmethod operator_-- ((itr multimap_const_reverse_iterator))
  (setf (__assoc-rev-itr-node itr) (__rbnode-increment (__assoc-rev-itr-node itr)))
  itr)

(locally (declare (optimize speed))
  (defmethod advance ((itr multimap_const_reverse_iterator) (n integer))
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
  (defmethod distance ((itr1 multimap_const_reverse_iterator)
					   (itr2 multimap_const_reverse_iterator))
	(let ((cnt 0))
	  (declare (type fixnum cnt))
	  (do ((node1 (__assoc-rev-itr-node itr1))
		   (node2 (__assoc-rev-itr-node itr2)))
		  ((eq node1 node2) cnt)
		(incf cnt)
		(setf node1 (__rbnode-decrement node1))))))

(defmethod base ((rev-itr multimap_const_reverse_iterator))
  (make-instance 'multimap_const_iterator
				 :node  (__rbnode-increment (__assoc-rev-itr-node rev-itr))))

;; creating reverse iterator.
(define-constructor reverse_iterator ((itr multimap_const_reverse_iterator))
  (make-instance 'multimap_const_iterator
				 :node (__rbnode-increment (__assoc-rev-itr-node itr))))


;;------------------------------------------------------------------------------
;;
;; methods for multimap_reverse_iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_clone ((itr multimap_reverse_iterator))
  (make-instance 'multimap_reverse_iterator :node (__assoc-rev-itr-node itr)))

(defmethod operator_cast ((itr multimap_reverse_iterator)
						  (typename (eql 'multimap_const_reverse_iterator)))
  (__check-exact-type-of-cast itr 'multimap_reverse_iterator 'multimap_const_reverse_iterator)
  (make-instance 'multimap_const_reverse_iterator :node (__assoc-rev-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr multimap_reverse_iterator))
  (_= (__rbnode-value (__assoc-rev-itr-node itr)) new-val))

(defmethod base ((rev-itr multimap_reverse_iterator))
  (make-instance 'multimap_iterator
				 :node  (__rbnode-increment (__assoc-rev-itr-node rev-itr))))

;; creating reverse iterator.
(define-constructor reverse_iterator ((itr multimap_reverse_iterator))
  (make-instance 'multimap_iterator
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
(defmethod check_integrity ((container multimap) &optional (stream t))
  (declare (ignorable stream))
  (__rbtree-verify (__assoc-tree container)))

