(in-package :cl-stl)

;;------------------------------------------------------------------------------
;;
;; class difinition
;;
;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  (defclass set (bidirectional_container)
	((rbtree :type     :rbtree
			 :initform nil
			 :initarg  :core
			 :accessor __assoc-tree)
;TMP;	 #+cl-stl-debug
;TMP;	 (id     :type     :symbol
;TMP;			 :initform (gensym "SET-")
;TMP;			 :initarg  :id
;TMP;			 :accessor __instance-id)
	))

  (defclass set_const_iterator (bidirectional_iterator)
	((node :type     :rbnode
		   :initform nil
		   :initarg  :node
		   :accessor __assoc-itr-node)))

  (defclass set_const_reverse_iterator (bidirectional_iterator)
	((node :type     :rbnode
		   :initform nil
		   :initarg  :node
		   :accessor __assoc-rev-itr-node)))

  (defclass set_iterator (set_const_iterator) ())
  (defclass set_reverse_iterator (set_const_reverse_iterator) ()))


;;--------------------------------------------------------------------
;;
;; internal utilities
;;
;;--------------------------------------------------------------------
#+cl-stl-debug
(labels ((__set-check-iterator-belong-imp (itr cont)
		   (let* ((tree (__assoc-tree cont))
				  (node (__assoc-itr-node itr)))
			 (if (eq node (__rbtree-end tree))
				 t
				 (handler-case
					 (eq node (__rbtree-lower_bound tree (__rbnode-value node)))
				   (error () nil))))))

  (defun __set-check-iterator-belong (itr cont)
	(unless (__set-check-iterator-belong-imp itr cont)
	  (error 'undefined-behavior :what "Not a iterator of container.")))

  (defun __set-check-iterator-range (cont itr1 itr2)
	(let* ((tree  (__assoc-tree cont))
		   (nodeZ (__rbtree-end tree))
		   (node1 (__assoc-itr-node itr1))
		   (node2 (__assoc-itr-node itr2)))
	  (if (eq node2 nodeZ)
		  (if (or (eq node1 nodeZ)
				  (__set-check-iterator-belong-imp itr1 cont))
			  t
			  (error 'undefined-behavior :what "Invalid iterator range."))
		  (if (eq node1 nodeZ)
			  (error 'undefined-behavior :what "Invalid iterator range.")
			  (if (handler-case
					  (let ((val1 (__rbnode-value node1))
							(val2 (__rbnode-value node2)))
						(and (eq node1 (__rbtree-lower_bound tree val1))
							 (eq node2 (__rbtree-lower_bound tree val2))
							 (not (funcall (value_comp cont) val2 val1))))
					(error () nil))
				  t
				  (error 'undefined-behavior :what "Invalid iterator range.")))))))


;;--------------------------------------------------------------------
;;
;; method implementation
;;
;;--------------------------------------------------------------------
(locally (declare (optimize speed))

  (defun __create-set (comp)
	;; MEMO : comp copy in __rbtree-ctor.
	(let ((tree (__rbtree-ctor comp #'identity)))
	  (make-instance 'stl::set :core tree)))

  (defun __create-set-with-range (comp itr1 itr2)
	;; MEMO : comp copy in __rbtree-ctor.
	(let ((tree (__rbtree-ctor comp #'identity)))
	  (__rbtree-insert-range-unique tree itr1 itr2 t)
	  (make-instance 'stl::set :core tree)))

  (defun __create-set-with-array (comp arr idx1 idx2)
	;; MEMO : comp copy in __rbtree-ctor.
	(let ((tree (__rbtree-ctor comp #'identity)))
	  (__rbtree-insert-array-unique tree arr idx1 idx2 t)
	  (make-instance 'stl::set :core tree))))
  

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
(define-constructor set ((comp cl:function))
  (__create-set comp))

; empty constructor 3
(define-constructor set ((comp #-cl-stl-0x98 functor
							   #+cl-stl-0x98 binary_function))
  (__create-set comp))

; copy constructor
(define-constructor set ((obj stl::set))
  (clone obj))

; constructor with initializer list 1
#-cl-stl-0x98
(define-constructor set ((il initializer_list))
  (declare (type initializer_list il))
  (let ((arr (__initlist-data il)))
	(declare (type simple-vector arr))
	(__create-set-with-array #'operator_< arr 0 (length arr))))

; constructor with initializer list 2
#-cl-stl-0x98
(define-constructor set ((il initializer_list) (comp cl:function))
  (declare (type initializer_list il))
  (let ((arr (__initlist-data il)))
	(declare (type simple-vector arr))
	(__create-set-with-array comp arr 0 (length arr))))

; constructor with initializer list 3
#-cl-stl-0x98
(define-constructor set ((il   initializer_list)
						 (comp #-cl-stl-0x98 functor
							   #+cl-stl-0x98 binary_function))
  (declare (type initializer_list il))
  (let ((arr (__initlist-data il)))
	(declare (type simple-vector arr))
	(__create-set-with-array comp arr 0 (length arr))))

; move constructor
#-cl-stl-0x98
(define-constructor set ((arg& remove-reference))
  (with-reference (arg)
	(let ((cont arg))
	  (__check-type-of-move-constructor cont stl::set)
	  (let ((obj (__create-set (key_comp cont))))
		(__rbtree-swap (__assoc-tree obj) (__assoc-tree cont))
		obj))))

; range constructor
(define-constructor set ((itr1 input_iterator) (itr2 input_iterator))
  (__create-set-with-range #'operator_< itr1 itr2))

(define-constructor set ((itr1 input_iterator)
						 (itr2 input_iterator) (comp cl:function))
  (__create-set-with-range comp itr1 itr2))

(define-constructor set ((itr1 input_iterator)
						 (itr2 input_iterator) (comp #-cl-stl-0x98 functor
													 #+cl-stl-0x98 binary_function))
  (__create-set-with-range comp itr1 itr2))

; range constructor for const-vector-pointer.
(define-constructor set ((ptr1 const-vector-pointer) (ptr2 const-vector-pointer))
  (__pointer-check-iterator-range ptr1 ptr2)
  (__create-set-with-array #'operator_<
							(opr::vec-ptr-buffer ptr1)
							(opr::vec-ptr-index  ptr1)
							(opr::vec-ptr-index  ptr2)))

(define-constructor set ((ptr1 const-vector-pointer)
						 (ptr2 const-vector-pointer) (comp cl:function))
  (__pointer-check-iterator-range ptr1 ptr2)
  (__create-set-with-array comp
							(opr::vec-ptr-buffer ptr1)
							(opr::vec-ptr-index  ptr1)
							(opr::vec-ptr-index  ptr2)))

(define-constructor set ((ptr1 const-vector-pointer)
						 (ptr2 const-vector-pointer) (comp #-cl-stl-0x98 functor
														   #+cl-stl-0x98 binary_function))
  (__pointer-check-iterator-range ptr1 ptr2)
  (__create-set-with-array comp
							(opr::vec-ptr-buffer ptr1)
							(opr::vec-ptr-index  ptr1)
							(opr::vec-ptr-index  ptr2)))



(defmethod operator_clone ((container stl::set))
  (make-instance 'stl::set
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
  
  (defmethod operator_= ((cont1 stl::set) (cont2 stl::set))
	(__rbtree-assign (__assoc-tree cont1) (__assoc-tree cont2))
	cont1)

  #-cl-stl-0x98
  (defmethod operator_move ((cont1 stl::set) (cont2 stl::set))
	(unless (eq cont1 cont2)
	  (let ((tree1 (__assoc-tree cont1))
			(tree2 (__assoc-tree cont2)))
		(__rbtree-clear tree1)
		(__rbtree-swap tree1 tree2)
		(setf (__rbtree-key_comp tree2) (clone (__rbtree-key_comp tree1)))))
	(values cont1 cont2))

  #-cl-stl-0x98
  (defmethod operator_= ((cont stl::set) (il initializer_list))
	(declare (type initializer_list il))
	(let ((tree (__assoc-tree cont))
		  (arr  (__initlist-data il)))
	  (declare (type rbtree        tree))
	  (declare (type simple-vector arr))
	  (__rbtree-clear tree)
	  (__rbtree-insert-array-unique tree arr 0 (length arr) t))
	cont))


;;----------------------------------------------------------
;; iterators
;;----------------------------------------------------------
(locally (declare (optimize speed))

  (defmethod begin ((container stl::set))
	(make-instance 'set_iterator
				   :node (__rbtree-begin (__assoc-tree container))))

  (defmethod end ((container stl::set))
	(make-instance 'set_iterator
				   :node (__rbtree-end (__assoc-tree container))))

  (defmethod rbegin ((container stl::set))
	(make-instance 'set_reverse_iterator
				   :node (__rbtree-rbegin (__assoc-tree container))))

  (defmethod rend ((container stl::set))
	(make-instance 'set_reverse_iterator
				   :node (__rbtree-rend (__assoc-tree container))))

  #-cl-stl-0x98
  (defmethod cbegin ((container stl::set))
	(make-instance 'set_const_iterator
				   :node (__rbtree-begin (__assoc-tree container))))

  #-cl-stl-0x98
  (defmethod cend ((container stl::set))
	(make-instance 'set_const_iterator
				   :node (__rbtree-end (__assoc-tree container))))

  #-cl-stl-0x98
  (defmethod crbegin ((container stl::set))
	(make-instance 'set_const_reverse_iterator
				   :node (__rbtree-rbegin (__assoc-tree container))))

  #-cl-stl-0x98
  (defmethod crend ((container stl::set))
	(make-instance 'set_const_reverse_iterator
				   :node (__rbtree-rend (__assoc-tree container)))))


;;----------------------------------------------------------
;; capacity
;;----------------------------------------------------------
(locally (declare (optimize speed))

  (defmethod empty ((container stl::set))
	(zerop (__rbtree-size (__assoc-tree container))))

  (defmethod size ((container stl::set))
	(__rbtree-size (__assoc-tree container)))

  (defmethod max_size ((container stl::set))
	(__rbtree-max_size (__assoc-tree container))))


;;----------------------------------------------------------
;; element access
;;----------------------------------------------------------
; NONE.

;;----------------------------------------------------------
;; modifiers
;;----------------------------------------------------------
(locally (declare (optimize speed))

  ;; insert ( single element ) - returns pair<iterator,bool>.
  (defmethod-overload insert ((container stl::set) value)
	(multiple-value-bind (node success)
		(__rbtree-insert-unique (__assoc-tree container) value t)
	  (make_pair (make-instance 'set_iterator :node node) success)))

  ;; insert ( single element by remove reference ) - returns pair<iterator,bool>.
  #-cl-stl-0x98
  (defmethod-overload insert ((container stl::set) (rm& remove-reference))
	(with-reference (rm)
	  (let ((val rm))
		(setf rm nil)
		(multiple-value-bind (node success)
			(__rbtree-insert-unique (__assoc-tree container) val nil)
		  (make_pair (make-instance 'set_iterator :node node) success)))))

  ;; insert ( single element with hint ) - returns iterator.
  (defmethod-overload insert ((container stl::set)
							  (itr #+cl-stl-0x98 set_iterator
								   #-cl-stl-0x98 set_const_iterator) value)
	#+cl-stl-0x98  ;; HACK
	(when (and (typep itr   'set_const_iterator)
			   (typep value 'set_const_iterator))
	  (__rbtree-insert-range-unique (__assoc-tree container) itr value t)
	  (return-from __insert-3 nil))
	
	#+cl-stl-debug (__set-check-iterator-belong itr container)
	(make-instance 'set_iterator
				   :node (__rbtree-insert-hint-unique (__assoc-tree container)
													  (__assoc-itr-node itr) value t)))

  ;; insert ( single element with hint by remove reference ) - returns iterator.
  #-cl-stl-0x98
  (defmethod-overload insert ((container stl::set)
							  (itr set_const_iterator) (rm& remove-reference))
	#+cl-stl-debug (__set-check-iterator-belong itr container)
	(with-reference (rm)
	  (let ((val rm))
		(setf rm nil)
		(make-instance 'set_iterator
					   :node (__rbtree-insert-hint-unique (__assoc-tree container)
														  (__assoc-itr-node itr) val nil)))))

  ;; insert ( initializer list ) - returns nil.
  #-cl-stl-0x98
  (defmethod-overload insert ((container stl::set) (il initializer_list))
	(declare (type initializer_list il))
	(let ((arr (__initlist-data il)))
	  (declare (type simple-vector arr))
	  (__rbtree-insert-array-unique (__assoc-tree container) arr 0 (length arr) t)
	  nil)))


;; range insert - returns nil.
(locally (declare (optimize speed))

  (defmethod-overload insert ((container stl::set) (itr1 input_iterator) (itr2 input_iterator))
	(__rbtree-insert-range-unique (__assoc-tree container) itr1 itr2 t)
	nil)

  (defmethod-overload insert ((container stl::set) (itr1 set_const_iterator) (itr2 set_const_iterator))
	(__rbtree-insert-range-unique (__assoc-tree container) itr1 itr2 t)
	nil)

  (defmethod-overload insert ((container stl::set) (ptr1 const-vector-pointer) (ptr2 const-vector-pointer))
	(__pointer-check-iterator-range ptr1 ptr2)
	(__rbtree-insert-array-unique (__assoc-tree container)
								  (opr::vec-ptr-buffer ptr1)
								  (opr::vec-ptr-index  ptr1)
								  (opr::vec-ptr-index  ptr2) t)
	nil))

;; emplace
#-cl-stl-0x98
(locally (declare (optimize speed))

  ;;returns pair<iterator, bool>.
  (defmethod-overload emplace ((container stl::set) new-val)
	(multiple-value-bind (node success)
		(__rbtree-emplace-unique (__assoc-tree container) new-val)
	  (make_pair (make-instance 'set_iterator :node node) success)))

  ;;returns iterator.
  (defmethod-overload emplace_hint ((container stl::set)
									(itr set_const_iterator) new-val)
	#+cl-stl-debug (__set-check-iterator-belong itr container)
	(make-instance 'set_iterator
				   :node (__rbtree-emplace_hint-unique (__assoc-tree container)
													   (__assoc-itr-node itr) new-val))))

;;erase
(locally (declare (optimize speed))

  ;; In 0x98, returns nil. In 0x11 returns iterator.
  (defmethod-overload erase ((container stl::set)
							 (itr #+cl-stl-0x98 set_iterator
								  #-cl-stl-0x98 set_const_iterator))
	#+cl-stl-debug (__set-check-iterator-belong itr container)
	(let ((node (__rbtree-erase-node (__assoc-tree container) (__assoc-itr-node itr))))
	  (declare (ignorable node))
	  #+cl-stl-0x98 nil
	  #-cl-stl-0x98 (make-instance 'set_iterator :node node)))

  ;; In 0x98, returns nil. In 0x11 returns iterator.
  (defmethod-overload erase ((container stl::set)
							 (first #+cl-stl-0x98 set_iterator	#-cl-stl-0x98 set_const_iterator)
							 (last  #+cl-stl-0x98 set_iterator	#-cl-stl-0x98 set_const_iterator))
	#+cl-stl-debug (__set-check-iterator-range container first last)
	(let ((node (__rbtree-erase-range (__assoc-tree container)
									  (__assoc-itr-node first) (__assoc-itr-node last))))
	  (declare (ignorable node))
	  #+cl-stl-0x98 nil
	  #-cl-stl-0x98 (make-instance 'set_iterator :node node)))

  ;; returns deleted node count.
  (defmethod-overload erase ((container stl::set) key)
	(__rbtree-erase-key (__assoc-tree container) key)))


(defmethod-overload swap ((cont1 stl::set) (cont2 stl::set))
  (__rbtree-swap (__assoc-tree cont1) (__assoc-tree cont2))
  (values cont1 cont2))

(defmethod clear ((container stl::set))
  (__rbtree-clear (__assoc-tree container))
  nil)

;;----------------------------------------------------------
;; specific operations
;;----------------------------------------------------------
(locally (declare (optimize speed))

  ;; returns iterator.
  (defmethod-overload find ((container stl::set) key)
	(make-instance 'set_iterator
				   :node (__rbtree-find (__assoc-tree container) key)))

  ;; returns integer.
  (defmethod-overload count ((container stl::set) key)
	(__rbtree-count (__assoc-tree container) key))

  ;; returns iterator.
  (defmethod-overload lower_bound ((container stl::set) key)
	(make-instance 'set_iterator
				   :node (__rbtree-lower_bound (__assoc-tree container) key)))

  ;; returns iterator.
  (defmethod-overload upper_bound ((container stl::set) key)
	(make-instance 'set_iterator
				   :node (__rbtree-upper_bound (__assoc-tree container) key)))

  ;; returns pair(itr,itr).
  (defmethod-overload equal_range ((container stl::set) key)
	(let ((tree (__assoc-tree container)))
	  (make_pair (make-instance 'set_iterator
								:node (__rbtree-lower_bound tree key))
				 (make-instance 'set_iterator
								:node (__rbtree-upper_bound tree key))))))


;;----------------------------------------------------------
;; observers
;;----------------------------------------------------------
(locally (declare (optimize speed))

  (defmethod key_comp ((container stl::set))
	(clone (__rbtree-key_comp (__assoc-tree container))))

  (defmethod value_comp ((container stl::set))
	(clone (__rbtree-key_comp (__assoc-tree container)))))


;;----------------------------------------------------------
;; compare
;;----------------------------------------------------------
(locally (declare (optimize speed))

  (defmethod operator_== ((cont1 stl::set) (cont2 stl::set))
	(__rbtree-equal (__assoc-tree cont1) (__assoc-tree cont2) #'operator_==))

  (defmethod operator_/= ((cont1 stl::set) (cont2 stl::set))
	(not (__rbtree-equal (__assoc-tree cont1) (__assoc-tree cont2) #'operator_==)))

  (defmethod operator_< ((cont1 stl::set) (cont2 stl::set))
	(__rbtree-less (__assoc-tree cont1) (__assoc-tree cont2) #'operator_<))

  (defmethod operator_<= ((cont1 stl::set) (cont2 stl::set))
	(not (__rbtree-less (__assoc-tree cont2) (__assoc-tree cont1) #'operator_<)))

  (defmethod operator_> ((cont1 stl::set) (cont2 stl::set))
	(__rbtree-less (__assoc-tree cont2) (__assoc-tree cont1) #'operator_<))

  (defmethod operator_>= ((cont1 stl::set) (cont2 stl::set))
	(not (__rbtree-less (__assoc-tree cont1) (__assoc-tree cont2) #'operator_<))))



;;----------------------------------------------------------
;; enumeration
;;----------------------------------------------------------
#-cl-stl-0x98
(defmethod-overload for ((cont stl::set) func)
  ;MEMO : func is always lambda function ( see stl:for ). 
  (__rbtree-enumerate (__assoc-tree cont) func))


;;------------------------------------------------------------------------------
;;
;; methods for set_const_iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_= ((itr1 set_const_iterator) (itr2 set_const_iterator))
  (__error-when-const-removing-assign itr1 set_iterator
									  itr2 set_const_iterator)
  (setf (__assoc-itr-node itr1) (__assoc-itr-node itr2))
  itr1)

(defmethod operator_clone ((itr set_const_iterator))
  (make-instance 'set_const_iterator :node (__assoc-itr-node itr)))

(defmethod operator_== ((itr1 set_const_iterator) (itr2 set_const_iterator))
  (eq (__assoc-itr-node itr1) (__assoc-itr-node itr2)))

(defmethod operator_/= ((itr1 set_const_iterator) (itr2 set_const_iterator))
  (not (eq (__assoc-itr-node itr1) (__assoc-itr-node itr2))))

(defmethod operator_* ((itr set_const_iterator))
  (__rbnode-value (__assoc-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr set_const_iterator))
  (error 'setf-to-const :what "setf to (_* set_const_iterator)."))

(defmethod operator_++ ((itr set_const_iterator))
  (setf (__assoc-itr-node itr) (__rbnode-increment (__assoc-itr-node itr)))
  itr)

(defmethod operator_-- ((itr set_const_iterator))
  (setf (__assoc-itr-node itr) (__rbnode-decrement (__assoc-itr-node itr)))
  itr)

(locally (declare (optimize speed))
  (defmethod advance ((itr set_const_iterator) (n integer))
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
  (defmethod distance ((itr1 set_const_iterator) (itr2 set_const_iterator))
	(let ((cnt 0))
	  (declare (type fixnum cnt))
	  (do ((node1 (__assoc-itr-node itr1))
		   (node2 (__assoc-itr-node itr2)))
		  ((eq node1 node2) cnt)
		(incf cnt)
		(setf node1 (__rbnode-increment node1))))))

;; creating reverse iterator.
(define-constructor reverse_iterator ((itr set_const_iterator))
  (make-instance 'set_const_reverse_iterator
				 :node (__rbnode-decrement (__assoc-itr-node itr))))


;;------------------------------------------------------------------------------
;;
;; methods for set_iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_clone ((itr set_iterator))
  (make-instance 'set_iterator :node (__assoc-itr-node itr)))

(defmethod operator_cast ((itr set_iterator)
						  (typename (eql 'set_const_iterator)))
  (__check-exact-type-of-cast itr 'set_iterator 'set_const_iterator)
  (make-instance 'set_const_iterator :node (__assoc-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr set_iterator))
  (_= (__rbnode-value (__assoc-itr-node itr)) new-val))

;; creating reverse iterator.
(define-constructor reverse_iterator ((itr set_iterator))
  (make-instance 'set_reverse_iterator
				 :node (__rbnode-decrement (__assoc-itr-node itr))))



;;------------------------------------------------------------------------------
;;
;; methods for set_const_reverse_iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_= ((itr1 set_const_reverse_iterator)
					   (itr2 set_const_reverse_iterator))
  (__error-when-const-removing-assign itr1 set_reverse_iterator
									  itr2 set_const_reverse_iterator)
  (setf (__assoc-rev-itr-node itr1) (__assoc-rev-itr-node itr2))
  itr1)

(defmethod operator_clone ((itr set_const_reverse_iterator))
  (make-instance 'set_const_reverse_iterator :node (__assoc-rev-itr-node itr)))

(defmethod operator_== ((itr1 set_const_reverse_iterator)
						(itr2 set_const_reverse_iterator))
  (eq (__assoc-rev-itr-node itr1) (__assoc-rev-itr-node itr2)))

(defmethod operator_/= ((itr1 set_const_reverse_iterator)
						(itr2 set_const_reverse_iterator))
  (not (eq (__assoc-rev-itr-node itr1) (__assoc-rev-itr-node itr2))))

(defmethod operator_* ((itr set_const_reverse_iterator))
  (__rbnode-value (__assoc-rev-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr set_const_reverse_iterator))
  (error 'setf-to-const :what "setf to (_* set_const_reverse_iterator)."))

(defmethod operator_++ ((itr set_const_reverse_iterator))
  (setf (__assoc-rev-itr-node itr) (__rbnode-decrement (__assoc-rev-itr-node itr)))
  itr)

(defmethod operator_-- ((itr set_const_reverse_iterator))
  (setf (__assoc-rev-itr-node itr) (__rbnode-increment (__assoc-rev-itr-node itr)))
  itr)

(locally (declare (optimize speed))
  (defmethod advance ((itr set_const_reverse_iterator) (n integer))
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
  (defmethod distance ((itr1 set_const_reverse_iterator)
					   (itr2 set_const_reverse_iterator))
	(let ((cnt 0))
	  (declare (type fixnum cnt))
	  (do ((node1 (__assoc-rev-itr-node itr1))
		   (node2 (__assoc-rev-itr-node itr2)))
		  ((eq node1 node2) cnt)
		(incf cnt)
		(setf node1 (__rbnode-decrement node1))))))

(defmethod base ((rev-itr set_const_reverse_iterator))
  (make-instance 'set_const_iterator
				 :node  (__rbnode-increment (__assoc-rev-itr-node rev-itr))))

;; creating reverse iterator.
(define-constructor reverse_iterator ((itr set_const_reverse_iterator))
  (make-instance 'set_const_iterator
				 :node (__rbnode-increment (__assoc-rev-itr-node itr))))


;;------------------------------------------------------------------------------
;;
;; methods for set_reverse_iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_clone ((itr set_reverse_iterator))
  (make-instance 'set_reverse_iterator :node (__assoc-rev-itr-node itr)))

(defmethod operator_cast ((itr set_reverse_iterator)
						  (typename (eql 'set_const_reverse_iterator)))
  (__check-exact-type-of-cast itr 'set_reverse_iterator 'set_const_reverse_iterator)
  (make-instance 'set_const_reverse_iterator :node (__assoc-rev-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr set_reverse_iterator))
  (_= (__rbnode-value (__assoc-rev-itr-node itr)) new-val))

(defmethod base ((rev-itr set_reverse_iterator))
  (make-instance 'set_iterator
				 :node  (__rbnode-increment (__assoc-rev-itr-node rev-itr))))

;; creating reverse iterator.
(define-constructor reverse_iterator ((itr set_reverse_iterator))
  (make-instance 'set_iterator
				 :node (__rbnode-increment (__assoc-rev-itr-node itr))))


;;------------------------------------------------------------------------------
;;
;; debug methods for stl::set
;;
;;------------------------------------------------------------------------------
#+cl-stl-debug
(defmethod dump ((container stl::set) &optional (stream t) (value-printer nil))
  (format stream "begin dump ---------------------~%")
  (__rbtree-dump (__assoc-tree container) stream value-printer)
  (format stream "end dump -----------------------~%")
  nil)

#+cl-stl-debug
(defmethod check_integrity ((container stl::set) &optional (stream t))
  (declare (ignorable stream))
  (__rbtree-verify (__assoc-tree container)))

