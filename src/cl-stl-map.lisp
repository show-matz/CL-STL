(in-package :cl-stl)

;;------------------------------------------------------------------------------
;;
;; class difinition
;;
;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  (defclass map (bidirectional_container)
	((rbtree :type     :rbtree
			 :initform nil
			 :initarg  :core
			 :accessor __assoc-tree)
;TMP;	 #+cl-stl-debug
;TMP;	 (id     :type     :symbol
;TMP;			 :initform (gensym "MAP-")
;TMP;			 :initarg  :id
;TMP;			 :accessor __instance-id)
	))

  (defclass map_const_iterator (bidirectional_iterator)
	((node :type     :rbnode
		   :initform nil
		   :initarg  :node
		   :accessor __assoc-itr-node)))

  (defclass map_const_reverse_iterator (bidirectional_iterator)
	((node :type     :rbnode
		   :initform nil
		   :initarg  :node
		   :accessor __assoc-rev-itr-node)))

  (defclass map_iterator (map_const_iterator) ())
  (defclass map_reverse_iterator (map_const_reverse_iterator) ()))


;;--------------------------------------------------------------------
;;
;; internal utilities
;;
;;--------------------------------------------------------------------
#+cl-stl-debug
(locally (declare (optimize speed))
  (defun __map-item-checker (val)
	(unless (typep val 'stl:pair)
	  (error 'type-mismatch :what "Item must be pair."))
	val))

#+cl-stl-debug
(labels ((__map-check-iterator-belong-imp (itr cont)
		   (let* ((tree (__assoc-tree cont))
				  (node (__assoc-itr-node itr)))
			 (if (eq node (__rbtree-end tree))
				 t
				 (let ((val (__rbnode-value node)))
				   (handler-case
					   (and (typep val 'pair)
							(eq node (__rbtree-lower_bound tree (stl:first val))))
					 (error () nil)))))))

  (defun __map-check-iterator-belong (itr cont)
	(unless (__map-check-iterator-belong-imp itr cont)
	  (error 'undefined-behavior :what "Not a iterator of container.")))
		
  (defun __map-check-iterator-range (cont itr1 itr2)
	(let* ((tree  (__assoc-tree cont))
		   (nodeZ (__rbtree-end tree))
		   (node1 (__assoc-itr-node itr1))
		   (node2 (__assoc-itr-node itr2)))
	  (if (eq node2 nodeZ)
		  (if (or (eq node1 nodeZ)
				  (__map-check-iterator-belong-imp itr1 cont))
			  t
			  (error 'undefined-behavior :what "Invalid iterator range."))
		  (if (eq node1 nodeZ)
			  (error 'undefined-behavior :what "Invalid iterator range.")
			  (if (handler-case
					  (let ((key1 (stl:first (__rbnode-value node1)))
							(key2 (stl:first (__rbnode-value node2))))
						(and (eq node1 (__rbtree-lower_bound tree key1))
							 (eq node2 (__rbtree-lower_bound tree key2))
							 (not (funcall (key_comp cont) key2 key1))))
					(error () nil))
				  t
				  (error 'undefined-behavior :what "Invalid iterator range.")))))))


;;--------------------------------------------------------------------
;;
;; method implementation
;;
;;--------------------------------------------------------------------
(locally (declare (optimize speed))

  (defun __create-map (key_comp)
	;; MEMO : key_comp copy in __rbtree-ctor.
	(let ((tree (__rbtree-ctor key_comp #'stl:first)))
	  #+cl-stl-debug (setf (__rbtree-checker tree) #'__map-item-checker)
	  (make-instance 'stl::map :core tree)))

  (defun __create-map-with-range (key_comp itr1 itr2)
	;; MEMO : key_comp copy in __rbtree-ctor.
	;; MEMO : [itr1, itr2) is 'input_iterator'...
	(let ((tree (__rbtree-ctor key_comp #'stl:first)))
	  #+cl-stl-debug (setf (__rbtree-checker tree) #'__map-item-checker)
	  (__rbtree-insert-range-unique tree itr1 itr2 t)
	  (make-instance 'stl::map :core tree)))

  (defun __create-map-with-array (key_comp arr idx1 idx2)
	(declare (type cl:vector arr))
	(declare (type fixnum idx1 idx2))
	;; MEMO : key_comp copy in __rbtree-ctor.
	(let ((tree (__rbtree-ctor key_comp #'stl:first)))
	  #+cl-stl-debug (setf (__rbtree-checker tree) #'__map-item-checker)
	  (__rbtree-insert-array-unique tree arr idx1 idx2 t)
	  (make-instance 'stl::map :core tree))))


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
(define-constructor map ((comp cl:function))
  (__create-map comp))

; empty constructor 3
(define-constructor map ((comp #-cl-stl-0x98 functor
							   #+cl-stl-0x98 binary_function))
  (__create-map comp))

; copy constructor
(define-constructor map ((obj stl::map))
  (clone obj))

; constructor with initializer list 1
#-cl-stl-0x98
(define-constructor map ((il initializer_list))
  (declare (type initializer_list il))
  (let ((arr (__initlist-data il)))
	(declare (type simple-vector arr))
	(__create-map-with-array #'operator_< arr 0 (length arr))))

; constructor with initializer list 2
#-cl-stl-0x98
(define-constructor map ((il initializer_list) (comp cl:function))
  (declare (type initializer_list il))
  (let ((arr (__initlist-data il)))
	(declare (type simple-vector arr))
	(__create-map-with-array comp arr 0 (length arr))))

; constructor with initializer list 3
#-cl-stl-0x98
(define-constructor map ((il   initializer_list)
						 (comp #-cl-stl-0x98 functor
							   #+cl-stl-0x98 binary_function))
  (declare (type initializer_list il))
  (let ((arr (__initlist-data il)))
	(declare (type simple-vector arr))
	(__create-map-with-array comp arr 0 (length arr))))

; move constructor
#-cl-stl-0x98
(define-constructor map ((arg remove-reference))
  (let ((cont (funcall (the cl:function (__rm-ref-closure arg)))))
	(__check-type-of-move-constructor cont stl::map)
	(let ((obj (__create-map (key_comp cont))))
	  (__rbtree-swap (__assoc-tree obj) (__assoc-tree cont))
	  obj)))

;; range constructor
(define-constructor map ((itr1 input_iterator) (itr2 input_iterator))
  (__create-map-with-range #'operator_< itr1 itr2))

(define-constructor map ((itr1 input_iterator)
						 (itr2 input_iterator) (comp cl:function))
  (__create-map-with-range comp itr1 itr2))

(define-constructor map ((itr1 input_iterator)
						 (itr2 input_iterator) (comp #-cl-stl-0x98 functor
													 #+cl-stl-0x98 binary_function))
  (__create-map-with-range comp itr1 itr2))

;; range constructor for const-vector-pointer.
(define-constructor map ((ptr1 const-vector-pointer) (ptr2 const-vector-pointer))
  (__pointer-check-iterator-range ptr1 ptr2)
  (__create-map-with-array #'operator_<
							(opr::vec-ptr-buffer ptr1)
							(opr::vec-ptr-index  ptr1)
							(opr::vec-ptr-index  ptr2)))

(define-constructor map ((ptr1 const-vector-pointer)
						 (ptr2 const-vector-pointer) (comp cl:function))
  (__pointer-check-iterator-range ptr1 ptr2)
  (__create-map-with-array comp
							(opr::vec-ptr-buffer ptr1)
							(opr::vec-ptr-index  ptr1)
							(opr::vec-ptr-index  ptr2)))

(define-constructor map ((ptr1 const-vector-pointer)
						 (ptr2 const-vector-pointer) (comp #-cl-stl-0x98 functor
														   #+cl-stl-0x98 binary_function))
  (__pointer-check-iterator-range ptr1 ptr2)
  (__create-map-with-array comp
							(opr::vec-ptr-buffer ptr1)
							(opr::vec-ptr-index  ptr1)
							(opr::vec-ptr-index  ptr2)))


(defmethod operator_clone ((container stl::map))
  (make-instance 'stl::map
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
  
  (defmethod operator_= ((cont1 stl::map) (cont2 stl::map))
	(__rbtree-assign (__assoc-tree cont1) (__assoc-tree cont2))
	cont1)

  #-cl-stl-0x98
  (defmethod operator_move ((cont1 stl::map) (cont2 stl::map))
	(unless (eq cont1 cont2)
	  (let ((tree1 (__assoc-tree cont1))
			(tree2 (__assoc-tree cont2)))
		(__rbtree-clear tree1)
		(__rbtree-swap tree1 tree2)
		(setf (__rbtree-key_comp tree2) (clone (__rbtree-key_comp tree1)))))
	(values cont1 cont2))

  #-cl-stl-0x98
  (defmethod operator_= ((cont stl::map) (il initializer_list))
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

  (defmethod begin ((container stl::map))
	(make-instance 'map_iterator
				   :node (__rbtree-begin (__assoc-tree container))))

  (defmethod end ((container stl::map))
	(make-instance 'map_iterator
				   :node (__rbtree-end (__assoc-tree container))))

  (defmethod rbegin ((container stl::map))
	(make-instance 'map_reverse_iterator
				   :node (__rbtree-rbegin (__assoc-tree container))))

  (defmethod rend ((container stl::map))
	(make-instance 'map_reverse_iterator
				   :node (__rbtree-rend (__assoc-tree container))))

  #-cl-stl-0x98
  (defmethod cbegin ((container stl::map))
	(make-instance 'map_const_iterator
				   :node (__rbtree-begin (__assoc-tree container))))

  #-cl-stl-0x98
  (defmethod cend ((container stl::map))
	(make-instance 'map_const_iterator
				   :node (__rbtree-end (__assoc-tree container))))

  #-cl-stl-0x98
  (defmethod crbegin ((container stl::map))
	(make-instance 'map_const_reverse_iterator
				   :node (__rbtree-rbegin (__assoc-tree container))))

  #-cl-stl-0x98
  (defmethod crend ((container stl::map))
	(make-instance 'map_const_reverse_iterator
				   :node (__rbtree-rend (__assoc-tree container)))))


;;----------------------------------------------------------
;; capacity
;;----------------------------------------------------------
(locally (declare (optimize speed))

  (defmethod empty ((container stl::map))
	(zerop (__rbtree-size (__assoc-tree container))))

  (defmethod size ((container stl::map))
	(__rbtree-size (__assoc-tree container)))

  (defmethod max_size ((container stl::map))
	(__rbtree-max_size (__assoc-tree container))))

;;----------------------------------------------------------
;; element access
;;----------------------------------------------------------
(locally (declare (optimize speed))

  (defmethod at ((cont stl::map) key)
	(let ((tree (__assoc-tree cont)))
	  (declare (type rbtree tree))
	  (let ((key-cmp (__rbtree-key_comp tree))
			(node    (__rbtree-lower_bound tree key)))
		(declare (type cl:function key-cmp))
		(declare (type rbnode      node))
		(when (or (eq node (__rbtree-end tree))
				  (funcall key-cmp key (stl:first (__rbnode-value node))))
		  (error 'out_of_range :what "(stl:at stl::map key)"))
		(stl:second (__rbnode-value node)))))

  (defmethod (setf at) (val (cont stl::map) key)
	(let ((tree (__assoc-tree cont)))
	  (declare (type rbtree tree))
	  (let ((key-cmp (__rbtree-key_comp tree))
			(node    (__rbtree-lower_bound tree key)))
		(declare (type cl:function key-cmp))
		(declare (type rbnode      node))
		(when (or (eq node (__rbtree-end tree))
				  (funcall key-cmp key (stl:first (__rbnode-value node))))
		  (error 'out_of_range :what "(stl:at stl::map key)"))
		(_= (stl:second (__rbnode-value node)) val))))

  (defmethod operator_[] ((cont stl::map) key)
	(let ((tree (__assoc-tree cont)))
	  (declare (type rbtree tree))
	  (let ((key-cmp (__rbtree-key_comp tree))
			(node    (__rbtree-lower_bound tree key)))
		(declare (type cl:function key-cmp))
		(declare (type rbnode      node))
		(when (or (eq node (__rbtree-end tree))
				  (funcall key-cmp key (stl:first (__rbnode-value node))))
		  (setf node
				#+cl-stl-0x98 (__rbtree-insert-hint-unique  tree node (stl:make_pair key nil) nil)
				#-cl-stl-0x98 (__rbtree-emplace_hint-unique tree node (stl:make_pair key nil))))
		(stl:second (__rbnode-value node)))))

  (defmethod (setf operator_[]) (val (cont stl::map) key)
	(let ((tree (__assoc-tree cont)))
	  (declare (type rbtree tree))
	  (let ((key-cmp (__rbtree-key_comp tree))
			(node    (__rbtree-lower_bound tree key)))
		(declare (type cl:function key-cmp))
		(declare (type rbnode      node))
		(when (or (eq node (__rbtree-end tree))
				  (funcall key-cmp key (stl:first (__rbnode-value node))))
		  (setf node
				#+cl-stl-0x98 (__rbtree-insert-hint-unique  tree node (stl:make_pair key nil) nil)
				#-cl-stl-0x98 (__rbtree-emplace_hint-unique tree node (stl:make_pair key nil))))
		(_= (stl:second (__rbnode-value node)) val))))

  (defmethod operator_& ((cont stl::map) key)
	(_& (_[] cont key)))

  (defmethod operator_const& ((cont stl::map) key)
	(const_& (_[] cont key))))


;;----------------------------------------------------------
;; modifiers
;;----------------------------------------------------------
(locally (declare (optimize speed))

  ;; insert ( single element ) - returns pair<iterator,bool>.
  (defmethod-overload insert ((container stl::map) value)
	(multiple-value-bind (node success)
		(__rbtree-insert-unique (__assoc-tree container) value t)
	  (make_pair (make-instance 'map_iterator :node node) success)))

  ;; insert ( single element by remove reference ) - returns pair<iterator,bool>.
  #-cl-stl-0x98
  (defmethod-overload insert ((container stl::map) (rm remove-reference))
	(let ((val (funcall (the cl:function (__rm-ref-closure rm)))))
	  (funcall (the cl:function (__rm-ref-closure rm)) nil)
	  (multiple-value-bind (node success)
		  (__rbtree-insert-unique (__assoc-tree container) val nil)
		(make_pair (make-instance 'map_iterator :node node) success))))

  ;; insert ( single element with hint ) - returns iterator.
  (defmethod-overload insert ((container stl::map)
							  (itr #+cl-stl-0x98 map_iterator
								   #-cl-stl-0x98 map_const_iterator) value)
	#+cl-stl-0x98  ;; HACK
	(when (and (typep itr   'map_const_iterator)
			   (typep value 'map_const_iterator))
	  (__rbtree-insert-range-unique (__assoc-tree container) itr value t)
	  (return-from __insert-3 nil))
	
	#+cl-stl-debug (__map-check-iterator-belong itr container)
	(make-instance 'map_iterator
				   :node (__rbtree-insert-hint-unique (__assoc-tree container)
													  (__assoc-itr-node itr) value t)))

  ;; insert ( single element with hint by remove reference ) - returns iterator.
  #-cl-stl-0x98
  (defmethod-overload insert ((container stl::map)
							  (itr map_const_iterator) (rm remove-reference))
	#+cl-stl-debug (__map-check-iterator-belong itr container)
	(let ((val (funcall (the cl:function (__rm-ref-closure rm)))))
	  (funcall (the cl:function (__rm-ref-closure rm)) nil)
	  (make-instance 'map_iterator
					 :node (__rbtree-insert-hint-unique (__assoc-tree container)
														(__assoc-itr-node itr) val nil))))

  ;; insert ( initializer list ) - returns nil.
  #-cl-stl-0x98
  (defmethod-overload insert ((container stl::map) (il initializer_list))
	(declare (type initializer_list il))
	(let* ((arr (__initlist-data il))
		   (cnt (length arr)))
	  (declare (type simple-vector arr))
	  (declare (type fixnum cnt))
	  (__rbtree-insert-array-unique (__assoc-tree container) arr 0 cnt t)
	  nil)))

;; range insert - returns nil.
(locally (declare (optimize speed))

  (defmethod-overload insert ((container stl::map) (itr1 input_iterator) (itr2 input_iterator))
	(__rbtree-insert-range-unique (__assoc-tree container) itr1 itr2 t)
	nil)

  (defmethod-overload insert ((container stl::map)
							  (itr1 map_const_iterator) (itr2 map_const_iterator))
	(__rbtree-insert-range-unique (__assoc-tree container) itr1 itr2 t)
	nil)

  (defmethod-overload insert ((container stl::map) (ptr1 const-vector-pointer) (ptr2 const-vector-pointer))
	(__pointer-check-iterator-range ptr1 ptr2)
	(__rbtree-insert-array-unique (__assoc-tree container)
								  (opr::vec-ptr-buffer ptr1)
								  (opr::vec-ptr-index  ptr1)
								  (opr::vec-ptr-index  ptr2) t)
	nil))


;; emplace
#-cl-stl-0x98
(locally (declare (optimize speed))

  ;;returns pair<iterator,bool>.
  (defmethod-overload emplace ((container stl::map) new-val)
	(multiple-value-bind (node success)
		(__rbtree-emplace-unique (__assoc-tree container) new-val)
	  (make_pair (make-instance 'map_iterator :node node) success)))

  ;;returns iterator
  (defmethod-overload emplace_hint ((container stl::map)
									(itr map_const_iterator) new-val)
	#+cl-stl-debug (__map-check-iterator-belong itr container)
	(make-instance 'map_iterator
				   :node (__rbtree-emplace_hint-unique (__assoc-tree container)
													   (__assoc-itr-node itr) new-val))))


;;erase
(locally (declare (optimize speed))

  ;; In 0x98, returns nil. In 0x11 returns iterator.
  (defmethod-overload erase ((container stl::map)
							 (itr #+cl-stl-0x98 map_iterator
								  #-cl-stl-0x98 map_const_iterator))
	#+cl-stl-debug (__map-check-iterator-belong itr container)
	(let ((node (__rbtree-erase-node (__assoc-tree container) (__assoc-itr-node itr))))
	  (declare (ignorable node))
	  #+cl-stl-0x98 nil
	  #-cl-stl-0x98 (make-instance 'map_iterator :node node)))

  ;; In 0x98, returns nil. In 0x11 returns iterator.
  (defmethod-overload erase ((container stl::map)
							 (first #+cl-stl-0x98 map_iterator #-cl-stl-0x98 map_const_iterator)
							 (last  #+cl-stl-0x98 map_iterator #-cl-stl-0x98 map_const_iterator))
	#+cl-stl-debug (__map-check-iterator-range  container first last)
	(let ((node (__rbtree-erase-range (__assoc-tree container)
									  (__assoc-itr-node first) (__assoc-itr-node last))))
	  (declare (ignorable node))
	  #+cl-stl-0x98 nil
	  #-cl-stl-0x98 (make-instance 'map_iterator :node node)))

  ;; returns deleted node count.
  (defmethod-overload erase ((container stl::map) key)
	(__rbtree-erase-key (__assoc-tree container) key)))



(defmethod-overload swap ((cont1 stl::map) (cont2 stl::map))
  (__rbtree-swap (__assoc-tree cont1) (__assoc-tree cont2))
  (values cont1 cont2))

(defmethod clear ((container stl::map))
  (__rbtree-clear (__assoc-tree container))
  nil)

;;----------------------------------------------------------
;; specific operations
;;----------------------------------------------------------
(locally (declare (optimize speed))

  ;; returns iterator.
  (defmethod-overload find ((container stl::map) key)
	(make-instance 'map_iterator
				   :node (__rbtree-find (__assoc-tree container) key)))
  
  ;; returns fixnum.
  (defmethod-overload count ((container stl::map) key)
	(__rbtree-count (__assoc-tree container) key))

  ;; returns iterator.
  (defmethod-overload lower_bound ((container stl::map) key)
	(make-instance 'map_iterator
				   :node (__rbtree-lower_bound (__assoc-tree container) key)))

  ;; returns iterator.
  (defmethod-overload upper_bound ((container stl::map) key)
	(make-instance 'map_iterator
				   :node (__rbtree-upper_bound (__assoc-tree container) key)))

  ;; returns pair(itr,itr).
  (defmethod-overload equal_range ((container stl::map) key)
	(let ((tree (__assoc-tree container)))
	  (make_pair (make-instance 'map_iterator
								:node (__rbtree-lower_bound tree key))
				 (make-instance 'map_iterator
								:node (__rbtree-upper_bound tree key))))))


;;----------------------------------------------------------
;; observers
;;----------------------------------------------------------
(locally (declare (optimize speed))

  (defmethod key_comp ((container stl::map))
	(clone (__rbtree-key_comp (__assoc-tree container))))

  (defmethod value_comp ((container stl::map))
	(let ((fnc (functor_function (clone (__rbtree-key_comp (__assoc-tree container))))))
	  (declare (type cl:function fnc))
	  (lambda (pr1 pr2)
		(funcall fnc (stl:first pr1) (stl:first pr2))))))


;;----------------------------------------------------------
;; compare
;;----------------------------------------------------------
(locally (declare (optimize speed))

  (defmethod operator_== ((cont1 stl::map) (cont2 stl::map))
	(__rbtree-equal (__assoc-tree cont1) (__assoc-tree cont2) #'operator_==))

  (defmethod operator_/= ((cont1 stl::map) (cont2 stl::map))
	(not (__rbtree-equal (__assoc-tree cont1) (__assoc-tree cont2) #'operator_==)))

  (defmethod operator_< ((cont1 stl::map) (cont2 stl::map))
	(__rbtree-less (__assoc-tree cont1) (__assoc-tree cont2) #'operator_<))

  (defmethod operator_<= ((cont1 stl::map) (cont2 stl::map))
	(not (__rbtree-less (__assoc-tree cont2) (__assoc-tree cont1) #'operator_<)))

  (defmethod operator_> ((cont1 stl::map) (cont2 stl::map))
	(__rbtree-less (__assoc-tree cont2) (__assoc-tree cont1) #'operator_<))

  (defmethod operator_>= ((cont1 stl::map) (cont2 stl::map))
	(not (__rbtree-less (__assoc-tree cont1) (__assoc-tree cont2) #'operator_<))))



;;----------------------------------------------------------
;; enumeration
;;----------------------------------------------------------
#-cl-stl-0x98
(defmethod-overload for ((cont stl::map) func)
  ;MEMO : func is always lambda function ( see stl:for ). 
  (__rbtree-enumerate (__assoc-tree cont) func))


;;------------------------------------------------------------------------------
;;
;; methods for map_const_iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_= ((itr1 map_const_iterator) (itr2 map_const_iterator))
  (__error-when-const-removing-assign itr1 map_iterator
									  itr2 map_const_iterator)
  (setf (__assoc-itr-node itr1) (__assoc-itr-node itr2))
  itr1)

(defmethod operator_clone ((itr map_const_iterator))
  (make-instance 'map_const_iterator :node (__assoc-itr-node itr)))

(defmethod operator_== ((itr1 map_const_iterator) (itr2 map_const_iterator))
  (eq (__assoc-itr-node itr1) (__assoc-itr-node itr2)))

(defmethod operator_/= ((itr1 map_const_iterator) (itr2 map_const_iterator))
  (not (eq (__assoc-itr-node itr1) (__assoc-itr-node itr2))))

(defmethod operator_* ((itr map_const_iterator))
  (__rbnode-value (__assoc-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr map_const_iterator))
  (error 'setf-to-const :what "setf to (_* map_const_iterator)."))

(defmethod operator_++ ((itr map_const_iterator))
  (setf (__assoc-itr-node itr) (__rbnode-increment (__assoc-itr-node itr)))
  itr)

(defmethod operator_-- ((itr map_const_iterator))
  (setf (__assoc-itr-node itr) (__rbnode-decrement (__assoc-itr-node itr)))
  itr)

(locally (declare (optimize speed))
  (defmethod advance ((itr map_const_iterator) (n integer))
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
  (defmethod distance ((itr1 map_const_iterator) (itr2 map_const_iterator))
	(let ((cnt 0))
	  (declare (type fixnum cnt))
	  (do ((node1 (__assoc-itr-node itr1))
		   (node2 (__assoc-itr-node itr2)))
		  ((eq node1 node2) cnt)
		(incf cnt)
		(setf node1 (__rbnode-increment node1))))))

;; creating reverse iterator.
(define-constructor reverse_iterator ((itr map_const_iterator))
  (make-instance 'map_const_reverse_iterator
				 :node (__rbnode-decrement (__assoc-itr-node itr))))


;;------------------------------------------------------------------------------
;;
;; methods for map_iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_clone ((itr map_iterator))
  (make-instance 'map_iterator :node (__assoc-itr-node itr)))

(defmethod operator_cast ((itr map_iterator)
						  (typename (eql 'map_const_iterator)))
  (__check-exact-type-of-cast itr 'map_iterator 'map_const_iterator)
  (make-instance 'map_const_iterator :node (__assoc-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr map_iterator))
  (_= (__rbnode-value (__assoc-itr-node itr)) new-val))

;; creating reverse iterator.
(define-constructor reverse_iterator ((itr map_iterator))
  (make-instance 'map_reverse_iterator
				 :node (__rbnode-decrement (__assoc-itr-node itr))))



;;------------------------------------------------------------------------------
;;
;; methods for map_const_reverse_iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_= ((itr1 map_const_reverse_iterator)
					   (itr2 map_const_reverse_iterator))
  (__error-when-const-removing-assign itr1 map_reverse_iterator
									  itr2 map_const_reverse_iterator)
  (setf (__assoc-rev-itr-node itr1) (__assoc-rev-itr-node itr2))
  itr1)

(defmethod operator_clone ((itr map_const_reverse_iterator))
  (make-instance 'map_const_reverse_iterator :node (__assoc-rev-itr-node itr)))

(defmethod operator_== ((itr1 map_const_reverse_iterator)
						(itr2 map_const_reverse_iterator))
  (eq (__assoc-rev-itr-node itr1) (__assoc-rev-itr-node itr2)))

(defmethod operator_/= ((itr1 map_const_reverse_iterator)
						(itr2 map_const_reverse_iterator))
  (not (eq (__assoc-rev-itr-node itr1) (__assoc-rev-itr-node itr2))))

(defmethod operator_* ((itr map_const_reverse_iterator))
  (__rbnode-value (__assoc-rev-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr map_const_reverse_iterator))
  (error 'setf-to-const :what "setf to (_* map_const_reverse_iterator)."))

(defmethod operator_++ ((itr map_const_reverse_iterator))
  (setf (__assoc-rev-itr-node itr) (__rbnode-decrement (__assoc-rev-itr-node itr)))
  itr)

(defmethod operator_-- ((itr map_const_reverse_iterator))
  (setf (__assoc-rev-itr-node itr) (__rbnode-increment (__assoc-rev-itr-node itr)))
  itr)

(locally (declare (optimize speed))
  (defmethod advance ((itr map_const_reverse_iterator) (n integer))
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
  (defmethod distance ((itr1 map_const_reverse_iterator)
					   (itr2 map_const_reverse_iterator))
	(let ((cnt 0))
	  (declare (type fixnum cnt))
	  (do ((node1 (__assoc-rev-itr-node itr1))
		   (node2 (__assoc-rev-itr-node itr2)))
		  ((eq node1 node2) cnt)
		(incf cnt)
		(setf node1 (__rbnode-decrement node1))))))

(defmethod base ((rev-itr map_const_reverse_iterator))
  (make-instance 'map_const_iterator
				 :node  (__rbnode-increment (__assoc-rev-itr-node rev-itr))))

;; creating reverse iterator.
(define-constructor reverse_iterator ((itr map_const_reverse_iterator))
  (make-instance 'map_const_iterator
				 :node (__rbnode-increment (__assoc-rev-itr-node itr))))


;;------------------------------------------------------------------------------
;;
;; methods for map_reverse_iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_clone ((itr map_reverse_iterator))
  (make-instance 'map_reverse_iterator :node (__assoc-rev-itr-node itr)))

(defmethod operator_cast ((itr map_reverse_iterator)
						  (typename (eql 'map_const_reverse_iterator)))
  (__check-exact-type-of-cast itr 'map_reverse_iterator 'map_const_reverse_iterator)
  (make-instance 'map_const_reverse_iterator :node (__assoc-rev-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr map_reverse_iterator))
  (_= (__rbnode-value (__assoc-rev-itr-node itr)) new-val))

(defmethod base ((rev-itr map_reverse_iterator))
  (make-instance 'map_iterator
				 :node  (__rbnode-increment (__assoc-rev-itr-node rev-itr))))

;; creating reverse iterator.
(define-constructor reverse_iterator ((itr map_reverse_iterator))
  (make-instance 'map_iterator
				 :node (__rbnode-increment (__assoc-rev-itr-node itr))))


;;------------------------------------------------------------------------------
;;
;; debug methods for stl::map
;;
;;------------------------------------------------------------------------------
#+cl-stl-debug
(defmethod dump ((container stl::map) &optional (stream t) (value-printer nil))
  (format stream "begin dump ---------------------~%")
  (__rbtree-dump (__assoc-tree container) stream value-printer)
  (format stream "end dump -----------------------~%")
  nil)

#+cl-stl-debug
(defmethod check_integrity ((container stl::map) &optional (stream t))
  (declare (ignorable stream))
  (__rbtree-verify (__assoc-tree container)))

