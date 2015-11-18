(in-package :cl-stl)

;;------------------------------------------------------------------------------
;;
;; class difinition
;;
;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  (defclass set (bidirectional-container)
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

  (defclass set-const-iterator (bidirectional-iterator)
	((node :type     :rbnode
		   :initform nil
		   :initarg  :node
		   :accessor __assoc-itr-node)))

  (defclass set-const-reverse-iterator (bidirectional-iterator)
	((node :type     :rbnode
		   :initform nil
		   :initarg  :node
		   :accessor __assoc-rev-itr-node)))

  (defclass set-iterator (set-const-iterator) ())
  (defclass set-reverse-iterator (set-const-reverse-iterator) ()))


;;--------------------------------------------------------------------
;;
;; internal utilities
;;
;;--------------------------------------------------------------------
(defmacro __set-check-iterator-belong (itr cont)
  (declare (ignore itr cont))
  nil)
;;  #-cl-stl-debug nil
;;  #+cl-stl-debug
;;  (let ((g-node (gensym "NODE")))
;;	`(unless (_== ,itr (stl:end ,cont))
;;	   (let ((,g-node (set-itr-node ,itr)))
;;		 (unless (eq ,g-node (__rbtree-find (__assoc-tree ,cont) (__rbnode-value ,g-node)))
;;		   (error 'undefined-behavior :what ,(format nil "~A is not iterator of ~A." itr cont)))))))

;;YET : (defmacro __set-check-iterator-range (itr1 itr2)
;;YET :   (declare (ignorable itr1 itr2))
;;YET :   #-cl-stl-debug nil
;;YET :   #+cl-stl-debug
;;YET :   `(unless (__rbnode-check-reachable (set-itr-node ,itr1) (set-itr-node ,itr2))
;;YET : 	 (error 'undefined-behavior :what ,(format nil "[~A ~A) is not valid range." itr1 itr2))))


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
							   #+cl-stl-0x98 binary-function))
  (__create-set comp))

; copy constructor
(define-constructor set ((obj stl::set))
  (clone obj))

; constructor with initializer list 1
#-cl-stl-0x98
(define-constructor set ((il initializer-list))
  (declare (type initializer-list il))
  (let ((arr (__initlist-data il)))
	(__create-set-with-array #'operator_< arr 0 (length arr))))

; constructor with initializer list 2
#-cl-stl-0x98
(define-constructor set ((il initializer-list) (comp cl:function))
  (declare (type initializer-list il))
  (let ((arr (__initlist-data il)))
	(__create-set-with-array comp arr 0 (length arr))))

; constructor with initializer list 3
#-cl-stl-0x98
(define-constructor set ((il   initializer-list)
						 (comp #-cl-stl-0x98 functor
							   #+cl-stl-0x98 binary-function))
  (declare (type initializer-list il))
  (let ((arr (__initlist-data il)))
	(__create-set-with-array comp arr 0 (length arr))))

; move constructor
#-cl-stl-0x98
(define-constructor set ((arg remove-reference))
  (let ((cont (funcall (__rm-ref-closure arg))))
	(__check-type-of-move-constructor cont stl::set)
	(let ((obj (__create-set (key-comp cont))))
	  (__rbtree-swap (__assoc-tree obj) (__assoc-tree cont))
	  obj)))

; range constructor
(define-constructor set ((itr1 input-iterator) (itr2 input-iterator))
  (__create-set-with-range #'operator_< itr1 itr2))

(define-constructor set ((itr1 input-iterator)
						 (itr2 input-iterator) (comp cl:function))
  (__create-set-with-range comp itr1 itr2))

(define-constructor set ((itr1 input-iterator)
						 (itr2 input-iterator) (comp #-cl-stl-0x98 functor
													 #+cl-stl-0x98 binary-function))
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
														   #+cl-stl-0x98 binary-function))
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
		(setf (__rbtree-key-comp tree2) (clone (__rbtree-key-comp tree1)))))
	(values cont1 cont2))

  #-cl-stl-0x98
  (defmethod operator_= ((cont stl::set) (il initializer-list))
	(declare (type initializer-list il))
	(let ((tree (__assoc-tree cont))
		  (arr  (__initlist-data il)))
	  (declare (type rbtree tree))
	  (declare (type cl:vector arr))
	  (__rbtree-clear tree)
	  (__rbtree-insert-array-unique tree arr 0 (length arr) t))
	cont))


;;----------------------------------------------------------
;; iterators
;;----------------------------------------------------------
(locally (declare (optimize speed))

  (defmethod begin ((container stl::set))
	(make-instance 'set-iterator
				   :node (__rbtree-begin (__assoc-tree container))))

  (defmethod end ((container stl::set))
	(make-instance 'set-iterator
				   :node (__rbtree-end (__assoc-tree container))))

  (defmethod rbegin ((container stl::set))
	(make-instance 'set-reverse-iterator
				   :node (__rbtree-rbegin (__assoc-tree container))))

  (defmethod rend ((container stl::set))
	(make-instance 'set-reverse-iterator
				   :node (__rbtree-rend (__assoc-tree container))))

  #-cl-stl-0x98
  (defmethod cbegin ((container stl::set))
	(make-instance 'set-const-iterator
				   :node (__rbtree-begin (__assoc-tree container))))

  #-cl-stl-0x98
  (defmethod cend ((container stl::set))
	(make-instance 'set-const-iterator
				   :node (__rbtree-end (__assoc-tree container))))

  #-cl-stl-0x98
  (defmethod crbegin ((container stl::set))
	(make-instance 'set-const-reverse-iterator
				   :node (__rbtree-rbegin (__assoc-tree container))))

  #-cl-stl-0x98
  (defmethod crend ((container stl::set))
	(make-instance 'set-const-reverse-iterator
				   :node (__rbtree-rend (__assoc-tree container)))))


;;----------------------------------------------------------
;; capacity
;;----------------------------------------------------------
(locally (declare (optimize speed))

  (defmethod empty ((container stl::set))
	(zerop (__rbtree-size (__assoc-tree container))))

  (defmethod size ((container stl::set))
	(__rbtree-size (__assoc-tree container)))

  (defmethod max-size ((container stl::set))
	(__rbtree-max-size (__assoc-tree container))))


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
	  (make-pair (make-instance 'set-iterator :node node) success)))

  ;; insert ( single element by remove reference ) - returns pair<iterator,bool>.
  #-cl-stl-0x98
  (defmethod-overload insert ((container stl::set) (rm remove-reference))
	(let ((val (funcall (__rm-ref-closure rm))))
	  (funcall (__rm-ref-closure rm) nil)
	  (multiple-value-bind (node success)
		  (__rbtree-insert-unique (__assoc-tree container) val nil)
		(make-pair (make-instance 'set-iterator :node node) success))))

  ;; insert ( single element with hint ) - returns iterator.
  (defmethod-overload insert ((container stl::set)
							  (itr #+cl-stl-0x98 set-iterator
								   #-cl-stl-0x98 set-const-iterator) value)
	#+cl-stl-0x98  ;; HACK
	(when (and (typep itr   'set-const-iterator)
			   (typep value 'set-const-iterator))
	  (__rbtree-insert-range-unique (__assoc-tree container) itr value t)
	  (return-from __insert-3 nil))
	
	(__set-check-iterator-belong itr container)
	(make-instance 'set-iterator
				   :node (__rbtree-insert-hint-unique (__assoc-tree container)
													  (__assoc-itr-node itr) value t)))

  ;; insert ( single element with hint by remove reference ) - returns iterator.
  #-cl-stl-0x98
  (defmethod-overload insert ((container stl::set)
							  (itr set-const-iterator) (rm remove-reference))
	(__set-check-iterator-belong itr container)
	(let ((val (funcall (__rm-ref-closure rm))))
	  (funcall (__rm-ref-closure rm) nil)
	  (make-instance 'set-iterator
					 :node (__rbtree-insert-hint-unique (__assoc-tree container)
														(__assoc-itr-node itr) val nil))))

  ;; insert ( initializer list ) - returns nil.
  #-cl-stl-0x98
  (defmethod-overload insert ((container stl::set) (il initializer-list))
	(declare (type initializer-list il))
	(let ((arr (__initlist-data il)))
	  (__rbtree-insert-array-unique (__assoc-tree container) arr 0 (length arr) t)
	  nil)))


;; range insert - returns nil.
(locally (declare (optimize speed))

  (defmethod-overload insert ((container stl::set) (itr1 input-iterator) (itr2 input-iterator))
	(__rbtree-insert-range-unique (__assoc-tree container) itr1 itr2 t)
	nil)

  (defmethod-overload insert ((container stl::set) (itr1 set-const-iterator) (itr2 set-const-iterator))
	(__rbtree-insert-range-unique (__assoc-tree container) itr1 itr2 t)
	nil)

  (defmethod-overload insert ((container stl::set) (ptr1 const-vector-pointer) (ptr2 const-vector-pointer))
	(__pointer-check-iterator-range ptr1 ptr2)
	(let ((arr (opr::vec-ptr-buffer ptr1)))
	  (declare (type cl:vector arr))
	  (__rbtree-insert-array-unique (__assoc-tree container) arr 0 (length arr) t)
	nil)))

;; emplace
#-cl-stl-0x98
(locally (declare (optimize speed))

  ;;returns pair<iterator, bool>.
  (defmethod-overload emplace ((container stl::set) new-val)
	(multiple-value-bind (node success)
		(__rbtree-emplace-unique (__assoc-tree container) new-val)
	  (make-pair (make-instance 'set-iterator :node node) success)))

  ;;returns iterator.
  (defmethod-overload emplace-hint ((container stl::set)
									(itr set-const-iterator) new-val)
	(__set-check-iterator-belong itr container)
	(make-instance 'set-iterator
				   :node (__rbtree-emplace-hint-unique (__assoc-tree container)
													   (__assoc-itr-node itr) new-val))))

;;erase
(locally (declare (optimize speed))

  ;; In 0x98, returns nil. In 0x11 returns iterator.
  (defmethod-overload erase ((container stl::set)
							 (itr #+cl-stl-0x98 set-iterator
								  #-cl-stl-0x98 set-const-iterator))
	(__set-check-iterator-belong itr container)
	(let ((node (__rbtree-erase-node (__assoc-tree container) (__assoc-itr-node itr))))
	  (declare (ignorable node))
	  #+cl-stl-0x98 nil
	  #-cl-stl-0x98 (make-instance 'set-iterator :node node)))

  ;; In 0x98, returns nil. In 0x11 returns iterator.
  (defmethod-overload erase ((container stl::set)
							 (first #+cl-stl-0x98 set-iterator	#-cl-stl-0x98 set-const-iterator)
							 (last  #+cl-stl-0x98 set-iterator	#-cl-stl-0x98 set-const-iterator))
	(__set-check-iterator-belong first container)
	;;(__set-check-iterator-range  first last)         ;ToDo :
	(let ((node (__rbtree-erase-range (__assoc-tree container)
									  (__assoc-itr-node first) (__assoc-itr-node last))))
	  (declare (ignorable node))
	  #+cl-stl-0x98 nil
	  #-cl-stl-0x98 (make-instance 'set-iterator :node node)))

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
	(make-instance 'set-iterator
				   :node (__rbtree-find (__assoc-tree container) key)))

  ;; returns integer.
  (defmethod-overload count ((container stl::set) key)
	(__rbtree-count (__assoc-tree container) key))

  ;; returns iterator.
  (defmethod-overload lower-bound ((container stl::set) key)
	(make-instance 'set-iterator
				   :node (__rbtree-lower-bound (__assoc-tree container) key)))

  ;; returns iterator.
  (defmethod-overload upper-bound ((container stl::set) key)
	(make-instance 'set-iterator
				   :node (__rbtree-upper-bound (__assoc-tree container) key)))

  ;; returns pair(itr,itr).
  (defmethod-overload equal-range ((container stl::set) key)
	(let ((tree (__assoc-tree container)))
	  (make-pair (make-instance 'set-iterator
								:node (__rbtree-lower-bound tree key))
				 (make-instance 'set-iterator
								:node (__rbtree-upper-bound tree key))))))


;;----------------------------------------------------------
;; observers
;;----------------------------------------------------------
(locally (declare (optimize speed))

  (defmethod key-comp ((container stl::set))
	(clone (__rbtree-key-comp (__assoc-tree container))))

  (defmethod value-comp ((container stl::set))
	(clone (__rbtree-key-comp (__assoc-tree container)))))


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
;; methods for set-const-iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_= ((itr1 set-const-iterator) (itr2 set-const-iterator))
  (__error-when-const-removing-assign itr1 set-iterator
									  itr2 set-const-iterator)
  (setf (__assoc-itr-node itr1) (__assoc-itr-node itr2))
  itr1)

(defmethod operator_clone ((itr set-const-iterator))
  (make-instance 'set-const-iterator :node (__assoc-itr-node itr)))

(defmethod operator_== ((itr1 set-const-iterator) (itr2 set-const-iterator))
  (eq (__assoc-itr-node itr1) (__assoc-itr-node itr2)))

(defmethod operator_/= ((itr1 set-const-iterator) (itr2 set-const-iterator))
  (not (eq (__assoc-itr-node itr1) (__assoc-itr-node itr2))))

(defmethod operator_* ((itr set-const-iterator))
  (__rbnode-value (__assoc-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr set-const-iterator))
  (error 'setf-to-const :what "setf to (_* set-const-iterator)."))

(defmethod operator_++ ((itr set-const-iterator))
  (setf (__assoc-itr-node itr) (__rbnode-increment (__assoc-itr-node itr)))
  itr)

(defmethod operator_-- ((itr set-const-iterator))
  (setf (__assoc-itr-node itr) (__rbnode-decrement (__assoc-itr-node itr)))
  itr)

(locally (declare (optimize speed))
  (defmethod advance ((itr set-const-iterator) (n integer))
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
  (defmethod distance ((itr1 set-const-iterator) (itr2 set-const-iterator))
	(let ((cnt 0))
	  (declare (type fixnum cnt))
	  (do ((node1 (__assoc-itr-node itr1))
		   (node2 (__assoc-itr-node itr2)))
		  ((eq node1 node2) cnt)
		(incf cnt)
		(setf node1 (__rbnode-increment node1))))))

;; creating reverse iterator.
(define-constructor reverse-iterator ((itr set-const-iterator))
  (make-instance 'set-const-reverse-iterator
				 :node (__rbnode-decrement (__assoc-itr-node itr))))


;;------------------------------------------------------------------------------
;;
;; methods for set-iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_clone ((itr set-iterator))
  (make-instance 'set-iterator :node (__assoc-itr-node itr)))

(defmethod operator_cast ((itr set-iterator)
						  (typename (eql 'set-const-iterator)))
  (__check-exact-type-of-cast itr 'set-iterator 'set-const-iterator)
  (make-instance 'set-const-iterator :node (__assoc-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr set-iterator))
  (_= (__rbnode-value (__assoc-itr-node itr)) new-val))

;; creating reverse iterator.
(define-constructor reverse-iterator ((itr set-iterator))
  (make-instance 'set-reverse-iterator
				 :node (__rbnode-decrement (__assoc-itr-node itr))))



;;------------------------------------------------------------------------------
;;
;; methods for set-const-reverse-iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_= ((itr1 set-const-reverse-iterator)
					   (itr2 set-const-reverse-iterator))
  (__error-when-const-removing-assign itr1 set-reverse-iterator
									  itr2 set-const-reverse-iterator)
  (setf (__assoc-rev-itr-node itr1) (__assoc-rev-itr-node itr2))
  itr1)

(defmethod operator_clone ((itr set-const-reverse-iterator))
  (make-instance 'set-const-reverse-iterator :node (__assoc-rev-itr-node itr)))

(defmethod operator_== ((itr1 set-const-reverse-iterator)
						(itr2 set-const-reverse-iterator))
  (eq (__assoc-rev-itr-node itr1) (__assoc-rev-itr-node itr2)))

(defmethod operator_/= ((itr1 set-const-reverse-iterator)
						(itr2 set-const-reverse-iterator))
  (not (eq (__assoc-rev-itr-node itr1) (__assoc-rev-itr-node itr2))))

(defmethod operator_* ((itr set-const-reverse-iterator))
  (__rbnode-value (__assoc-rev-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr set-const-reverse-iterator))
  (error 'setf-to-const :what "setf to (_* set-const-reverse-iterator)."))

(defmethod operator_++ ((itr set-const-reverse-iterator))
  (setf (__assoc-rev-itr-node itr) (__rbnode-decrement (__assoc-rev-itr-node itr)))
  itr)

(defmethod operator_-- ((itr set-const-reverse-iterator))
  (setf (__assoc-rev-itr-node itr) (__rbnode-increment (__assoc-rev-itr-node itr)))
  itr)

(locally (declare (optimize speed))
  (defmethod advance ((itr set-const-reverse-iterator) (n integer))
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
  (defmethod distance ((itr1 set-const-reverse-iterator)
					   (itr2 set-const-reverse-iterator))
	(let ((cnt 0))
	  (declare (type fixnum cnt))
	  (do ((node1 (__assoc-rev-itr-node itr1))
		   (node2 (__assoc-rev-itr-node itr2)))
		  ((eq node1 node2) cnt)
		(incf cnt)
		(setf node1 (__rbnode-decrement node1))))))

(defmethod base ((rev-itr set-const-reverse-iterator))
  (make-instance 'set-const-iterator
				 :node  (__rbnode-increment (__assoc-rev-itr-node rev-itr))))

;; creating reverse iterator.
(define-constructor reverse-iterator ((itr set-const-reverse-iterator))
  (make-instance 'set-const-iterator
				 :node (__rbnode-increment (__assoc-rev-itr-node itr))))


;;------------------------------------------------------------------------------
;;
;; methods for set-reverse-iterator
;;
;;------------------------------------------------------------------------------
(defmethod operator_clone ((itr set-reverse-iterator))
  (make-instance 'set-reverse-iterator :node (__assoc-rev-itr-node itr)))

(defmethod operator_cast ((itr set-reverse-iterator)
						  (typename (eql 'set-const-reverse-iterator)))
  (__check-exact-type-of-cast itr 'set-reverse-iterator 'set-const-reverse-iterator)
  (make-instance 'set-const-reverse-iterator :node (__assoc-rev-itr-node itr)))

(defmethod (setf operator_*) (new-val (itr set-reverse-iterator))
  (_= (__rbnode-value (__assoc-rev-itr-node itr)) new-val))

(defmethod base ((rev-itr set-reverse-iterator))
  (make-instance 'set-iterator
				 :node  (__rbnode-increment (__assoc-rev-itr-node rev-itr))))

;; creating reverse iterator.
(define-constructor reverse-iterator ((itr set-reverse-iterator))
  (make-instance 'set-iterator
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
(defmethod check-integrity ((container stl::set) &optional (stream t))
  (declare (ignorable stream))
  (__rbtree-verify (__assoc-tree container)))

