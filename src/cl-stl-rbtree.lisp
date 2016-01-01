(in-package :cl-stl)

(declaim (inline __rbtree-key-func
				 __rbtree-key-comp
				 __rbtree-node-count
				 __rbtree-header
				 #+cl-stl-debug __rbtree-checker
				 (setf __rbtree-key-func)
				 (setf __rbtree-key-comp)
				 (setf __rbtree-node-count)
				 (setf __rbtree-header)
				 #+cl-stl-debug (setf __rbtree-checker)
				 __rbtree-begin-node
				 __rbtree-end-node
				 __rbtree-begin
				 __rbtree-end
				 __rbtree-rbegin
				 __rbtree-rend))


;;------------------------------------------------------------------------------
;;
;; internal utilities
;;
;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  (defun __rbtree-create-node (val need-copy)
	(let ((v nil))
	  (if need-copy
		  (_= v val)
		  (setf v val))
	  (make-rbnode :value v)))

  (defun __rbtree-destroy-node (node)
	(setf (__rbnode-parent node) nil)
	(setf (__rbnode-left   node) nil)
	(setf (__rbnode-right  node) nil)
	(setf (__rbnode-value  node) nil)
	nil))


;; Erase without rebalancing.
(locally (declare (optimize speed))
  (defun __rbtree-erase-imp (node)
	;;MEMO : node maybe nil.
	(do ()
		((null node))
	  (__rbtree-erase-imp (__rbnode-right node))
	  (setf (__rbnode-right node) nil)
	  (let ((node2 (__rbnode-left node)))
		(setf (__rbnode-left node) nil)
		(setf node node2)))))


;; Structural copy.  node and parent must be non-nil.
(locally (declare (optimize speed))
  (defun __rbtree-copy-imp (node parent)
	(declare (type rbnode parent))    ;MEMO : node may BE nil
	(labels ((clone-node (node)
			   (let ((tmp (__rbtree-create-node (__rbnode-value node) t)))
				 (setf (__rbnode-color tmp) (__rbnode-color node))
				 (setf (__rbnode-left  tmp) nil)
				 (setf (__rbnode-right tmp) nil)
				 tmp)))
	  (let ((top (clone-node node)))
		(setf (__rbnode-parent top) parent)
		(handler-case
			(progn
			  (when (__rbnode-right node)
				(setf (__rbnode-right top) (__rbtree-copy-imp (__rbnode-right node) top)))
			  (setf parent top)
			  (setf node (__rbnode-left node))
			  (do ()
				  ((null node) top)
				(let ((node-y (clone-node node)))
				  (setf (__rbnode-left parent) node-y)
				  (setf (__rbnode-parent node-y) parent)
				  (when (__rbnode-right node)
					(setf (__rbnode-right node-y) (__rbtree-copy-imp (__rbnode-right node) node-y)))
				  (setf parent node-y)
				  (setf node (__rbnode-left node)))))
		  (error (c)
			(__rbtree-erase-imp top)
			(error c)))))))


;;------------------------------------------------------------------------------
;;
;; class difinition
;;
;;------------------------------------------------------------------------------
(locally (declare (optimize speed))
  (defstruct (rbtree (:conc-name __rbtree-))
	(key-func   nil)          ; always cl:function ( ordinary #'identity or #'stl:first )
	(key-comp   nil)          ; functor object.
	(node-count 0             :type integer)
	(header     (make-rbnode) :type rbnode)
	#+cl-stl-debug
	(checker    #'identity    :type cl:function)))



;;------------------------------------------------------------------------------
;;
;; rbtree utilities
;;
;;------------------------------------------------------------------------------
(locally (declare (optimize speed))
  (defun __rbtree-initialize (tree)
	(declare (type rbtree tree))
	(let ((header (__rbtree-header tree)))
	  (declare (type rbnode header))
	  (setf (__rbnode-color  header) :red)
	  (setf (__rbnode-parent header) nil)
	  (setf (__rbnode-left   header) header)
	  (setf (__rbnode-right  header) header))))



(defmacro __rbtree-root (tree)
  `(__rbnode-parent (__rbtree-header ,tree)))

(defmacro __rbtree-leftmost (tree)
  `(__rbnode-left (__rbtree-header ,tree)))

(defmacro __rbtree-rightmost (tree)
  `(__rbnode-right (__rbtree-header ,tree)))

(defmacro __rbtree-empty (tree)
  `(zerop (__rbtree-node-count ,tree)))

(defmacro __rbtree-size (tree)
  `(__rbtree-node-count ,tree))

(defmacro __rbtree-max-size (tree)
  (declare (ignore tree))
  most-positive-fixnum)


;; accessors.
(locally (declare (optimize speed))

  ;; returns rbnode.
  ;; MEMO : begin means begin of tree.
  (defun __rbtree-begin-node (tree)
	(declare (type rbtree tree))
	(__rbnode-parent (__rbtree-header tree)))

  ;; returns rbnode.
  (defun __rbtree-end-node (tree)
	(declare (type rbtree tree))
	(__rbtree-header tree))

  ;; returns rbnode.
  (defun __rbtree-begin (tree)
	(declare (type rbtree tree))
	(__rbnode-left (__rbtree-header tree)))

  ;; returns rbnode.
  (defun __rbtree-end (tree)
	(declare (type rbtree tree))
	(__rbtree-header tree))

  ;; returns rbnode.
  (defun __rbtree-rbegin (tree)
	(declare (type rbtree tree))
	(let ((node (__rbtree-end tree)))
	  (declare (type rbnode node))
	  (if (zerop (__rbtree-node-count tree))
		  node
		  (__rbnode-decrement node))))

  ;; returns rbnode.
  (defun __rbtree-rend (tree)
	(declare (type rbtree tree))
	(let ((node (__rbtree-begin tree)))
	  (declare (type rbnode node))
	  (if (zerop (__rbtree-node-count tree))
		  node
		  (__rbnode-decrement node)))))



;; allocation/deallocation
(locally (declare (optimize speed))

  (defun __rbtree-ctor (key-comp key-func)
	(declare (type cl:function key-func))
	;;MEMO : key-comp may be functor object.
	(let ((obj (make-rbtree :key-func key-func :key-comp (clone key-comp))))
	  (__rbtree-initialize obj)
	  obj))

  (defun __rbtree-copy-ctor (tree)
	(declare (type rbtree tree))
	(let ((obj (make-rbtree :key-func (clone (__rbtree-key-func tree))
							:key-comp (clone (__rbtree-key-comp tree)))))
	  #+cl-stl-debug (setf (__rbtree-checker obj) (__rbtree-checker tree))
	  (__rbtree-initialize obj)
	  (when (__rbtree-root tree)
		(setf (__rbtree-root obj) (__rbtree-copy-imp (__rbtree-begin-node tree)
													 (__rbtree-end-node   obj)))
		(setf (__rbtree-leftmost   obj) (__rbnode-minimum (__rbtree-root obj)))
		(setf (__rbtree-rightmost  obj) (__rbnode-maximum (__rbtree-root obj)))
		(setf (__rbtree-node-count obj) (__rbtree-node-count tree)))
	  obj)))


(locally (declare (optimize speed))
  (defun __rbtree-insert-and-rebalance (insert-left new-node parent header)
	(declare (type rbnode new-node parent header))
	(symbol-macrolet ((root (__rbnode-parent header)))
	  ;; Initialize fields in new node to insert.
	  (progn
		(setf (__rbnode-parent new-node) parent)
		(setf (__rbnode-left   new-node) nil)
		(setf (__rbnode-right  new-node) nil)
		(setf (__rbnode-color  new-node) :red))
	  ;; Insert.
	  ;; Make new node child of parent and maintain root, leftmost and rightmost nodes.
	  ;; N.B. First node is always inserted left.
	  (if insert-left
		  (progn
			(setf (__rbnode-left parent) new-node) ; also makes leftmost = pNewNode when pParent == pHeader
			(if (eq parent header)
				(progn
				  (setf (__rbnode-parent header) new-node)
				  (setf (__rbnode-right  header) new-node))
				(when (eq parent (__rbnode-left header))
				  (setf (__rbnode-left  header) new-node)))) ; maintain leftmost pointing to min node
		  (progn
			(setf (__rbnode-right parent) new-node)
			(when (eq parent (__rbnode-right header))
			  (setf (__rbnode-right header) new-node))))     ; maintain rightmost pointing to max node
	  ;; Rebalance
	  (do nil
		  ((or (eq new-node root)
			   (not (__rbnode-is-red (__rbnode-parent new-node)))))
		(let ((grand-parent (__rbnode-parent (__rbnode-parent new-node))))
		  (declare (type rbnode grand-parent))
		  (if (eq (__rbnode-parent new-node) (__rbnode-left grand-parent))
			  (let ((right (__rbnode-right grand-parent)))
				(if (and right (__rbnode-is-red right))
					(locally
					  (declare (type rbnode right))
					  (setf (__rbnode-color (__rbnode-parent new-node)) :black)
					  (setf (__rbnode-color right) :black)
					  (setf (__rbnode-color grand-parent) :red)
					  (setf new-node grand-parent))
					(progn
					  (when (__rbnode-is-right-child new-node)
						(setf new-node (__rbnode-parent new-node))
						(setf root (__rbnode-rotate-left new-node root)))
					  (setf (__rbnode-color (__rbnode-parent new-node)) :black)
					  (setf (__rbnode-color grand-parent) :red)
					  (setf root (__rbnode-rotate-right grand-parent root)))))
			  (let ((left (__rbnode-left grand-parent)))
				(if (and left (__rbnode-is-red left))
					(locally
					  (declare (type rbnode left))
					  (setf (__rbnode-color (__rbnode-parent new-node)) :black)
					  (setf (__rbnode-color left) :black)
					  (setf (__rbnode-color grand-parent) :red)
					  (setf new-node grand-parent))
					(progn
					  (when (__rbnode-is-left-child new-node)
						(setf new-node (__rbnode-parent new-node))
						(setf root (__rbnode-rotate-right new-node root)))
					  (setf (__rbnode-color (__rbnode-parent new-node)) :black)
					  (setf (__rbnode-color grand-parent) :red)
					  (setf root (__rbnode-rotate-left grand-parent root))))))))
	  (setf (__rbnode-color root) :black))))



(locally (declare (optimize speed))
  (defun __rbtree-rebalance-for-erase (node-z header)
	(declare (type rbnode node-z header))
	(symbol-macrolet ((root      (__rbnode-parent header))
					  (leftmost  (__rbnode-left   header))
					  (rightmost (__rbnode-right  header)))
	  (let ((node-y   node-z)
			(node-x   nil)
			(parent-x nil))
		(declare (type rbnode node-y))
		;;---------------------------------------------------------------------------
		(cond
		  ((null (__rbnode-left  node-y)) (setf node-x (__rbnode-right node-y)))
		  ((null (__rbnode-right node-y)) (setf node-x (__rbnode-left  node-y)))
		  (t (setf node-y (__rbnode-right node-y))
			 (do ()
				 ((null (__rbnode-left node-y)))
			   (setf node-y (__rbnode-left node-y)))
			 (setf node-x (__rbnode-right node-y))))
		;;---------------------------------------------------------------------------
		(if (not (eq node-y node-z))
			(progn
			  (setf (__rbnode-parent (__rbnode-left node-z)) node-y)
			  (setf (__rbnode-left node-y) (__rbnode-left node-z))
			  (if (eq node-y (__rbnode-right node-z))
				  (setf parent-x node-y)
				  (progn
					(setf parent-x (__rbnode-parent node-y))
					(when node-x
					  (setf (__rbnode-parent node-x) (__rbnode-parent node-y)))
					(setf (__rbnode-left (__rbnode-parent node-y)) node-x)
					(setf (__rbnode-right node-y) (__rbnode-right node-z))
					(setf (__rbnode-parent (__rbnode-right node-z)) node-y)))
			  (if (eq root node-z)
				  (setf root node-y)
				  (if (eq (__rbnode-left (__rbnode-parent node-z)) node-z)
					  (setf (__rbnode-left  (__rbnode-parent node-z)) node-y)
					  (setf (__rbnode-right (__rbnode-parent node-z)) node-y)))
			  (setf (__rbnode-parent node-y) (__rbnode-parent node-z))
			  (swap (__rbnode-color node-y) (__rbnode-color node-z))
			  (setf node-y node-z))
			(progn
			  (setf parent-x (__rbnode-parent node-y))
			  (when node-x
				(setf (__rbnode-parent node-x) (__rbnode-parent node-y)))
			  (if (eq root node-z)
				  (setf root node-x)
				  (if (__rbnode-is-left-child node-z)
					  (setf (__rbnode-left  (__rbnode-parent node-z)) node-x)
					  (setf (__rbnode-right (__rbnode-parent node-z)) node-x)))
			  (when (eq leftmost node-z)
				(if (null (__rbnode-right node-z))
					(setf leftmost (__rbnode-parent node-z))
					(setf leftmost (__rbnode-minimum node-x))))
			  (when (eq rightmost node-z)
				(if (null (__rbnode-left node-z))
					(setf rightmost (__rbnode-parent node-z))
					(setf rightmost (__rbnode-maximum node-x))))))
		;;---------------------------------------------------------------------------
		(unless (__rbnode-is-red node-y)
		  (do ()
			  ((not (and (not (eq node-x root))
						 (or (null node-x)
							 (__rbnode-is-black node-x)))))
			(if (eq node-x (__rbnode-left parent-x))
				(let ((right (__rbnode-right parent-x)))
				  (when (__rbnode-is-red right)
					(setf (__rbnode-color right) :black)
					(setf (__rbnode-color parent-x) :red)
					(setf root (__rbnode-rotate-left parent-x root))
					(setf right (__rbnode-right parent-x)))
				  (if (and (or (null (__rbnode-left  right)) (__rbnode-is-black (__rbnode-left  right)))
						   (or (null (__rbnode-right right)) (__rbnode-is-black (__rbnode-right right))))
					  (progn
						(setf (__rbnode-color right) :red)
						(setf node-x parent-x)
						(setf parent-x (__rbnode-parent parent-x)))
					  (progn
						(when (or (null (__rbnode-right right)) (__rbnode-is-black (__rbnode-right right)))
						  (setf (__rbnode-color (__rbnode-left right)) :black)
						  (setf (__rbnode-color right) :red)
						  (setf root (__rbnode-rotate-right right root))
						  (setf right (__rbnode-right parent-x)))
						(setf (__rbnode-color right) (__rbnode-color parent-x))
						(setf (__rbnode-color parent-x) :black)
						(when (__rbnode-right right)
						  (setf (__rbnode-color (__rbnode-right right)) :black))
						(setf root (__rbnode-rotate-left parent-x root))
						(return))))
				(let ((left (__rbnode-left parent-x)))
				  ;; same as above, with right <-> left.
				  (when (__rbnode-is-red left)
					(setf (__rbnode-color left) :black)
					(setf (__rbnode-color parent-x) :red)
					(setf root (__rbnode-rotate-right parent-x root))
					(setf left (__rbnode-left parent-x)))
				  (if (and (or (null (__rbnode-right left)) (__rbnode-is-black (__rbnode-right left)))
						   (or (null (__rbnode-left  left)) (__rbnode-is-black (__rbnode-left  left))))
					  (progn
						(setf (__rbnode-color left) :red)
						(setf node-x parent-x)
						(setf parent-x (__rbnode-parent parent-x)))
					  (progn
						(when (or (null (__rbnode-left left)) (__rbnode-is-black (__rbnode-left left)))
						  (setf (__rbnode-color (__rbnode-right left)) :black)
						  (setf (__rbnode-color left) :red)
						  (setf root (__rbnode-rotate-left left root))
						  (setf left (__rbnode-left parent-x)))
						(setf (__rbnode-color left) (__rbnode-color parent-x))
						(setf (__rbnode-color parent-x) :black)
						(when (__rbnode-left left)
						  (setf (__rbnode-color (__rbnode-left left)) :black))
						(setf root (__rbnode-rotate-right parent-x root))
						(return))))))
		  (when node-x
			(setf (__rbnode-color node-x) :black)))
		node-y))))



(locally (declare (optimize speed))
  (defun __rbtree-clear (tree)
	(declare (type rbtree tree))
	(__rbtree-erase-imp (__rbtree-begin-node tree))
	(setf (__rbtree-leftmost   tree) (__rbtree-end-node tree))
	(setf (__rbtree-root       tree) nil)
	(setf (__rbtree-rightmost  tree) (__rbtree-end-node tree))
	(setf (__rbtree-node-count tree) 0)
	nil))


(locally (declare (optimize speed))
  (defun __rbtree-assign (tree1 tree2)
	(declare (type rbtree tree1 tree2))
	(unless (eq tree1 tree2)
	  (__rbtree-clear tree1)
	  (setf (__rbtree-key-func tree1) (clone (__rbtree-key-func tree2)))
	  (setf (__rbtree-key-comp tree1) (clone (__rbtree-key-comp tree2)))
	  #+cl-stl-debug (setf (__rbtree-checker  tree1) (__rbtree-checker tree2))
	  (unless (null (__rbtree-root tree2))
		(setf (__rbtree-root tree1) (__rbtree-copy-imp (__rbtree-begin-node tree2)
													   (__rbtree-end-node   tree1)))
		(setf (__rbtree-leftmost   tree1) (__rbnode-minimum (__rbtree-root tree1)))
		(setf (__rbtree-rightmost  tree1) (__rbnode-maximum (__rbtree-root tree1)))
		(setf (__rbtree-node-count tree1) (__rbtree-node-count tree2))))
	tree1))


(locally (declare (optimize speed))
  (defun __rbtree-swap (t1 t2)  ;; always returns nil.
	(declare (type rbtree t1 t2))
	(if (null (__rbtree-root t1))
		(unless (null (__rbtree-root t2))
		  (setf (__rbtree-root      t1) (__rbtree-root      t2))
		  (setf (__rbtree-leftmost  t1) (__rbtree-leftmost  t2))
		  (setf (__rbtree-rightmost t1) (__rbtree-rightmost t2))
		  (setf (__rbnode-parent (__rbtree-root t1)) (__rbtree-end-node t1))
		  (setf (__rbtree-root      t2) nil)
		  (setf (__rbtree-leftmost  t2) (__rbtree-end-node t2))
		  (setf (__rbtree-rightmost t2) (__rbtree-end-node t2)))
		(if (null (__rbtree-root t2))
			(progn
			  (setf (__rbtree-root      t2) (__rbtree-root      t1))
			  (setf (__rbtree-leftmost  t2) (__rbtree-leftmost  t1))
			  (setf (__rbtree-rightmost t2) (__rbtree-rightmost t1))
			  (setf (__rbnode-parent (__rbtree-root t2)) (__rbtree-end-node t2))
			  (setf (__rbtree-root      t1) nil)
			  (setf (__rbtree-leftmost  t1) (__rbtree-end-node t1))
			  (setf (__rbtree-rightmost t1) (__rbtree-end-node t1)))
			(progn
			  (swap (__rbtree-root      t1) (__rbtree-root      t2))
			  (swap (__rbtree-leftmost  t1) (__rbtree-leftmost  t2))
			  (swap (__rbtree-rightmost t1) (__rbtree-rightmost t2))
			  (setf (__rbnode-parent (__rbtree-root t1)) (__rbtree-end-node t1))
			  (setf (__rbnode-parent (__rbtree-root t2)) (__rbtree-end-node t2)))))
	;; No need to swap header's color as it does not change.
	(swap (__rbtree-node-count t1) (__rbtree-node-count t2))
	(swap (__rbtree-key-comp   t1) (__rbtree-key-comp   t2))
	(swap (__rbtree-key-func   t1) (__rbtree-key-func   t2))
	#+cl-stl-debug
	(swap (__rbtree-checker    t1) (__rbtree-checker    t2))
	nil))


(locally (declare (optimize speed))

  ;;NOTE : returns rbnode
  (defun __rbtree-lower-bound-imp (tree node-x node-y key)
	(declare (type rbtree tree))
	(declare (type rbnode node-y))    ; node-x maybe nil
	(let ((key-of (__rbtree-key-func tree))
		  (cmp    (functor-function (__rbtree-key-comp tree))))
	  (declare (type cl:function key-of cmp))
	  (do ()
		  ((null node-x) node-y)
		(if (not (funcall cmp (funcall key-of (__rbnode-value node-x)) key))
			(progn (setf node-y node-x)
				   (setf node-x (__rbnode-left node-x)))
			(setf node-x (__rbnode-right node-x))))))

  ;;NOTE : returns rbnode
  (defun __rbtree-upper-bound-imp (tree node-x node-y key)
	(declare (type rbtree tree))
	(declare (type rbnode node-y))    ; node-x maybe nil
	(let ((key-of (__rbtree-key-func tree))
		  (cmp    (functor-function (__rbtree-key-comp tree))))
	  (declare (type cl:function key-of cmp))
	  (do ()
		  ((null node-x) node-y)
		(if (funcall cmp key (funcall key-of (__rbnode-value node-x)))
			(progn (setf node-y node-x)
				   (setf node-x (__rbnode-left node-x)))
			(setf node-x (__rbnode-right node-x))))))

  ;;NOTE : returns 2 values : rbnode & rbnode.
  (defun __rbtree-equal-range (tree key)
	(declare (type rbtree tree))
	(let ((node-x (__rbtree-begin-node tree))
		  (node-y (__rbtree-end-node   tree))
		  (key-of (__rbtree-key-func   tree))
		  (cmp    (functor-function    (__rbtree-key-comp tree))))
	  (declare (type rbnode node-y))        ;MEMO : node-x maybe nil.
	  (declare (type cl:function key-of cmp))
	  (do ()
		  ((null node-x))
		(let ((key-x (funcall key-of (__rbnode-value node-x))))
		  (cond
			((funcall cmp key-x key)
			 (setf node-x (__rbnode-right node-x)))
			((funcall cmp key key-x)
			 (setf node-y node-x)
			 (setf node-x (__rbnode-left node-x)))
			(t
			 (let ((node-x2 node-x)
				   (node-y2 node-y))
			   (setf node-y  node-x)
			   (setf node-x  (__rbnode-left node-x))
			   (setf node-x2 (__rbnode-right node-x2))
			   (return-from __rbtree-equal-range
				 (values (__rbtree-lower-bound-imp tree node-x  node-y  key)
						 (__rbtree-upper-bound-imp tree node-x2 node-y2 key))))))))
	  (values node-y node-y))))



;; erase
(locally (declare (optimize speed))

  (labels (;; NOTE : always return nil.
		   (__erase-aux1 (tree node)
			 (declare (type rbtree tree))
			 (declare (type rbnode node))
			 (let ((node-y (__rbtree-rebalance-for-erase node (__rbtree-header tree))))
			   (declare (type rbnode node-y))
			   (__rbtree-destroy-node node-y)
			   (decf (the fixnum (__rbtree-node-count tree)))
			   nil))
		   ;; NOTE : always return nil.
		   (__erase-aux2 (tree node1 node2)
			 (declare (type rbtree tree))
			 (declare (type rbnode node1 node2))
			 (if (and (eq node1 (__rbtree-begin tree))
					  (eq node2 (__rbtree-end   tree)))
				 (__rbtree-clear tree)
				 (do ()
					 ((eq node1 node2))
				   (let ((tmp (__rbnode-increment node1)))
					 (__erase-aux1 tree node1)
					 (setf node1 tmp))))
			 nil))

	#+cl-stl-0x98
	(defun __rbtree-erase-node (tree node)
	  (declare (type rbtree tree))
	  (declare (type rbnode node))
	  (__erase-aux1 tree node)
	  nil)

	#-cl-stl-0x98
	(defun __rbtree-erase-node (tree node)
	  (declare (type rbtree tree))
	  (declare (type rbnode node))
	  (let ((node2 (__rbnode-increment node)))
		(__erase-aux1 tree node)
		node2))

	(defun __rbtree-erase-range (tree node1 node2)
	  (declare (type rbtree tree))
	  (declare (type rbnode node1 node2))
	  (__erase-aux2 tree node1 node2)
	  #+cl-stl-0x98 nil
	  #-cl-stl-0x98 node2)

	;; NOTE : returns count of erased items.
	(defun __rbtree-erase-key (tree key)
	  (declare (type rbtree tree))
	  (multiple-value-bind (node1 node2) (__rbtree-equal-range tree key)
		(declare (type rbnode node1 node2))
		(let ((old-size (__rbtree-size tree)))
		  (declare (fixnum old-size))
		  (__rbtree-erase-range tree node1 node2)
		  (the fixnum (- old-size (the fixnum (__rbtree-size tree)))))))

	;; NOTE : always return nil.
	(defun __rbtree-erase-key-range (tree ptr1 ptr2)
	  (do ()
		  ((_== ptr1 ptr2) nil)
		(let ((key (_* ptr1)))
		  (_++ ptr1)
		  (__rbtree-erase-key tree key)))
	  nil)))



(locally (declare (optimize speed))

  ;;returns 2 value : rbnode, rbnode
  (defun __rbtree-get-insert-unique-pos (tree key)
	(declare (type rbtree tree))
	(let ((comp-p t)
		  (node-x (__rbtree-begin-node tree))
		  (node-y (__rbtree-end-node   tree))
		  (key-of (__rbtree-key-func   tree))
		  (cmp    (functor-function    (__rbtree-key-comp tree))))
	  (declare (type rbnode node-y))    ; MEMO : node-x maybe nil.
	  (declare (type cl:function key-of cmp))
	  (do ()
		  ((null node-x) nil)
		(setf node-y node-x)
		(setf comp-p (funcall cmp key (funcall key-of (__rbnode-value node-x))))
		(setf node-x (if comp-p
						 (__rbnode-left  node-x)
						 (__rbnode-right node-x))))
	  (let ((node-j node-y))
		(declare (type rbnode node-j))
		(when comp-p
		  (if (eq node-j (__rbtree-begin tree))
			  (return-from __rbtree-get-insert-unique-pos (values node-x node-y))
			  (setf node-j (__rbnode-decrement node-j))))
		(if (funcall cmp (funcall key-of (__rbnode-value node-j)) key)
		  (values node-x node-y)
		  (values node-j    nil)))))

  ;;returns 2 value : rbnode, rbnode
  (defun __rbtree-get-insert-equal-pos (tree key)
	(declare (type rbtree tree))
	(let ((node-x (__rbtree-begin-node tree))
		  (node-y (__rbtree-end-node   tree))
		  (key-of (__rbtree-key-func   tree))
		  (cmp    (functor-function (__rbtree-key-comp tree))))
	  (declare (type rbnode node-y))    ; MEMO : node-x maybe nil.
	  (declare (type cl:function key-of cmp))
	  (do ()
		  ((null node-x) nil)
		(setf node-y node-x)
		(setf node-x (if (funcall cmp key (funcall key-of (__rbnode-value node-x)))
						 (__rbnode-left  node-x)
						 (__rbnode-right node-x))))
	  (values node-x node-y)))

  ;;returns 2 value : rbnode, rbnode
  (defun __rbtree-get-insert-hint-unique-pos (tree node key)
	(declare (type rbtree tree))
	(declare (type rbnode node))
	(let ((key-of (__rbtree-key-func tree))
		  (cmp    (functor-function (__rbtree-key-comp tree))))
	  (declare (type cl:function key-of cmp))
	  (cond
		;; when end
		((eq node (__rbtree-end-node tree))
		 (if (and (< 0 (the fixnum (__rbtree-size tree)))
				  (funcall cmp (funcall key-of (__rbnode-value (__rbtree-rightmost tree))) key))
			 (values nil (__rbtree-rightmost tree))
			 (__rbtree-get-insert-unique-pos tree key)))
		;; first, try before...
		((funcall cmp key (funcall key-of (__rbnode-value node)))
		 (if (eq node (__rbtree-leftmost tree)) ; begin
			 (values (__rbtree-leftmost tree) (__rbtree-leftmost tree))
			 (let ((before (__rbnode-decrement node)))
			   (if (funcall cmp (funcall key-of (__rbnode-value before)) key)
				   (progn
					 (if (null (__rbnode-right before))
						 (values  nil before)
						 (values node   node)))
				   (__rbtree-get-insert-unique-pos tree key)))))
		;; ... then try after.
		((funcall cmp (funcall key-of (__rbnode-value node)) key)
		 (if (eq node (__rbtree-rightmost tree))
			 (values nil (__rbtree-rightmost tree))
			 (let ((after (__rbnode-increment node)))
			   (if (funcall cmp key (funcall key-of (__rbnode-value after)))
				   (progn
					 (if (null (__rbnode-right node))
						 (values   nil  node)
						 (values after after)))
				   (__rbtree-get-insert-unique-pos tree key)))))
		(t ;;Equivalent keys.
		 (values node nil)))))

  ;;returns 2 value : rbnode, rbnode
  (defun __rbtree-get-insert-hint-equal-pos (tree node key)
	(declare (type rbtree tree))
	(declare (type rbnode node))
	(let ((key-of (__rbtree-key-func tree))
		  (cmp    (functor-function (__rbtree-key-comp tree))))
	  (declare (type cl:function key-of cmp))
	  (cond
		;; when end
		((eq node (__rbtree-end-node tree))
		 (if (and (< 0 (the fixnum (__rbtree-size tree)))
				  (not (funcall cmp key (funcall key-of (__rbnode-value (__rbtree-rightmost tree))))))
			 (values nil (__rbtree-rightmost tree))
			 (__rbtree-get-insert-equal-pos tree key)))
		;; first, try before...
		((not (funcall cmp (funcall key-of (__rbnode-value node)) key))
		 (if (eq node (__rbtree-leftmost tree)) ; begin
			 (values (__rbtree-leftmost tree) (__rbtree-leftmost tree))
			 (let ((before (__rbnode-decrement node)))
			   (if (not (funcall cmp key (funcall key-of (__rbnode-value before))))
				   (progn
					 (if (null (__rbnode-right before))
						 (values  nil before)
						 (values node   node)))
				   (__rbtree-get-insert-equal-pos tree key)))))
		;; ... then try after.
		(t
		 (if (eq node (__rbtree-rightmost tree))
			 (values nil (__rbtree-rightmost tree))
			 (let ((after (__rbnode-increment node)))
			   (if (not (funcall cmp (funcall key-of (__rbnode-value after)) key))
				   (progn
					 (if (null (__rbnode-right node))
						 (values   nil  node)
						 (values after after)))
				   (values nil nil)))))))))


;; insert
(locally (declare (optimize speed))

  ;; returns rbnode
  (defun __rbtree-insert (tree node-x node-p val need-copy)
	(declare (type rbtree tree))
	(declare (type rbnode node-p))    ; MEMO node-x maybe nil.
	(let ((key-of (__rbtree-key-func tree))
		  (cmp    (functor-function (__rbtree-key-comp tree))))
	  (declare (type cl:function key-of cmp))
	  (let ((node-z (__rbtree-create-node val need-copy))
			(insert-left-p (or node-x
							   (eq node-p (__rbtree-end-node tree))
							   (funcall cmp
										(funcall key-of val)
										(funcall key-of (__rbnode-value node-p))))))
		(__rbtree-insert-and-rebalance insert-left-p node-z node-p (__rbtree-header tree))
		(incf (the fixnum (__rbtree-node-count tree)))
		node-z)))

  ;; returns rbnode
  (defun __rbtree-insert-lower (tree node val need-copy)
	(declare (type rbtree tree))
	(declare (type rbnode node))
	(let ((key-of (__rbtree-key-func tree))
		  (cmp    (functor-function (__rbtree-key-comp tree))))
	  (declare (type cl:function key-of cmp))
	  (let ((node-z (__rbtree-create-node val need-copy))
			(insert-left-p (or (eq node (__rbtree-end-node tree))
							   (not (funcall cmp
											 (funcall key-of (__rbnode-value node))
											 (funcall key-of val))))))
		(__rbtree-insert-and-rebalance insert-left-p node-z node (__rbtree-header tree))
		(incf (the fixnum (__rbtree-node-count tree)))
		node-z)))

  ;; returns rbnode
  (defun __rbtree-insert-equal-lower (tree val need-copy)
	(declare (type rbtree tree))
	(let ((node-x (__rbtree-begin-node tree))
		  (node-y (__rbtree-end-node   tree))
		  (key-of (__rbtree-key-func   tree))
		  (cmp    (functor-function (__rbtree-key-comp tree))))
	  (declare (type rbnode node-y))        ;MEMO : node-x maybe nil.
	  (declare (type cl:function key-of cmp))
	  (do ()
		  ((null node-x) (__rbtree-insert-lower tree node-y val need-copy))
		(setf node-y node-x)
		(setf node-x (if (not (funcall cmp (funcall key-of (__rbnode-value node-x))
										   (funcall key-of val)))
						 (__rbnode-left  node-x)
						 (__rbnode-right node-x))))))

  ;; returns 2 value : rbnode & boolean value.
  (defun __rbtree-insert-unique (tree val need-copy)
	(declare (type rbtree tree))
	#+cl-stl-debug (setf val (funcall (the cl:function (__rbtree-checker tree)) val))
	(let ((key-of (__rbtree-key-func tree)))
	  (declare (type cl:function key-of))
	  (multiple-value-bind (node1 node2)
		  (__rbtree-get-insert-unique-pos tree (funcall key-of val))
		(if node2
			(values (__rbtree-insert tree node1 node2 val need-copy) t)
			(values node1 nil)))))

  ;; returns rbnode.
  (defun __rbtree-insert-equal (tree val need-copy)
	(declare (type rbtree tree))
	#+cl-stl-debug (setf val (funcall (the cl:function (__rbtree-checker tree)) val))
	(let ((key-of (__rbtree-key-func tree)))
	  (declare (type cl:function key-of))
	  (multiple-value-bind (node1 node2)
		  (__rbtree-get-insert-equal-pos tree (funcall key-of val))
		(__rbtree-insert tree node1 node2 val need-copy))))

  ;; returns rbnode.
  (defun __rbtree-insert-hint-unique (tree node val need-copy)
	(declare (type rbtree tree))
	(declare (type rbnode node))
	#+cl-stl-debug (setf val (funcall (the cl:function (__rbtree-checker tree)) val))
	(let ((key-of (__rbtree-key-func tree)))
	  (declare (type cl:function key-of))
	  (multiple-value-bind (node1 node2)
		  (__rbtree-get-insert-hint-unique-pos tree node (funcall key-of val))
		(if node2
			(__rbtree-insert tree node1 node2 val need-copy)
			node1))))

  ;; returns rbnode.
  (defun __rbtree-insert-hint-equal (tree node val need-copy)
	(declare (type rbtree tree))
	(declare (type rbnode node))
	#+cl-stl-debug (setf val (funcall (the cl:function (__rbtree-checker tree)) val))
	(let ((key-of (__rbtree-key-func tree)))
	  (declare (type cl:function key-of))
	  (multiple-value-bind (node1 node2)
		  (__rbtree-get-insert-hint-equal-pos tree node (funcall key-of val))
		(if node2
			(__rbtree-insert tree node1 node2 val need-copy)
			(__rbtree-insert-equal-lower tree val need-copy)))))

  (defun __rbtree-insert-range-unique (tree itr1 itr2 need-copy)
	(declare (type rbtree tree))
	(do ((end (__rbtree-end tree))
		 (itr (clone itr1) (_++ itr)))
		((_== itr itr2) nil)
	  (__rbtree-insert-hint-unique tree end (_* itr) need-copy)))

  (defun __rbtree-insert-range-equal (tree itr1 itr2 need-copy)
	(declare (type rbtree tree))
	(do ((end (__rbtree-end tree))
		 (itr (clone itr1) (_++ itr)))
		((_== itr itr2) nil)
	  (__rbtree-insert-hint-equal tree end (_* itr) need-copy)))

  (defun __rbtree-insert-array-unique (tree arr idx1 idx2 need-copy)
	(declare (type rbtree tree))
	(declare (type cl:vector arr))
	(declare (type fixnum idx1 idx2))
	(do ((end (__rbtree-end tree))
		 (idx idx1 (1+ idx)))
		((= idx idx2) nil)
	  (__rbtree-insert-hint-unique tree end (aref arr idx) need-copy)))

  (defun __rbtree-insert-array-equal (tree arr idx1 idx2 need-copy)
	(declare (type rbtree tree))
	(declare (type cl:vector arr))
	(declare (type fixnum idx1 idx2))
	(do ((end (__rbtree-end tree))
		 (idx idx1 (1+ idx)))
		((= idx idx2) nil)
	  (__rbtree-insert-hint-equal tree end (aref arr idx) need-copy))))


;; emplace
#-cl-stl-0x98
(locally (declare (optimize speed))

  ;; returns rbnode
  (defun __rbtree-insert-node (tree node-x node-p new-node)
	(declare (type rbtree tree))
	(declare (type rbnode node-p new-node))    ; MEMO node-x maybe nil.
	(let ((key-of (__rbtree-key-func tree))
		  (cmp    (functor-function (__rbtree-key-comp tree))))
	  (declare (type cl:function key-of cmp))
	  (let ((insert-left-p (or node-x
							   (eq node-p (__rbtree-end-node tree))
							   (funcall cmp
										(funcall key-of (__rbnode-value new-node))
										(funcall key-of (__rbnode-value node-p))))))
		(__rbtree-insert-and-rebalance insert-left-p new-node node-p (__rbtree-header tree))
		(incf (the fixnum (__rbtree-node-count tree)))
		new-node)))

  ;; returns rbnode
  (defun __rbtree-insert-lower-node (tree node new-node)
	(declare (type rbtree tree))
	(declare (type rbnode node new-node))
	(let ((key-of (__rbtree-key-func tree))
		  (cmp    (functor-function (__rbtree-key-comp tree))))
	  (declare (type cl:function key-of cmp))
	  (let ((insert-left-p (or (eq node (__rbtree-end-node tree))
							   (not (funcall cmp
											 (funcall key-of (__rbnode-value node))
											 (funcall key-of (__rbnode-value new-node)))))))
		(__rbtree-insert-and-rebalance insert-left-p new-node node (__rbtree-header tree))
		(incf (the fixnum (__rbtree-node-count tree)))
		new-node)))

  ;; returns rbnode
  (defun __rbtree-insert-equal-lower-node (tree new-node)
	(declare (type rbtree tree))
	(declare (type rbnode new-node))
	(let ((node-x (__rbtree-begin-node tree))
		  (node-y (__rbtree-end-node   tree))
		  (key-of (__rbtree-key-func   tree))
		  (cmp    (functor-function (__rbtree-key-comp tree))))
	  (declare (type rbnode node-y))        ;MEMO : node-x maybe nil.
	  (declare (type cl:function key-of cmp))
	  (do ()
		  ((null node-x) (__rbtree-insert-lower-node tree node-y new-node))
		(setf node-y node-x)
		(setf node-x (if (not (funcall cmp (funcall key-of (__rbnode-value node-x))
										   (funcall key-of (__rbnode-value new-node))))
						 (__rbnode-left  node-x)
						 (__rbnode-right node-x))))))

  ;; returns 2 value : rbnode & boolean value.
  (defun __rbtree-emplace-unique (tree val)
	(declare (type rbtree tree))
	#+cl-stl-debug (setf val (funcall (the cl:function (__rbtree-checker tree)) val))
	(let ((key-of   (__rbtree-key-func tree))
		  (new-node (__rbtree-create-node val nil)))
	  (declare (type cl:function key-of))
	  (handler-case
		  (multiple-value-bind (node1 node2) (__rbtree-get-insert-unique-pos tree (funcall key-of val))
			(if node2
				(values (__rbtree-insert-node tree node1 node2 new-node) t)
				(progn
				  ;;(__rbtree-destroy-node new-node)    ;;ToDo : need this ?
				  (values node1 nil))))
		(error (c)
		  ;;(__rbtree-destroy-node new-node)    ;;ToDo : need this ?
		  (error c)))))

  ;; returns rbnode.
  (defun __rbtree-emplace-equal (tree val)
	(declare (type rbtree tree))
	#+cl-stl-debug (setf val (funcall (the cl:function (__rbtree-checker tree)) val))
	(let ((key-of   (__rbtree-key-func tree))
		  (new-node (__rbtree-create-node val nil)))
	  (declare (type cl:function key-of))
	  (handler-case
		  (multiple-value-bind (node1 node2) (__rbtree-get-insert-equal-pos tree (funcall key-of val))
			(__rbtree-insert-node tree node1 node2 new-node))
		(error (c)
		  ;;(__rbtree-destroy-node new-node)    ;;ToDo : need this ?
		  (error c)))))

  ;; returns rbnode.
  (defun __rbtree-emplace-hint-unique (tree node val)
	(declare (type rbtree tree))
	(declare (type rbnode node))
	#+cl-stl-debug (setf val (funcall (the cl:function (__rbtree-checker tree)) val))
	(let ((key-of   (__rbtree-key-func tree))
		  (new-node (__rbtree-create-node val nil)))
	  (declare (type cl:function key-of))
	  (handler-case
		  (multiple-value-bind (node1 node2) (__rbtree-get-insert-hint-unique-pos tree node (funcall key-of val))
			(if node2
				(__rbtree-insert-node tree node1 node2 new-node)
				(progn
				  ;;(__rbtree-destroy-node new-node)    ;;ToDo : need this ?
				  node1)))
		(error (c)
		  ;;(__rbtree-destroy-node new-node)    ;;ToDo : need this ?
		  (error c)))))

  ;; returns rbnode.
  (defun __rbtree-emplace-hint-equal (tree node val)
	(declare (type rbtree tree))
	(declare (type rbnode node))
	#+cl-stl-debug (setf val (funcall (the cl:function (__rbtree-checker tree)) val))
	(let ((key-of   (__rbtree-key-func tree))
		  (new-node (__rbtree-create-node val nil)))
	  (declare (type cl:function key-of))
	  (handler-case
		  (multiple-value-bind (node1 node2) (__rbtree-get-insert-hint-equal-pos tree node (funcall key-of val))
			(if node2
				(__rbtree-insert-node tree node1 node2 new-node)
				(__rbtree-insert-equal-lower-node tree new-node)))
		(error (c)
		  ;;(__rbtree-destroy-node new-node)    ;;ToDo : need this ?
		  (error c))))))



;; set operations
(locally (declare (optimize speed))

  ;;NOTE : returns rbnode
  (defun __rbtree-find (tree key)
	(declare (type rbtree tree))
	(let ((node     (__rbtree-lower-bound-imp tree (__rbtree-begin-node tree)
												   (__rbtree-end-node   tree) key))
		  (end-node (__rbtree-end tree))
		  (key-of   (__rbtree-key-func tree))
		  (cmp      (functor-function (__rbtree-key-comp tree))))
	  (declare (type rbnode node end-node))
	  (declare (type cl:function key-of cmp))
	  (if (or (eq node end-node)
			  (funcall cmp key (funcall key-of (__rbnode-value node))))
		  end-node
		  node)))

  (defun __rbtree-count (tree key)
	(declare (type rbtree tree))
	(multiple-value-bind (node1 node2) (__rbtree-equal-range tree key)
	  (do ((cnt 0 (1+ cnt)))
		  ((eq node1 node2) cnt)
		(declare (type fixnum cnt))
		(setf node1 (__rbnode-increment node1)))))

  ;;NOTE : returns rbnode
  (defun __rbtree-lower-bound (tree key)
	(declare (type rbtree tree))
	(__rbtree-lower-bound-imp tree (__rbtree-begin-node tree)
								   (__rbtree-end-node   tree) key))

  ;;NOTE : returns rbnode
  (defun __rbtree-upper-bound (tree key)
	(declare (type rbtree tree))
	(__rbtree-upper-bound-imp tree (__rbtree-begin-node tree)
								   (__rbtree-end-node   tree) key)))


;; compare
(locally (declare (optimize speed))

  (defun __rbtree-equal (tree1 tree2 eql-func)
	(declare (type rbtree tree1 tree2))
	(declare (type cl:function eql-func))
	(if (eq tree1 tree2)
		t
		(let ((size1 (__rbtree-size tree1))
			  (size2 (__rbtree-size tree2)))
		  (declare (type fixnum size1 size2))
		  (if (/= size1 size2)
			  nil
			  (if (zerop size1)
				  t
				  (do ((node1 (__rbtree-begin tree1) (__rbnode-increment node1))
					   (end1  (__rbtree-end   tree1))
					   (node2 (__rbtree-begin tree2) (__rbnode-increment node2)))
					  ((eq node1 end1) t)
					(declare (type rbnode node1 end1 node2))
					(unless (funcall eql-func (__rbnode-value node1) (__rbnode-value node2))
					  (return-from __rbtree-equal nil))))))))

  (defun __rbtree-less (tree1 tree2 less-func)
	(declare (type rbtree tree1 tree2))
	(declare (type cl:function less-func))
	(if (eq tree1 tree2)
		nil
		(do ((node1 (__rbtree-begin tree1) (__rbnode-increment node1))
			 (end1  (__rbtree-end   tree1))
			 (node2 (__rbtree-begin tree2) (__rbnode-increment node2))
			 (end2  (__rbtree-end   tree2)))
			((and (eq node1 end1)
				  (eq node2 end2)) nil)
		  (if (eq node1 end1)
			  (return-from __rbtree-less t)
			  (if (eq node2 end2)
				  (return-from __rbtree-less nil)
				  (let ((val1 (__rbnode-value node1))
						(val2 (__rbnode-value node2)))
					(if (funcall less-func val1 val2)
						(return-from __rbtree-less t)
						(when (funcall less-func val2 val1)
						  (return-from __rbtree-less nil))))))))))



;; enumeration for stl:for
(locally (declare (optimize speed))
  (defun __rbtree-enumerate (tree func)
	(declare (type rbtree tree))
	(declare (type cl:function func))
	(let ((node1 (__rbtree-begin tree))
		  (end1  (__rbtree-end   tree)))
	  (declare (type rbnode node1 end1))
	  (do ()
		  ((eq node1 end1) nil)
		(funcall func (__rbnode-value node1))
		(setf node1 (__rbnode-increment node1))))))



#+cl-stl-debug
(defun __rbtree-dump (tree stream item-printer)
  (declare (type rbtree tree))
  (let ((item-printer (if item-printer
						  (functor-function (clone item-printer))
						  (lambda (s x) (format s "~A" x)))))
	(declare (type cl:function item-printer))
	(labels ((indent (cnt)
			   (declare (type fixnum cnt))
			   (with-output-to-string (s)
				 (do ((i 0 (incf i)))
					 ((= i cnt) nil)
				   (declare (type fixnum i))
				   (format s "    "))))
			 (imp (node level)
			   (declare (type rbnode node))
			   (declare (type fixnum level))
			   (let ((left  (__rbnode-left  node))
					 (right (__rbnode-right node)))
				 (when right (imp right (1+ level)))
				 (format stream "~A[~A] ~A~%"
						 (indent level)
						 (if (__rbnode-is-red node) "R" "B")
						 (with-output-to-string (tag)
						   (funcall item-printer tag (__rbnode-value node))))
				 (when left (imp left (1+ level))))))
	  (let ((root (__rbtree-root tree)))
		(when root
		  (imp root 0))))))

	  
			   

#+cl-stl-debug
(locally (declare (optimize speed))
  (labels ((__black-count (node root)
			 (declare (type rbnode node root))
			 (if (null node)
				 0
				 (let ((sum 0))
				   (declare (type fixnum sum))
				   (do ()
					   (nil)
					 (when (eq :black (__rbnode-color node))
					   (incf sum))
					 (when (eq node root)
					   (return))
					 (setf node (__rbnode-parent node)))
				   sum))))

	;; returns boolean value.
	(defun __rbtree-verify (tree)
	  (declare (type rbtree tree))
	  (if (or (zerop (__rbtree-node-count tree))
			  (eq (__rbtree-begin tree) (__rbtree-end tree)))
		  (and (zerop (__rbtree-node-count tree))
			   (eq (__rbtree-begin tree) (__rbtree-end tree))
			   (eq (__rbnode-left  (__rbtree-header tree)) (__rbtree-end-node tree))
			   (eq (__rbnode-right (__rbtree-header tree)) (__rbtree-end-node tree)))
		  (let ((key-of (__rbtree-key-func tree))
				(cmp    (functor-function (__rbtree-key-comp tree)))
				(len    (__black-count (__rbtree-leftmost tree) (__rbtree-root tree))))
			(declare (type cl:function key-of cmp))
			(do ((node1 (__rbtree-begin tree) (__rbnode-increment node1))
				 (node2 (__rbtree-end   tree)))
				((eq node1 node2) nil)
			  (let ((left  (__rbnode-left  node1))
					(right (__rbnode-right node1)))
				(when (__rbnode-is-red node1)
				  (when (or (and left  (__rbnode-is-red  left))
							(and right (__rbnode-is-red right)))
					(return-from __rbtree-verify nil)))
				(when (and left  (funcall cmp (funcall key-of (__rbnode-value node1))
											  (funcall key-of (__rbnode-value  left))))
				  (return-from __rbtree-verify nil))
				(when (and right (funcall cmp (funcall key-of (__rbnode-value right))
											  (funcall key-of (__rbnode-value node1))))
				  (return-from __rbtree-verify nil))
				(when (and (null  left)
						   (null right)
						   (/= len (__black-count node1 (__rbtree-root tree))))
				  (return-from __rbtree-verify nil))))
			(unless (eq (__rbtree-leftmost  tree) (__rbnode-minimum (__rbtree-root tree)))
			  (return-from __rbtree-verify nil))
			(unless (eq (__rbtree-rightmost tree) (__rbnode-maximum (__rbtree-root tree)))
			  (return-from __rbtree-verify nil))
			t)))))




