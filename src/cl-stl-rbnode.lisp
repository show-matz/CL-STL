(in-package :cl-stl)

(declaim (inline __rbnode-has-left-child
				 __rbnode-is-external
				 __rbnode-is-top
				 __rbnode-is-left
				 __rbnode-is-red
				 __rbnode-brother
				 __rbnode-connect-left
				 __rbnode-connect-right
				 __rbnode-max
				 __rbnode-min))

;;------------------------------------------------------------------------------
;;
;; class difinition
;;
;;------------------------------------------------------------------------------
(defstruct (rbnode (:conc-name __rbnode-))
  (rank   0 :type fixnum)
  (value  nil)
  (parent nil)
  (left   nil)
  (right  nil))

;;------------------------------------------------------------------------------
;;
;; rbnode utilities
;;
;;------------------------------------------------------------------------------
(defun __rbnode-has-left-child (node)
  (/= 0 (__rbnode-rank (__rbnode-left node))))

(defun __rbnode-is-external (node)
  (zerop (__rbnode-rank node)))

(defun __rbnode-is-top (node)
  (zerop (__rbnode-rank (__rbnode-parent node))))

(defun __rbnode-is-left (node)
  (eq node (__rbnode-left (__rbnode-parent node))))

(defun __rbnode-is-red (node)
  (= (__rbnode-rank node) (__rbnode-rank (__rbnode-parent node))))

(defun __rbnode-brother (node)
  (let* ((parent (__rbnode-parent node))
		 (left   (__rbnode-left parent)))
	(if (eq node left)
		(__rbnode-right parent)
		left)))

(defun __rbnode-connect-left (parent child)
  (setf (__rbnode-left   parent) child)
  (setf (__rbnode-parent  child) parent)
  child)

(defun __rbnode-connect-right (parent child)
  (setf (__rbnode-right  parent) child)
  (setf (__rbnode-parent  child) parent)
  child)

(defun __rbnode-max (node)
  (do ((right (__rbnode-right node)))
	  ((__rbnode-is-external right) node)
	(setf node right)
	(setf right (__rbnode-right right))))

(defun __rbnode-min (node)
  (do ((left (__rbnode-left node)))
	  ((__rbnode-is-external left) node)
	(setf node left)
	(setf left (__rbnode-left left))))

(defun __rbnode-next (node)
  (let ((right  (__rbnode-right  node))
		(parent (__rbnode-parent node)))
	(if (and right (not (__rbnode-is-external right)))
		;; if exists non-external right child, next node is left-most child of right child.
		(__rbnode-min right)
		;; otherwise...
		(if (or (null parent) (__rbnode-is-external parent))
			;; when parent is lack or external-node, node has max value. -> returns (external) right node.
			right
			;; otherwise...
			(if (eq node (__rbnode-left parent))
				;; when node is left-child of parent : next node is parent.
				parent
				;; otherwise ( it means that node is right child of parent )...
				(let (parent2)
				  ;; get left-most ancestor and it's parent
				  (labels ((imp ()
							 (setf parent2 (__rbnode-parent parent))
							 (when (eq parent (__rbnode-right parent2))
							   (setf parent parent2)
							   (if (__rbnode-is-external parent)
								   nil
								   (imp)))))
					(imp))
				  (if (and (not (__rbnode-is-external parent))
						   (not (__rbnode-is-external parent2)))
					  ;; if exists right-parent of left-most ancestor, it's next node.
					  parent2
					  ;; otherwise, node has max value. -> returns (external) right node.
					  right)))))))

(defun __rbnode-prev (node)
  (let ((left   (__rbnode-left   node))
		(parent (__rbnode-parent node)))
	(if (and left (not (__rbnode-is-external left)))
		;; if exists non-external left child, prev node is right-most child of left child.
		(__rbnode-max left)
		;; otherwise...
		(if (or (null parent) (__rbnode-is-external parent))
			;; when parent is lack or external node, node has min value. -> returns (external) left node.
			left
			;; otherwise...
			(if (eq node (__rbnode-right parent))
				;; when node is right-child of parent : prev node is parent.
				parent
				;; otherwise ( it means that node is left child of parent )...
				(let (parent2)
				  ;; get right-most ancestor and it's parent
				  (labels ((imp ()
							 (setf parent2 (__rbnode-parent parent))
							 (when (eq parent (__rbnode-left parent2))
							   (setf parent parent2)
							   (if (__rbnode-is-external parent)
								   nil
								   (imp)))))
					(imp))
				  (if (and (not (__rbnode-is-external parent))
						   (not (__rbnode-is-external parent2)))
					  ;; if exists left-parent of right-most ancestor, it's prev node.
					  parent2
					  ;; otherwise, node has min value. -> returns (external) left node.
					  left)))))))




;;------------------------------------------------------------------------------
;;
;; debug methods
;;
;;------------------------------------------------------------------------------
;; used only '__rbtree-check-integrity'...
#+cl-stl-debug
(defun __rbnode-depth (node)
  (if (__rbnode-is-external node)
	  (values 0 0)
	  (multiple-value-bind (l-min l-max)
		  (__rbnode-depth (__rbnode-left node))
		(multiple-value-bind (r-min r-max)
			(__rbnode-depth (__rbnode-right node))
		  (values (1+ (min l-min r-min)) (1+ (max l-max r-max)))))))

;; used only '__rbtree-check-integrity'...
#+cl-stl-debug
(defun __rbnode-size (node)
  (if (__rbnode-is-external node)
	  0
	  (+ 1
		 (__rbnode-size (__rbnode-left  node))
		 (__rbnode-size (__rbnode-right node)))))

#+cl-stl-debug
(defun __rbnode-check-reachable (node1 node2)
  (do ()
	  ((null node1) nil)
	(when (eq node1 node2)
	  (return-from __rbnode-check-reachable t))
	(setf node1 (__rbnode-next node1))))
  
#+cl-stl-debug
(defun __rbnode-check-integrity (node stream pred tag-fnc)
  (if (__rbnode-is-external node)
	  0
	  (let ((ret 0)
			(pred    (functor-function (clone pred)))
			(tag-fnc (functor-function (clone tag-fnc)))
			(left-node  (__rbnode-left  node))
			(right-node (__rbnode-right node)))
		;; recursive call for right child.
		(unless (__rbnode-is-external right-node)
		  (incf ret (__rbnode-check-integrity right-node stream pred tag-fnc)))

		(let ((my-tag    (funcall tag-fnc (__rbnode-value node)))
			  (tag-left  (funcall tag-fnc (__rbnode-value left-node)))
			  (tag-right (funcall tag-fnc (__rbnode-value right-node))))

		  ;; check ordering.
		  (unless (__rbnode-is-external left-node)
			(when (funcall pred (__rbnode-value node)(__rbnode-value left-node))
			  (incf ret)
			  (format stream "[~A] < left [~A]~%" my-tag tag-left)))

		  (unless (__rbnode-is-external right-node)
			(when (funcall pred (__rbnode-value right-node)(__rbnode-value node))
			  (incf ret)
			  (format stream "[~A] > right [~A]~%" my-tag tag-right)))

		  ;; check ranking.
		  (let* ((my-rank     (__rbnode-rank node))
				 (parent-rank (__rbnode-rank (__rbnode-parent node)))
				 (left-rank   (__rbnode-rank (__rbnode-left   node)))
				 (right-rank  (__rbnode-rank (__rbnode-right  node)))
				 (left-delta  (- my-rank left-rank))
				 (right-delta (- my-rank right-rank)))
			(if (= parent-rank my-rank)
				(progn
				  (when (/= left-delta 1)
					(incf ret)
					(format stream "parent(~A) - [~A](~A) - left(~A)"
							parent-rank my-tag my-rank left-rank))
				  (When (/= right-delta 1)
					(incf ret)
					(format stream "parent(~A) - [~A](~A) - right(~A)"
							parent-rank my-tag my-rank right-rank)))
				(progn
				  (when (and (/= left-delta 1) (/= left-delta 0))
					(incf ret)
					(format stream "parent(~A) - [~A](~A) - left(~A)"
							parent-rank my-tag my-rank left-rank))
				  (when (and (/= right-delta 1) (/= right-delta 0))
					(incf ret)
					(format stream "parent(~A) - [~A](~A) - right(~A)"
							parent-rank my-tag my-rank right-rank))))))

		;; recursive call for left child.
		(unless (__rbnode-is-external left-node)
		  (incf ret (__rbnode-check-integrity left-node stream pred tag-fnc)))
		ret)))


