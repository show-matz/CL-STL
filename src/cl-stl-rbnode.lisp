(in-package :cl-stl)

(declaim (inline __rbnode-color
				 __rbnode-parent
				 __rbnode-left
				 __rbnode-right
				 __rbnode-value
				 (setf __rbnode-color)
				 (setf __rbnode-parent)
				 (setf __rbnode-left)
				 (setf __rbnode-right)
				 (setf __rbnode-value)
				 __rbnode-is-red
				 __rbnode-is-black
				 __rbnode-is-left-child
				 __rbnode-is-right-child
				 __rbnode-minimum
				 __rbnode-maximum))



;;------------------------------------------------------------------------------
;;
;; class difinition
;;
;;------------------------------------------------------------------------------
(locally (declare (optimize speed))
  (defstruct (rbnode (:conc-name __rbnode-))
	(color  :red :type symbol)
	(parent nil)
	(left   nil)
	(right  nil)
	(value  nil)))


;;------------------------------------------------------------------------------
;;
;; rbnode utilities
;;
;;------------------------------------------------------------------------------
(locally (declare (optimize speed))

  (defun __rbnode-is-red (node)
	(declare (type rbnode node))
	(eq :red (__rbnode-color node)))

  (defun __rbnode-is-black (node)
	(declare (type rbnode node))
	(eq :black (__rbnode-color node)))

  (defun __rbnode-is-left-child (node)
	(declare (type rbnode node))
	(eq node (__rbnode-left (__rbnode-parent node))))

  (defun __rbnode-is-right-child (node)
	(declare (type rbnode node))
	(eq node (__rbnode-right (__rbnode-parent node)))))



(locally (declare (optimize speed))

  (defun __rbnode-minimum (node)
	(declare (type rbnode node))
	(let ((left (__rbnode-left node)))
	  (if (null left)
		  node
		  (__rbnode-minimum left))))

  (defun __rbnode-maximum (node)
	(declare (type rbnode node))
	(let ((right (__rbnode-right node)))
	  (if (null right)
		  node
		  (__rbnode-maximum right)))))



(locally (declare (optimize speed))
  (defun __rbnode-increment (node)
	(declare (type rbnode node))
	(if (__rbnode-right node)
		(progn
		  (setf node (__rbnode-right node))
		  (do ()
			  ((null (__rbnode-left node)) node)
			(setf node (__rbnode-left node))))
		(let ((node2 (__rbnode-parent node)))
		  (declare (type rbnode node2))
		  (do ()
			  ((not (eq node (__rbnode-right node2))))
			(setf node node2)
			(setf node2 (__rbnode-parent node2)))
		  (unless (eq (__rbnode-right node) node2)
			(setf node node2))
		  node))))
		  
(locally (declare (optimize speed))
  (defun __rbnode-decrement (node)
	(declare (type rbnode node))
	(if (and (__rbnode-is-red node)
			 (eq node (__rbnode-parent (__rbnode-parent node))))
		(__rbnode-right node)
		(if (__rbnode-left node)
			(let ((node2 (__rbnode-left node)))
			  (declare (type rbnode node2))
			  (do ()
				  ((null (__rbnode-right node2)) node2)
				(setf node2 (__rbnode-right node2))))
			(let ((node2 (__rbnode-parent node)))
			  (declare (type rbnode node2))
			  (do ()
				  ((not (eq node (__rbnode-left node2))) node2)
				(setf node node2)
				(setf node2 (__rbnode-parent node2))))))))


(locally (declare (optimize speed))
  (defun __rbnode-rotate-left (node root)
	(declare (type rbnode node root))
	(let ((right (__rbnode-right node)))
	  (declare (type rbnode right))
	  (setf (__rbnode-right node) (__rbnode-left right))
	  (when (__rbnode-left right)
		(setf (__rbnode-parent (__rbnode-left right)) node))
	  (setf (__rbnode-parent right) (__rbnode-parent node))
	  (if (eq node root)
		  (setf root right)
		  (if (eq node (__rbnode-left (__rbnode-parent node)))
			  (setf (__rbnode-left  (__rbnode-parent node)) right)
			  (setf (__rbnode-right (__rbnode-parent node)) right)))
	  (setf (__rbnode-left  right) node)
	  (setf (__rbnode-parent node) right)
	  root)))


(locally (declare (optimize speed))
  (defun __rbnode-rotate-right (node root)
	(declare (type rbnode node root))
	(let ((left (__rbnode-left node)))
	  (declare (type rbnode left))
	  (setf (__rbnode-left node) (__rbnode-right left))
	  (when (__rbnode-right left)
		(setf (__rbnode-parent (__rbnode-right left)) node))
	  (setf (__rbnode-parent left) (__rbnode-parent node))
	  (if (eq node root)
		  (setf root left)
		  (if (eq node (__rbnode-right (__rbnode-parent node)))
			  (setf (__rbnode-right (__rbnode-parent node)) left)
			  (setf (__rbnode-left  (__rbnode-parent node)) left)))
	  (setf (__rbnode-right  left) node)
	  (setf (__rbnode-parent node) left)
	  root)))

