(in-package :cl-stl)

(defmacro __algo-make-cns-iterator (base cell)
  (let ((g-itr (gensym)))
	`(let ((,g-itr (clone ,base)))
	   (setf (__cons-itr-cons ,g-itr) ,cell)
	   ,g-itr)))

(defmacro __algo-make-vct-iterator (base index)
  (let ((g-itr (gensym)))
	`(let ((,g-itr (clone ,base)))
	   (setf (opr::vec-ptr-index ,g-itr) ,index)
	   ,g-itr)))

