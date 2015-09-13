(in-package :cl-stl)


;;--------------------------------------------------------------------
;;
;; class difinition
;;
;;--------------------------------------------------------------------
#-cl-stl-0x98
(defclass forward-list (forward-container pushable-front-container)
  ((top-node  :type     :cons
			  :initform nil
			  :initarg  :top
			  :accessor __slst-top-node)))

#-cl-stl-0x98
(defclass forward-list-const-iterator (cons-const-iterator) ())

#-cl-stl-0x98
(defclass forward-list-iterator (cons-iterator forward-list-const-iterator) ())

;;--------------------------------------------------------------------
;;
;; internal utilities
;;
;;--------------------------------------------------------------------
#-cl-stl-0x98
(defmacro __slst-newnode (val &optional (next nil) (need-copy t))
  (if need-copy
	  (let ((node (gensym "NODE")))
		`(let ((,node (cons nil ,next)))
		   (_= (car ,node) ,val)
		   ,node))
	  `(cons ,val ,next)))

#-cl-stl-0x98
(defmacro __slst-error-when-empty (cont-sym op)
  (check-type cont-sym symbol)
  `(unless (cdr (__slst-top-node ,cont-sym))
	 (error 'undefined-behavior :what ,(format nil "~A for empty forward-list." op))))

#-cl-stl-0x98
(defmacro __slst-check-iterator-belong (itr slst)
  (declare (ignorable itr slst))
  #-cl-stl-debug nil
  #+cl-stl-debug
  `(unless (let ((chk (__cons-itr-cons ,itr)))
			 (if (null chk)
				 t
				 (do ((cns (__slst-top-node ,slst) (cdr cns)))
					 ((null cns) nil)
				   (when (eq cns chk) (return t)))))
	 (error 'undefined-behavior :what ,(format nil "~A is not iterator of ~A." itr slst))))

#-cl-stl-0x98
(defmacro __slst-check-iterator-range (itr1 itr2)
  (declare (ignorable itr1 itr2))
  #-cl-stl-debug nil
  #+cl-stl-debug
  `(unless (let ((cns2 (__cons-itr-cons ,itr2)))
			 (if (null cns2)
				 t
				 (do ((cns (__cons-itr-cons ,itr1) (cdr cns)))
					 ((null cns) nil)
				   (when (eq cns cns2) (return t)))))
	 (error 'undefined-behavior :what ,(format nil "[~A ~A) is not valid range." itr1 itr2))))




;;--------------------------------------------------------------------
;;
;; method implementation
;;
;;--------------------------------------------------------------------
#-cl-stl-0x98
(defun __slst-new (size &optional (init-elm nil))
  (__error-unless-non-negative-fixnum forward-list size)
  (labels ((make-cons-list (cnt acc)
			 (if (zerop cnt)
				 acc
				 (make-cons-list (1- cnt)
								 (__slst-newnode init-elm acc)))))
	(make-instance 'forward-list
				   :top (__slst-newnode nil (make-cons-list size nil) nil))))

#-cl-stl-0x98
(defun __slst-new-with-sequence (itr1 itr2)
  (with-operators
	  (labels ((make-cons-list (itr top last)
				 (if (_== itr itr2)
					 top
					 (let ((node (__slst-newnode *itr)))
					   (setf (cdr last) node)
					   (make-cons-list ++itr top node)))))
		(let ((top (__slst-newnode nil nil nil)))
		  (make-instance 'forward-list
						 :top (make-cons-list @~itr1 top top))))))

;;------------------------------------------------------------------------------
;;
;; constructors
;;
;;------------------------------------------------------------------------------
#-cl-stl-0x98
(declare-constructor forward-list (0 1 2))

; default constructor
#-cl-stl-0x98
(define-constructor forward-list ()
  (__slst-new 0))

; copy constructor
#-cl-stl-0x98
(define-constructor forward-list ((arg forward-list))
  (clone arg))

; constructor with initializer list
#-cl-stl-0x98
(locally (declare (optimize speed))
  (define-constructor forward-list ((arg initializer-list))
	(let* ((arr (__initlist-data arg))
		   (cnt (length arr)))
	  (declare (type cl:vector arr))
	  (declare (type fixnum    cnt))
	  (let ((idx 0))
		(declare (type fixnum idx))
		(for (((acc nil)) (< idx cnt) (incf idx)
						  :returns (make-instance 'forward-list
												  :top (__slst-newnode nil (nreverse acc) nil)))
		  (setf acc (__slst-newnode (aref arr idx) acc)))))))

; move constructor
#-cl-stl-0x98
(define-constructor forward-list ((arg remove-reference))
  (let ((cont (funcall (__rm-ref-closure arg))))
	(__check-type-of-move-constructor cont forward-list)
	(let ((lst (__slst-top-node cont)))
	  (prog1
		  (make-instance 'forward-list
						 :top (__slst-newnode nil (cdr lst) nil))
		(setf (cdr lst) nil)))))


; fill constructor 1
#-cl-stl-0x98
(define-constructor forward-list ((arg integer))
  (__slst-new arg))

; fill constructor 2
#-cl-stl-0x98
(define-constructor forward-list ((arg1 integer) arg2)
  (__slst-new arg1 arg2))

; range constructor 2
#-cl-stl-0x98
(define-constructor forward-list ((arg1 input-iterator) (arg2 input-iterator))
  (__slst-new-with-sequence arg1 arg2))

#-cl-stl-0x98
(define-constructor forward-list ((arg1 const-vector-pointer) (arg2 const-vector-pointer))
  (__pointer-check-iterator-range arg1 arg2)
  (__slst-new-with-sequence arg1 arg2))


; copy constructor
#-cl-stl-0x98
(defmethod operator_clone ((container forward-list))
  (let* ((src (cdr (__slst-top-node container)))
		 (lst (__conslist-clone src)))
	(make-instance 'forward-list
				   :top (__slst-newnode nil lst nil))))


;;------------------------------------------------------------------------------
;;
;; methods
;;
;;------------------------------------------------------------------------------

;-----------------------------------------------------
; assignment
;-----------------------------------------------------
#-cl-stl-0x98
(let ((eos (gensym "EOS")))
  (labels ((__assign-imp (dest get-next-fnc)
			 ;; MEMO : Must specify 'top sentinel' to dest parameter.
			 (let ((val (funcall get-next-fnc)))
			   (if (eq val eos)
				   (setf (cdr dest) nil)
				   (progn
					 (if (null (cdr dest))
						 (setf (cdr dest) (__slst-newnode val))
						 (_= (cadr dest) val))
					 (__assign-imp (cdr dest) get-next-fnc))))))

	(defmethod operator_= ((cont1 forward-list) (cont2 forward-list))
	  (let ((cns (cdr (__slst-top-node cont2))))
		(__assign-imp (__slst-top-node cont1)
					  (lambda ()
						(if (null cns)
							eos
							(prog1 (car cns) (setf cns (cdr cns)))))))
	  cont1)

	(locally (declare (optimize speed))
	  (defmethod operator_= ((cont forward-list) (il initializer-list))
		(let* ((arr (__initlist-data il))
			   (idx 0)
			   (cnt (length arr)))
		  (declare (type cl:vector arr))
		  (declare (type fixnum    idx cnt))
		  (__assign-imp (__slst-top-node cont)
						(lambda ()
						  (if (= idx cnt)
							  eos
							  (prog1 (aref arr idx) (incf idx))))))
		cont))

	(defmethod-overload assign ((cont forward-list) (count integer) value)
	  ;;MEMO : always returns nil.
	  (__error-unless-non-negative-fixnum assign count)
	  (let ((cnt count))
		(__assign-imp (__slst-top-node cont)
					  (lambda ()
						(if (zerop cnt)
							eos
							(progn (decf cnt) value)))))
	  nil)

	(defmethod-overload assign ((cont forward-list) (itr1 input-iterator) (itr2 input-iterator))
	  ;;MEMO : always returns nil.
	  (let ((itr (clone itr1)))
		(__assign-imp (__slst-top-node cont)
					  (lambda ()
						(with-operators
							(if (_== itr itr2)
								eos
								(prog1 *itr ++itr))))))
	  nil)

	(locally (declare (optimize speed))
	  (defmethod-overload assign ((cont forward-list) (itr1 const-vector-pointer) (itr2 const-vector-pointer))
		;;MEMO : always returns nil.
		(__pointer-check-iterator-range itr1 itr2)
		(let ((idx1 (opr::vec-ptr-index  itr1))
			  (idx2 (opr::vec-ptr-index  itr2))
			  (buf  (opr::vec-ptr-buffer itr1)))
		  (declare (type fixnum idx1 idx2))
		  (declare (type cl:vector buf))
		  (__assign-imp (__slst-top-node cont)
						(lambda ()
						  (with-operators
							  (if (<= idx2 idx1)
								  eos
								  (prog1 (aref buf idx1) (incf idx1)))))))
		nil))

	(locally (declare (optimize speed))
	  (defmethod-overload assign ((cont forward-list) (il initializer-list))
	  ;;MEMO : always returns nil.
		(let* ((arr (__initlist-data il))
			   (idx 0)
			   (cnt (length arr)))
		  (declare (type cl:vector arr))
		  (declare (type fixnum    idx cnt))
		  (__assign-imp (__slst-top-node cont)
						(lambda ()
						  (if (= idx cnt)
							  eos
							  (prog1 (aref arr idx) (incf idx))))))
	  nil))))
			
#-cl-stl-0x98
(defmethod operator_move ((cont1 forward-list) (cont2 forward-list))
  (unless (eq cont1 cont2)
	(setf (__slst-top-node cont1) (__slst-top-node cont2))
	(setf (__slst-top-node cont2) (__slst-newnode nil nil nil)))
  (values cont1 cont2))




;-----------------------------------------------------
; iterators
;-----------------------------------------------------
#-cl-stl-0x98
(defmethod before-begin ((cont forward-list))
  (make-instance 'forward-list-iterator :node (__slst-top-node cont)))

#-cl-stl-0x98
(defmethod begin ((cont forward-list))
  (make-instance 'forward-list-iterator :node (cdr (__slst-top-node cont))))

#-cl-stl-0x98
(defmethod end ((cont forward-list))
  (make-instance 'forward-list-iterator :node nil))

#-cl-stl-0x98
(defmethod cbefore-begin ((cont forward-list))
  (make-instance 'forward-list-const-iterator :node (__slst-top-node cont)))

#-cl-stl-0x98
(defmethod cbegin ((cont forward-list))
  (make-instance 'forward-list-const-iterator :node (cdr (__slst-top-node cont))))

#-cl-stl-0x98
(defmethod cend ((cont forward-list))
  (make-instance 'forward-list-const-iterator :node nil))

;-----------------------------------------------------
; capacity
;-----------------------------------------------------
#-cl-stl-0x98
(defmethod empty ((cont forward-list))
  (null (cdr (__slst-top-node cont))))

#+cl-stl-extra  ; forward-list::size
(progn
  #-cl-stl-0x98 
  (defmethod size ((cont forward-list))
	(__conslist-count-nodes (cdr (__slst-top-node cont)) nil)))

#-cl-stl-0x98
(defmethod max-size ((cont forward-list))
  most-positive-fixnum)

#-cl-stl-0x98
(labels ((__resize-imp (node idx count init-elm)
		   (if (= idx count)
			   (setf (cdr node) nil)
			   (progn
				 (when (null (cdr node))
				   (setf (cdr node) (__slst-newnode init-elm)))
				 (__resize-imp (cdr node) (1+ idx) count init-elm)))))

  (defmethod-overload resize ((cont forward-list) (new-size integer))
	(__resize-imp (__slst-top-node cont) 0 new-size nil))

  (defmethod-overload resize ((cont forward-list) (new-size integer) init-elm)
	(__resize-imp (__slst-top-node cont) 0 new-size init-elm)))

;-----------------------------------------------------
; element access
;-----------------------------------------------------
#-cl-stl-0x98
(defmethod front ((cont forward-list))
  (__slst-error-when-empty cont "front")
  (cadr (__slst-top-node cont)))

#-cl-stl-0x98
(defmethod (setf front) (val (cont forward-list))
  (__slst-error-when-empty cont "front")
  (_= (cadr (__slst-top-node cont)) val))

#+cl-stl-extra ; forward-list::data
(progn
  #-cl-stl-0x98 
  (defmethod data ((cont forward-list))
	(cdr (__slst-top-node cont))))

;-----------------------------------------------------
; modifiers
;-----------------------------------------------------
#-cl-stl-0x98
(defmethod push-front ((cont forward-list) new-val)
  (let ((top-sentinel (__slst-top-node cont))
		(new-node (__slst-newnode new-val)))
	(setf (cdr new-node) (cdr top-sentinel))
	(setf (cdr top-sentinel) new-node))
  nil)

#-cl-stl-0x98
(defmethod pop-front ((cont forward-list))
  (__slst-error-when-empty cont "pop-front")
  (let ((top-sentinel (__slst-top-node cont)))
	(setf (cdr top-sentinel) (cddr top-sentinel)))
  nil)

#-cl-stl-0x98    ; emplace-front
(defmethod-overload emplace-front ((cont forward-list) new-val)
  (let ((top-sentinel (__slst-top-node cont))
		(new-node (__slst-newnode new-val nil nil)))
	(setf (cdr new-node) (cdr top-sentinel))
	(setf (cdr top-sentinel) new-node))
  nil)

#-cl-stl-0x98
(let ((eos (gensym "EOS")))
  (labels ((__insert-imp (node need-copy get-next-fnc)
			 (let ((val (funcall get-next-fnc)))
			   (if (eq eos val)
				   nil
				   (let ((new-node (__slst-newnode nil (cdr node) nil)))
					 (if need-copy
						 (_= (car new-node) val)
						 (setf (car new-node) val))
					 (setf (cdr node) new-node)
					 (__insert-imp new-node need-copy get-next-fnc))))))

	;; insert-after ( single element ) - returns iterator
	(defmethod-overload insert-after ((cont forward-list)
									  (itr  forward-list-const-iterator) value)
	  (__slst-check-iterator-belong itr cont)
	  (let ((node (__cons-itr-cons itr)))
		(__insert-imp node t (lambda ()
							   (prog1 value (setf value eos))))
		(make-instance 'forward-list-iterator :node (cdr node))))

	;; insert-after ( move ) - returns iterator
	(defmethod-overload insert-after ((cont forward-list)
									  (itr  forward-list-const-iterator) (rm remove-reference))
	  (__slst-check-iterator-belong itr cont)
	  (let ((node (__cons-itr-cons itr))
			(val  (funcall (__rm-ref-closure rm))))
		(funcall (__rm-ref-closure rm) nil)
		(__insert-imp node nil (lambda ()
								 (prog1 val (setf val eos))))
		(make-instance 'forward-list-iterator :node (cdr node))))

	;; insert-after ( fill ) - returns iterator
	(defmethod-overload insert-after ((cont forward-list)
									  (itr  forward-list-const-iterator) (count integer) value)
	  (__slst-check-iterator-belong itr cont)
	  (let ((node (__cons-itr-cons itr)))
		(__insert-imp node t (lambda ()
							   (if (zerop count)
								   eos
								   (progn (decf count) value))))
		(make-instance 'forward-list-iterator :node (cdr node))))

	;; insert-after ( range ) - returns iterator.
	(defmethod-overload insert-after ((cont forward-list)
									  (itr  forward-list-const-iterator)
									  (itr1 input-iterator) (itr2 input-iterator))
	  (__slst-check-iterator-belong itr cont)
	  (let ((itr1 (clone itr1))
			(node (__cons-itr-cons itr)))
		(__insert-imp node t (lambda ()
							   (with-operators
								   (if (_== itr1 itr2)
									   eos
									   (prog1 *itr1 ++itr1)))))
		(make-instance 'forward-list-iterator :node (cdr node))))

	;; insert-after ( range ) - returns iterator.
	(locally (declare (optimize speed))
	  (defmethod-overload insert-after ((cont forward-list)
										(itr  forward-list-const-iterator)
										(itr1 const-vector-pointer) (itr2 const-vector-pointer))
		(__slst-check-iterator-belong itr cont)
		(__pointer-check-iterator-range itr1 itr2)
		(let ((node (__cons-itr-cons itr))
			  (idx1 (opr::vec-ptr-index  itr1))
			  (idx2 (opr::vec-ptr-index  itr2))
			  (buf  (opr::vec-ptr-buffer itr1)))
		  (declare (type fixnum idx1 idx2))
		  (declare (type cl:vector buf))
		  (__insert-imp node t (lambda ()
								 (with-operators
									 (if (<= idx2 idx1)
										 eos
										 (prog1 (aref buf idx1) (incf idx1))))))
		  (make-instance 'forward-list-iterator :node (cdr node)))))

	;; insert-after ( initializer list ) - returns iterator.
	(locally (declare (optimize speed))
	  (defmethod-overload insert-after ((cont forward-list)
										(itr  forward-list-const-iterator) (il initializer-list))
		(__slst-check-iterator-belong itr cont)
		(let* ((arr  (__initlist-data il))
			   (idx  0)
			   (cnt  (length arr))
			   (node (__cons-itr-cons itr)))
		  (declare (type cl:vector arr))
		  (declare (type fixnum    idx cnt))
		  (__insert-imp node t (lambda ()
								 (if (= idx cnt)
									 eos
									 (prog1
										 (aref arr idx)
									   (incf idx)))))
		  (make-instance 'forward-list-iterator :node (cdr node)))))

	;; emplace-after
	(defmethod-overload emplace-after ((cont forward-list)
									   (itr  forward-list-const-iterator) value)
	  (__slst-check-iterator-belong itr cont)
	  (let ((node (__cons-itr-cons itr)))
		(__insert-imp node nil (lambda ()
								 (prog1 value (setf value eos))))
		(make-instance 'forward-list-iterator :node (cdr node))))))


#-cl-stl-0x98
(labels ((__erase-imp (node1 node2)
		   (if (eq node1 node2)
			   node2
			   (progn
				 (setf (cdr node1) (cddr node1))
				 (__erase-imp (cdr node1) node2)))))

  (defmethod-overload erase-after ((cont forward-list)
								   (itr  forward-list-const-iterator))
	(__slst-check-iterator-belong itr cont)
	(let ((node (__cons-itr-cons itr)))
	  (make-instance 'forward-list-iterator
					 :node (__erase-imp node (cddr node)))))

  (defmethod-overload erase-after ((cont  forward-list)
								   (first forward-list-const-iterator)
								   (last  forward-list-const-iterator))
	(__slst-check-iterator-belong first cont)
	(__slst-check-iterator-range  first last)
	(make-instance 'forward-list-iterator
				   :node (__erase-imp (__cons-itr-cons first)
									  (__cons-itr-cons last)))))

#-cl-stl-0x98
(defmethod-overload swap ((cont1 forward-list) (cont2 forward-list))
  (let* ((top1  (__slst-top-node cont1))
		 (top2  (__slst-top-node cont2))
		 (node1 (cdr top1))
		 (node2 (cdr top2)))
	(setf (cdr top1) node2)
	(setf (cdr top2) node1))
  (values cont1 cont2))

#-cl-stl-0x98
(defmethod clear ((cont forward-list))
  (setf (cdr (__slst-top-node cont)) nil)
  nil)

;-----------------------------------------------------
; specific operations
;-----------------------------------------------------
#-cl-stl-0x98
(labels ((__get-end-node-of (node1 node2)
		   (if (eq (cdr node1) node2)
			   node1
			   (__get-end-node-of (cdr node1) node2)))
		 (__splice-imp (dest-before src-before src-last)
		   (let ((dest-next (cdr dest-before))
				 (src-begin (cdr src-before))
				 (src-end   (cdr src-last)))
			 (setf (cdr dest-before) src-begin)
			 (setf (cdr src-last)    dest-next)
			 (setf (cdr src-before)  src-end)
			 nil)))
  ;;MEMO : always return nil.
  (defmethod-overload splice-after ((this forward-list)
									(position forward-list-const-iterator)
									(fwdlst forward-list))
	(__slst-check-iterator-belong position this)
	(when (eq this fwdlst)
	  (error 'undefined-behavior :what "splice-after : (eq this fwdlst)."))
	(let ((node1  (__cons-itr-cons position))
		  (srctop (__slst-top-node fwdlst)))
	  (unless node1
		(error 'undefined-behavior :what "splice-after : invalid 'position'."))
	  (__splice-imp node1 srctop (__get-end-node-of srctop nil))))

  ;;MEMO : always return nil.
  (defmethod-overload splice-after ((this forward-list)
									(position forward-list-const-iterator)
									(fwdlst forward-list)
									(i forward-list-const-iterator))
	(__slst-check-iterator-belong position this)
	(__slst-check-iterator-belong i fwdlst)
	(let* ((node1  (__cons-itr-cons position))
		   (srctop (__cons-itr-cons i)))
	  (unless node1
		(error 'undefined-behavior :what "splice-after : invalid 'position'."))
	  (__splice-imp node1 srctop (cdr srctop))))

  ;;MEMO : always return nil.
  (defmethod-overload splice-after ((this forward-list)
									(position forward-list-const-iterator)
									(fwdlst forward-list)
									(first forward-list-const-iterator)
									(last forward-list-const-iterator))
	(__slst-check-iterator-belong position this)
	(__slst-check-iterator-belong first fwdlst)
	(__slst-check-iterator-range first last)
	(let ((node1  (__cons-itr-cons position))
		  (srctop (__cons-itr-cons first)))
	  (unless node1
		(error 'undefined-behavior :what "splice-after : invalid 'position'."))
	  (__splice-imp node1 srctop
					(__get-end-node-of srctop (__cons-itr-cons last))))))


#-cl-stl-0x98
(labels ((__remove-imp (lst pred)
		   (if (null (cdr lst))
			   nil
			   (progn
				 (when (funcall pred (cadr lst))
				   (setf (cdr lst) (cddr lst)))
				 (__remove-imp (cdr lst) pred)))))

  (defmethod-overload remove ((lst forward-list) val)
	(__remove-imp (__slst-top-node lst)
		 (lambda (cur)
		   (_== val cur))))

  #+cl-stl-extra
  (defmethod-overload remove ((lst forward-list) val eql-bf)
	(let ((eql-bf (clone eql-bf)))
	  (__remove-imp (__slst-top-node lst)
		   (lambda (cur)
			 (funcall eql-bf val cur)))))

  (defmethod-overload remove-if ((lst forward-list) pred-uf)
	(let ((pred-uf (clone pred-uf)))
	  (__remove-imp (__slst-top-node lst)
		   (lambda (cur)
			 (funcall pred-uf cur))))))


#-cl-stl-0x98
(labels ((__unique-imp (prev-node next-node eql-bf)
		   ;; MEMO : eql-bf copied by caller. 
		   (unless (null next-node)
			 (if (funcall eql-bf (car prev-node) (car next-node))
				 (progn
				   (setf next-node (cdr next-node))
				   (setf (cdr prev-node) next-node))
				 (progn
				   (setf prev-node next-node)
				   (setf next-node (cdr next-node))))
			 (__unique-imp prev-node next-node eql-bf))))

  (defmethod-overload unique ((lst forward-list))
	(let ((first (cdr (__slst-top-node lst))))
	  (when first
		(__unique-imp first (cdr first) #'operator_==))
	  nil))

  (defmethod-overload unique ((lst forward-list) eql-bf)
	(let ((first (cdr (__slst-top-node lst))))
	  (when first
		(__unique-imp first (cdr first) (functor-function (clone eql-bf))))
	  nil)))
				   

#-cl-stl-0x98
(labels ((__merge-imp (lst1 lst2 top last less-bf)
		   ;; MEMO : less-bf copied by caller. 
		   (if (and (null lst1) (null lst2))
			   top
			   (let ((the-cons nil))
				 (cond
				   ((null lst2)
					(setf the-cons lst1)
					(setf lst1 (cdr lst1)))
				   ((null lst1)
					(setf the-cons lst2)
					(setf lst2 (cdr lst2)))
				   (t
					(if (funcall less-bf (car lst2) (car lst1))
						(progn (setf the-cons lst2) (setf lst2 (cdr lst2)))
						(progn (setf the-cons lst1) (setf lst1 (cdr lst1))))))
				 (setf (cdr the-cons) nil)
				 (if (null top)
					 (progn
					   (setf top the-cons)
					   (setf last the-cons))
					 (progn
					   (setf (cdr last) the-cons)
					   (setf last the-cons)))
				 (__merge-imp lst1 lst2 top last less-bf)))))

  (defmethod-overload merge ((lst1 forward-list)
							 (lst2 forward-list))
	(let ((top1 (__slst-top-node lst1))
		  (top2 (__slst-top-node lst1)))
	  (setf (cdr top1) (__merge-imp top1 top2 nil nil #'operator_<))
	  (setf (cdr top2) nil)
	  nil))

  (defmethod-overload merge ((lst1 forward-list)
							 (lst2 forward-list) less-bf)
	(let ((top1 (__slst-top-node lst1))
		  (top2 (__slst-top-node lst1)))
	  (setf (cdr top1) (__merge-imp top1 top2 nil nil
									(functor-function (clone less-bf))))
	  (setf (cdr top2) nil)
	  nil)))

#-cl-stl-0x98
(defmethod-overload sort ((lst forward-list))
  (setf (cdr (__slst-top-node lst))
		(cl:sort (cdr (__slst-top-node lst))
				 (lambda (a b) (_< a b)))))

#-cl-stl-0x98
(defmethod-overload sort ((lst forward-list) less-bf)
  (setf (cdr (__slst-top-node lst))
		(cl:sort (cdr (__slst-top-node lst))
				 (lambda (a b) (funcall less-bf a b)))))

#-cl-stl-0x98
(defmethod-overload reverse ((lst forward-list))
  (let ((top-sentinel (__slst-top-node lst)))
	(setf (cdr top-sentinel) (nreverse (cdr top-sentinel))))
  nil)

;-----------------------------------------------------
; compare
;-----------------------------------------------------
#-cl-stl-0x98
(locally (declare (optimize speed))

  (defmethod operator_== ((cont1 forward-list) (cont2 forward-list))
	(if (eq cont1 cont2)
		t
		(__conslist-equal (cdr (__slst-top-node cont1))
						  (cdr (__slst-top-node cont2)))))

  (defmethod operator_/= ((cont1 forward-list) (cont2 forward-list))
	(if (eq cont1 cont2)
		nil
		(not (__conslist-equal (cdr (__slst-top-node cont1))
							   (cdr (__slst-top-node cont2))))))

  (defmethod operator_< ((cont1 forward-list) (cont2 forward-list))
	(< (__conslist-compare (cdr (__slst-top-node cont1))
						   (cdr (__slst-top-node cont2))) 0))

  (defmethod operator_<= ((cont1 forward-list) (cont2 forward-list))
	(<= (__conslist-compare (cdr (__slst-top-node cont1))
							(cdr (__slst-top-node cont2))) 0))

  (defmethod operator_> ((cont1 forward-list) (cont2 forward-list))
	(< 0 (__conslist-compare (cdr (__slst-top-node cont1))
							 (cdr (__slst-top-node cont2)))))

  (defmethod operator_>= ((cont1 forward-list) (cont2 forward-list))
	(<= 0 (__conslist-compare (cdr (__slst-top-node cont1))
							  (cdr (__slst-top-node cont2))))))
			 

;-----------------------------------------------------
; enumeration
;-----------------------------------------------------
#-cl-stl-0x98
(locally (declare (optimize speed))
  (defmethod-overload for ((cont forward-list) func)
	;MEMO : func is always lambda function ( see stl:for ). 
	(dolist (itm (cdr (__slst-top-node cont)))
	  (funcall func itm))))


;;------------------------------------------------------------------------------
;;
;; methods for forward-list-const-iterator
;;
;;------------------------------------------------------------------------------
#-cl-stl-0x98
(defmethod operator_= ((itr1 forward-list-const-iterator) (itr2 forward-list-const-iterator))
  (__error-when-const-removing-assign itr1 forward-list-iterator
									  itr2 forward-list-const-iterator)
  (setf (__cons-itr-cons itr1) (__cons-itr-cons itr2))
  itr1)

#-cl-stl-0x98
(defmethod operator_clone ((itr forward-list-const-iterator))
  (make-instance 'forward-list-const-iterator :node (__cons-itr-cons itr)))


;;------------------------------------------------------------------------------
;;
;; methods for forward-list-iterator
;;
;;------------------------------------------------------------------------------
#-cl-stl-0x98
(defmethod operator_clone ((itr forward-list-iterator))
  (make-instance 'forward-list-iterator :node (__cons-itr-cons itr)))

#-cl-stl-0x98
(defmethod operator_cast ((itr forward-list-iterator)
						  (typename (eql 'forward-list-const-iterator)))
  (__check-exact-type-of-cast itr 'forward-list-iterator
								  'forward-list-const-iterator)
  (make-instance 'forward-list-iterator :node (__cons-itr-cons itr)))



;;------------------------------------------------------------------------------
;;
;; debug methods for forward-list
;;
;;------------------------------------------------------------------------------

#+cl-stl-debug
(progn
  #-cl-stl-0x98
  (defmethod dump ((container forward-list) &optional (stream t) (print-item-fnc nil))
	(setf print-item-fnc (if print-item-fnc
							 (functor-function (clone print-item-fnc))
							 (lambda (s x) (format s "~A" x))))
	(format stream "begin dump ---------------------~%")
	(let ((idx 0))
	  (stl:for (v container)
			   (format stream "~A : " idx)
			   (funcall print-item-fnc stream v)
			   (format stream "~%")
			   (incf idx)))
	(format stream "end dump -----------------------~%")
	nil))

#+cl-stl-debug
(progn
  #-cl-stl-0x98
  (defmethod check-integrity ((container forward-list) &optional (stream t))
	(let ((err-count 0)
		  (node (__slst-top-node container)))
	  ;; top-node member must be cons.
	  (unless (consp node)
		(incf err-count)
		(format stream "  ERROR : top sentinel is missing.~%"))
	  ;; cdr chain is must be cons or nil.
	  (do ()
		  ((null node) nil)
		(unless (or (null  (cdr node))
					(consp (cdr node)))
		  (incf err-count)
		  (format stream "  ERROR : list chain is corrupted.~%")
		  (return))
		(setf node (cdr node)))
	  (zerop err-count))))
