
(in-package :cl-stl)

#-cl-stl-0x98
(declaim (inline make-move-iterator))


;;------------------------------------------------------------------------------
;;
;; move iterator classes
;;
;;------------------------------------------------------------------------------
#-cl-stl-0x98
(progn
  (defclass move-iterator_in (input-iterator)
	((itr  :initform nil
		   :initarg  :iterator
		   :accessor __moveitr-iterator)
	 (rm   :type     :remove-reference
		   :initarg  :rm-ref
		   :accessor __moveitr-rm-ref)))
  (defclass move-iterator_fwd (      forward-iterator move-iterator_in ) ())
  (defclass move-iterator_bid (bidirectional-iterator move-iterator_fwd) ())
  (defclass move-iterator_rdm ( randomaccess-iterator move-iterator_bid) ()))


#-cl-stl-0x98
(macrolet ((movitr-ctor (param-type itr-type)
			 `(define-constructor move-iterator ((itr ,param-type))
				(let* ((itr (clone itr))
					   (rm  (move (_* itr))))
				  (make-instance ',itr-type :iterator itr :rm-ref rm)))))
  (movitr-ctor         input-iterator move-iterator_in)
  (movitr-ctor       forward-iterator move-iterator_fwd)
  (movitr-ctor bidirectional-iterator move-iterator_bid)
  (movitr-ctor  randomaccess-iterator move-iterator_rdm))

#-cl-stl-0x98
(defun make-move-iterator (itr)
  (new stl::move-iterator itr))


;;------------------------------------------------------------------------------
;; implementation of move-iterator_in
;;------------------------------------------------------------------------------
#-cl-stl-0x98
(defmethod operator_= ((itr1 move-iterator_in) (itr2 move-iterator_in))
  (_= (__moveitr-iterator itr1) (__moveitr-iterator itr2))
  itr1)

#-cl-stl-0x98
(defmethod operator_clone ((itr move-iterator_in))
  (let* ((tmp (clone (__moveitr-iterator itr)))
		 (rm  (move (_* tmp))))
	(make-instance (type-of itr) :iterator tmp :rm-ref rm)))

#-cl-stl-0x98
(defmethod operator_* ((itr move-iterator_in))
  (__moveitr-rm-ref itr))

#-cl-stl-0x98
(defmethod operator_++ ((itr move-iterator_in))
  (setf (__moveitr-iterator itr)
		(operator_++ (__moveitr-iterator itr)))
  itr)

#-cl-stl-0x98
(defmethod operator_== ((itr1 move-iterator_in) (itr2 move-iterator_in))
  (_== (__moveitr-iterator itr1) (__moveitr-iterator itr2)))

#-cl-stl-0x98
(defmethod operator_/= ((itr1 move-iterator_in) (itr2 move-iterator_in))
  (_/= (__moveitr-iterator itr1) (__moveitr-iterator itr2)))

#-cl-stl-0x98
(defmethod base ((itr move-iterator_in))
  (clone (__moveitr-iterator itr)))



;;------------------------------------------------------------------------------
;; implementation of move-iterator_fwd
;;------------------------------------------------------------------------------
#-cl-stl-0x98
(defmethod (setf operator_*) (new-val (itr move-iterator_fwd))
  (setf (_* (__moveitr-iterator itr)) new-val)
  new-val)

#-cl-stl-0x98
(defmethod advance ((itr move-iterator_fwd) (n integer))
  (advance (__moveitr-iterator itr) n)
  nil)

#-cl-stl-0x98
(defmethod distance ((itr1 move-iterator_fwd) (itr2 move-iterator_fwd))
  (distance (__moveitr-iterator itr1) (__moveitr-iterator itr2)))


;;------------------------------------------------------------------------------
;; implementation of move-iterator_bid
;;------------------------------------------------------------------------------
#-cl-stl-0x98
(defmethod operator_-- ((itr move-iterator_bid))
  (setf (__moveitr-iterator itr)
		(operator_-- (__moveitr-iterator itr)))
  itr)

;; CAN'T creating reverse iterator.
#-cl-stl-0x98
(define-constructor reverse-iterator ((itr move-iterator_bid))
  (error 'type-mismatch :what "reverse-iterator can't create from move-iterator"))

;;------------------------------------------------------------------------------
;; implementation of move-iterator_rdm
;;------------------------------------------------------------------------------
#-cl-stl-0x98
(defmethod operator_< ((itr1 move-iterator_rdm) (itr2 move-iterator_rdm))
  (_< (__moveitr-iterator itr1) (__moveitr-iterator itr2)))

#-cl-stl-0x98
(defmethod operator_<= ((itr1 move-iterator_rdm) (itr2 move-iterator_rdm))
  (_<= (__moveitr-iterator itr1) (__moveitr-iterator itr2)))

#-cl-stl-0x98
(defmethod operator_> ((itr1 move-iterator_rdm) (itr2 move-iterator_rdm))
  (_> (__moveitr-iterator itr1) (__moveitr-iterator itr2)))

#-cl-stl-0x98
(defmethod operator_>= ((itr1 move-iterator_rdm) (itr2 move-iterator_rdm))
  (_>= (__moveitr-iterator itr1) (__moveitr-iterator itr2)))

#-cl-stl-0x98
(defmethod operator_+ ((itr move-iterator_rdm) (n integer))
  (let ((r (clone itr)))
	(advance r n)
	r))

#-cl-stl-0x98
(defmethod operator_+= ((itr move-iterator_rdm) (n integer))
  (advance itr n)
  itr)

#-cl-stl-0x98
(defmethod operator_- ((itr move-iterator_rdm) (n integer))
  (let ((r (clone itr)))
	(advance r (* -1 n))
	r))

#-cl-stl-0x98
(defmethod operator_- ((itr1 move-iterator_rdm) (itr2 move-iterator_rdm))
  (_- (__moveitr-iterator itr1)  (__moveitr-iterator itr2)))

#-cl-stl-0x98
(defmethod operator_-= ((itr move-iterator_rdm) (n integer))
  (advance itr (* -1 n))
  itr)

#-cl-stl-0x98
(defmethod operator_[] ((itr move-iterator_rdm) (idx integer))
  (move (_[] (__moveitr-iterator itr) idx)))

#-cl-stl-0x98
(defmethod (setf operator_[]) (new-val (itr move-iterator_rdm) (idx integer))
  (_= (_[] (__moveitr-iterator itr) idx) new-val)
  new-val)

;; CAN'T creating reverse iterator.
#-cl-stl-0x98
(define-constructor reverse-iterator ((itr move-iterator_rdm))
  (error 'type-mismatch :what "reverse-iterator can't create from move-iterator"))

