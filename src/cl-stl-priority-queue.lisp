(in-package :cl-stl)

;;------------------------------------------------------------------------------
;;
;; class difinition
;;
;;------------------------------------------------------------------------------
(defclass priority-queue (clonable)
  ((pred      :initform  #'operator_<
			  :initarg   :pred
			  :accessor  __prique-pred)
   (container :type     :randomaccess-container
			  :initform (new stl:vector)
			  :initarg  :container
			  :accessor __prique-container)))


;;------------------------------------------------------------------------------
;;
;; internal utilities
;;
;;------------------------------------------------------------------------------
(defmacro check-underlying-container-of-prique (cont)
  (check-type cont symbol)
  `(unless (and (typep ,cont 'randomaccess-container)
				(typep ,cont 'pushable-back-container))
	 (error 'type-mismatch :what "Underlying container of priority-queue must be randomaccess-container and be pushable-back-container.")))


;;------------------------------------------------------------------------------
;;
;; constructors
;;
;;------------------------------------------------------------------------------
(declare-constructor priority-queue (0 1 2 3 4))

; empty
(define-constructor priority-queue ()
  (make-instance 'priority-queue
				 :pred #'operator_< :container (new stl:vector)))

; initialize - 1
(define-constructor priority-queue ((comp cl:function))
  (make-instance 'priority-queue
				 :pred comp :container (new stl:vector)))

; initialize - 2
(define-constructor priority-queue ((comp #-cl-stl-0x98 functor
										  #+cl-stl-0x98 binary-function))
  (make-instance 'priority-queue
				 :pred (clone comp) :container (new stl:vector)))

; initialize - 3
(define-constructor priority-queue ((comp cl:function)
									(ctnr randomaccess-container))
  (let ((tmp (clone ctnr)))
	(make-heap (begin tmp) (end tmp) comp)
	(make-instance 'priority-queue :pred comp :container tmp)))

; initialize - 4
(define-constructor priority-queue ((comp #-cl-stl-0x98 functor
										  #+cl-stl-0x98 binary-function)
									(ctnr randomaccess-container))
  (let ((tmp (clone ctnr)))
	(make-heap (begin tmp) (end tmp) comp)
	(make-instance 'priority-queue
				   :pred (clone comp) :container tmp)))

; range - 1
(define-constructor priority-queue ((itr1 input-iterator) (itr2 input-iterator))
  (let ((tmp (new stl:vector itr1 itr2)))
	(make-heap (begin tmp) (end tmp) #'operator_<)
	(make-instance 'priority-queue :pred #'operator_< :container tmp)))

(define-constructor priority-queue ((itr1 const-vector-pointer) (itr2 const-vector-pointer))
  (__pointer-check-iterator-range itr1 itr2)
  (let ((tmp (new stl:vector itr1 itr2)))
	(make-heap (begin tmp) (end tmp) #'operator_<)
	(make-instance 'priority-queue :pred #'operator_< :container tmp)))

; range - 2
(define-constructor priority-queue ((itr1 input-iterator) (itr2 input-iterator) (comp cl:function))
  (let ((tmp (new stl:vector itr1 itr2)))
	(make-heap (begin tmp) (end tmp) comp)
	(make-instance 'priority-queue :pred comp :container tmp)))

(define-constructor priority-queue ((itr1 const-vector-pointer) (itr2 const-vector-pointer) (comp cl:function))
  (__pointer-check-iterator-range itr1 itr2)
  (let ((tmp (new stl:vector itr1 itr2)))
	(make-heap (begin tmp) (end tmp) comp)
	(make-instance 'priority-queue :pred comp :container tmp)))

; range - 3
(define-constructor priority-queue ((itr1 input-iterator) (itr2 input-iterator)
									(comp #-cl-stl-0x98 functor
										  #+cl-stl-0x98 binary-function))
  (let ((tmp (new stl:vector itr1 itr2)))
	(make-heap (begin tmp) (end tmp) comp)
	(make-instance 'priority-queue :pred (clone comp) :container tmp)))

(define-constructor priority-queue ((itr1 const-vector-pointer) (itr2 const-vector-pointer)
									(comp #-cl-stl-0x98 functor
										  #+cl-stl-0x98 binary-function))
  (__pointer-check-iterator-range itr1 itr2)
  (let ((tmp (new stl:vector itr1 itr2)))
	(make-heap (begin tmp) (end tmp) comp)
	(make-instance 'priority-queue :pred (clone comp) :container tmp)))

; range - 4
(define-constructor priority-queue ((itr1 input-iterator) (itr2 input-iterator)
									(comp cl:function) (ctnr randomaccess-container))
  (let ((tmp (clone ctnr)))
	(insert tmp (end tmp) itr1 itr2)
	(make-heap (begin tmp) (end tmp) comp)
	(make-instance 'priority-queue :pred comp :container tmp)))

(define-constructor priority-queue ((itr1 const-vector-pointer) (itr2 const-vector-pointer)
									(comp cl:function) (ctnr randomaccess-container))
  (__pointer-check-iterator-range itr1 itr2)
  (let ((tmp (clone ctnr)))
	(insert tmp (end tmp) itr1 itr2)
	(make-heap (begin tmp) (end tmp) comp)
	(make-instance 'priority-queue :pred comp :container tmp)))

; range - 5
(define-constructor priority-queue ((itr1 input-iterator) (itr2 input-iterator)
									(comp #-cl-stl-0x98 functor
										  #+cl-stl-0x98 binary-function) (ctnr randomaccess-container))
  (let ((tmp (clone ctnr)))
	(insert tmp (end tmp) itr1 itr2)
	(make-heap (begin tmp) (end tmp) comp)
	(make-instance 'priority-queue :pred (clone comp) :container tmp)))

(define-constructor priority-queue ((itr1 const-vector-pointer) (itr2 const-vector-pointer)
									(comp #-cl-stl-0x98 functor
										  #+cl-stl-0x98 binary-function) (ctnr randomaccess-container))
  (__pointer-check-iterator-range itr1 itr2)
  (let ((tmp (clone ctnr)))
	(insert tmp (end tmp) itr1 itr2)
	(make-heap (begin tmp) (end tmp) comp)
	(make-instance 'priority-queue :pred (clone comp) :container tmp)))

; copy constructor
(define-constructor priority-queue ((cont priority-queue))
  (make-instance 'priority-queue
				 :pred (clone (__prique-pred cont))
				 :container (clone (__prique-container cont))))

; move-initialize - 1
#-cl-stl-0x98
(define-constructor priority-queue ((comp cl:function)
									(rm-ref remove-reference))
  (let ((cont (funcall (__rm-ref-closure rm-ref))))
	(check-underlying-container-of-prique cont)
	(let ((tmp (dynamic-new (type-of cont))))
	  (swap tmp cont)
	  (make-heap (begin tmp) (end tmp) comp)
	  (make-instance 'priority-queue :pred comp :container tmp))))

; move-initialize - 2
#-cl-stl-0x98
(define-constructor priority-queue ((comp #-cl-stl-0x98 functor
										  #+cl-stl-0x98 binary-function)
									(rm-ref remove-reference))
  (let ((cont (funcall (__rm-ref-closure rm-ref))))
	(check-underlying-container-of-prique cont)
	(let ((tmp (dynamic-new (type-of cont))))
	  (swap tmp cont)
	  (make-heap (begin tmp) (end tmp) comp)
	  (make-instance 'priority-queue :pred (clone comp) :container tmp))))

#-cl-stl-0x98
(labels ((__ctor-imp (itr1 itr2 comp rm-ref)
		   (let ((cont (funcall (__rm-ref-closure rm-ref))))
			 (check-underlying-container-of-prique cont)
			 (let ((tmp (dynamic-new (type-of cont))))
			   ;;(funcall (__rm-ref-closure rm-ref) nil)
			   (swap tmp cont)
			   (insert tmp (end tmp) itr1 itr2)
			   (make-heap (begin tmp) (end tmp) comp)
			   (make-instance 'priority-queue :pred comp :container tmp)))))

  ;; move-range - 1
  (define-constructor priority-queue ((itr1 input-iterator) (itr2 input-iterator)
									  (comp cl:function) (rm-ref remove-reference))
	(__ctor-imp itr1 itr2 comp rm-ref))
  
  (define-constructor priority-queue ((itr1 const-vector-pointer) (itr2 const-vector-pointer)
									  (comp cl:function) (rm-ref remove-reference))
	(__pointer-check-iterator-range itr1 itr2)
	(__ctor-imp itr1 itr2 comp rm-ref))

  ;; move-range - 2
  (define-constructor priority-queue ((itr1 input-iterator) (itr2 input-iterator)
									  (comp #-cl-stl-0x98 functor
											#+cl-stl-0x98 binary-function) (rm-ref remove-reference))
	(__ctor-imp itr1 itr2 comp rm-ref))

  (define-constructor priority-queue ((itr1 const-vector-pointer) (itr2 const-vector-pointer)
									  (comp #-cl-stl-0x98 functor
											#+cl-stl-0x98 binary-function) (rm-ref remove-reference))
	(__pointer-check-iterator-range itr1 itr2)
	(__ctor-imp itr1 itr2 comp rm-ref)))

; move constructor.
#-cl-stl-0x98
(define-constructor priority-queue ((rm-ref remove-reference))
  (let ((cont (funcall (__rm-ref-closure rm-ref))))
	(__check-type-of-move-constructor cont priority-queue)
	(let* ((src-pred (__prique-pred cont))
		   (src-cont (__prique-container cont))
		   (new-pred (clone src-pred))
		   (new-cont (dynamic-new (type-of src-cont))))
	  (swap new-cont src-cont)
	  (make-instance 'priority-queue
					 :pred new-pred :container new-cont))))



(defmethod operator_clone ((arg priority-queue))
  (make-instance 'priority-queue
				 :pred (clone (__prique-pred arg))
				 :container (clone (__prique-container arg))))


;;------------------------------------------------------------------------------
;;
;; methods
;;
;;------------------------------------------------------------------------------

;-----------------------------------------------------
; capacity
;-----------------------------------------------------
(defmethod empty ((cont priority-queue))
  (zerop (size cont)))

(defmethod size ((cont priority-queue))
  (size (__prique-container cont)))

;-----------------------------------------------------
; element access
;-----------------------------------------------------
(defmethod top ((cont priority-queue))
  (front (__prique-container cont)))

;-----------------------------------------------------
; modifiers
;-----------------------------------------------------
(defmethod push ((cont priority-queue) val)
  (let ((pred (__prique-pred      cont))
		(cont (__prique-container cont)))
	(push-back cont val)
	(push-heap (begin cont) (end cont) pred))
  nil)

(defmethod pop ((cont priority-queue))
  (let ((pred (__prique-pred      cont))
		(cont (__prique-container cont)))
	(pop-heap (begin cont) (end cont) pred)
	(pop-back cont))
  nil)


#-cl-stl-0x98    ; emplace
(defmethod-overload emplace ((container priority-queue) new-val)
  (let ((pred (__prique-pred      container))
		(cont (__prique-container container)))
	(__emplace-back-2 cont new-val)
	(push-heap (begin cont) (end cont) pred))
  nil)

#-cl-stl-0x98
(defmethod-overload swap ((cont1 priority-queue)
							 (cont2 priority-queue))
  (swap (__que-container cont1) (__que-container cont2))
  (values cont1 cont2))

