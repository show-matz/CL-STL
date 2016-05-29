(in-package :cl-stl)

;;------------------------------------------------------------------------------
;;
;; class difinition
;;
;;------------------------------------------------------------------------------
(defclass priority_queue (clonable)
  ((pred      :initform  #'operator_<
			  :initarg   :pred
			  :accessor  __prique-pred)
   (container :type     :randomaccess_container
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
  `(unless (and (typep ,cont 'randomaccess_container)
				(typep ,cont 'pushable_back_container))
	 (error 'type-mismatch :what "Underlying container of priority_queue must be randomaccess_container and be pushable_back_container.")))


;;------------------------------------------------------------------------------
;;
;; constructors
;;
;;------------------------------------------------------------------------------
(declare-constructor priority_queue (0 1 2 3 4))

; empty
(define-constructor priority_queue ()
  (make-instance 'priority_queue
				 :pred #'operator_< :container (new stl:vector)))

; initialize - 1
(define-constructor priority_queue ((comp cl:function))
  (make-instance 'priority_queue
				 :pred comp :container (new stl:vector)))

; initialize - 2
(define-constructor priority_queue ((comp #-cl-stl-0x98 functor
										  #+cl-stl-0x98 binary_function))
  (make-instance 'priority_queue
				 :pred (clone comp) :container (new stl:vector)))

; initialize - 3
(define-constructor priority_queue ((comp cl:function)
									(ctnr randomaccess_container))
  (let ((tmp (clone ctnr)))
	(make_heap (begin tmp) (end tmp) comp)
	(make-instance 'priority_queue :pred comp :container tmp)))

; initialize - 4
(define-constructor priority_queue ((comp #-cl-stl-0x98 functor
										  #+cl-stl-0x98 binary_function)
									(ctnr randomaccess_container))
  (let ((tmp (clone ctnr)))
	(make_heap (begin tmp) (end tmp) comp)
	(make-instance 'priority_queue
				   :pred (clone comp) :container tmp)))

; range - 1
(define-constructor priority_queue ((itr1 input_iterator) (itr2 input_iterator))
  (let ((tmp (new stl:vector itr1 itr2)))
	(make_heap (begin tmp) (end tmp) #'operator_<)
	(make-instance 'priority_queue :pred #'operator_< :container tmp)))

(define-constructor priority_queue ((itr1 const-vector-pointer) (itr2 const-vector-pointer))
  (__pointer-check-iterator-range itr1 itr2)
  (let ((tmp (new stl:vector itr1 itr2)))
	(make_heap (begin tmp) (end tmp) #'operator_<)
	(make-instance 'priority_queue :pred #'operator_< :container tmp)))

; range - 2
(define-constructor priority_queue ((itr1 input_iterator) (itr2 input_iterator) (comp cl:function))
  (let ((tmp (new stl:vector itr1 itr2)))
	(make_heap (begin tmp) (end tmp) comp)
	(make-instance 'priority_queue :pred comp :container tmp)))

(define-constructor priority_queue ((itr1 const-vector-pointer) (itr2 const-vector-pointer) (comp cl:function))
  (__pointer-check-iterator-range itr1 itr2)
  (let ((tmp (new stl:vector itr1 itr2)))
	(make_heap (begin tmp) (end tmp) comp)
	(make-instance 'priority_queue :pred comp :container tmp)))

; range - 3
(define-constructor priority_queue ((itr1 input_iterator) (itr2 input_iterator)
									(comp #-cl-stl-0x98 functor
										  #+cl-stl-0x98 binary_function))
  (let ((tmp (new stl:vector itr1 itr2)))
	(make_heap (begin tmp) (end tmp) comp)
	(make-instance 'priority_queue :pred (clone comp) :container tmp)))

(define-constructor priority_queue ((itr1 const-vector-pointer) (itr2 const-vector-pointer)
									(comp #-cl-stl-0x98 functor
										  #+cl-stl-0x98 binary_function))
  (__pointer-check-iterator-range itr1 itr2)
  (let ((tmp (new stl:vector itr1 itr2)))
	(make_heap (begin tmp) (end tmp) comp)
	(make-instance 'priority_queue :pred (clone comp) :container tmp)))

; range - 4
(define-constructor priority_queue ((itr1 input_iterator) (itr2 input_iterator)
									(comp cl:function) (ctnr randomaccess_container))
  (let ((tmp (clone ctnr)))
	(insert tmp (end tmp) itr1 itr2)
	(make_heap (begin tmp) (end tmp) comp)
	(make-instance 'priority_queue :pred comp :container tmp)))

(define-constructor priority_queue ((itr1 const-vector-pointer) (itr2 const-vector-pointer)
									(comp cl:function) (ctnr randomaccess_container))
  (__pointer-check-iterator-range itr1 itr2)
  (let ((tmp (clone ctnr)))
	(insert tmp (end tmp) itr1 itr2)
	(make_heap (begin tmp) (end tmp) comp)
	(make-instance 'priority_queue :pred comp :container tmp)))

; range - 5
(define-constructor priority_queue ((itr1 input_iterator) (itr2 input_iterator)
									(comp #-cl-stl-0x98 functor
										  #+cl-stl-0x98 binary_function) (ctnr randomaccess_container))
  (let ((tmp (clone ctnr)))
	(insert tmp (end tmp) itr1 itr2)
	(make_heap (begin tmp) (end tmp) comp)
	(make-instance 'priority_queue :pred (clone comp) :container tmp)))

(define-constructor priority_queue ((itr1 const-vector-pointer) (itr2 const-vector-pointer)
									(comp #-cl-stl-0x98 functor
										  #+cl-stl-0x98 binary_function) (ctnr randomaccess_container))
  (__pointer-check-iterator-range itr1 itr2)
  (let ((tmp (clone ctnr)))
	(insert tmp (end tmp) itr1 itr2)
	(make_heap (begin tmp) (end tmp) comp)
	(make-instance 'priority_queue :pred (clone comp) :container tmp)))

; copy constructor
(define-constructor priority_queue ((cont priority_queue))
  (make-instance 'priority_queue
				 :pred (clone (__prique-pred cont))
				 :container (clone (__prique-container cont))))

; move-initialize - 1
#-cl-stl-0x98
(define-constructor priority_queue ((comp cl:function)
									(rm-ref remove-reference))
  (let ((cont (funcall (the cl:function (opr::__rm-ref-closure rm-ref)))))
	(check-underlying-container-of-prique cont)
	(let ((tmp (dynamic-new (type-of cont))))
	  (swap tmp cont)
	  (make_heap (begin tmp) (end tmp) comp)
	  (make-instance 'priority_queue :pred comp :container tmp))))

; move-initialize - 2
#-cl-stl-0x98
(define-constructor priority_queue ((comp #-cl-stl-0x98 functor
										  #+cl-stl-0x98 binary_function)
									(rm-ref remove-reference))
  (let ((cont (funcall (the cl:function (opr::__rm-ref-closure rm-ref)))))
	(check-underlying-container-of-prique cont)
	(let ((tmp (dynamic-new (type-of cont))))
	  (swap tmp cont)
	  (make_heap (begin tmp) (end tmp) comp)
	  (make-instance 'priority_queue :pred (clone comp) :container tmp))))

#-cl-stl-0x98
(labels ((__ctor-imp (itr1 itr2 comp rm-ref)
		   (let ((cont (funcall (the cl:function (opr::__rm-ref-closure rm-ref)))))
			 (check-underlying-container-of-prique cont)
			 (let ((tmp (dynamic-new (type-of cont))))
			   ;;(funcall (the cl:function (opr::__rm-ref-closure rm-ref)) nil)
			   (swap tmp cont)
			   (insert tmp (end tmp) itr1 itr2)
			   (make_heap (begin tmp) (end tmp) comp)
			   (make-instance 'priority_queue :pred comp :container tmp)))))

  ;; move-range - 1
  (define-constructor priority_queue ((itr1 input_iterator) (itr2 input_iterator)
									  (comp cl:function) (rm-ref remove-reference))
	(__ctor-imp itr1 itr2 comp rm-ref))
  
  (define-constructor priority_queue ((itr1 const-vector-pointer) (itr2 const-vector-pointer)
									  (comp cl:function) (rm-ref remove-reference))
	(__pointer-check-iterator-range itr1 itr2)
	(__ctor-imp itr1 itr2 comp rm-ref))

  ;; move-range - 2
  (define-constructor priority_queue ((itr1 input_iterator) (itr2 input_iterator)
									  (comp #-cl-stl-0x98 functor
											#+cl-stl-0x98 binary_function) (rm-ref remove-reference))
	(__ctor-imp itr1 itr2 comp rm-ref))

  (define-constructor priority_queue ((itr1 const-vector-pointer) (itr2 const-vector-pointer)
									  (comp #-cl-stl-0x98 functor
											#+cl-stl-0x98 binary_function) (rm-ref remove-reference))
	(__pointer-check-iterator-range itr1 itr2)
	(__ctor-imp itr1 itr2 comp rm-ref)))

; move constructor.
#-cl-stl-0x98
(define-constructor priority_queue ((rm-ref remove-reference))
  (let ((cont (funcall (the cl:function (opr::__rm-ref-closure rm-ref)))))
	(__check-type-of-move-constructor cont priority_queue)
	(let* ((src-pred (__prique-pred cont))
		   (src-cont (__prique-container cont))
		   (new-pred (clone src-pred))
		   (new-cont (dynamic-new (type-of src-cont))))
	  (swap new-cont src-cont)
	  (make-instance 'priority_queue
					 :pred new-pred :container new-cont))))



(defmethod operator_clone ((arg priority_queue))
  (make-instance 'priority_queue
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
(defmethod empty ((cont priority_queue))
  (zerop (size cont)))

(defmethod size ((cont priority_queue))
  (size (__prique-container cont)))

;-----------------------------------------------------
; element access
;-----------------------------------------------------
(defmethod top ((cont priority_queue))
  (front (__prique-container cont)))

;-----------------------------------------------------
; modifiers
;-----------------------------------------------------
(defmethod push ((cont priority_queue) val)
  (let ((pred (__prique-pred      cont))
		(cont (__prique-container cont)))
	(push_back cont val)
	(push_heap (begin cont) (end cont) pred))
  nil)

(defmethod pop ((cont priority_queue))
  (let ((pred (__prique-pred      cont))
		(cont (__prique-container cont)))
	(pop_heap (begin cont) (end cont) pred)
	(pop_back cont))
  nil)


#-cl-stl-0x98    ; emplace
(defmethod-overload emplace ((container priority_queue) new-val)
  (let ((pred (__prique-pred      container))
		(cont (__prique-container container)))
	(__emplace_back-2 cont new-val)
	(push_heap (begin cont) (end cont) pred))
  nil)

#-cl-stl-0x98
(defmethod-overload swap ((cont1 priority_queue)
							 (cont2 priority_queue))
  (swap (__que-container cont1) (__que-container cont2))
  (values cont1 cont2))

