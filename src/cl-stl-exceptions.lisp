
(in-package :cl-stl)


;;----------------------------------------------------
(define-condition logic-error   (exception) ())
(define-condition runtime-error (exception) ())


;;----------------------------------------------------
(define-condition domain-error     (logic-error) ())
(define-condition invalid-argument (logic-error) ())
(define-condition length-error     (logic-error) ())
(define-condition out-of-range     (logic-error) ())

;;----------------------------------------------------
(define-condition range-error      (runtime-error) ())
(define-condition overflow-error   (runtime-error) ())
(define-condition underflow-error  (runtime-error) ())


;;----------------------------------------------------
;; 0x11
;;----------------------------------------------------
#-cl-stl-0x98
(define-condition bad-function-call (exception) ())


