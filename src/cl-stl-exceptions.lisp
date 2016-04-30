
(in-package :cl-stl)


;;----------------------------------------------------
(define-condition logic_error   (exception) ())
(define-condition runtime_error (exception) ())


;;----------------------------------------------------
(define-condition domain_error     (logic_error) ())
(define-condition invalid_argument (logic_error) ())
(define-condition length_error     (logic_error) ())
(define-condition out_of_range     (logic_error) ())

;;----------------------------------------------------
(define-condition range_error      (runtime_error) ())
(define-condition overflow_error   (runtime_error) ())
(define-condition underflow_error  (runtime_error) ())


;;----------------------------------------------------
;; 0x11
;;----------------------------------------------------
#-cl-stl-0x98
(define-condition bad_function_call (exception) ())


