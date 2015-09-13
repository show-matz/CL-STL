(provide :cl-stl)

(defpackage		:cl-stl
  (:nicknames	:stl)
  (:use			:common-lisp
				:cl-overload
				:cl-operator)
  (:export		:stl-version
				;--------------------------------------------------------------------
				; operators
				;--------------------------------------------------------------------
  #-cl-stl-0x98 :operator_move
				;--------------------------------------------------------------------
				; exceptions
				;--------------------------------------------------------------------
				:logic-error
				:domain-error
				:length-error
				:invalid-argument
				:out-of-range
				:runtime-error
				:range-error
				:overflow-error
				:underflow-error
  #-cl-stl-0x98 :bad-function-call    ; [0x11]
				;--------------------------------------------------------------------
				; support for operators
				;--------------------------------------------------------------------
 #+cl-stl-extra :cons-iterator
 #+cl-stl-extra :cons-const-iterator
				;--------------------------------------------------------------------
				; container
				;--------------------------------------------------------------------
				; base types
				;--------------------------
				:pushable-back-container
				:pushable-front-container
				:forward-container
				:bidirectional-container
				:randomaccess-container
				;--------------------------
				; container types
				;--------------------------
  #-cl-stl-0x98 :array
  #-cl-stl-0x98 :array-iterator
  #-cl-stl-0x98 :array-const-iterator
  #-cl-stl-0x98 :array-reverse-iterator
  #-cl-stl-0x98 :array-const-reverse-iterator
				;----
				:vector
				:vector-iterator
				:vector-const-iterator
				:vector-reverse-iterator
				:vector-const-reverse-iterator
				;----
				:deque
				:deque-iterator
				:deque-const-iterator
				:deque-reverse-iterator
				:deque-const-reverse-iterator
				;----
				:list
				:list-iterator
				:list-const-iterator
				:list-reverse-iterator
				:list-const-reverse-iterator
				;----
  #-cl-stl-0x98 :forward-list
  #-cl-stl-0x98 :forward-list-iterator
  #-cl-stl-0x98 :forward-list-const-iterator
				;----
				:set
				:set-iterator
				:set-const-iterator
				:set-reverse-iterator
				:set-const-reverse-iterator
				;----
				:multiset
				:multiset-iterator
				:multiset-const-iterator
				:multiset-reverse-iterator
				:multiset-const-reverse-iterator
				;----
				:map
				:map-iterator
				:map-const-iterator
				:map-reverse-iterator
				:map-const-reverse-iterator
				;----
				:multimap
				:multimap-iterator
				:multimap-const-iterator
				:multimap-reverse-iterator
				:multimap-const-reverse-iterator
				;----
				:stack
				:queue
				:priority-queue
				;--------------------------
				; container methods
				;--------------------------
				; assignment
				:assign
				; iterators
				:begin
				:end
				:rbegin
				:rend
  #-cl-stl-0x98 :cbegin
  #-cl-stl-0x98 :cend
  #-cl-stl-0x98 :crbegin
  #-cl-stl-0x98 :crend
  #-cl-stl-0x98 :before-begin
  #-cl-stl-0x98 :cbefore-begin
				; capacity
				:empty
				:size
				:max-size
				:resize
				:capacity
				:reserve
				; element access
				:front
				:back
				:at
  #-cl-stl-0x98 :data
				; modifiers
				:push-back
				:push-front
				:pop-back
				:pop-front
  #-cl-stl-0x98 :emplace-back
  #-cl-stl-0x98 :emplace-front
  #-cl-stl-0x98 :shrink-to-fit
				:insert
  #-cl-stl-0x98 :insert-after
  #-cl-stl-0x98 :emplace
  #-cl-stl-0x98 :emplace-hint
  #-cl-stl-0x98 :emplace-after
				:erase
  #-cl-stl-0x98 :erase-after
;				:swap        ; ( exported in algorithm )
				:clear
				:top
				:push
				:pop
				; specific operations
				:splice      ; list
  #-cl-stl-0x98 :splice-after
;				:remove      ; list ( exported in algorithm )
;				:remove-if   ; list ( exported in algorithm )
;				:unique      ; list ( exported in algorithm )
;				:merge       ; list ( exported in algorithm )
;				:sort        ; list ( exported in algorithm )
;				:reverse     ; list ( exported in algorithm )
; #-cl-stl-0x98 :fill        ; array ( exported in algorithm )
;				:find        ; associative container ( exported in algorithm )
;				:count       ; associative container ( exported in algorithm )
;				:lower-bound ; associative container ( exported in algorithm )
;				:upper-bound ; associative container ( exported in algorithm )
;				:equal-range ; associative container ( exported in algorithm )
				; observers
				:key-comp
				:value-comp
				; debug
 #+cl-stl-debug :dump
 #+cl-stl-debug :check-integrity
				;----------------------------------
				;iterator base types
				:input-iterator
				:output-iterator
				:forward-iterator
				:bidirectional-iterator
				:randomaccess-iterator
				:reverse-iterator
				;----------------------------------
				;iterator methods
				:advance
				:distance
				:base
  #-cl-stl-0x98 :prev
  #-cl-stl-0x98 :next
				;iterator utilities
  #-cl-stl-0x98 :make-move-iterator
				:back-inserter
				:front-inserter
				:inserter
				:stream-writer
				:stream-reader
				:with-sequence
				:with-stream-reader
				:with-stream-writer
				:with-file-reader
				:with-file-writer
				:with-buffer-reader
				:with-buffer-writer
				;----------------------------------
				;tuple
  #-cl-stl-0x98 :tuple
  #-cl-stl-0x98 :make-tuple
  #-cl-stl-0x98 :get
  #-cl-stl-0x98 :tuple-cat
  #-cl-stl-0x98 :tie
  #+(and cl-stl-extra
		 (not cl-stl-0x98))	:with-tie
				;----------------------------------
				;utility
				:pair
				:make-pair
				:first
				:second
				;----------------------------------
				;functional
				:functor
				:functor-function
				:functor-call
				:unary-function    ; deprecated in 0x11 or later
				:binary-function   ; deprecated in 0x11 or later
				;classes
				:plus
				:minus
				:multiplies
				:divides
				:modulus
				:negate
				:equal-to
				:not-equal-to
				:greater
				:less
				:greater-equal
				:less-equal
				:logical-and
				:logical-or
				:logical-not
				:unary-negate
				:binary-negate
				:binder1st
				:binder2nd
  #-cl-stl-0x98 :bit-and
  #-cl-stl-0x98 :bit-or
  #-cl-stl-0x98 :bit-xor
				:pointer-to-unary-function
				:pointer-to-binary-function
				:mem-fun-t
				:mem-fun1-t
  #-cl-stl-0x98 :function
  #-cl-stl-0x98 :target
				;utility functions
				:not1
				:not2
  #-cl-stl-0x98 :is-placeholder
  #-cl-stl-0x98 :is-bind-expression
  #-cl-stl-0x98 :bind
				:bind1st
				:bind2nd
				:ptr-fun1
				:ptr-fun2
				:mem-fun
				:mem-fun1
				:mem-fun-ref
				:mem-fun1-ref
  #-cl-stl-0x98 :mem-fn
				;+-----------------------------------------+
				;| numeric                                 |
				;+-----------------------------------------+
				:numeric-limits
				:accumulate
				:adjacent-difference
				:inner-product
  #-cl-stl-0x98 :iota
				:partial-sum
				;+-----------------------------------------+
				;| algorithm                               |
				;+-----------------------------------------+
				; 25.1, non-modifying sequence operations:
  #-cl-stl-0x98 :all-of
  #-cl-stl-0x98 :any-of
  #-cl-stl-0x98 :none-of
				:for-each
				:find
				:find-if
  #-cl-stl-0x98 :find-if-not
				:find-end
				:find-first-of
				:adjacent-find
				:count
				:count-if
				:mismatch
				:equal
  #-cl-stl-0x98 :is-permutation
				:search
				:search-n
				; 25.2, modifying sequence operations:
				; 25.2.1, copy:
				:copy
  #-cl-stl-0x98 :copy-n
  #-cl-stl-0x98 :copy-if
				:copy-backward
  #-cl-stl-0x98 :move
  #-cl-stl-0x98 :move-backward
				; 25.2.2, swap:
				:swap
				:swap-ranges
				:iter-swap
				:transform
				:replace
				:replace-if
				:replace-copy
				:replace-copy-if
				:fill
				:fill-n
				:generate
				:generate-n
				:remove
				:remove-if
				:remove-copy
				:remove-copy-if
				:unique
				:unique-copy
				:reverse
				:reverse-copy
				:rotate
				:rotate-copy
				:random-shuffle
				; 25.2.12, partitions:
  #-cl-stl-0x98 :is-partitioned
				:partition
				:stable-partition
  #-cl-stl-0x98 :partition-copy
  #-cl-stl-0x98 :partition-point
				; 25.3, sorting and related operations:
				; 25.3.1, sorting:
				:sort
				:stable-sort
				:partial-sort
				:partial-sort-copy
  #-cl-stl-0x98 :is-sorted
  #-cl-stl-0x98 :is-sorted-until
				:nth-element
				; 25.3.3, binary search:
				:lower-bound
				:upper-bound
				:equal-range
				:binary-search
				; 25.3.4, merge:
				:merge
				:inplace-merge
				; 25.3.5, set operations:
				:includes
				:set-union
				:set-intersection
				:set-difference
				:set-symmetric-difference
				; 25.3.6, heap operations:
				:push-heap
				:pop-heap
				:make-heap
				:sort-heap
  #-cl-stl-0x98 :is-heap
  #-cl-stl-0x98 :is-heap-until
				; 25.3.7, minimum and maximum:
				:min
				:max
  #-cl-stl-0x98 :minmax
				:min-element
				:max-element
  #-cl-stl-0x98 :minmax-element
				:lexicographical-compare
				; 25.3.9, permutations
				:next-permutation
				:prev-permutation
				;+-----------------------------------------+
				;| misc                                    |
				;+-----------------------------------------+
				:for
  #-cl-stl-0x98 :initializer-list)
  (:shadow		;container
				:array
				:vector
				:list
				:set
				:map
				:push
				:pop
				;tuple
  #-cl-stl-0x98 :get
				;utility
				:first
				:second
				;functional
  #-cl-stl-0x98 :function
  #-cl-stl-0x98 :bit-and
  #-cl-stl-0x98 :bit-or
  #-cl-stl-0x98 :bit-xor
				;algorithm
				:count
				:count-if
				:equal
				:find
				:find-if
  #-cl-stl-0x98 :find-if-not
				:fill
				:max
				:merge
				:min
				:mismatch
				:remove
				:remove-copy
				:remove-copy-if
				:remove-if
				:replace
				:replace-copy
				:replace-copy-if
				:reverse
				:search
				:set-difference
				:sort
				:stable-sort))


(in-package :cl-stl)

;;------------------------------------------------------------------------------
;;
;; CL-STL support version information
;;
;;------------------------------------------------------------------------------
(when (< 1 (+ #+CL-STL-0x98 1
              #+CL-STL-0x11 1
              #+CL-STL-0x14 1))
  (error "version features duplicated."))

(defun stl-version ()
  #+cl-stl-0x98			:cl-stl-0x98
  #+cl-stl-0x11			:cl-stl-0x11
  #+cl-stl-0x14			:cl-stl-0x14
  #-(or cl-stl-0x98
		cl-stl-0x11
		cl-stl-0x14)	:cl-stl-0x14)


;;------------------------------------------------------------------------------
;;
;; utilities from 'On Lisp'
;;
;;------------------------------------------------------------------------------
#-cl-stl-0x98
(let ((rpar (get-macro-character #\))))
  (defun onlisp/ddfn (left right fn)
    (set-macro-character right rpar)
    (set-dispatch-macro-character #\# left
       #'(lambda (stream char1 char2)
		   (declare (ignore char1 char2))
           (apply fn
                  (read-delimited-list right stream t))))))

#-cl-stl-0x98
(defmacro onlisp/defdelim (left right parms &body body)
  `(onlisp/ddfn ,left ,right #'(lambda ,parms ,@body)))


;;------------------------------------------------------------------------------
;;
;; internal utilities
;;
;;------------------------------------------------------------------------------
(defmacro __check-type-of-move-constructor (cont type &optional (typename type))
  (check-type cont symbol)
  (check-type typename symbol)
  `(unless (eq (type-of ,cont) ',type)
	 (error 'type-mismatch :what ,(format nil "Type unmatch in move constructor of ~A." typename))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro __check-exact-type-of-cast (obj fromtype desttype)
	`(unless (eq (type-of ,obj) ,fromtype)
	   (error 'type-mismatch :what (format nil "Can't cast ~A to ~A." (type-of ,obj) ,desttype)))))

(defmacro __error-when-const-removing-assign (itr1 itrtype itr2 const-itrtype)
  `(when (and (eq (type-of ,itr1) ',itrtype)
			  (eq (type-of ,itr2) ',const-itrtype))
	 (error 'type-mismatch :what (format nil "~A can't assign to ~A" ',const-itrtype ',itrtype))))

(defmacro __error-unless-non-negative-fixnum (op val)
  (check-type val symbol)
  `(unless (typep ,val '(and fixnum (integer 0 *)))
	 (error 'invalid-argument :what ,(format nil "~A : ~A is not non-negative fixnum." op val))))



;;------------------------------------------------------------------------------
;;
;; operator declaration
;;
;;------------------------------------------------------------------------------

#-cl-stl-0x98 (defgeneric operator_move (lhs rhs))

;; helper class fo move semantics.
#-cl-stl-0x98
(defclass remove-reference ()
  ((closure :type     cl:function
			:initarg  :closure
			:accessor __rm-ref-closure)))


;;------------------------------------------------------------------------------
;;
;; generic functions declaration of functional
;;
;;------------------------------------------------------------------------------
(defgeneric functor-function (func))

(defmacro functor-call (func &rest args)
  #+cl-stl-0x98
  (ecase (length args)
	(1 `(funcall (functor-function ,func) ,@args))
	(2 `(funcall (functor-function ,func) ,@args)))
  #-cl-stl-0x98
  `(funcall (functor-function ,func) ,@args))


;;------------------------------------------------------------------------------
;;
;;
;;
;;------------------------------------------------------------------------------
#-cl-stl-0x98
(progn
  (declare-method-overload get (2) :make-setf t :make-top nil)
  (defmacro get (idx obj)
	(if (or (integerp idx)
			(and (symbolp idx)
				 (constantp idx)
				 (not (eq idx t))
				 (not (eq idx nil))
				 (not (keywordp idx))))
		`(,(make-overload-name 'cl-stl:get 2) ,idx ,obj)
		(error 'type-mismatch :what "First parameter of get must be constant number."))))

#-cl-stl-0x98    ; support for 'get in tie'
(progn
  (defgeneric __tie-get (idx obj))
  #+cl-stl-extra
  (defgeneric (setf __tie-get) (new-val idx obj)))


;;------------------------------------------------------------------------------
;;
;; base types of iterator
;;
;;------------------------------------------------------------------------------

(defclass input-iterator (clonable) ())
(defclass output-iterator (clonable) ())
(defclass forward-iterator (input-iterator output-iterator) ())
(defclass bidirectional-iterator (forward-iterator) ())
(defclass randomaccess-iterator (bidirectional-iterator) ())


;;------------------------------------------------------------------------------
;;
;; generic functions declaration of iterator
;;
;;------------------------------------------------------------------------------

;; method requirement
;
; operation         in	out	fwd	bid	rdm
;----------------------------------------
; operator_=				o	o	o	o	o
; operator_clone	o	o	o	o	o
; operator_*				o	-	o	o	o
; setf operator_*			-	o	o	o	o
; operator_++				o	o	o	o	o
; operator_--				-	-	-	o	o
; operator_==				o	-	o	o	o
; operator_/=				o	-	o	o	o
; operator_<				-	-	-	-	o
; operator_<=				-	-	-	-	o
; operator_>				-	-	-	-	o
; operator_>=				-	-	-	-	o
; operator_+				-	-	-	-	o
; operator_-				-	-	-	-	o
; operator_+=				-	-	-	-	o
; operator_-=				-	-	-	-	o
; operator_[]				-	-	-	-	o
; setf operator_[]		-	-	-	-	o
; advance			o	-	o	o	o
; offset			o	-	o	o	o
; distance			o	-	o	o	o
; base				-	-	-	o	o  * reverse iterator only


(defgeneric advance (itr n))
(defgeneric distance (itr1 itr2))
(defgeneric base (rev-itr))

;; creating reverse-iterator
(declare-constructor reverse-iterator (1))

;; creating move-iterator
#-cl-stl-0x98
(declare-constructor move-iterator (1))

;;------------------------------------------------------------------------------
;;
;; base types of container
;;
;;------------------------------------------------------------------------------
(defclass pushable-back-container (clonable) ())
(defclass pushable-front-container (clonable) ())
(defclass forward-container (clonable) ())
(defclass bidirectional-container (clonable) ())
(defclass randomaccess-container (clonable) ())



;;------------------------------------------------------------------------------
;;
;; generic functions declaration of container
;;
;;------------------------------------------------------------------------------
(declare-method-overload assign (2 3)
 :documentation "
<<signature>>
  1) (cl-stl:assign container itr1 itr2)
  2) (cl-stl:assign container count value)
  3) (cl-stl:assign container il)           [0x11]
  4) (cl-stl:assign fnc1 fnc2)              [0x11]

<<parameters>>
  container : stl:vector, stl:deque, stl:forward-list[0x11], stl:list.
  itr1      : input-iterator.
  itr2      : input-iterator.
  count     : fixnum.
  value     : value to assign.
  il        : initializer-list[0x11].
  fnc1      : cl-stl:function.
  fnc2      : cl-stl:function.

<<return value>>
  nil.
")

; iterators
(defgeneric begin (container)
  (:documentation "
<<signature>>
  (cl-stl:begin object)

<<parameters>>
  object : one of this.

     cl-stl:array            [0x11]
     cl-stl:vector
     cl-stl:deque
     cl-stl:forward-list     [0x11]
     cl-stl:list
     cl-stl:set
     cl-stl:multiset
     cl-stl:map
     cl-stl:multimap
     cl-stl:initializer-list [0x11]
     cl:vector               [0x11]

<<return value>>
  iterator points to top of object's sequence.
"))

(defgeneric end (container)
  (:documentation "
<<signature>>
  (cl-stl:end object)

<<parameters>>
  object : one of this.

     cl-stl:array            [0x11]
     cl-stl:vector
     cl-stl:deque
     cl-stl:forward-list     [0x11]
     cl-stl:list
     cl-stl:set
     cl-stl:multiset
     cl-stl:map
     cl-stl:multimap
     cl-stl:initializer-list [0x11]
     cl:vector               [0x11]

<<return value>>
  iterator points to next to the end of object's sequence.
"))

(defgeneric rbegin (container)
  (:documentation "
<<signature>>
  (cl-stl:rbegin object)

<<parameters>>
  object : one of this.

     cl-stl:array     [0x11]
     cl-stl:vector
     cl-stl:deque
     cl-stl:list
     cl-stl:set
     cl-stl:multiset
     cl-stl:map
     cl-stl:multimap
     cl:vector        [0x11 & extra]

<<return value>>
  reverse-iterator points to end of object's sequence.
"))

(defgeneric rend (container)
  (:documentation "
<<signature>>
  (cl-stl:rend object)

<<parameters>>
  object : one of this.

     cl-stl:array    [0x11]
     cl-stl:vector
     cl-stl:deque
     cl-stl:list
     cl-stl:set
     cl-stl:multiset
     cl-stl:map
     cl-stl:multimap
     cl:vector       [0x11 & extra]

<<return value>>
  reverse-iterator points to previous to the top of object's sequence.
"))

#-cl-stl-0x98 (defgeneric cbegin (container)
  (:documentation "
<<signature>>
  (cl-stl:cbegin object)

<<parameters>>
  object : one of this.

     cl-stl:array        [0x11]
     cl-stl:vector
     cl-stl:deque
     cl-stl:forward-list [0x11]
     cl-stl:list
     cl-stl:set
     cl-stl:multiset
     cl-stl:map
     cl-stl:multimap
     cl:vector           [0x11 & extra]

<<return value>>
  const iterator points to top of object's sequence.
"))

#-cl-stl-0x98 (defgeneric cend (container)
  (:documentation "
<<signature>>
  (cl-stl:cend object)

<<parameters>>
  object : one of this.

     cl-stl:array        [0x11]
     cl-stl:vector
     cl-stl:deque
     cl-stl:forward-list [0x11]
     cl-stl:list
     cl-stl:set
     cl-stl:multiset
     cl-stl:map
     cl-stl:multimap
     cl:vector           [0x11 & extra]

<<return value>>
  const iterator points to next to the end of object's sequence.
"))

#-cl-stl-0x98 (defgeneric crbegin (container))                  ; const-reverse-iterator ; A V D L S MS M MM --
#-cl-stl-0x98 (defgeneric crend (container))                    ; const-reverse-iterator ; A V D L S MS M MM --
#-cl-stl-0x98 (defgeneric before-begin (container))             ; iterator               ; - - - - - -- - -- FL
#-cl-stl-0x98 (defgeneric cbefore-begin (container))            ; const-iterator         ; - - - - - -- - -- FL

; capacity
(defgeneric empty (container))                                     ; t / nil             ; A V D L S MS M MM FL
(defgeneric size (container))                                      ; fixnum              ; A V D L S MS M MM -- ( FL is ok in extra )
(defgeneric max-size (container))                                  ; fixnum              ; A V D L S MS M MM FL
(declare-method-overload resize (2 3))                          ; nil                 ; - V D L - -- - -- FL
(defgeneric capacity (container))                                  ; fixnum              ; - V - - - -- - -- --
(defgeneric reserve (container size))                              ; nil                 ; - V - - - -- - -- 

; element access
(defgeneric front (container))                                     ; value               ; A V D L - -- - --
(defgeneric (setf front) (val container))                          ; val                 ; A V D L - -- - --
(defgeneric back (container))                                      ; value               ; A V D L - -- - --
(defgeneric (setf back) (val container))                           ; val                 ; A V D L - -- - --
(defgeneric at (container idx))                                    ; value               ; A V D - - -- M --
(defgeneric (setf at) (val container idx))                         ; val                 ; A V D - - -- M --
(defgeneric data (container)                                       ; cl:vector           ; A V - - - -- - -- -- ( FL is ok in extra )
  (:documentation "
<<signature>>
  (cl-stl:data container)    [0x11]

<<parameters>>
  container : one of this.

     cl-stl:array        [0x11]
     cl-stl:vector
     cl-stl:forward-list [0x11 & extra]
     cl:vector           [0x11 & extra]

<<return value>>
  internal data array ( or cons-list ) of container.
"))
(defgeneric top (container))                                       ; value               ; - - - - - -- - --
(defgeneric (setf top) (val container))                            ; value               ; - - - - - -- - --
(defgeneric push (container val))                                  ; value               ; - - - - - -- - --
(defgeneric pop (container))                                       ; value               ; - - - - - -- - --

; modifiers
(defgeneric push-back (container val))                             ; nil                 ; - V D L - -- - --
(defgeneric push-front (container val))                            ; nil                 ; - - D L - -- - --
(defgeneric pop-back (container))                                  ; nil                 ; - V D L - -- - --
(defgeneric pop-front (container))                                 ; nil                 ; - - D L - -- - --

#-cl-stl-0x98    ; emplace back
(progn
  (declare-method-overload emplace-back (2) :make-top nil)
  (defmacro emplace-back (cont type-name (&rest args))
	`(,(make-overload-name 'cl-stl:emplace-back 2) ,cont (new ,type-name ,@args))))

#-cl-stl-0x98    ; emplace front
(progn
  (declare-method-overload emplace-front (2) :make-top nil)
  (defmacro emplace-front (cont type-name (&rest args))
	`(,(make-overload-name 'cl-stl:emplace-front 2) ,cont (new ,type-name ,@args))))

#-cl-stl-0x98 (defgeneric shrink-to-fit (container))               ; nil                 ; - V D - - -- - --

;(defgeneric insert (container itr value))                         ; iterator            ; - V D L S MS M MM
;(defgeneric insert (container itr count value))                   ; nil                 ; - V D L - -- - --
;(defgeneric insert (container itr itr1 itr2))                     ; nil                 ; - V D L - -- - --
;(defgeneric insert (container value))                             ; iterator or pair    ; - - - - S MS M MM
;(defgeneric insert (container itr1 itr2))                         ; nil                 ; - - - - S MS M MM
(declare-method-overload insert (2 3 4))
#-cl-stl-0x98 (declare-method-overload insert-after (3 4))
#-cl-stl-0x98 (declare-macro-overload emplace (3 4))            ; nil                 ; - V D L S MS M MM ST Q PQ

#-cl-stl-0x98 ; emplace
(progn
  (declare-method-overload emplace (2 3) :make-top nil)
  (defmacro-overload emplace (cont type-name (&rest args))
	`(,(make-overload-name 'cl-stl:emplace 2) ,cont (new ,type-name ,@args)))
  (defmacro-overload emplace (cont itr type-name (&rest args))
	`(,(make-overload-name 'cl-stl:emplace 3) ,cont ,itr (new ,type-name ,@args))))

#-cl-stl-0x98 ; emplace-hint
(progn
  (declare-method-overload emplace-hint (3) :make-top nil)
  (defmacro emplace-hint (cont itr type-name (&rest args))
	`(,(make-overload-name 'cl-stl:emplace-hint 3) ,cont ,itr (new ,type-name ,@args))))

#-cl-stl-0x98 ; emplace-after
(progn
  (declare-method-overload emplace-after (3) :make-top nil)
  (defmacro emplace-after (cont itr type-name (&rest args))
	`(,(make-overload-name 'cl-stl:emplace-after 3) ,cont ,itr (new ,type-name ,@args))))



;(defgeneric erase (container itr))                                ; iterator            ; - V D L S MS M MM
;(defgeneric erase (container itr1 itr2))                          ; nil                 ; - V D L S MS M MM
;(defgeneric erase (container key))                                ; fixnum              ; - - - - S MS M MM
(declare-method-overload erase (2 3))
#-cl-stl-0x98 (declare-method-overload erase-after (2 3))

;(defgeneric swap (cont1 cont2))                                   ; nil                 ; A V D L S MS M MM ( declare in algorithm )
(defgeneric clear (container))                                     ; nil                 ; - V D L S MS M MM

; specific operations
;(defgeneric splice (lst1 itr lst2 &optional itr1 itr2))           ; nil                 ; - - - L - -- - --
(declare-method-overload splice (3 4 5))
#-cl-stl-0x98 (declare-method-overload splice-after (3 4 5))

;(defgeneric remove (lst val &optional eql-bf))                    ; nil                 ; - - - L - -- - -- ( declare in algorithm )
;(defgeneric remove-if (lst pred-uf))                              ; nil                 ; - - - L - -- - -- ( declare in algorithm )
;(defgeneric unique (lst &optional eql-bf))                        ; nil                 ; - - - L - -- - -- ( declare in algorithm )
;(defgeneric merge (lst1 lst2 &optional less-bf))                  ; nil                 ; - - - L - -- - -- ( declare in algorithm )
;(defgeneric sort (lst &optional less-bf))                         ; nil                 ; - - - L - -- - -- ( declare in algorithm )
;(defgeneric reverse (lst))                                        ; nil                 ; - - - L - -- - -- ( declare in algorithm )
;#-cl-stl-0x98 (defgeneric fill (container val))                   ; nil                 ; A - - - - -- - -- ( declare in algorithm )
;(defgeneric find (container key))                                 ; iterator            ; - - - - S MS M MM ( declare in algorithm )
;(defgeneric count (container key))                                ; fixnum              ; - - - - S MS M MM ( declare in algorithm )
;(defgeneric lower-bound (container key))                          ; iterator            ; - - - - S MS M MM ( declare in algorithm )
;(defgeneric upper-bound (container key))                          ; iterator            ; - - - - S MS M MM ( declare in algorithm )
;(defgeneric equal-range (container key))                          ; pair(itr,itr)       ; - - - - S MS M MM ( declare in algorithm )

; observers
(defgeneric key-comp (container))                                  ; binary-function     ; - - - - S MS M MM
(defgeneric value-comp (container))                                ; binary-function     ; - - - - S MS M MM


; debug
#+cl-stl-debug (defgeneric dump (container &optional stream print-item-fnc))
#+cl-stl-debug (defgeneric check-integrity (container &optional stream))


; enumeration
#-cl-stl-0x98 (declare-method-overload for (2) :make-top nil)

(defmacro for ((&rest args) &body body)
"
<<signature>>
  1) (cl-stl:for (vars condition update &key (returns nil)) &body body)
  2) (cl-stl:for (var container &key (returns nil)) &body body)          [0x11]

<<parameters>>
  vars      : variable initialzation list.
  condition : condition for loop continuation.
  update    : expression for variable update.
  var       : symbol for value iteration.
  container : value sequence. see below.
  returns   : expression of return value.
  body      : loop body.

 container is :
    cl-stl:array
    cl-stl:vector
    cl-stl:deque
    cl-stl:forward-list
    cl-stl:list
    cl-stl:set
    cl-stl:multiset
    cl-stl:map
    cl-stl:multimap
    cl-stl:initializer-list
    cl:vector

<<return value>>
  1) returns
  2) returns
"
  (if (listp (car args))
	  (destructuring-bind (inits condition update &key (returns nil)) args
		`(do ,inits
			 ((not ,condition) ,returns)
		   (progn
			 ,@body
			 ,update)))
	  #+cl-stl-0x98 (error "0x11 style for is not supported.")
	  #-cl-stl-0x98
	  (destructuring-bind (var-sym cont &key (returns nil)) args
		`(progn
		   (,(make-overload-name 'cl-stl:for 2) ,cont
			 (lambda (,var-sym)
			   ,@body))
		   ,returns))))

;;------------------------------------------------------------------------------
;;
;; generic functions declaration of numeric
;;
;;------------------------------------------------------------------------------

;26.4.1 Accumulate
(declare-method-overload accumulate (3 4)
  :documentation "
<<signature>>
  (cl-stl:accumulate first last init)
  (cl-stl:accumulate first last init binary-op)

<<parameters>>
  first     : input-iterator.
  last      : input-iterator.
  init      : initial value.
  binary-op : binary functor ( use #'+ default ).

<<return value>>
  accumulated value.
")


;26.4.2 Inner product
(declare-method-overload inner-product (4 6)
  :documentation "
<<signature>>
  (cl-stl:inner-product first1 last1 first2 init)
  (cl-stl:inner-product first1 last1 first2 init binary-op1 binary-op2)

<<parameters>>
  first1     : input-iterator.
  last1      : input-iterator.
  first2     : input-iterator.
  init       : initial value.
  binary-op1 : binary functor ( use #'+ default ).
  binary-op2 : binary functor ( use #'* default ).

<<return value>>
  inner product.
")


;26.4.3 Partial sum
(declare-method-overload partial-sum (3 4)
  :documentation "
<<signature>>
  (cl-stl:partial-sum first last result)
  (cl-stl:partial-sum first last result binary-op)

<<parameters>>
  first     : input-iterator.
  last      : input-iterator.
  result    : output-iterator.
  binary-op : binary functor ( use #'+ default ).

<<return value>>
  copy of result ( point to end of sequence ).
")


;26.4.4 Adjacent difference
(declare-method-overload adjacent-difference (3 4)
  :documentation "
<<signature>>
  (cl-stl:adjacent-difference first last result)
  (cl-stl:adjacent-difference first last result binary-op)

<<parameters>>
  first     : input-iterator.
  last      : input-iterator.
  result    : output-iterator.
  binary-op : binary functor ( use #'- default ).

<<return value>>
  copy of result ( point to end of sequence ).
")

#-cl-stl-0x98
(declare-method-overload iota (3 #+cl-stl-extra 4)
  :documentation "
<<signature>>
  (cl-stl:iota first last init)            [0x11]
  (cl-stl:iota first last init unary-op)   [0x11 & extra]

<<parameters>>
  first    : forward-iterator.
  last     : forward-iterator.
  init     : initial value.
  unary-op : binary functor ( use #'1+ default ).

<<return value>>
  nil.
")



;;------------------------------------------------------------------------------
;;
;; generic functions declaration of algorithm
;;
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; 25.1, non-modifying sequence operations:
;;------------------------------------------------------------------------------

#-cl-stl-0x98 (defgeneric all-of (first last pred)
  (:documentation "
<<signature>>
  (cl-stl:all-of first last pred)   [0x11]

<<parameters>>
  first : input-iterator.
  last  : input-iterator.
  pred  : unary functor.

<<return value>>
  boolean value.
"))

#-cl-stl-0x98 (defgeneric any-of (first last pred)
  (:documentation "
<<signature>>
  (cl-stl:any-of first last pred)   [0x11]

<<parameters>>
  first : input-iterator.
  last  : input-iterator.
  pred  : unary functor.

<<return value>>
  boolean value.
"))

#-cl-stl-0x98 (defgeneric none-of (first last pred)
  (:documentation "
<<signature>>
  (cl-stl:none-of first last pred)   [0x11]

<<parameters>>
  first : input-iterator.
  last  : input-iterator.
  pred  : unary functor.

<<return value>>
  boolean value.
"))

(defgeneric for-each (first last func)
  (:documentation "
<<signature>>
  (cl-stl:for-each first last func)

<<parameters>>
  first : input-iterator.
  last  : input-iterator.
  func  : unary functor.

<<return value>>
  copy of func.
"))

(declare-method-overload find (2 3 #+cl-stl-extra 4)
  :documentation "
<<signature>>
  1) (cl-stl:find container  value)
  2) (cl-stl:find first last value)
  3) (cl-stl:find first last value eql-bf)    [extra]

<<parameters>>
  container : stl:set, stl:multiset, stl:map, stl:multimap.
  first     : input-iterator.
  last      : input-iterator.
  value     : value to find.
  eql-bf    : binary functor ( use #'operator_== by default ).

<<return value>>
  iterator point to found element.
")

(defgeneric find-if (first last pred)
  (:documentation "
<<signature>>
  (cl-stl:find-if first last pred)

<<parameters>>
  first : input-iterator.
  last  : input-iterator.
  pred  : unary functor.

<<return value>>
  iterator point to found element.
"))

#-cl-stl-0x98 (defgeneric find-if-not (first last pred)
  (:documentation "
<<signature>>
  (cl-stl:find-if-not first last pred)    [0x11]

<<parameters>>
  first : input-iterator.
  last  : input-iterator.
  pred  : unary functor.

<<return value>>
  iterator point to found element.
"))

(declare-method-overload find-end (4 5)
 :documentation "
<<signature>>
  (cl-stl:find-end first1 last1 first2 last2)
  (cl-stl:find-end first1 last1 first2 last2 eql-bf)

<<parameters>>
  first1 : forward-iterator.
  last1  : forward-iterator.
  first2 : forward-iterator.
  last2  : forward-iterator.
  eql-bf : binary functor ( use #'operator_== by default ).

<<return value>>
  iterator point to found element.

 MEMO : [0x98] If [first2,last2) is an empty range, the result is unspecified.
        [0x11] If [first2,last2) is an empty range, the function returns last1.
")

(declare-method-overload find-first-of (4 5)
 :documentation "
<<signature>>
  (cl-stl:find-first-of first1 last1 first2 last2)
  (cl-stl:find-first-of first1 last1 first2 last2 eql-bf)

<<parameters>>
  first1 : [0x98] forward-iterator / [0x11] input-iterator
  last1  : [0x98] forward-iterator / [0x11] input-iterator
  first2 : forward-iterator.
  last2  : forward-iterator.
  eql-bf : binary functor ( use #'operator_== by default ).

<<return value>>
  iterator point to found element.

 MEMO : [0x98] If [first2,last2) is an empty range, the result is unspecified.
        [0x11] If [first2,last2) is an empty range, the function returns last1.
")

(declare-method-overload adjacent-find (2 3)
 :documentation "
<<signature>>
  (cl-stl:adjacent-find first last)
  (cl-stl:adjacent-find first last eql-bf)

<<parameters>>
  first  : forward-iterator.
  last   : forward-iterator.
  eql-bf : binary functor ( use #'operator_== by default ).

<<return value>>
  iterator point to found element.
")

(declare-method-overload count (2 3 #+cl-stl-extra 4)
  :documentation "
<<signature>>
  1) (cl-stl:count container  value)
  2) (cl-stl:count first last value)
  3) (cl-stl:count first last value eql-bf)    [extra]

<<parameters>>
  container : stl:set, stl:multiset, stl:map, stl:multimap.
  first     : input-iterator.
  last      : input-iterator.
  value     : value to count.
  eql-bf    : binary functor ( use #'operator_== by default ).

<<return value>>
  count of found element.
")

(defgeneric count-if (first last pred)
  (:documentation "
<<signature>>
  (cl-stl:count-if first last pred)

<<parameters>>
  first : input-iterator.
  last  : input-iterator.
  pred  : unary functor.

<<return value>>
  count of found element.
"))

(declare-method-overload mismatch (3 4 #-(or cl-stl-0x98 cl-stl-0x11) 5)
  :documentation "
<<signature>>
    (cl-stl:mismatch first1 last1 first2)
    (cl-stl:mismatch first1 last1 first2 eql-bf)
    (cl-stl:mismatch first1 last1 first2 last2)        [0x14]
    (cl-stl:mismatch first1 last1 first2 last2 eql-bf) [0x14]

<<parameters>>
  first1 : forward-iterator.
  last1  : forward-iterator.
  first2 : forward-iterator.
  last2  : forward-iterator.
  eql-bf : binary functor ( use #'operator_== by default ).

<<return value>>
  pair of iterators.
")

(declare-method-overload equal (3 4 #-(or cl-stl-0x98 cl-stl-0x11) 5)
  :documentation "
<<signature>>
    (cl-stl:equal first1 last1 first2)
    (cl-stl:equal first1 last1 first2 eql-bf)
    (cl-stl:equal first1 last1 first2 last2)        [0x14]
    (cl-stl:equal first1 last1 first2 last2 eql-bf) [0x14]

<<parameters>>
  first1 : forward-iterator.
  last1  : forward-iterator.
  first2 : forward-iterator.
  last2  : forward-iterator.
  eql-bf : binary functor ( use #'operator_== by default ).

<<return value>>
  boolean value.
")

#-cl-stl-0x98 (declare-method-overload is-permutation (3 4 #-(or cl-stl-0x98 cl-stl-0x11) 5)
  :documentation "
<<signature>>
    (cl-stl:is-permutation first1 last1 first2)              [0x11]
    (cl-stl:is-permutation first1 last1 first2 eql-bf)       [0x11]
    (cl-stl:is-permutation first1 last1 first2 last2)        [0x14]
    (cl-stl:is-permutation first1 last1 first2 last2 eql-bf) [0x14]

<<parameters>>
  first1 : forward-iterator.
  last1  : forward-iterator.
  first2 : forward-iterator.
  last2  : forward-iterator.
  eql-bf : binary functor ( use #'operator_== by default ).

<<return value>>
  boolean value.
")

(declare-method-overload search (4 5)
  :documentation "
<<signature>>
    (cl-stl:search first1 last1 first2 last2)
    (cl-stl:search first1 last1 first2 last2 eql-bf)

<<parameters>>
  first1 : forward-iterator.
  last1  : forward-iterator.
  first2 : forward-iterator.
  last2  : forward-iterator.
  eql-bf : binary functor ( use #'operator_== by default ).

<<return value>>
  iterator point to top of found sequence.

 MEMO : [0x98] If [first2,last2) is an empty range, the result is unspecified.
        [0x11] If [first2,last2) is an empty range, the function returns last1.
")

(declare-method-overload search-n (4 5)
  :documentation "
<<signature>>
    (cl-stl:search-n first last count val)
    (cl-stl:search-n first last count val pred)

<<parameters>>
  first : forward-iterator.
  last  : forward-iterator.
  count : fixnum.
  val   : value to find.
  pred  : binary functor ( use #'operator_== by default ).

<<return value>>
  iterator point to top of found sequence.
")


;;------------------------------------------------------------------------------
;; 25.2, modifying sequence operations:
;;------------------------------------------------------------------------------
;; 25.2.1, copy:

(defgeneric copy (first last result)
  (:documentation "
<<signature>>
  (cl-stl:copy first last result)

<<parameters>>
  first  : input-iterator.
  last   : input-iterator.
  result : output-iterator.

<<return value>>
  iterator point to the end of the copied sequence.
"))

#-cl-stl-0x98 (defgeneric copy-n (first n result)
  (:documentation "
<<signature>>
  (cl-stl:copy-n first n result)    [0x11]

<<parameters>>
  first  : input-iterator.
  n      : fixnum.
  result : output-iterator.

<<return value>>
  iterator point to the end of the copied sequence.
"))

#-cl-stl-0x98 (defgeneric copy-if (first last result pred)
  (:documentation "
<<signature>>
  (cl-stl:copy-if first last result pred)    [0x11]

<<parameters>>
  first  : input-iterator.
  last   : input-iterator.
  result : output-iterator.
  pred   : unary functor.

<<return value>>
  iterator point to the end of the copied sequence.
"))

(defgeneric copy-backward (first last result)
  (:documentation "
<<signature>>
  (cl-stl:copy-backward first last result)

<<parameters>>
  first  : bidirectional-iterator.
  last   : bidirectional-iterator.
  result : bidirectional-iterator.

<<return value>>
  iterator point to the top of the copied sequence.
"))

#-cl-stl-0x98
(progn
  (declare-macro-overload move (1 3)
  :documentation "
<<signature>>
  1)  (cl-stl:move place)                [0x11]
  2)  (cl-stl:move first last result)    [0x11]

<<parameters>>
  place  : place to move source.
  first  : input-iterator.
  last   : input-iterator.
  result : output-iterator.

<<return value>>
  1) remove-reference to place.
  2) iterator point to end of moved sequence.
")
  (declare-method-overload move (3) :make-top nil)
  (defmacro-overload move (first last result)
	`(,(make-overload-name 'cl-stl:move 3) ,first ,last ,result))
  (defmacro-overload move (place)
	(let ((g-get (gensym "GET")))
	  (multiple-value-bind (vars forms var set ref)
		  (get-setf-expansion place)
		`(let* (,@(mapcar #'cl:list vars forms))
		   (if (eq (type-of ,ref) 'remove-reference)
			   ,ref
			   (make-instance 'remove-reference
							  :closure (lambda (&optional (,@var ',g-get))
										 (if (eq ,@var ',g-get) ,ref ,set)))))))))


#-cl-stl-0x98 (defgeneric move-backward (first last result)
  (:documentation "
<<signature>>
  (cl-stl:move-backward first last result)    [0x11]

<<parameters>>
  first  : bidirectional-iterator.
  last   : bidirectional-iterator.
  result : bidirectional-iterator.

<<return value>>
  iterator point to the top of the moved sequence.
"))


;; 25.2.2, swap:

; returns : nil
(declare-method-overload swap (2) :make-top nil)
(defmacro swap (a b)
  "
<<signature>>
  (cl-stl:swap a b)

<<parameters>>
  a  : place of contents swapped.
  b  : place of contents swapped.

<<return value>>
  nil.
"
  (multiple-value-bind (vars1 forms1 var1 set1 ref1) (get-setf-expansion a)
    (multiple-value-bind (vars2 forms2 var2 set2 ref2) (get-setf-expansion b)
      `(let* (,@(mapcar #'cl:list vars1 forms1)
              ,@(mapcar #'cl:list vars2 forms2))
		 (multiple-value-bind (,@var1 ,@var2)
			 (,(make-overload-name 'cl-stl:swap 2) ,ref1 ,ref2)
           ,set1
           ,set2
           nil)))))


(defgeneric swap-ranges (first1 last1 first2)
  (:documentation "
<<signature>>
  (cl-stl:swap-ranges first1 last1 first2)

<<parameters>>
  first1 : forward-iterator.
  last1  : forward-iterator.
  first2 : forward-iterator.

<<return value>>
  iterator points to last element swapped in the second sequence.
"))

(defgeneric iter-swap (a b)
  (:documentation "
<<signature>>
  (cl-stl:iter-swap a b)

<<parameters>>
  a : forward-iterator.
  b : forward-iterator.

<<return value>>
  nil.
"))

(declare-method-overload transform (4 5)
  :documentation "
<<signature>>
  1)  (cl-stl:transform first last result op)
  2)  (cl-stl:transform first1 last1 first2 result binary-op)

<<parameters>>
  first     : input-iterator.
  last      : input-iterator.
  first1    : input-iterator.
  last1     : input-iterator.
  first2    : input-iterator.
  result    : output-iterator.
  op        : unary functor.
  binary-op : binary functor.

<<return value>>
  iterator points to last of result sequence.
")

(declare-method-overload replace (4 #+cl-stl-extra 5)
  :documentation "
<<signature>>
  (cl-stl:replace first last old-val new-val)
  (cl-stl:replace first last old-val new-val eql-bf)    [extra]

<<parameters>>
  first   : forward-iterator.
  last    : forward-iterator.
  old-val : value to be replaced.
  new-val : new replacement value.
  eql-bf  : binary functor ( use #'operator_== by default ).

<<return value>>
  nil.
")

(defgeneric replace-if (first last pred new-value)
  (:documentation "
<<signature>>
  (cl-stl:replace-if first last pred new-value)

<<parameters>>
  first     : forward-iterator.
  last      : forward-iterator.
  pred      : unary functor.
  new-value : new replacement value.

<<return value>>
  nil.
"))

(declare-method-overload replace-copy (5 #+cl-stl-extra 6)
  :documentation "
<<signature>>
  (cl-stl:replace-copy first last result old-val new-val)
  (cl-stl:replace-copy first last result old-val new-val eql-bf)    [extra]

<<parameters>>
  first   : input-iterator.
  last    : input-iterator.
  result  : output-iterator.
  old-val : value to be replaced.
  new-val : new replacement value.
  eql-bf  : binary functor ( use #'operator_== by default ).

<<return value>>
  iterator points to end of result sequence.
")

(defgeneric replace-copy-if (first last result pred new-value)
  (:documentation "
<<signature>>
  (cl-stl:replace-copy-if first last result pred new-value)

<<parameters>>
  first     : input-iterator.
  last      : input-iterator.
  result    : output-iterator.
  pred      : unary functor.
  new-value : new replacement value.

<<return value>>
  iterator points to end of result sequence.
"))

(declare-method-overload fill (#-cl-stl-0x98 2 3)
  :documentation "
<<signature>>
  1)  (cl-stl:fill container value)   [0x11]
  2)  (cl-stl:fill first last value)

<<parameters>>
  container : stl:array.
  first     : forward-iterator.
  last      : forward-iterator.
  value     : value to fill.

<<return value>>
  1) nil.
  2) nil.
")

(defgeneric fill-n (first n value)
  (:documentation "
<<signature>>
  (cl-stl:fill-n first n value)

<<parameters>>
  first : output-iterator.
  n     : fixnum. count to fill.
  value : value to fill.

<<return value>>
  [0x98] nil.
  [0x11] iterator point to element that follows the last filled element.
"))

(defgeneric generate (first last gen)
  (:documentation "
<<signature>>
  (cl-stl:generate first last gen)

<<parameters>>
  first : forward-iterator.
  last  : forward-iterator.
  gen   : functor take non-argument.

<<return value>>
  nil.
"))

(defgeneric generate-n (first n gen)
  (:documentation "
<<signature>>
  (cl-stl:generate-n first n gen)

<<parameters>>
  first : output-iterator.
  n     : fixnum. count to generate.
  gen   : functor take non-argument.

<<return value>>
  [0x98] nil.
  [0x11] iterator point to first + n.
"))

(declare-method-overload remove (2 3 #+cl-stl-extra 4)
  :documentation "
<<signature>>
  1) (cl-stl:remove container value)
  2) (cl-stl:remove container value eql-bf)     [extra]
  3) (cl-stl:remove first last value)
  4) (cl-stl:remove first last value eql-bf)    [extra]

<<parameters>>
  container : list or forward-list[0x11].
  first     : forward-iterator.
  last      : forward-iterator.
  value     : value to remove.
  eql-bf    : binary functor ( use #'operator_== by default ).

<<return value>>
  1) & 2) nil.
  3) & 4) iterator point to new end of sequence.
")

(declare-method-overload remove-if (2 3)
  :documentation "
<<signature>>
  1) (cl-stl:remove-if container pred)
  2) (cl-stl:remove-if first last pred)

<<parameters>>
  container : list or forward-list[0x11].
  first     : forward-iterator.
  last      : forward-iterator.
  pred      : unary functor.

<<return value>>
  1) nil.
  2) iterator point to new end of sequence.
")

(declare-method-overload remove-copy (4 #+cl-stl-extra 5)
  :documentation "
<<signature>>
  (cl-stl:remove-copy first last result val)
  (cl-stl:remove-copy first alst result val eql-bf)    [extra]

<<parameters>>
  first     : input-iterator.
  last      : input-iterator.
  result    : output-iterator.
  val       : value to remove.
  eql-bf    : binary functor ( use #'operator_== by default ).

<<return value>>
  iterator points to end of result sequence.
")

(defgeneric remove-copy-if (first last result pred)
  (:documentation "
<<signature>>
  (cl-stl:remove-copy-if first last result pred)

<<parameters>>
  first  : input-iterator.
  last   : input-iterator.
  result : output-iterator.
  pred   : unary functor.

<<return value>>
  iterator points to end of result sequence.
"))

(declare-method-overload unique (1 2 3)
  :documentation "
<<signature>>
  1) (cl-stl:unique container)
  2) (cl-stl:unique container pred)
  3) (cl-stl:unique first last)
  4) (cl-stl:unique first last pred)

<<parameters>>
  container : list or forward-list[0x11].
  first     : forward-iterator.
  last      : forward-iterator.
  pred      : binary functor ( use #'operator_== by default ).

<<return value>>
  1) & 2) nil.
  3) & 4) iterator point to new end of sequence.
")

(declare-method-overload unique-copy (3 4)
  :documentation "
<<signature>>
  (cl-stl:unique-copy first last result)
  (cl-stl:unique-copy first last result pred)

<<parameters>>
  first     : input-iterator.
  last      : input-iterator.
  result    : output-iterator.
  pred      : binary functor ( use #'operator_== by default ).

<<return value>>
  iterator point to new end of copied sequence.
")

(declare-method-overload reverse (1 2)
  :documentation "
<<signature>>
  1) (cl-stl:reverse container)
  2) (cl-stl:reverse first last)

<<parameters>>
  container : list or forward-list[0x11].
  first     : bidirectional-iterator.
  last      : bidirectional-iterator.

<<return value>>
  nil.
")

(defgeneric reverse-copy (first last result)
  (:documentation "
<<signature>>
  (cl-stl:reverse-copy first last result)

<<parameters>>
  first     : bidirectional-iterator.
  last      : bidirectional-iterator.
  result    : output-iterator.

<<return value>>
  iterator points to the end of copied sequence.
"))

(defgeneric rotate (first middle last)
  (:documentation "
<<signature>>
  (cl-stl:rotate first middle last)

<<parameters>>
  first     : forward-iterator.
  middle    : forward-iterator.
  last      : forward-iterator.

<<return value>>
  [0x98] nil.
  [0x11] iterator pointing to the element that now contains the value previously pointed by first.
"))

(defgeneric rotate-copy (first middle last result)
  (:documentation "
<<signature>>
  (cl-stl:rotate-copy first middle last result)

<<parameters>>
  first     : forward-iterator.
  middle    : forward-iterator.
  last      : forward-iterator.
  result    : output-iterator.

<<return value>>
  iterator pointing to the end of copied sequence.
"))

(declare-method-overload random-shuffle (2 3)
  :documentation "
<<signature>>
  (cl-stl:random-shuffle first last)
  (cl-stl:random-shuffle first last gen)

<<parameters>>
  first : randomaccess-iterator.
  last  : randomaccess-iterator.
  gen   : unary functor ( use #'random by default ).

<<return value>>
  nil.
")

;ToDo : (defgeneric shuffle (...))   ;ToDo : C++11


;; 25.2.12, partitions:

#-cl-stl-0x98 (defgeneric is-partitioned (first last pred)
  (:documentation "
<<signature>>
  (cl-stl:is-partitioned first last pred)    [0x11]

<<parameters>>
  first : input-iterator.
  last  : input-iterator.
  pred  : unary functor.

<<return value>>
  boolean value.
"))

(defgeneric partition (first last pred)
  (:documentation "
<<signature>>
  (cl-stl:partition first last pred)

<<parameters>>
  first : forward-iterator.
  last  : forward-iterator.
  pred  : unary functor.

<<return value>>
  iterator point to partitioned position.
"))

(defgeneric stable-partition (first last pred)
  (:documentation "
<<signature>>
  (cl-stl:stable-partition first last pred)

<<parameters>>
  first : bidirectional-iterator.
  last  : bidirectional-iterator.
  pred  : unary functor.

<<return value>>
  iterator point to partitioned position.
"))

#-cl-stl-0x98 (defgeneric partition-copy (first last result-true result-false pred)
  (:documentation "
<<signature>>
  (cl-stl:partition-copy first last result-true result-false pred)    [0x11]

<<parameters>>
  first        : input-iterator.
  last         : input-iterator.
  result-true  : output-iterator.
  result-false : output-iterator.
  pred         : unary functor.

<<return value>>
  pair of iterator ( copy of result-true & result-false ).
"))

#-cl-stl-0x98 (defgeneric partition-point (first last pred)
  (:documentation "
<<signature>>
  (cl-stl:partition-point first last pred)    [0x11]

<<parameters>>
  first : forward-iterator.
  last  : forward-iterator.
  pred  : unary functor.

<<return value>>
  iterator point to partition point.
"))


;;------------------------------------------------------------------------------
;; 25.3, sorting and related operations:
;;------------------------------------------------------------------------------
;; 25.3.1, sorting:

(declare-method-overload sort (1 2 3)
  :documentation "
<<signature>>
  1) (cl-stl:sort container)
  2) (cl-stl:sort container pred)
  3) (cl-stl:sort first last)
  4) (cl-stl:sort first last pred)

<<parameters>>
  container : list or forward-list[0x11].
  first     : randomaccess-iterator.
  last      : randomaccess-iterator.
  pred      : binary functor ( use #'operator_< by default ).

<<return value>>
  nil.
")

(declare-method-overload stable-sort (2 3)
  :documentation "
<<signature>>
  (cl-stl:stable-sort first last)
  (cl-stl:stable-sort first last pred)

<<parameters>>
  first     : randomaccess-iterator.
  last      : randomaccess-iterator.
  pred      : binary functor ( use #'operator_< by default ).

<<return value>>
  nil.
")

(declare-method-overload partial-sort (3 4)
  :documentation "
<<signature>>
  (cl-stl:partial-sort first middle last)
  (cl-stl:partial-sort first middle last pred)

<<parameters>>
  first  : randomaccess-iterator.
  middle : randomaccess-iterator.
  last   : randomaccess-iterator.
  pred   : binary functor ( use #'operator_< by default ).

<<return value>>
  nil.
")

(declare-method-overload partial-sort-copy (4 5)
  :documentation "
<<signature>>
  (cl-stl:partial-sort-copy first last result-first result-last)
  (cl-stl:partial-sort-copy first last result-first result-last pred)

<<parameters>>
  first        : input-iterator.
  last         : input-iterator.
  result-first : randomaccess-iterator.
  result-last  : randomaccess-iterator.
  pred         : binary functor ( use #'operator_< by default ).

<<return value>>
  iterator points to end of copied sequence.
")

#-cl-stl-0x98 (declare-method-overload is-sorted (2 3)
  :documentation "
<<signature>>
  (cl-stl:is-sorted first last)        [0x11]
  (cl-stl:is-sorted first last comp)   [0x11]

<<parameters>>
  first : input-iterator.
  last  : input-iterator.
  comp  : binary functor ( use #'operator_< by default ).

<<return value>>
  boolean value.
")

#-cl-stl-0x98 (declare-method-overload is-sorted-until (2 3)
  :documentation "
<<signature>>
  (cl-stl:is-sorted-until first last)        [0x11]
  (cl-stl:is-sorted-until first last comp)   [0x11]

<<parameters>>
  first : input-iterator.
  last  : input-iterator.
  comp  : binary functor ( use #'operator_< by default ).

<<return value>>
  iterator points to result.
")

(declare-method-overload nth-element (3 4)
  :documentation "
<<signature>>
  (cl-stl:nth-element first nth last)
  (cl-stl:nth-element first nth last comp)

<<parameters>>
  first : randomaccess-iterator.
  nth   : randomaccess-iterator.
  last  : randomaccess-iterator.
  comp  : binary functor ( use #'operator_< by default ).

<<return value>>
  nil.
")


;; 25.3.3, binary search:

(declare-method-overload lower-bound (2 3 4)
  :documentation "
<<signature>>
  1) (cl-stl:lower-bound container value)
  2) (cl-stl:lower-bound first last value)
  3) (cl-stl:lower-bound first last value comp)

<<parameters>>
  container : multimap, map, multiset, set.
  first     : forward-iterator.
  last      : forward-iterator.
  value     : value ( or key ) to find.
  comp      : binary functor ( use #'operator_< by default ).

<<return value>>
  iterator points to result.
")

(declare-method-overload upper-bound (2 3 4)
  :documentation "
<<signature>>
  1) (cl-stl:upper-bound container value)
  2) (cl-stl:upper-bound first last value)
  3) (cl-stl:upper-bound first last value comp)

<<parameters>>
  container : multimap, map, multiset, set.
  first     : forward-iterator.
  last      : forward-iterator.
  value     : value ( or key ) to find.
  comp      : binary functor ( use #'operator_< by default ).

<<return value>>
  iterator points to result.
")

(declare-method-overload equal-range (2 3 4)
  :documentation "
<<signature>>
  1) (cl-stl:equal-range container value)
  2) (cl-stl:equal-range first last value)
  3) (cl-stl:equal-range first last value comp)

<<parameters>>
  container : multimap, map, multiset, set.
  first     : forward-iterator.
  last      : forward-iterator.
  value     : value ( or key ) to find.
  comp      : binary functor ( use #'operator_< by default ).

<<return value>>
  pair of iterator points to result.
")

(declare-method-overload binary-search (3 4)
  :documentation "
<<signature>>
  (cl-stl:binary-search first last value)
  (cl-stl:binary-search first last value comp)

<<parameters>>
  first     : forward-iterator.
  last      : forward-iterator.
  value     : value to find.
  comp      : binary functor ( use #'operator_< by default ).

<<return value>>
  boolean value.
")

;; 25.3.4, merge:

(declare-method-overload merge (2 3 5 6)
  :documentation "
<<signature>>
  1)  (cl-stl:merge container1 container2)
  2)  (cl-stl:merge container1 container2 comp)
  3)  (cl-stl:merge first1 last1 first2 last2 result)
  4)  (cl-stl:merge first1 last1 first2 last2 result comp)

<<parameters>>
  container1 : list or forward-list[0x11].
  container2 : same type of container1.
  first1     : input-iterator.
  last1      : input-iterator.
  first2     : input-iterator.
  last2      : input-iterator.
  result     : output-iterator.
  comp       : binary functor ( use #'operator_< by default ).

<<return value>>
  1) & 2) nil.
  3) & 4) iterator points to end of the copied sequence.
")

(declare-method-overload inplace-merge (3 4)
  :documentation "
<<signature>>
  (cl-stl:inplace-merge first middle last)
  (cl-stl:inplace-merge first middle last comp)

<<parameters>>
  first  : bidirectional-iterator.
  middle : bidirectional-iterator.
  last   : bidirectional-iterator.
  comp   : binary functor ( use #'operator_< by default ).

<<return value>>
  nil.
")


;; 25.3.5, set operations:

(declare-method-overload includes (4 5)
 :documentation "
<<signature>>
  (cl-stl:includes first1 last1 first2 last2)
  (cl-stl:includes first1 last1 first2 last2 comp)

<<parameters>>
  first1 : input-iterator.
  last1  : input-iterator.
  first2 : input-iterator.
  last2  : input-iterator.
  comp   : binary functor ( use #'operator_< by default ).

<<return value>>
  boolean value.
")

(declare-method-overload set-union (5 6)
 :documentation "
<<signature>>
  (cl-stl:set-union first1 last1 first2 last2 result)
  (cl-stl:set-union first1 last1 first2 last2 result comp)

<<parameters>>
  first1 : input-iterator.
  last1  : input-iterator.
  first2 : input-iterator.
  last2  : input-iterator.
  result : output-iterator.
  comp   : binary functor ( use #'operator_< by default ).

<<return value>>
  iterator points to end of the result sequence.
")

(declare-method-overload set-intersection (5 6)
 :documentation "
<<signature>>
  (cl-stl:set-intersection first1 last1 first2 last2 result)
  (cl-stl:set-intersection first1 last1 first2 last2 result comp)

<<parameters>>
  first1 : input-iterator.
  last1  : input-iterator.
  first2 : input-iterator.
  last2  : input-iterator.
  result : output-iterator.
  comp   : binary functor ( use #'operator_< by default ).

<<return value>>
  iterator points to end of the result sequence.
")

(declare-method-overload set-difference (5 6)
 :documentation "
<<signature>>
  (cl-stl:set-difference first1 last1 first2 last2 result)
  (cl-stl:set-difference first1 last1 first2 last2 result comp)

<<parameters>>
  first1 : input-iterator.
  last1  : input-iterator.
  first2 : input-iterator.
  last2  : input-iterator.
  result : output-iterator.
  comp   : binary functor ( use #'operator_< by default ).

<<return value>>
  iterator points to end of the result sequence.
")

(declare-method-overload set-symmetric-difference (5 6)
 :documentation "
<<signature>>
  (cl-stl:set-symmetric-difference first1 last1 first2 last2 result)
  (cl-stl:set-symmetric-difference first1 last1 first2 last2 result comp)

<<parameters>>
  first1 : input-iterator.
  last1  : input-iterator.
  first2 : input-iterator.
  last2  : input-iterator.
  result : output-iterator.
  comp   : binary functor ( use #'operator_< by default ).

<<return value>>
  iterator points to end of the result sequence.
")


;; 25.3.6, heap operations:

(declare-method-overload push-heap (2 3)
 :documentation "
<<signature>>
  (cl-stl:push-heap first last)
  (cl-stl:push-heap first last comp)

<<parameters>>
  first : randomaccess-iterator.
  last  : randomaccess-iterator.
  comp  : binary functor ( use #'operator_< by default ).

<<return value>>
  nil.
")

(declare-method-overload pop-heap (2 3)
 :documentation "
<<signature>>
  (cl-stl:pop-heap first last)
  (cl-stl:pop-heap first last comp)

<<parameters>>
  first : randomaccess-iterator.
  last  : randomaccess-iterator.
  comp  : binary functor ( use #'operator_< by default ).

<<return value>>
  nil.
")

(declare-method-overload make-heap (2 3)
 :documentation "
<<signature>>
  (cl-stl:make-heap first last)
  (cl-stl:make-heap first last comp)

<<parameters>>
  first : randomaccess-iterator.
  last  : randomaccess-iterator.
  comp  : binary functor ( use #'operator_< by default ).

<<return value>>
  nil.
")

(declare-method-overload sort-heap (2 3)
 :documentation "
<<signature>>
  (cl-stl:sort-heap first last)
  (cl-stl:sort-heap first last comp)

<<parameters>>
  first : randomaccess-iterator.
  last  : randomaccess-iterator.
  comp  : binary functor ( use #'operator_< by default ).

<<return value>>
  nil.
")

#-cl-stl-0x98 (declare-method-overload is-heap (2 3)
 :documentation "
<<signature>>
  (cl-stl:is-heap first last)        [0x11]
  (cl-stl:is-heap first last comp)   [0x11]

<<parameters>>
  first : randomaccess-iterator.
  last  : randomaccess-iterator.
  comp  : binary functor ( use #'operator_< by default ).

<<return value>>
  boolean value.
")

#-cl-stl-0x98 (declare-method-overload is-heap-until (2 3)
 :documentation "
<<signature>>
  (cl-stl:is-heap-until first last)        [0x11]
  (cl-stl:is-heap-until first last comp)   [0x11]

<<parameters>>
  first : randomaccess-iterator.
  last  : randomaccess-iterator.
  comp  : binary functor ( use #'operator_< by default ).

<<return value>>
  iterator points to result.
")


;; 25.3.7, minimum and maximum:

(declare-method-overload min (#-cl-stl-0x98 1 2 3)
 :documentation "
<<signature>>
  (cl-stl:min il)        [0x11]
  (cl-stl:min il comp)   [0x11]
  (cl-stl:min a b)
  (cl-stl:min a b comp)

<<parameters>>
  il   : initializer-list[0x11].
  a    : value to compare.
  b    : value to compare.
  comp : binary functor ( use #'operator_< by default ).

<<return value>>
  minimum value.
")

(declare-method-overload max (#-cl-stl-0x98 1 2 3)
 :documentation "
<<signature>>
  (cl-stl:max il)        [0x11]
  (cl-stl:max il comp)   [0x11]
  (cl-stl:max a b)
  (cl-stl:max a b comp)

<<parameters>>
  il   : initializer-list[0x11].
  a    : value to compare.
  b    : value to compare.
  comp : binary functor ( use #'operator_< by default ).

<<return value>>
  maximum value.
")

#-cl-stl-0x98 (declare-method-overload minmax (1 2 3)
  :documentation "
<<signature>>
  (cl-stl:minmax il)        [0x11]
  (cl-stl:minmax il comp)   [0x11]
  (cl-stl:minmax a b)       [0x11]
  (cl-stl:minmax a b comp)  [0x11]

<<parameters>>
  il   : initializer-list[0x11].
  a    : value to compare.
  b    : value to compare.
  comp : binary functor ( use #'operator_< by default ).

<<return value>>
  pair of minimum & maximum value.
")

(declare-method-overload min-element (2 3)
  :documentation "
<<signature>>
  (cl-stl:min-element first last)
  (cl-stl:min-element first last comp)

<<parameters>>
  first :forward-iterator.
  last  :forward-iterator.
  comp : binary functor ( use #'operator_< by default ).

<<return value>>
  iterator points to minimum element.
")

(declare-method-overload max-element (2 3)
  :documentation "
<<signature>>
  (cl-stl:max-element first last)
  (cl-stl:max-element first last comp)

<<parameters>>
  first :forward-iterator.
  last  :forward-iterator.
  comp : binary functor ( use #'operator_< by default ).

<<return value>>
  iterator points to maximum element.
")

#-cl-stl-0x98 (declare-method-overload minmax-element (2 3)
  :documentation "
<<signature>>
  (cl-stl:minmax-element first last)         [0x11]
  (cl-stl:minmax-element first last comp)    [0x11]

<<parameters>>
  first :forward-iterator.
  last  :forward-iterator.
  comp : binary functor ( use #'operator_< by default ).

<<return value>>
  pair of iterators points to minimum & maximum element.
")

(declare-method-overload lexicographical-compare (4 5)
  :documentation "
<<signature>>
  (cl-stl:lexicographical-compare first1 last1 first2 last2)
  (cl-stl:lexicographical-compare first1 last1 first2 last2 comp)

<<parameters>>
  first1 : input-iterator.
  last1  : input-iterator.
  first2 : input-iterator.
  last2  : input-iterator.
  comp   : binary functor ( use #'operator_< by default ).

<<return value>>
  boolean value.
")


;; 25.3.9, permutations

(declare-method-overload next-permutation (2 3)
  :documentation "
<<signature>>
  (cl-stl:next-permutation first last)
  (cl-stl:next-permutation first last comp)

<<parameters>>
  first : bidirectional-iterator.
  last  : bidirectional-iterator.
  comp  : binary functor ( use #'operator_< by default ).

<<return value>>
  boolean value.
")

(declare-method-overload prev-permutation (2 3)
  :documentation "
<<signature>>
  (cl-stl:prev-permutation first last)
  (cl-stl:prev-permutation first last comp)

<<parameters>>
  first : bidirectional-iterator.
  last  : bidirectional-iterator.
  comp  : binary functor ( use #'operator_< by default ).

<<return value>>
  boolean value.
")


;;------------------------------------------------------------------------------
;;
;; default operator implementation.
;;
;;------------------------------------------------------------------------------


#-cl-stl-0x98    ; for move semantics
(progn
  (defmethod operator_= :around (a (b remove-reference))
	(multiple-value-bind (lhs rhs)
		(operator_move a (funcall (__rm-ref-closure b)))
	  (funcall (__rm-ref-closure b) rhs)
	  lhs))

  (defmethod operator_cast ((obj remove-reference) typename)
	(operator_cast (funcall (__rm-ref-closure obj)) typename))

  (defmethod operator_move (lhs rhs)
	(declare (ignore lhs))
	(values rhs nil)))



; MEMO : implementation for cons, see cl-stl-utility.lisp




;; operator_* ( for move semantics & operator_= ).
(progn
  #-cl-stl-0x98
  (defmethod (setf operator_*) (new-val (itr input-iterator))
	new-val)
  (defmethod operator_* ((itr output-iterator))
	nil))


