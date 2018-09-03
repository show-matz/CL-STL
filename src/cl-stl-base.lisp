(provide :cl-stl)

(defpackage		:cl-stl
  (:nicknames	:stl)
  (:use			:common-lisp
				:cl-overload
				:cl-operator)
  (:export		:stl_version
				;--------------------------------------------------------------------
				; exceptions
				;--------------------------------------------------------------------
				:logic_error
				:domain_error
				:length_error
				:invalid_argument
				:out_of_range
				:runtime_error
				:range_error
				:overflow_error
				:underflow_error
  #-cl-stl-0x98 :bad_function_call    ; [0x11]
				;--------------------------------------------------------------------
				; support for operators
				;--------------------------------------------------------------------
#-cl-stl-noextra :cons_iterator
#-cl-stl-noextra :cons_const_iterator
				;--------------------------------------------------------------------
				; container
				;--------------------------------------------------------------------
				; base types
				;--------------------------
				:pushable_back_container
				:pushable_front_container
				:forward_container
				:bidirectional_container
				:randomaccess_container
				;--------------------------
				; container types
				;--------------------------
  #-cl-stl-0x98 :array
  #-cl-stl-0x98 :array_iterator
  #-cl-stl-0x98 :array_const_iterator
  #-cl-stl-0x98 :array_reverse_iterator
  #-cl-stl-0x98 :array_const_reverse_iterator
				;----
				:vector
				:vector_iterator
				:vector_const_iterator
				:vector_reverse_iterator
				:vector_const_reverse_iterator
				;----
				:deque
				:deque_iterator
				:deque_const_iterator
				:deque_reverse_iterator
				:deque_const_reverse_iterator
				;----
				:list
				:list_iterator
				:list_const_iterator
				:list_reverse_iterator
				:list_const_reverse_iterator
				;----
  #-cl-stl-0x98 :forward_list
  #-cl-stl-0x98 :forward_list_iterator
  #-cl-stl-0x98 :forward_list_const_iterator
				;----
				:set
				:set_iterator
				:set_const_iterator
				:set_reverse_iterator
				:set_const_reverse_iterator
				;----
				:multiset
				:multiset_iterator
				:multiset_const_iterator
				:multiset_reverse_iterator
				:multiset_const_reverse_iterator
				;----
				:map
				:map_iterator
				:map_const_iterator
				:map_reverse_iterator
				:map_const_reverse_iterator
				;----
				:multimap
				:multimap_iterator
				:multimap_const_iterator
				:multimap_reverse_iterator
				:multimap_const_reverse_iterator
				;----
				:stack
				:queue
				:priority_queue
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
  #-cl-stl-0x98 :before_begin
  #-cl-stl-0x98 :cbefore_begin
				; capacity
				:empty
				:size
				:max_size
				:resize
				:capacity
				:reserve
				; element access
				:front
				:back
				:at
  #-cl-stl-0x98 :data
				; modifiers
				:push_back
				:push_front
				:pop_back
				:pop_front
  #-cl-stl-0x98 :emplace_back
  #-cl-stl-0x98 :emplace_front
  #-cl-stl-0x98 :shrink_to_fit
				:insert
  #-cl-stl-0x98 :insert_after
  #-cl-stl-0x98 :emplace
  #-cl-stl-0x98 :emplace_hint
  #-cl-stl-0x98 :emplace_after
				:erase
  #-cl-stl-0x98 :erase_after
;				:swap        ; ( moved to CL-OPERATOR )
				:clear
				:top
				:push
				:pop
				; specific operations
				:splice      ; list
  #-cl-stl-0x98 :splice_after
;				:remove      ; list ( exported in algorithm )
;				:remove_if   ; list ( exported in algorithm )
;				:unique      ; list ( exported in algorithm )
;				:merge       ; list ( exported in algorithm )
;				:sort        ; list ( exported in algorithm )
;				:reverse     ; list ( exported in algorithm )
; #-cl-stl-0x98 :fill        ; array ( exported in algorithm )
;				:find        ; associative container ( exported in algorithm )
;				:count       ; associative container ( exported in algorithm )
;				:lower_bound ; associative container ( exported in algorithm )
;				:upper_bound ; associative container ( exported in algorithm )
;				:equal_range ; associative container ( exported in algorithm )
				; observers
				:key_comp
				:value_comp
				; debug
 #+cl-stl-debug :dump
 #+cl-stl-debug :check_integrity
				;----------------------------------
				;iterator base types
				:input_iterator
				:output_iterator
				:forward_iterator
				:bidirectional_iterator
				:randomaccess_iterator
				:reverse_iterator
				;----------------------------------
				;iterator methods
				:advance
				:distance
				:base
  #-cl-stl-0x98 :prev
  #-cl-stl-0x98 :next
				;iterator utilities
  #-cl-stl-0x98 :make_move_iterator
				:back_inserter
				:front_inserter
				:inserter
				:stream_writer
				:stream_reader
				:with_sequence
				:with_stream_reader
				:with_stream_writer
				:with_file_reader
				:with_file_writer
				:with_buffer_reader
				:with_buffer_writer
				;----------------------------------
				;tuple
  #-cl-stl-0x98 :tuple
  #-cl-stl-0x98 :make_tuple
  #-cl-stl-0x98 :get
  #-cl-stl-0x98 :tuple_cat
  #-cl-stl-0x98 :tie
  #-(or cl-stl-noextra cl-stl-0x98)	:with_tie
				;----------------------------------
				;utility
				:pair
				:make_pair
				:first
				:second
				;----------------------------------
				;functional
				:functor
				:functor_function
				:functor_call      ; deprecated in version 0.8.3 or later
				:unary_function    ; deprecated in 0x11 or later
				:binary_function   ; deprecated in 0x11 or later
				:define-functor
#-(or
   cl-stl-0x98
   cl-stl-0x11
   cl-stl-0x14) :apply
				;classes
				:plus
				:minus
				:multiplies
				:divides
				:modulus
				:negate
				:equal_to
				:not_equal_to
				:greater
				:less
				:greater_equal
				:less_equal
				:logical_and
				:logical_or
				:logical_not
				:unary_negate
				:binary_negate
				:binder1st
				:binder2nd
  #-cl-stl-0x98 :bit_and
  #-cl-stl-0x98 :bit_or
  #-cl-stl-0x98 :bit_xor
				:pointer_to_unary_function
				:pointer_to_binary_function
				:mem_fun_t
				:mem_fun1_t
  #-cl-stl-0x98 :function
  #-cl-stl-0x98 :target
				;utility functions
				:not1
				:not2
  #-cl-stl-0x98 :is_placeholder
  #-cl-stl-0x98 :is_bind_expression
  #-cl-stl-0x98 :bind
				:bind1st
				:bind2nd
				:ptr_fun1
				:ptr_fun2
				:mem_fun
				:mem_fun1
				:mem_fun_ref
				:mem_fun1_ref
  #-cl-stl-0x98 :mem_fn
				;+-----------------------------------------+
				;| numeric                                 |
				;+-----------------------------------------+
				:numeric_limits
				:accumulate
				:adjacent_difference
				:inner_product
  #-cl-stl-0x98 :iota
				:partial_sum
				;+-----------------------------------------+
				;| algorithm                               |
				;+-----------------------------------------+
				; 25.1, non-modifying sequence operations:
  #-cl-stl-0x98 :all_of
  #-cl-stl-0x98 :any_of
  #-cl-stl-0x98 :none_of
				:for_each
				:find
				:find_if
  #-cl-stl-0x98 :find_if_not
				:find_end
				:find_first_of
				:adjacent_find
				:count
				:count_if
				:mismatch
				:equal
  #-cl-stl-0x98 :is_permutation
				:search
				:search_n
				; 25.2, modifying sequence operations:
				; 25.2.1, copy:
				:copy
  #-cl-stl-0x98 :copy_n
  #-cl-stl-0x98 :copy_if
				:copy_backward
  #-cl-stl-0x98 :move
  #-cl-stl-0x98 :move_backward
				; 25.2.2, swap:
;				:swap                ( moved to CL-OPERATOR )
				:swap_ranges
				:iter_swap
				:transform
				:replace
				:replace_if
				:replace_copy
				:replace_copy_if
				:fill
				:fill_n
				:generate
				:generate_n
				:remove
				:remove_if
				:remove_copy
				:remove_copy_if
				:unique
				:unique_copy
				:reverse
				:reverse_copy
				:rotate
				:rotate_copy
				:random_shuffle
  #-cl-stl-0x98 :shuffle
				; 25.2.12, partitions:
  #-cl-stl-0x98 :is_partitioned
				:partition
				:stable_partition
  #-cl-stl-0x98 :partition_copy
  #-cl-stl-0x98 :partition_point
				; 25.3, sorting and related operations:
				; 25.3.1, sorting:
				:sort
				:stable_sort
				:partial_sort
				:partial_sort_copy
  #-cl-stl-0x98 :is_sorted
  #-cl-stl-0x98 :is_sorted_until
				:nth_element
				; 25.3.3, binary search:
				:lower_bound
				:upper_bound
				:equal_range
				:binary_search
				; 25.3.4, merge:
				:merge
				:inplace_merge
				; 25.3.5, set operations:
				:includes
				:set_union
				:set_intersection
				:set_difference
				:set_symmetric_difference
				; 25.3.6, heap operations:
				:push_heap
				:pop_heap
				:make_heap
				:sort_heap
  #-cl-stl-0x98 :is_heap
  #-cl-stl-0x98 :is_heap_until
				; 25.3.7, minimum and maximum:
				:min
				:max
  #-cl-stl-0x98 :minmax
				:min_element
				:max_element
  #-cl-stl-0x98 :minmax_element
				:lexicographical_compare
				; 25.3.9, permutations
				:next_permutation
				:prev_permutation
				;+-----------------------------------------+
				;| misc                                    |
				;+-----------------------------------------+
				:for
  #-cl-stl-0x98 :initializer_list)
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
#-(or
   cl-stl-0x98
   cl-stl-0x11
   cl-stl-0x14) :apply
				;algorithm
				:count
				:equal
				:find
				:fill
				:max
				:merge
				:min
				:mismatch
				:move
				:remove
				:replace
				:reverse
				:search
				:sort))


(in-package :cl-stl)

(declaim (inline __get-imp
				 (setf __get-imp)))

;;------------------------------------------------------------------------------
;;
;; CL-STL support version information
;;
;;------------------------------------------------------------------------------
(when (< 1 (+ #+CL-STL-0x98 1
              #+CL-STL-0x11 1
              #+CL-STL-0x14 1
              #+CL-STL-0x17 1))
  (error "version features duplicated."))

(defun stl_version ()
  #+cl-stl-0x98			:cl-stl-0x98
  #+cl-stl-0x11			:cl-stl-0x11
  #+cl-stl-0x14			:cl-stl-0x14
  #+cl-stl-0x17			:cl-stl-0x17
  #-(or cl-stl-0x98
		cl-stl-0x11
		cl-stl-0x14
		cl-stl-0x17)	:cl-stl-0x17)


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
           (cl:apply fn
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
	 (error 'invalid_argument :what ,(format nil "~A : ~A is not non-negative fixnum." op val))))




;;------------------------------------------------------------------------------
;;
;; generic functions declaration of functional
;;
;;------------------------------------------------------------------------------
(defgeneric functor_function (func))

(defmacro functor_call (func &rest args)
  #+cl-stl-warn-deprecated (warn "functor_call is deprecated.")
  `(funcall ,func ,@args))


;;------------------------------------------------------------------------------
;;
;;
;;
;;------------------------------------------------------------------------------
#-cl-stl-0x98
(locally (declare (optimize speed))

  (defun __get-imp (idx arr)
	(declare (type integer idx))
	(declare (type cl:simple-vector arr))
	(unless (and (<= 0 idx) (< idx (length arr)))
	  (error 'out_of_range :what "Index specified to get is out of range."))
	(svref arr idx))

  (defun (setf __get-imp) (new-val idx arr)
	(declare (type integer idx))
	(declare (type cl:simple-vector arr))
	(unless (and (<= 0 idx) (< idx (length arr)))
	  (error 'out_of_range :what "Index specified to get is out of range."))
	(setf (svref arr idx) new-val)))

#-cl-stl-0x98
(defmacro get (idx obj)
  ;; obj : array, tuple, pair
  (if (or (integerp idx)
		  (and (symbolp idx)
			   (constantp idx)
			   (not (eq idx t))
			   (not (eq idx nil))
			   (not (keywordp idx))))
	  `(__get-imp ,idx (__inner-array ,obj))
	  (error 'type-mismatch :what "First parameter of get must be constant number.")))


;;------------------------------------------------------------------------------
;;
;; base types of iterator
;;
;;------------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass input_iterator (clonable) ())
  (defclass output_iterator (clonable) ())
  (defclass forward_iterator (input_iterator output_iterator) ())
  (defclass bidirectional_iterator (forward_iterator) ())
  (defclass randomaccess_iterator (bidirectional_iterator) ()))


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

;; creating reverse_iterator
(declare-constructor reverse_iterator (1))

;; creating move-iterator
#-cl-stl-0x98
(declare-constructor move-iterator (1))

;;------------------------------------------------------------------------------
;;
;; base types of container
;;
;;------------------------------------------------------------------------------
(defclass pushable_back_container (clonable) ())
(defclass pushable_front_container (clonable) ())
(defclass forward_container (clonable) ())
(defclass bidirectional_container (clonable) ())
(defclass randomaccess_container (clonable) ())



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
  container : stl:vector, stl:deque, stl:forward_list[0x11], stl:list.
  itr1      : input_iterator.
  itr2      : input_iterator.
  count     : fixnum.
  value     : value to assign.
  il        : initializer_list[0x11].
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
     cl-stl:forward_list     [0x11]
     cl-stl:list
     cl-stl:set
     cl-stl:multiset
     cl-stl:map
     cl-stl:multimap
     cl-stl:initializer_list [0x11]
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
     cl-stl:forward_list     [0x11]
     cl-stl:list
     cl-stl:set
     cl-stl:multiset
     cl-stl:map
     cl-stl:multimap
     cl-stl:initializer_list [0x11]
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
  reverse_iterator points to end of object's sequence.
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
  reverse_iterator points to previous to the top of object's sequence.
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
     cl-stl:forward_list [0x11]
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
     cl-stl:forward_list [0x11]
     cl-stl:list
     cl-stl:set
     cl-stl:multiset
     cl-stl:map
     cl-stl:multimap
     cl:vector           [0x11 & extra]

<<return value>>
  const iterator points to next to the end of object's sequence.
"))

#-cl-stl-0x98 (defgeneric crbegin (container))                  ; const-reverse_iterator ; A V D L S MS M MM --
#-cl-stl-0x98 (defgeneric crend (container))                    ; const-reverse_iterator ; A V D L S MS M MM --
#-cl-stl-0x98 (defgeneric before_begin (container))             ; iterator               ; - - - - - -- - -- FL
#-cl-stl-0x98 (defgeneric cbefore_begin (container))            ; const-iterator         ; - - - - - -- - -- FL

; capacity
(defgeneric empty (container))                                     ; t / nil             ; A V D L S MS M MM FL
(defgeneric size (container))                                      ; fixnum              ; A V D L S MS M MM -- ( FL is ok in extra )
(defgeneric max_size (container))                                  ; fixnum              ; A V D L S MS M MM FL
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
     cl-stl:forward_list [0x11 & extra]
     cl:vector           [0x11 & extra]

<<return value>>
  internal data array ( or cons-list ) of container.
"))
(defgeneric top (container))                                       ; value               ; - - - - - -- - --
(defgeneric (setf top) (val container))                            ; value               ; - - - - - -- - --
(defgeneric push (container val))                                  ; value               ; - - - - - -- - --
(defgeneric pop (container))                                       ; value               ; - - - - - -- - --

; modifiers
(defgeneric push_back (container val))                             ; nil                 ; - V D L - -- - --
(defgeneric push_front (container val))                            ; nil                 ; - - D L - -- - --
(defgeneric pop_back (container))                                  ; nil                 ; - V D L - -- - --
(defgeneric pop_front (container))                                 ; nil                 ; - - D L - -- - --

#-cl-stl-0x98    ; emplace back
(progn
  (declare-method-overload emplace_back (2) :make-top nil)
  (defmacro emplace_back (cont type-name (&rest args))
	`(,(make-overload-name 'cl-stl:emplace_back 2) ,cont (new ,type-name ,@args))))

#-cl-stl-0x98    ; emplace front
(progn
  (declare-method-overload emplace_front (2) :make-top nil)
  (defmacro emplace_front (cont type-name (&rest args))
	`(,(make-overload-name 'cl-stl:emplace_front 2) ,cont (new ,type-name ,@args))))

#-cl-stl-0x98 (defgeneric shrink_to_fit (container))               ; nil                 ; - V D - - -- - --

;(defgeneric insert (container itr value))                         ; iterator            ; - V D L S MS M MM
;(defgeneric insert (container itr count value))                   ; nil                 ; - V D L - -- - --
;(defgeneric insert (container itr itr1 itr2))                     ; nil                 ; - V D L - -- - --
;(defgeneric insert (container value))                             ; iterator or pair    ; - - - - S MS M MM
;(defgeneric insert (container itr1 itr2))                         ; nil                 ; - - - - S MS M MM
(declare-method-overload insert (2 3 4))
#-cl-stl-0x98 (declare-method-overload insert_after (3 4))
#-cl-stl-0x98 (declare-macro-overload emplace (3 4))            ; nil                 ; - V D L S MS M MM ST Q PQ

#-cl-stl-0x98 ; emplace
(progn
  (declare-method-overload emplace (2 3) :make-top nil)
  (defmacro-overload emplace (cont type-name (&rest args))
	`(,(make-overload-name 'cl-stl:emplace 2) ,cont (new ,type-name ,@args)))
  (defmacro-overload emplace (cont itr type-name (&rest args))
	`(,(make-overload-name 'cl-stl:emplace 3) ,cont ,itr (new ,type-name ,@args))))

#-cl-stl-0x98 ; emplace_hint
(progn
  (declare-method-overload emplace_hint (3) :make-top nil)
  (defmacro emplace_hint (cont itr type-name (&rest args))
	`(,(make-overload-name 'cl-stl:emplace_hint 3) ,cont ,itr (new ,type-name ,@args))))

#-cl-stl-0x98 ; emplace_after
(progn
  (declare-method-overload emplace_after (3) :make-top nil)
  (defmacro emplace_after (cont itr type-name (&rest args))
	`(,(make-overload-name 'cl-stl:emplace_after 3) ,cont ,itr (new ,type-name ,@args))))



;(defgeneric erase (container itr))                                ; iterator            ; - V D L S MS M MM
;(defgeneric erase (container itr1 itr2))                          ; nil                 ; - V D L S MS M MM
;(defgeneric erase (container key))                                ; fixnum              ; - - - - S MS M MM
(declare-method-overload erase (2 3))
#-cl-stl-0x98 (declare-method-overload erase_after (2 3))

;(defgeneric swap (cont1 cont2))                                   ; nil                 ; A V D L S MS M MM ( moved to CL-OPERATOR )
(defgeneric clear (container))                                     ; nil                 ; - V D L S MS M MM

; specific operations
;(defgeneric splice (lst1 itr lst2 &optional itr1 itr2))           ; nil                 ; - - - L - -- - --
(declare-method-overload splice (3 4 5))
#-cl-stl-0x98 (declare-method-overload splice_after (3 4 5))

;(defgeneric remove (lst val &optional eql-bf))                    ; nil                 ; - - - L - -- - -- ( declare in algorithm )
;(defgeneric remove_if (lst pred-uf))                              ; nil                 ; - - - L - -- - -- ( declare in algorithm )
;(defgeneric unique (lst &optional eql-bf))                        ; nil                 ; - - - L - -- - -- ( declare in algorithm )
;(defgeneric merge (lst1 lst2 &optional less-bf))                  ; nil                 ; - - - L - -- - -- ( declare in algorithm )
;(defgeneric sort (lst &optional less-bf))                         ; nil                 ; - - - L - -- - -- ( declare in algorithm )
;(defgeneric reverse (lst))                                        ; nil                 ; - - - L - -- - -- ( declare in algorithm )
;#-cl-stl-0x98 (defgeneric fill (container val))                   ; nil                 ; A - - - - -- - -- ( declare in algorithm )
;(defgeneric find (container key))                                 ; iterator            ; - - - - S MS M MM ( declare in algorithm )
;(defgeneric count (container key))                                ; fixnum              ; - - - - S MS M MM ( declare in algorithm )
;(defgeneric lower_bound (container key))                          ; iterator            ; - - - - S MS M MM ( declare in algorithm )
;(defgeneric upper_bound (container key))                          ; iterator            ; - - - - S MS M MM ( declare in algorithm )
;(defgeneric equal_range (container key))                          ; pair(itr,itr)       ; - - - - S MS M MM ( declare in algorithm )

; observers
(defgeneric key_comp (container))                                  ; binary_function     ; - - - - S MS M MM
(defgeneric value_comp (container))                                ; binary_function     ; - - - - S MS M MM


; debug
#+cl-stl-debug (defgeneric dump (container &optional stream print-item-fnc))
#+cl-stl-debug (defgeneric check_integrity (container &optional stream))


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
    cl-stl:forward_list
    cl-stl:list
    cl-stl:set
    cl-stl:multiset
    cl-stl:map
    cl-stl:multimap
    cl-stl:initializer_list
    cl:vector

<<return value>>
  1) returns
  2) returns
"
  (if (listp (car args))
	  (destructuring-bind (inits condition update &key (returns nil)) args
		`(do ,inits
			 ((not ,condition) ,returns)
		   (locally
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
  first     : input_iterator.
  last      : input_iterator.
  init      : initial value.
  binary-op : binary functor ( use #'+ default ).

<<return value>>
  accumulated value.
")


;26.4.2 Inner product
(declare-method-overload inner_product (4 6)
  :documentation "
<<signature>>
  (cl-stl:inner_product first1 last1 first2 init)
  (cl-stl:inner_product first1 last1 first2 init binary-op1 binary-op2)

<<parameters>>
  first1     : input_iterator.
  last1      : input_iterator.
  first2     : input_iterator.
  init       : initial value.
  binary-op1 : binary functor ( use #'+ default ).
  binary-op2 : binary functor ( use #'* default ).

<<return value>>
  inner product.
")


;26.4.3 Partial sum
(declare-method-overload partial_sum (3 4)
  :documentation "
<<signature>>
  (cl-stl:partial_sum first last result)
  (cl-stl:partial_sum first last result binary-op)

<<parameters>>
  first     : input_iterator.
  last      : input_iterator.
  result    : output_iterator.
  binary-op : binary functor ( use #'+ default ).

<<return value>>
  copy of result ( point to end of sequence ).
")


;26.4.4 Adjacent difference
(declare-method-overload adjacent_difference (3 4)
  :documentation "
<<signature>>
  (cl-stl:adjacent_difference first last result)
  (cl-stl:adjacent_difference first last result binary-op)

<<parameters>>
  first     : input_iterator.
  last      : input_iterator.
  result    : output_iterator.
  binary-op : binary functor ( use #'- default ).

<<return value>>
  copy of result ( point to end of sequence ).
")

#-cl-stl-0x98
(declare-method-overload iota (3 #-cl-stl-noextra 4)
  :documentation "
<<signature>>
  (cl-stl:iota first last init)            [0x11]
  (cl-stl:iota first last init unary-op)   [0x11 & extra]

<<parameters>>
  first    : forward_iterator.
  last     : forward_iterator.
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

#-cl-stl-0x98 (defgeneric all_of (first last pred)
  (:documentation "
<<signature>>
  (cl-stl:all_of first last pred)   [0x11]

<<parameters>>
  first : input_iterator.
  last  : input_iterator.
  pred  : unary functor.

<<return value>>
  boolean value.
"))

#-cl-stl-0x98 (defgeneric any_of (first last pred)
  (:documentation "
<<signature>>
  (cl-stl:any_of first last pred)   [0x11]

<<parameters>>
  first : input_iterator.
  last  : input_iterator.
  pred  : unary functor.

<<return value>>
  boolean value.
"))

#-cl-stl-0x98 (defgeneric none_of (first last pred)
  (:documentation "
<<signature>>
  (cl-stl:none_of first last pred)   [0x11]

<<parameters>>
  first : input_iterator.
  last  : input_iterator.
  pred  : unary functor.

<<return value>>
  boolean value.
"))

(defgeneric for_each (first last func)
  (:documentation "
<<signature>>
  (cl-stl:for_each first last func)

<<parameters>>
  first : input_iterator.
  last  : input_iterator.
  func  : unary functor.

<<return value>>
  copy of func.
"))

(declare-method-overload find (2 3 #-cl-stl-noextra 4)
  :documentation "
<<signature>>
  1) (cl-stl:find container  value)
  2) (cl-stl:find first last value)
  3) (cl-stl:find first last value eql-bf)    [extra]

<<parameters>>
  container : stl:set, stl:multiset, stl:map, stl:multimap.
  first     : input_iterator.
  last      : input_iterator.
  value     : value to find.
  eql-bf    : binary functor ( use #'operator_== by default ).

<<return value>>
  iterator point to found element.
")

(defgeneric find_if (first last pred)
  (:documentation "
<<signature>>
  (cl-stl:find_if first last pred)

<<parameters>>
  first : input_iterator.
  last  : input_iterator.
  pred  : unary functor.

<<return value>>
  iterator point to found element.
"))

#-cl-stl-0x98 (defgeneric find_if_not (first last pred)
  (:documentation "
<<signature>>
  (cl-stl:find_if_not first last pred)    [0x11]

<<parameters>>
  first : input_iterator.
  last  : input_iterator.
  pred  : unary functor.

<<return value>>
  iterator point to found element.
"))

(declare-method-overload find_end (4 5)
 :documentation "
<<signature>>
  (cl-stl:find_end first1 last1 first2 last2)
  (cl-stl:find_end first1 last1 first2 last2 eql-bf)

<<parameters>>
  first1 : forward_iterator.
  last1  : forward_iterator.
  first2 : forward_iterator.
  last2  : forward_iterator.
  eql-bf : binary functor ( use #'operator_== by default ).

<<return value>>
  iterator point to found element.

 MEMO : [0x98] If [first2,last2) is an empty range, the result is unspecified.
        [0x11] If [first2,last2) is an empty range, the function returns last1.
")

(declare-method-overload find_first_of (4 5)
 :documentation "
<<signature>>
  (cl-stl:find_first_of first1 last1 first2 last2)
  (cl-stl:find_first_of first1 last1 first2 last2 eql-bf)

<<parameters>>
  first1 : [0x98] forward_iterator / [0x11] input_iterator
  last1  : [0x98] forward_iterator / [0x11] input_iterator
  first2 : forward_iterator.
  last2  : forward_iterator.
  eql-bf : binary functor ( use #'operator_== by default ).

<<return value>>
  iterator point to found element.

 MEMO : [0x98] If [first2,last2) is an empty range, the result is unspecified.
        [0x11] If [first2,last2) is an empty range, the function returns last1.
")

(declare-method-overload adjacent_find (2 3)
 :documentation "
<<signature>>
  (cl-stl:adjacent_find first last)
  (cl-stl:adjacent_find first last eql-bf)

<<parameters>>
  first  : forward_iterator.
  last   : forward_iterator.
  eql-bf : binary functor ( use #'operator_== by default ).

<<return value>>
  iterator point to found element.
")

(declare-method-overload count (2 3 #-cl-stl-noextra 4)
  :documentation "
<<signature>>
  1) (cl-stl:count container  value)
  2) (cl-stl:count first last value)
  3) (cl-stl:count first last value eql-bf)    [extra]

<<parameters>>
  container : stl:set, stl:multiset, stl:map, stl:multimap.
  first     : input_iterator.
  last      : input_iterator.
  value     : value to count.
  eql-bf    : binary functor ( use #'operator_== by default ).

<<return value>>
  count of found element.
")

(defgeneric count_if (first last pred)
  (:documentation "
<<signature>>
  (cl-stl:count_if first last pred)

<<parameters>>
  first : input_iterator.
  last  : input_iterator.
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
  first1 : forward_iterator.
  last1  : forward_iterator.
  first2 : forward_iterator.
  last2  : forward_iterator.
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
  first1 : forward_iterator.
  last1  : forward_iterator.
  first2 : forward_iterator.
  last2  : forward_iterator.
  eql-bf : binary functor ( use #'operator_== by default ).

<<return value>>
  boolean value.
")

#-cl-stl-0x98 (declare-method-overload is_permutation (3 4 #-(or cl-stl-0x98 cl-stl-0x11) 5)
  :documentation "
<<signature>>
    (cl-stl:is_permutation first1 last1 first2)              [0x11]
    (cl-stl:is_permutation first1 last1 first2 eql-bf)       [0x11]
    (cl-stl:is_permutation first1 last1 first2 last2)        [0x14]
    (cl-stl:is_permutation first1 last1 first2 last2 eql-bf) [0x14]

<<parameters>>
  first1 : forward_iterator.
  last1  : forward_iterator.
  first2 : forward_iterator.
  last2  : forward_iterator.
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
  first1 : forward_iterator.
  last1  : forward_iterator.
  first2 : forward_iterator.
  last2  : forward_iterator.
  eql-bf : binary functor ( use #'operator_== by default ).

<<return value>>
  iterator point to top of found sequence.

 MEMO : [0x98] If [first2,last2) is an empty range, the result is unspecified.
        [0x11] If [first2,last2) is an empty range, the function returns last1.
")

(declare-method-overload search_n (4 5)
  :documentation "
<<signature>>
    (cl-stl:search_n first last count val)
    (cl-stl:search_n first last count val pred)

<<parameters>>
  first : forward_iterator.
  last  : forward_iterator.
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
  first  : input_iterator.
  last   : input_iterator.
  result : output_iterator.

<<return value>>
  iterator point to the end of the copied sequence.
"))

#-cl-stl-0x98 (defgeneric copy_n (first n result)
  (:documentation "
<<signature>>
  (cl-stl:copy_n first n result)    [0x11]

<<parameters>>
  first  : input_iterator.
  n      : fixnum.
  result : output_iterator.

<<return value>>
  iterator point to the end of the copied sequence.
"))

#-cl-stl-0x98 (defgeneric copy_if (first last result pred)
  (:documentation "
<<signature>>
  (cl-stl:copy_if first last result pred)    [0x11]

<<parameters>>
  first  : input_iterator.
  last   : input_iterator.
  result : output_iterator.
  pred   : unary functor.

<<return value>>
  iterator point to the end of the copied sequence.
"))

(defgeneric copy_backward (first last result)
  (:documentation "
<<signature>>
  (cl-stl:copy_backward first last result)

<<parameters>>
  first  : bidirectional_iterator.
  last   : bidirectional_iterator.
  result : bidirectional_iterator.

<<return value>>
  iterator point to the top of the copied sequence.
"))

#-cl-stl-0x98 (defgeneric move (first last result)
  (:documentation "
<<signature>>
  (cl-stl:move first last result)    [0x11]

<<parameters>>
  first  : input_iterator.
  last   : input_iterator.
  result : output_iterator.

<<return value>>
  iterator point to end of moved sequence.
"))


#-cl-stl-0x98 (defgeneric move_backward (first last result)
  (:documentation "
<<signature>>
  (cl-stl:move_backward first last result)    [0x11]

<<parameters>>
  first  : bidirectional_iterator.
  last   : bidirectional_iterator.
  result : bidirectional_iterator.

<<return value>>
  iterator point to the top of the moved sequence.
"))


;; 25.2.2, swap:

;(declare-method-overload swap (2) :make-top nil)  -> moved to CL-OPERATOR
;(defmacro swap (a b) ... )                        -> moved to CL-OPERATOR


(defgeneric swap_ranges (first1 last1 first2)
  (:documentation "
<<signature>>
  (cl-stl:swap_ranges first1 last1 first2)

<<parameters>>
  first1 : forward_iterator.
  last1  : forward_iterator.
  first2 : forward_iterator.

<<return value>>
  iterator points to last element swapped in the second sequence.
"))

(defgeneric iter_swap (a b)
  (:documentation "
<<signature>>
  (cl-stl:iter_swap a b)

<<parameters>>
  a : forward_iterator.
  b : forward_iterator.

<<return value>>
  nil.
"))

(declare-method-overload transform (4 5)
  :documentation "
<<signature>>
  1)  (cl-stl:transform first last result op)
  2)  (cl-stl:transform first1 last1 first2 result binary-op)

<<parameters>>
  first     : input_iterator.
  last      : input_iterator.
  first1    : input_iterator.
  last1     : input_iterator.
  first2    : input_iterator.
  result    : output_iterator.
  op        : unary functor.
  binary-op : binary functor.

<<return value>>
  iterator points to last of result sequence.
")

(declare-method-overload replace (4 #-cl-stl-noextra 5)
  :documentation "
<<signature>>
  (cl-stl:replace first last old-val new-val)
  (cl-stl:replace first last old-val new-val eql-bf)    [extra]

<<parameters>>
  first   : forward_iterator.
  last    : forward_iterator.
  old-val : value to be replaced.
  new-val : new replacement value.
  eql-bf  : binary functor ( use #'operator_== by default ).

<<return value>>
  nil.
")

(defgeneric replace_if (first last pred new-value)
  (:documentation "
<<signature>>
  (cl-stl:replace_if first last pred new-value)

<<parameters>>
  first     : forward_iterator.
  last      : forward_iterator.
  pred      : unary functor.
  new-value : new replacement value.

<<return value>>
  nil.
"))

(declare-method-overload replace_copy (5 #-cl-stl-noextra 6)
  :documentation "
<<signature>>
  (cl-stl:replace_copy first last result old-val new-val)
  (cl-stl:replace_copy first last result old-val new-val eql-bf)    [extra]

<<parameters>>
  first   : input_iterator.
  last    : input_iterator.
  result  : output_iterator.
  old-val : value to be replaced.
  new-val : new replacement value.
  eql-bf  : binary functor ( use #'operator_== by default ).

<<return value>>
  iterator points to end of result sequence.
")

(defgeneric replace_copy_if (first last result pred new-value)
  (:documentation "
<<signature>>
  (cl-stl:replace_copy_if first last result pred new-value)

<<parameters>>
  first     : input_iterator.
  last      : input_iterator.
  result    : output_iterator.
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
  first     : forward_iterator.
  last      : forward_iterator.
  value     : value to fill.

<<return value>>
  1) nil.
  2) nil.
")

(defgeneric fill_n (first n value)
  (:documentation "
<<signature>>
  (cl-stl:fill_n first n value)

<<parameters>>
  first : output_iterator.
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
  first : forward_iterator.
  last  : forward_iterator.
  gen   : functor take non-argument.

<<return value>>
  nil.
"))

(defgeneric generate_n (first n gen)
  (:documentation "
<<signature>>
  (cl-stl:generate_n first n gen)

<<parameters>>
  first : output_iterator.
  n     : fixnum. count to generate.
  gen   : functor take non-argument.

<<return value>>
  [0x98] nil.
  [0x11] iterator point to first + n.
"))

(declare-method-overload remove (2 3 #-cl-stl-noextra 4)
  :documentation "
<<signature>>
  1) (cl-stl:remove container value)
  2) (cl-stl:remove container value eql-bf)     [extra]
  3) (cl-stl:remove first last value)
  4) (cl-stl:remove first last value eql-bf)    [extra]

<<parameters>>
  container : list or forward_list[0x11].
  first     : forward_iterator.
  last      : forward_iterator.
  value     : value to remove.
  eql-bf    : binary functor ( use #'operator_== by default ).

<<return value>>
  1) & 2) nil.
  3) & 4) iterator point to new end of sequence.
")

(declare-method-overload remove_if (2 3)
  :documentation "
<<signature>>
  1) (cl-stl:remove_if container pred)
  2) (cl-stl:remove_if first last pred)

<<parameters>>
  container : list or forward_list[0x11].
  first     : forward_iterator.
  last      : forward_iterator.
  pred      : unary functor.

<<return value>>
  1) nil.
  2) iterator point to new end of sequence.
")

(declare-method-overload remove_copy (4 #-cl-stl-noextra 5)
  :documentation "
<<signature>>
  (cl-stl:remove_copy first last result val)
  (cl-stl:remove_copy first alst result val eql-bf)    [extra]

<<parameters>>
  first     : input_iterator.
  last      : input_iterator.
  result    : output_iterator.
  val       : value to remove.
  eql-bf    : binary functor ( use #'operator_== by default ).

<<return value>>
  iterator points to end of result sequence.
")

(defgeneric remove_copy_if (first last result pred)
  (:documentation "
<<signature>>
  (cl-stl:remove_copy_if first last result pred)

<<parameters>>
  first  : input_iterator.
  last   : input_iterator.
  result : output_iterator.
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
  container : list or forward_list[0x11].
  first     : forward_iterator.
  last      : forward_iterator.
  pred      : binary functor ( use #'operator_== by default ).

<<return value>>
  1) & 2) nil.
  3) & 4) iterator point to new end of sequence.
")

(declare-method-overload unique_copy (3 4)
  :documentation "
<<signature>>
  (cl-stl:unique_copy first last result)
  (cl-stl:unique_copy first last result pred)

<<parameters>>
  first     : input_iterator.
  last      : input_iterator.
  result    : output_iterator.
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
  container : list or forward_list[0x11].
  first     : bidirectional_iterator.
  last      : bidirectional_iterator.

<<return value>>
  nil.
")

(defgeneric reverse_copy (first last result)
  (:documentation "
<<signature>>
  (cl-stl:reverse_copy first last result)

<<parameters>>
  first     : bidirectional_iterator.
  last      : bidirectional_iterator.
  result    : output_iterator.

<<return value>>
  iterator points to the end of copied sequence.
"))

(defgeneric rotate (first middle last)
  (:documentation "
<<signature>>
  (cl-stl:rotate first middle last)

<<parameters>>
  first     : forward_iterator.
  middle    : forward_iterator.
  last      : forward_iterator.

<<return value>>
  [0x98] nil.
  [0x11] iterator pointing to the element that now contains the value previously pointed by first.
"))

(defgeneric rotate_copy (first middle last result)
  (:documentation "
<<signature>>
  (cl-stl:rotate_copy first middle last result)

<<parameters>>
  first     : forward_iterator.
  middle    : forward_iterator.
  last      : forward_iterator.
  result    : output_iterator.

<<return value>>
  iterator pointing to the end of copied sequence.
"))

(declare-method-overload random_shuffle (2 3)
  :documentation "
<<signature>>
  (cl-stl:random_shuffle first last)
  (cl-stl:random_shuffle first last gen)

<<parameters>>
  first : randomaccess_iterator.
  last  : randomaccess_iterator.
  gen   : unary functor ( use #'random by default ).

<<return value>>
  nil.
")

#-cl-stl-0x98 (defgeneric shuffle (first last gen)
  (:documentation "
<<signature>>
  (cl-stl:shuffle first last gen)

<<parameters>>
  first : randomaccess_iterator.
  last  : randomaccess_iterator.
  gen   : unary functor.

<<return value>>
  nil.
"))


;; 25.2.12, partitions:

#-cl-stl-0x98 (defgeneric is_partitioned (first last pred)
  (:documentation "
<<signature>>
  (cl-stl:is_partitioned first last pred)    [0x11]

<<parameters>>
  first : input_iterator.
  last  : input_iterator.
  pred  : unary functor.

<<return value>>
  boolean value.
"))

(defgeneric partition (first last pred)
  (:documentation "
<<signature>>
  (cl-stl:partition first last pred)

<<parameters>>
  first : forward_iterator.
  last  : forward_iterator.
  pred  : unary functor.

<<return value>>
  iterator point to partitioned position.
"))

(defgeneric stable_partition (first last pred)
  (:documentation "
<<signature>>
  (cl-stl:stable_partition first last pred)

<<parameters>>
  first : bidirectional_iterator.
  last  : bidirectional_iterator.
  pred  : unary functor.

<<return value>>
  iterator point to partitioned position.
"))

#-cl-stl-0x98 (defgeneric partition_copy (first last result-true result-false pred)
  (:documentation "
<<signature>>
  (cl-stl:partition_copy first last result-true result-false pred)    [0x11]

<<parameters>>
  first        : input_iterator.
  last         : input_iterator.
  result-true  : output_iterator.
  result-false : output_iterator.
  pred         : unary functor.

<<return value>>
  pair of iterator ( copy of result-true & result-false ).
"))

#-cl-stl-0x98 (defgeneric partition_point (first last pred)
  (:documentation "
<<signature>>
  (cl-stl:partition_point first last pred)    [0x11]

<<parameters>>
  first : forward_iterator.
  last  : forward_iterator.
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
  container : list or forward_list[0x11].
  first     : randomaccess_iterator.
  last      : randomaccess_iterator.
  pred      : binary functor ( use #'operator_< by default ).

<<return value>>
  nil.
")

(declare-method-overload stable_sort (2 3)
  :documentation "
<<signature>>
  (cl-stl:stable_sort first last)
  (cl-stl:stable_sort first last pred)

<<parameters>>
  first     : randomaccess_iterator.
  last      : randomaccess_iterator.
  pred      : binary functor ( use #'operator_< by default ).

<<return value>>
  nil.
")

(declare-method-overload partial_sort (3 4)
  :documentation "
<<signature>>
  (cl-stl:partial_sort first middle last)
  (cl-stl:partial_sort first middle last pred)

<<parameters>>
  first  : randomaccess_iterator.
  middle : randomaccess_iterator.
  last   : randomaccess_iterator.
  pred   : binary functor ( use #'operator_< by default ).

<<return value>>
  nil.
")

(declare-method-overload partial_sort_copy (4 5)
  :documentation "
<<signature>>
  (cl-stl:partial_sort_copy first last result-first result-last)
  (cl-stl:partial_sort_copy first last result-first result-last pred)

<<parameters>>
  first        : input_iterator.
  last         : input_iterator.
  result-first : randomaccess_iterator.
  result-last  : randomaccess_iterator.
  pred         : binary functor ( use #'operator_< by default ).

<<return value>>
  iterator points to end of copied sequence.
")

#-cl-stl-0x98 (declare-method-overload is_sorted (2 3)
  :documentation "
<<signature>>
  (cl-stl:is_sorted first last)        [0x11]
  (cl-stl:is_sorted first last comp)   [0x11]

<<parameters>>
  first : input_iterator.
  last  : input_iterator.
  comp  : binary functor ( use #'operator_< by default ).

<<return value>>
  boolean value.
")

#-cl-stl-0x98 (declare-method-overload is_sorted_until (2 3)
  :documentation "
<<signature>>
  (cl-stl:is_sorted_until first last)        [0x11]
  (cl-stl:is_sorted_until first last comp)   [0x11]

<<parameters>>
  first : input_iterator.
  last  : input_iterator.
  comp  : binary functor ( use #'operator_< by default ).

<<return value>>
  iterator points to result.
")

(declare-method-overload nth_element (3 4)
  :documentation "
<<signature>>
  (cl-stl:nth_element first nth last)
  (cl-stl:nth_element first nth last comp)

<<parameters>>
  first : randomaccess_iterator.
  nth   : randomaccess_iterator.
  last  : randomaccess_iterator.
  comp  : binary functor ( use #'operator_< by default ).

<<return value>>
  nil.
")


;; 25.3.3, binary search:

(declare-method-overload lower_bound (2 3 4)
  :documentation "
<<signature>>
  1) (cl-stl:lower_bound container value)
  2) (cl-stl:lower_bound first last value)
  3) (cl-stl:lower_bound first last value comp)

<<parameters>>
  container : multimap, map, multiset, set.
  first     : forward_iterator.
  last      : forward_iterator.
  value     : value ( or key ) to find.
  comp      : binary functor ( use #'operator_< by default ).

<<return value>>
  iterator points to result.
")

(declare-method-overload upper_bound (2 3 4)
  :documentation "
<<signature>>
  1) (cl-stl:upper_bound container value)
  2) (cl-stl:upper_bound first last value)
  3) (cl-stl:upper_bound first last value comp)

<<parameters>>
  container : multimap, map, multiset, set.
  first     : forward_iterator.
  last      : forward_iterator.
  value     : value ( or key ) to find.
  comp      : binary functor ( use #'operator_< by default ).

<<return value>>
  iterator points to result.
")

(declare-method-overload equal_range (2 3 4)
  :documentation "
<<signature>>
  1) (cl-stl:equal_range container value)
  2) (cl-stl:equal_range first last value)
  3) (cl-stl:equal_range first last value comp)

<<parameters>>
  container : multimap, map, multiset, set.
  first     : forward_iterator.
  last      : forward_iterator.
  value     : value ( or key ) to find.
  comp      : binary functor ( use #'operator_< by default ).

<<return value>>
  pair of iterator points to result.
")

(declare-method-overload binary_search (3 4)
  :documentation "
<<signature>>
  (cl-stl:binary_search first last value)
  (cl-stl:binary_search first last value comp)

<<parameters>>
  first     : forward_iterator.
  last      : forward_iterator.
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
  container1 : list or forward_list[0x11].
  container2 : same type of container1.
  first1     : input_iterator.
  last1      : input_iterator.
  first2     : input_iterator.
  last2      : input_iterator.
  result     : output_iterator.
  comp       : binary functor ( use #'operator_< by default ).

<<return value>>
  1) & 2) nil.
  3) & 4) iterator points to end of the copied sequence.
")

(declare-method-overload inplace_merge (3 4)
  :documentation "
<<signature>>
  (cl-stl:inplace_merge first middle last)
  (cl-stl:inplace_merge first middle last comp)

<<parameters>>
  first  : bidirectional_iterator.
  middle : bidirectional_iterator.
  last   : bidirectional_iterator.
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
  first1 : input_iterator.
  last1  : input_iterator.
  first2 : input_iterator.
  last2  : input_iterator.
  comp   : binary functor ( use #'operator_< by default ).

<<return value>>
  boolean value.
")

(declare-method-overload set_union (5 6)
 :documentation "
<<signature>>
  (cl-stl:set_union first1 last1 first2 last2 result)
  (cl-stl:set_union first1 last1 first2 last2 result comp)

<<parameters>>
  first1 : input_iterator.
  last1  : input_iterator.
  first2 : input_iterator.
  last2  : input_iterator.
  result : output_iterator.
  comp   : binary functor ( use #'operator_< by default ).

<<return value>>
  iterator points to end of the result sequence.
")

(declare-method-overload set_intersection (5 6)
 :documentation "
<<signature>>
  (cl-stl:set_intersection first1 last1 first2 last2 result)
  (cl-stl:set_intersection first1 last1 first2 last2 result comp)

<<parameters>>
  first1 : input_iterator.
  last1  : input_iterator.
  first2 : input_iterator.
  last2  : input_iterator.
  result : output_iterator.
  comp   : binary functor ( use #'operator_< by default ).

<<return value>>
  iterator points to end of the result sequence.
")

(declare-method-overload set_difference (5 6)
 :documentation "
<<signature>>
  (cl-stl:set_difference first1 last1 first2 last2 result)
  (cl-stl:set_difference first1 last1 first2 last2 result comp)

<<parameters>>
  first1 : input_iterator.
  last1  : input_iterator.
  first2 : input_iterator.
  last2  : input_iterator.
  result : output_iterator.
  comp   : binary functor ( use #'operator_< by default ).

<<return value>>
  iterator points to end of the result sequence.
")

(declare-method-overload set_symmetric_difference (5 6)
 :documentation "
<<signature>>
  (cl-stl:set_symmetric_difference first1 last1 first2 last2 result)
  (cl-stl:set_symmetric_difference first1 last1 first2 last2 result comp)

<<parameters>>
  first1 : input_iterator.
  last1  : input_iterator.
  first2 : input_iterator.
  last2  : input_iterator.
  result : output_iterator.
  comp   : binary functor ( use #'operator_< by default ).

<<return value>>
  iterator points to end of the result sequence.
")


;; 25.3.6, heap operations:

(declare-method-overload push_heap (2 3)
 :documentation "
<<signature>>
  (cl-stl:push_heap first last)
  (cl-stl:push_heap first last comp)

<<parameters>>
  first : randomaccess_iterator.
  last  : randomaccess_iterator.
  comp  : binary functor ( use #'operator_< by default ).

<<return value>>
  nil.
")

(declare-method-overload pop_heap (2 3)
 :documentation "
<<signature>>
  (cl-stl:pop_heap first last)
  (cl-stl:pop_heap first last comp)

<<parameters>>
  first : randomaccess_iterator.
  last  : randomaccess_iterator.
  comp  : binary functor ( use #'operator_< by default ).

<<return value>>
  nil.
")

(declare-method-overload make_heap (2 3)
 :documentation "
<<signature>>
  (cl-stl:make_heap first last)
  (cl-stl:make_heap first last comp)

<<parameters>>
  first : randomaccess_iterator.
  last  : randomaccess_iterator.
  comp  : binary functor ( use #'operator_< by default ).

<<return value>>
  nil.
")

(declare-method-overload sort_heap (2 3)
 :documentation "
<<signature>>
  (cl-stl:sort_heap first last)
  (cl-stl:sort_heap first last comp)

<<parameters>>
  first : randomaccess_iterator.
  last  : randomaccess_iterator.
  comp  : binary functor ( use #'operator_< by default ).

<<return value>>
  nil.
")

#-cl-stl-0x98 (declare-method-overload is_heap (2 3)
 :documentation "
<<signature>>
  (cl-stl:is_heap first last)        [0x11]
  (cl-stl:is_heap first last comp)   [0x11]

<<parameters>>
  first : randomaccess_iterator.
  last  : randomaccess_iterator.
  comp  : binary functor ( use #'operator_< by default ).

<<return value>>
  boolean value.
")

#-cl-stl-0x98 (declare-method-overload is_heap_until (2 3)
 :documentation "
<<signature>>
  (cl-stl:is_heap_until first last)        [0x11]
  (cl-stl:is_heap_until first last comp)   [0x11]

<<parameters>>
  first : randomaccess_iterator.
  last  : randomaccess_iterator.
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
  il   : initializer_list[0x11].
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
  il   : initializer_list[0x11].
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
  il   : initializer_list[0x11].
  a    : value to compare.
  b    : value to compare.
  comp : binary functor ( use #'operator_< by default ).

<<return value>>
  pair of minimum & maximum value.
")

(declare-method-overload min_element (2 3)
  :documentation "
<<signature>>
  (cl-stl:min_element first last)
  (cl-stl:min_element first last comp)

<<parameters>>
  first :forward_iterator.
  last  :forward_iterator.
  comp : binary functor ( use #'operator_< by default ).

<<return value>>
  iterator points to minimum element.
")

(declare-method-overload max_element (2 3)
  :documentation "
<<signature>>
  (cl-stl:max_element first last)
  (cl-stl:max_element first last comp)

<<parameters>>
  first :forward_iterator.
  last  :forward_iterator.
  comp : binary functor ( use #'operator_< by default ).

<<return value>>
  iterator points to maximum element.
")

#-cl-stl-0x98 (declare-method-overload minmax_element (2 3)
  :documentation "
<<signature>>
  (cl-stl:minmax_element first last)         [0x11]
  (cl-stl:minmax_element first last comp)    [0x11]

<<parameters>>
  first :forward_iterator.
  last  :forward_iterator.
  comp : binary functor ( use #'operator_< by default ).

<<return value>>
  pair of iterators points to minimum & maximum element.
")

(declare-method-overload lexicographical_compare (4 5)
  :documentation "
<<signature>>
  (cl-stl:lexicographical_compare first1 last1 first2 last2)
  (cl-stl:lexicographical_compare first1 last1 first2 last2 comp)

<<parameters>>
  first1 : input_iterator.
  last1  : input_iterator.
  first2 : input_iterator.
  last2  : input_iterator.
  comp   : binary functor ( use #'operator_< by default ).

<<return value>>
  boolean value.
")


;; 25.3.9, permutations

(declare-method-overload next_permutation (2 3)
  :documentation "
<<signature>>
  (cl-stl:next_permutation first last)
  (cl-stl:next_permutation first last comp)

<<parameters>>
  first : bidirectional_iterator.
  last  : bidirectional_iterator.
  comp  : binary functor ( use #'operator_< by default ).

<<return value>>
  boolean value.
")

(declare-method-overload prev_permutation (2 3)
  :documentation "
<<signature>>
  (cl-stl:prev_permutation first last)
  (cl-stl:prev_permutation first last comp)

<<parameters>>
  first : bidirectional_iterator.
  last  : bidirectional_iterator.
  comp  : binary functor ( use #'operator_< by default ).

<<return value>>
  boolean value.
")


;;------------------------------------------------------------------------------
;;
;; default operator implementation.
;;
;;------------------------------------------------------------------------------

;; operator_* ( for move semantics & operator_= ).
(progn
  #-cl-stl-0x98
  (defmethod (setf operator_*) (new-val (itr input_iterator))
	new-val)
  (defmethod operator_* ((itr output_iterator))
	nil))


