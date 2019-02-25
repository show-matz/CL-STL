(provide :cl-stl-user)

(defpackage		:cl-stl-user
  (:use			:cl-stl)
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
; #-cl-stl-0x98 :array                           ; shadowed
  #-cl-stl-0x98 :array_iterator
  #-cl-stl-0x98 :array_const_iterator
  #-cl-stl-0x98 :array_reverse_iterator
  #-cl-stl-0x98 :array_const_reverse_iterator
				;----
;				:vector                          ; shadowed
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
;				:list                            ; shadowed
				:list_iterator
				:list_const_iterator
				:list_reverse_iterator
				:list_const_reverse_iterator
				;----
  #-cl-stl-0x98 :forward_list
  #-cl-stl-0x98 :forward_list_iterator
  #-cl-stl-0x98 :forward_list_const_iterator
				;----
;				:set                             ; shadowed
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
;				:map                             ; shadowed
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
;				:push                            ; shadowed
;				:pop                             ; shadowed
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
; #-cl-stl-0x98 :get                             ; shadowed
  #-cl-stl-0x98 :tuple_cat
  #-cl-stl-0x98 :tie
  #-(or cl-stl-noextra cl-stl-0x98)	:with_tie
#-(or
   cl-stl-0x98
   cl-stl-0x11
   cl-stl-0x14) :make_from_tuple
				;----------------------------------
				;utility
				:pair
				:make_pair
;				:first                           ; shadowed
;				:second                          ; shadowed
				;----------------------------------
				;functional
				:functor
				:functor_function
				:functor_call      ; deprecated in version 0.8.3 or later
				:unary_function    ; deprecated in 0x11 or later
				:binary_function   ; deprecated in 0x11 or later
				:define-functor
;#-(or
;   cl-stl-0x98
;   cl-stl-0x11
;   cl-stl-0x14) :apply                          ; shadowed
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
				:unary_negate	;; deprecated in c++17
				:binary_negate	;; deprecated in c++17
  #+(or cl-stl-0x98 cl-stl-0x11 cl-stl-0x14) :binder1st
  #+(or cl-stl-0x98 cl-stl-0x11 cl-stl-0x14) :binder2nd
  #-cl-stl-0x98 :bit_and
  #-cl-stl-0x98 :bit_or
  #-cl-stl-0x98 :bit_xor
  #+(or cl-stl-0x98 cl-stl-0x11 cl-stl-0x14) :pointer_to_unary_function
  #+(or cl-stl-0x98 cl-stl-0x11 cl-stl-0x14) :pointer_to_binary_function
  #+(or cl-stl-0x98 cl-stl-0x11 cl-stl-0x14) :mem_fun_t
  #+(or cl-stl-0x98 cl-stl-0x11 cl-stl-0x14) :mem_fun1_t
; #-cl-stl-0x98 :function                        ; shadowed
  #-cl-stl-0x98 :target
				;utility functions
				:not1			;; deprecated in c++17
				:not2			;; deprecated in c++17
  #-cl-stl-0x98 :is_placeholder
  #-cl-stl-0x98 :is_bind_expression
  #-cl-stl-0x98 :bind
  #+(or cl-stl-0x98 cl-stl-0x11 cl-stl-0x14) :bind1st
  #+(or cl-stl-0x98 cl-stl-0x11 cl-stl-0x14) :bind2nd
  #+(or cl-stl-0x98 cl-stl-0x11 cl-stl-0x14) :ptr_fun1
  #+(or cl-stl-0x98 cl-stl-0x11 cl-stl-0x14) :ptr_fun2
  #+(or cl-stl-0x98 cl-stl-0x11 cl-stl-0x14) :mem_fun
  #+(or cl-stl-0x98 cl-stl-0x11 cl-stl-0x14) :mem_fun1
  #+(or cl-stl-0x98 cl-stl-0x11 cl-stl-0x14) :mem_fun_ref
  #+(or cl-stl-0x98 cl-stl-0x11 cl-stl-0x14) :mem_fun1_ref
  #-cl-stl-0x98 :mem_fn
#-(or
   cl-stl-0x98
   cl-stl-0x11
   cl-stl-0x14) :not_fn
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
#-(or
   cl-stl-0x98
   cl-stl-0x11
   cl-stl-0x14) :clamp
				; 25.1, non-modifying sequence operations:
  #-cl-stl-0x98 :all_of
  #-cl-stl-0x98 :any_of
  #-cl-stl-0x98 :none_of
				:for_each
#-(or
   cl-stl-0x98
   cl-stl-0x11
   cl-stl-0x14) :for_each_n
;				:find                            ; shadowed
				:find_if
  #-cl-stl-0x98 :find_if_not
				:find_end
				:find_first_of
				:adjacent_find
;				:count                           ; shadowed
				:count_if
;				:mismatch                        ; shadowed
;				:equal                           ; shadowed
  #-cl-stl-0x98 :is_permutation
;				:search                          ; shadowed
				:search_n
				; 25.2, modifying sequence operations:
				; 25.2.1, copy:
				:copy
  #-cl-stl-0x98 :copy_n
  #-cl-stl-0x98 :copy_if
				:copy_backward
; #-cl-stl-0x98 :move                            ; shadowed
  #-cl-stl-0x98 :move_backward
				; 25.2.2, swap:
;				:swap            ( moved to CL-OPERATOR )
				:swap_ranges
				:iter_swap
				:transform
;				:replace                         ; shadowed
				:replace_if
				:replace_copy
				:replace_copy_if
;				:fill                            ; shadowed
				:fill_n
				:generate
				:generate_n
;				:remove                          ; shadowed
				:remove_if
				:remove_copy
				:remove_copy_if
				:unique
				:unique_copy
;				:reverse                         ; shadowed
				:reverse_copy
				:rotate
				:rotate_copy
  #+(or cl-stl-0x98 cl-stl-0x11 cl-stl-0x14) :random_shuffle
  #-cl-stl-0x98 :shuffle
				; 25.2.12, partitions:
  #-cl-stl-0x98 :is_partitioned
				:partition
				:stable_partition
  #-cl-stl-0x98 :partition_copy
  #-cl-stl-0x98 :partition_point
				; 25.3, sorting and related operations:
				; 25.3.1, sorting:
;				:sort                            ; shadowed
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
;				:merge                           ; shadowed
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
;				:min                             ; shadowed
;				:max                             ; shadowed
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
  #-cl-stl-0x98 :initializer_list))


(in-package :cl-stl-user)

