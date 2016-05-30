(defsystem :CL-STL
  :description "CL-STL : a port of C++ standard template library for Common Lisp."
  :version "0.8.4"
  :author "show-matz <show@architect-matsuoka.jpn.org>"
  :licence "LLGPL"
  :depends-on ("closer-mop"
			   "cl-overload"
			   "cl-operator")
  :components ((:file "cl-stl-base")
			   (:file "cl-stl-utility"          :depends-on ("cl-stl-base"))
			   (:file "cl-stl-exceptions"       :depends-on ("cl-stl-base"))
			   (:file "cl-stl-iterator"         :depends-on ("cl-stl-base"))
			   (:file "cl-stl-move-iterator"    :depends-on ("cl-stl-iterator"))
			   (:file "cl-stl-cl-conslist"      :depends-on ("cl-stl-iterator"))
			   (:file "cl-stl-cl-vector"        :depends-on ("cl-stl-iterator"))
			   (:file "cl-stl-functional"       :depends-on ("cl-stl-utility"))
			   (:file "cl-stl-initializer-list" :depends-on ("cl-stl-cl-vector"))
			   (:file "cl-stl-algo-base"        :depends-on ("cl-stl-cl-vector"))
			   (:file "cl-stl-array"            :depends-on ("cl-stl-cl-vector"))
			   (:file "cl-stl-tuple"            :depends-on ("cl-stl-array" "cl-stl-utility"))
			   (:file "cl-stl-vector"           :depends-on ("cl-stl-iterator"))
			   (:file "cl-stl-deque"            :depends-on ("cl-stl-iterator"))
			   (:file "cl-stl-list"             :depends-on ("cl-stl-iterator"))
			   (:file "cl-stl-forward-list"     :depends-on ("cl-stl-cl-conslist"))
			   (:file "cl-stl-numeric"          :depends-on ("cl-stl-iterator" "cl-stl-algo-base" "cl-stl-vector" "cl-stl-array"))
			   (:file "cl-stl-algorithm"        :depends-on ("cl-stl-base" "cl-stl-algo-base" "cl-stl-vector" "cl-stl-array"))
			   (:file "cl-stl-rbnode"           :depends-on ("cl-stl-base"))
			   (:file "cl-stl-rbtree"           :depends-on ("cl-stl-rbnode"))
			   (:file "cl-stl-set"              :depends-on ("cl-stl-iterator" "cl-stl-rbtree"))
			   (:file "cl-stl-multiset"         :depends-on ("cl-stl-iterator" "cl-stl-rbtree"))
			   (:file "cl-stl-map"              :depends-on ("cl-stl-iterator" "cl-stl-rbtree" "cl-stl-utility"))
			   (:file "cl-stl-multimap"         :depends-on ("cl-stl-map"))
			   (:file "cl-stl-stack"            :depends-on ("cl-stl-deque"))
			   (:file "cl-stl-queue"            :depends-on ("cl-stl-deque"))
			   (:file "cl-stl-priority-queue"   :depends-on ("cl-stl-vector" "cl-stl-algorithm"))))
