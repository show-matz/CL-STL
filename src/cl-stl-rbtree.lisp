(in-package :cl-stl)

(declaim (inline __rbtree-begin
				 __rbtree-end
				 __rbtree-rbegin
				 __rbtree-rend))

;;------------------------------------------------------------------------------
;;
;; class difinition
;;
;;------------------------------------------------------------------------------
(defstruct (rbtree (:conc-name __rbtree-))
  (cmp-func nil)              ; functor object or function.
  (cur-size 0             :type fixnum)
  (top-node (make-rbnode) :type rbnode)
  (min-end  (make-rbnode) :type rbnode)
  (max-end  (make-rbnode) :type rbnode))

;;------------------------------------------------------------------------------
;;
;; rbtree utilities
;;
;;------------------------------------------------------------------------------
(defun __rbtree-create (cmpfunc)
  ;; MEMO : cmpfunc not copied here
  (let ((top-node (make-rbnode))
		(min-end  (make-rbnode))
		(max-end  (make-rbnode)))
	(setf (__rbnode-parent top-node) top-node) ; MEMO : need this in rebalance operation.
	(__rbnode-connect-left  top-node min-end)
	(__rbnode-connect-right top-node max-end)
	(make-rbtree :cur-size 0 :cmp-func cmpfunc
				 :top-node top-node :min-end min-end :max-end max-end)))

(defun __rbtree-begin (tree)
  (if (zerop (__rbtree-cur-size tree))
	  (__rbtree-max-end tree)
	  (__rbnode-parent (__rbtree-min-end tree))))

(defun __rbtree-end (tree)
  (__rbtree-max-end tree))

(defun __rbtree-rbegin (tree)
  (if (zerop (__rbtree-cur-size tree))
	  (__rbtree-min-end tree)
	  (__rbnode-parent (__rbtree-max-end tree))))

(defun __rbtree-rend (tree)
  (__rbtree-min-end tree))

(locally (declare (optimize speed))
  (defun __rbtree-for-each (tree uf)
	;; MEMO : uf is always lambda function ( see stl:for ).
	(declare (type cl:function uf))
	(if (zerop (__rbtree-cur-size tree))
		uf
		(do ((node1 (__rbtree-begin tree))
			 (node2 (__rbtree-end   tree)))
			((eq node1 node2) uf)
		  (funcall uf (__rbnode-value node1))
		  (setf node1 (__rbnode-next  node1))))))

;; val より小さくない最小のノードを検索
(locally (declare (optimize speed))
  (defun __rbtree-lower-bound (tree val)
	(let ((cmp (functor-function (__rbtree-cmp-func tree))))
	  (declare (type cl:function cmp))
	  (labels ((imp (mark node)
				 (if (__rbnode-is-external node)
					 (or mark node)
					 (if (funcall cmp (__rbnode-value node) val)
						 (imp mark (__rbnode-right node))
						 (imp node (__rbnode-left  node))))))
		(imp nil (__rbnode-right (__rbtree-top-node tree)))))))

;; val より大きな最小のノードを検索
(locally (declare (optimize speed))
  (defun __rbtree-upper-bound (tree val)
	(let ((cmp (functor-function (__rbtree-cmp-func tree))))
	  (declare (type cl:function cmp))
	  (labels ((imp (mark node)
				 (if (__rbnode-is-external node)
					 (or mark node)
					 (if (funcall cmp val (__rbnode-value node))
						 (imp node (__rbnode-left  node))
						 (imp mark (__rbnode-right node))))))
		(imp nil (__rbnode-right (__rbtree-top-node tree)))))))

(locally (declare (optimize speed))
  (defun __rbtree-count (tree val)
	(let ((cnt 0))
	  (declare (type fixnum cnt))
	  (do ((node1 (__rbtree-lower-bound tree val))
		   (node2 (__rbtree-upper-bound tree val)))
		  ((eq node1 node2) cnt)
		(incf cnt)
		(setf node1 (__rbnode-next node1))))))

;; MEMO : returns found node. if not found, returns external node.
(locally (declare (optimize speed))
  (defun __rbtree-find (tree val)
	(let ((cmp (functor-function (__rbtree-cmp-func tree))))
	  (declare (type cl:function cmp))
	  (labels ((imp (node)
				 (if (__rbnode-is-external node)
					 node
					 (let ((cur (__rbnode-value node)))
					   (if (funcall cmp val cur)
						   (imp (__rbnode-left node))
						   (if (funcall cmp cur val)
							   (imp (__rbnode-right node))
							   node))))))
		(imp (__rbnode-right (__rbtree-top-node tree)))))))


(defun __rbtree-erase-tree (node)
  ;; MEMO : used only in __rbtree-clear
  (let ((left-node  (__rbnode-left  node))
		(right-node (__rbnode-right node)))
	(when left-node
	  (__rbtree-erase-tree left-node)
	  (setf (__rbnode-left node) nil))
	(when right-node
	  (__rbtree-erase-tree right-node)
	  (setf (__rbnode-right node) nil))))

(defun __rbtree-clear (tree)
  (__rbtree-erase-tree (__rbtree-top-node tree))
  (let ((top-node (make-rbnode))
		(min-end  (make-rbnode))
		(max-end  (make-rbnode)))
	(setf (__rbnode-parent top-node) top-node)
	(__rbnode-connect-left  top-node min-end)
	(__rbnode-connect-right top-node max-end)
	(setf (__rbtree-cur-size tree) 0)
	(setf (__rbtree-top-node tree) top-node)
	(setf (__rbtree-min-end  tree) min-end)
	(setf (__rbtree-max-end  tree) max-end)))


(defun __rbtree-single-rotate (node)
  (let* ((parent1 (__rbnode-parent node))
		 (parent2 (__rbnode-parent parent1))
		 (parent3 (__rbnode-parent parent2)))
	(if (__rbnode-is-left parent2)
		(setf (__rbnode-left  parent3) parent1)
		(setf (__rbnode-right parent3) parent1))
	(if (__rbnode-is-left node)
		(progn
		  (setf (__rbnode-left   parent2)                 (__rbnode-right parent1))
		  (setf (__rbnode-parent (__rbnode-left parent2)) parent2)
		  (setf (__rbnode-right  parent1)                 parent2)
		  (setf (__rbnode-parent parent2)                 parent1))
		(progn
		  (setf (__rbnode-right  parent2)                  (__rbnode-left parent1))
		  (setf (__rbnode-parent (__rbnode-right parent2)) parent2)
		  (setf (__rbnode-left   parent1)                  parent2)
		  (setf (__rbnode-parent parent2)                  parent1)))
	(setf (__rbnode-parent parent1) parent3)))


(defun __rbtree-double-rotate (node)
  (let ((parent (__rbnode-parent node)))
	(if (__rbnode-is-left node)
		(__rbtree-single-rotate (__rbnode-left node))
		(__rbtree-single-rotate (__rbnode-right node)))
	(__rbtree-single-rotate parent)))

(defun __rbtree-rebalance-on-insert (node)
  (let* ((p  (__rbnode-parent node))
		 (pp (__rbnode-parent p)))
	(if (or (__rbnode-is-top p) (not (__rbnode-is-red p)))
		nil
		(let ((w nil)
			  (p-is-left (__rbnode-is-left p)))
		  (if p-is-left
			  (setf w (__rbnode-right pp))
			  (setf w (__rbnode-left  pp)))
		  (if (__rbnode-is-red w)
			  (progn
				(incf (__rbnode-rank pp))
				(when (and (not (__rbnode-is-top pp))
						   (__rbnode-is-red (__rbnode-parent pp)))
				  (__rbtree-rebalance-on-insert pp)))
			  (if (__rbnode-is-left node)
				  (if p-is-left
					  (__rbtree-single-rotate node)
					  (__rbtree-double-rotate node))
				  (if p-is-left
					  (__rbtree-double-rotate node)
					  (__rbtree-single-rotate node))))))))

(defun __rbtree-rebalance-on-delete (node)
  (let ((brother       nil)
		(brother-left  nil)
		(brother-right nil)
		(parent (__rbnode-parent node)))
	(if (>= (1+ (__rbnode-rank node)) (__rbnode-rank parent))
		nil
		(progn
		  (setf brother (__rbnode-brother node))
		  (if (__rbnode-is-left node)
			  (progn (setf brother-right (__rbnode-right brother))
					 (setf brother-left  (__rbnode-left  brother)))
			  (progn (setf brother-right (__rbnode-left  brother))
					 (setf brother-left  (__rbnode-right brother))))
		  (let ((parent-was-red (__rbnode-is-red parent)))
			(if (__rbnode-is-red brother)
				(if (or (__rbnode-is-red brother-left)
						(__rbnode-is-red brother-right))
					nil
					(progn (__rbtree-single-rotate brother-right)
						   (__rbtree-rebalance-on-delete node)))
				(if (__rbnode-is-red brother-right)
					(progn
					  (__rbtree-single-rotate brother-right)
					  (incf (__rbnode-rank brother))
					  (decf (__rbnode-rank parent)))
					(if (__rbnode-is-red brother-left)
						(progn
						  (__rbtree-double-rotate brother-left)
						  (incf (__rbnode-rank brother-left))
						  (decf (__rbnode-rank parent)))
						(progn
						  (decf (__rbnode-rank parent))
						  (when (not parent-was-red)
							(__rbtree-rebalance-on-delete parent)))))))))))

(defun __rbtree-delete-node (tree node)
  ;; operate unless node is external.
  (unless (__rbnode-is-external node)
	(let ((left-node nil))
	  (if (not (__rbnode-has-left-child node))
		  ; 対象のノードに左子がない場合
		  (let ((prev-node (__rbnode-right node)))
			(if (__rbnode-is-left node)
				(setf (__rbnode-left  (__rbnode-parent node)) prev-node)
				(setf (__rbnode-right (__rbnode-parent node)) prev-node))
			(setf (__rbnode-parent prev-node) (__rbnode-parent node))
			(setf (__rbnode-left node) nil)
			(setf left-node prev-node))
		  ; 対象のノードに左子がある場合
		  (let ((prev-node (__rbnode-max (__rbnode-left node))))
			(when (not (eq node (__rbnode-parent prev-node)))
			  (setf (__rbnode-right  (__rbnode-parent prev-node)) (__rbnode-left prev-node))
			  (setf (__rbnode-parent (__rbnode-left   prev-node)) (__rbnode-parent prev-node)))
			(setf left-node (__rbnode-left prev-node))
			(setf (__rbnode-right prev-node) nil)
			(if (__rbnode-is-left node)
				(setf (__rbnode-left  (__rbnode-parent node)) prev-node)
				(setf (__rbnode-right (__rbnode-parent node)) prev-node))
			(unless (eq node (__rbnode-parent prev-node))
			  (setf (__rbnode-left prev-node) (__rbnode-left node))
			  (setf (__rbnode-parent (__rbnode-left prev-node)) prev-node))
			(setf (__rbnode-rank   prev-node) (__rbnode-rank   node))
			(setf (__rbnode-parent prev-node) (__rbnode-parent node))
			(setf (__rbnode-right  prev-node) (__rbnode-right  node))
			(setf (__rbnode-parent (__rbnode-right prev-node)) prev-node)))
	  (__rbtree-rebalance-on-delete left-node)
	  (decf (__rbtree-cur-size tree)))
	nil))


(labels ((add-node (tree ex-node val need-copy)
		   (let* ((parent     (__rbnode-parent ex-node))
				  (new-node   (make-rbnode :rank 1))
				  (new-exnode (make-rbnode :rank 0)))
			 ;; set val to new-node
			 (if need-copy
				 (_=   (__rbnode-value new-node) val)
				 (setf (__rbnode-value new-node) val))
			 (if (eq ex-node (__rbnode-left parent))
				 (progn
				   (__rbnode-connect-left  parent   new-node)
				   (__rbnode-connect-left  new-node  ex-node)
				   (__rbnode-connect-right new-node new-exnode))
				 (progn
				   (__rbnode-connect-right parent   new-node)
				   (__rbnode-connect-right new-node  ex-node)
				   (__rbnode-connect-left  new-node new-exnode)))
			 ;; keep balancing
			 (__rbtree-rebalance-on-insert new-node)
			 (incf (__rbtree-cur-size tree))
			 new-node)))

  ;;MEMO : hint 形式の insert で渡される反復子は end かそれ以外であり、end でなければそれは
  ;;       外部ノードではありえない。だから、外部ノードであればそれは end と判断できるが、果たして
  ;;       それに依存して良いものか？
  ;;ToDo : implement '__rbtree-insert-with-hint'...
  (defun __rbtree-insert-with-hint (hint tree val allow-duplicate need-copy)
	(declare (ignorable hint tree val allow-duplicate need-copy))
	nil)

; 	;; hint が外部ノードならば（それはendのはず）親ノードへ移動
;	(when (__rbnode-is-external hint)
;	  (setf hint (__rbnode-parent hint)))
;	(let ((cmp (functor-function (__rbtree-cmp-func tree))))
;	  ;; 
;	  (if (funcall cmp val (__rbnode-value hint))
;		  (let ((next (__rbnode-next hint)))
;			
;
;
; 	;; 要素の重複が許可されず、かつ　hint が該当値ならばそれを返却
;	(if (and (not allow-duplicate)
;			 (_== val (__rbnode-value hint)))
;		hint
;		;; hint が外部ノードかどうかで分岐
;		(if (__rbnode-is-external hint)
;			;; 外部ノードの場合
;			nil
;			;; 外部ノードでない場合
;			nil)))

  (defun __rbtree-insert (tree val allow-duplicate need-copy)
	(if (zerop (__rbtree-cur-size tree))
		;; 空ツリーの場合のみ特別扱い
		(let ((node (add-node tree (__rbtree-max-end tree) val need-copy)))
		  (setf (__rbtree-min-end tree) (__rbnode-left node))
		  (values node t))
		;; 要素の重複が許可されるかどうかで分岐
		(if (not allow-duplicate)
			;; 許可されない場合、findで指定値のノードを検索
			(let ((node (__rbtree-find tree val)))
			  ;; 見つかったのが外部ノードかどうか検査
			  (if (__rbnode-is-external node)
				  ;; 外部ノードの場合、そのノード位置に新しいノ－ドを設定し返却
				  (values (add-node tree node val need-copy) t)
				  ;; 外部ノードでない場合、そのノード自体を返却
				  (values node nil)))
			;; 許可される場合、upper-bound で指定値より大きい最小ノードを検索
			(let ((node (__rbtree-upper-bound tree val)))
			  ;; 見つかったのが外部ノードでない場合、その左子を取得
			  (unless (__rbnode-is-external node)
				(setf node (__rbnode-left node))
				;; それも外部ノードでない場合、その右子配下の最大ノードヘ移動
				(unless (__rbnode-is-external node)
				  (setf node (__rbnode-right (__rbnode-max node)))))
			  ;; 到達したノードに対して add-node をコールして復帰
			  (values (add-node tree node val need-copy) t))))))



;;------------------------------------------------------------------------------
;;
;; debug methods
;;
;;------------------------------------------------------------------------------
#+cl-stl-debug
(defun __rbtree-dump (tree stream print-item-fnc)
  (let ((print-item-fnc (if print-item-fnc
							(functor-function (clone print-item-fnc))
							(lambda (s x) (format s "~A" x)))))
	(labels ((indent (cnt)
			   (with-output-to-string (s)
				 (do ((i 0 (incf i)))
					 ((= i cnt) nil)
				   (format s "    "))))
			 (imp (node stream level)
			   (unless (or (null node) (__rbnode-is-external node))
				 (imp (__rbnode-right node) stream (1+ level))
				 (format stream "~A~A[~A] ~A~%"
						 (indent level)
						 (if (__rbnode-is-red node) "R" "B")
						 (__rbnode-rank node)
						 (with-output-to-string (tag)
						   (funcall print-item-fnc tag (__rbnode-value node))))
				 (imp (__rbnode-left node) stream (1+ level)))))
	  (imp (__rbnode-right (__rbtree-top-node tree)) stream 0))))


#+cl-stl-debug
(defun __rbtree-check-integrity (tree stream)

  (let* ((err-count  0)
		 (the-top    (__rbnode-right (__rbtree-top-node tree)))
		 (node-count (__rbnode-size the-top))
		 (cur-size   (__rbtree-cur-size tree)))
	
	;; get depth information.
	(multiple-value-bind (min-depth max-depth) (__rbnode-depth the-top)

	  ;; write report.
	  (progn
		(format stream "  cur-size cache : ~A~%" cur-size)
		(format stream "  count of node  : ~A~%" node-count)
		(format stream "  min depth      : ~A~%" min-depth)
		(format stream "  max depth      : ~A~%" max-depth))

	  ;; checking cache of node count.
	  (format stream "testing node count cache ... ")
	  (if (= node-count cur-size)
		  (format stream "OK.~%")
		  (progn
			(incf err-count)
			(format stream "NG.~%")))

	  ;; reachablity check ( first -> last )
	  (format stream "testing forward traversal integrity ... ")
	  (let ((node1 (__rbtree-begin tree))
			(node2 (__rbtree-end   tree)))
		(let ((cnt (do ((acc 0))
					   ((eq node1 node2) acc)
					 (unless (__rbnode-is-external node1)
					   (incf acc))
					 (setf node1 (__rbnode-next node1)))))
		  (if (= cnt node-count)
			  (format stream "OK.~%")
			  (progn
				(incf err-count)
				(format stream "NG : cnt = ~A~%" cnt)))))

	  ;; reachablity check ( last -> first )
	  (format stream "testing backward traversal integrity ... ")
	  (let ((node1 (__rbtree-begin tree))
			(node2 (__rbtree-end   tree)))
		(unless (__rbnode-is-external node1)
		  (setf node1 (__rbnode-prev node1)))
		(let ((cnt (do ((acc 0))
					   ((eq node1 node2) acc)
					 (unless (__rbnode-is-external node2)
					   (incf acc))
					 (setf node2 (__rbnode-prev node2)))))
		  (if (= cnt node-count)
			  (format stream "OK.~%")
			  (progn
				(incf err-count)
				(format stream "NG : cnt = ~A~%" cnt)))))

	  ;; check depth...
	  (format stream "testing tree depth integrity ... ")
	  (if (<= max-depth (* 2 min-depth))
		  (format stream "OK.~%")
		  (progn
			(incf err-count)
			(format stream "NG.~%")))

	  ;; check ordering...
	  (format stream "testing node order integrity ... ")
	  (if (zerop (__rbnode-check-integrity the-top stream
										   (__rbtree-cmp-func tree)
										   (lambda (x) ; temporary...
											 (declare (ignore x)) "")))
		  (format stream "OK.~%")
		  (progn
			(incf err-count)
			(format stream "NG.~%"))))
	(format stream "RESULT : integrity check has detected ~A corruption.~%" err-count)
	(zerop err-count)))

