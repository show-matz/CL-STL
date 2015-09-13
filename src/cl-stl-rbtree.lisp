(in-package :cl-stl)

;;------------------------------------------------------------------------------
;;
;; class difinition
;;
;;------------------------------------------------------------------------------
(defstruct (rbtree (:conc-name __rbtree-))
  (cmp-func nil)              ; functor object or function.
  (cur-size 0             :type fixnum)
  (top-node (make-rbnode) :type rbnode))

;;------------------------------------------------------------------------------
;;
;; rbtree utilities
;;
;;------------------------------------------------------------------------------
(defun __rbtree-create (cmpfunc)
  ;; MEMO : cmpfunc not copied here
  (let ((top-node  (make-rbnode))
		(left-node (make-rbnode)))
	(setf (__rbnode-parent top-node) top-node)
	(setf (__rbnode-left   top-node) left-node)
	(setf (__rbnode-right  top-node) nil)
	(setf (__rbnode-rank   top-node) 0)
	(setf (__rbnode-parent left-node) top-node)
	(make-rbtree :cur-size 0 :top-node top-node :cmp-func cmpfunc)))

(defun __rbtree-single-rotate (node)
  (let* ((parent1 (__rbnode-parent node))
		 (parent2 (__rbnode-parent parent1))
		 (parent3 (__rbnode-parent parent2)))
	(if (__rbnode-is-left parent2)
		(setf (__rbnode-left  parent3) parent1)
		(setf (__rbnode-right parent3) parent1))
	(if (__rbnode-is-left node)
		(progn
		  (setf (__rbnode-left parent2) (__rbnode-right parent1))
		  (setf (__rbnode-parent (__rbnode-left parent2)) parent2)
		  (setf (__rbnode-right parent1) parent2)
		  (setf (__rbnode-parent parent2) parent1))
		(progn
		  (setf (__rbnode-right parent2) (__rbnode-left parent1))
		  (setf (__rbnode-parent (__rbnode-right parent2)) parent2)
		  (setf (__rbnode-left parent1) parent2)
		  (setf (__rbnode-parent parent2) parent1)))
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
				(__rbnode-incf-rank pp)
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
					  (__rbnode-incf-rank brother)
					  (__rbnode-decf-rank parent))
					(if (__rbnode-is-red brother-left)
						(progn
						  (__rbtree-double-rotate brother-left)
						  (__rbnode-incf-rank brother-left)
						  (__rbnode-decf-rank parent))
						(progn
						  (__rbnode-decf-rank parent)
						  (when (not parent-was-red)
							(__rbtree-rebalance-on-delete parent)))))))))))

(defun __rbtree-erase-tree (node)
  (let ((left-node  (__rbnode-left  node))
		(right-node (__rbnode-right node)))
	(when left-node
	  (__rbtree-erase-tree left-node)
	  (setf (__rbnode-left node) nil))
	(when right-node
	  (__rbtree-erase-tree right-node)
	  (setf (__rbnode-right node) nil))))

(defun __rbtree-get-first (tree)
  (let ((node (__rbnode-left (__rbtree-top-node tree))))
	(if (zerop (__rbtree-cur-size tree))
		node
		(__rbnode-min node))))

(defun __rbtree-get-end (tree)
  (let ((node (__rbnode-left (__rbtree-top-node tree))))
	(if (zerop (__rbtree-cur-size tree))
		node
		(__rbnode-max node))))

(defun __rbtree-clear (tree)
  (let ((top-node  (__rbtree-top-node tree))
		(left-node (make-rbnode)))
	(__rbtree-erase-tree top-node)
	(setf (__rbnode-parent top-node)  top-node)
	(setf (__rbnode-right  top-node)  nil)
	(setf (__rbnode-rank   top-node)  0)
	(setf (__rbnode-left   top-node)  left-node)
	(setf (__rbnode-parent left-node) top-node)
	(setf (__rbtree-cur-size tree) 0)))

(defun __rbtree-lower-bound (tree val)
  (let ((ret nil)
		(cmp (functor-function (__rbtree-cmp-func tree))))
	; val より小さくない最小のノードを検索
	(labels ((imp (node)
			   (if (__rbnode-is-external node)
				   node
				   (if (funcall cmp (__rbnode-value node) val)
					   (imp (__rbnode-right node))
					   (imp (__rbnode-left (setf ret node)))))))
	  (let ((node (imp (__rbnode-left (__rbtree-top-node tree)))))
		(if (null ret) node ret)))))

(defun __rbtree-upper-bound (tree val)
  (let ((ret nil)
		(cmp (functor-function (__rbtree-cmp-func tree))))
	; val より大きな最小のノードを検索
	(labels ((imp (node)
			   (if (__rbnode-is-external node)
				   node
				   (if (funcall cmp val (__rbnode-value node))
					   (imp (__rbnode-left (setf ret node)))
					   (imp (__rbnode-right node))))))
	  (let ((node (imp (__rbnode-left (__rbtree-top-node tree)))))
		(if (null ret) node ret)))))

(defun __rbtree-count (tree val)
  (do ((acc 0)
	   (node1 (__rbtree-lower-bound tree val))
	   (node2 (__rbtree-upper-bound tree val)))
	  ((eq node1 node2) acc)
	(incf acc)
	(setf node1 (__rbnode-next node1))))

(defun __rbtree-find (tree val)
  (let ((cmp (functor-function (__rbtree-cmp-func tree))))
	(labels ((imp (node)
			   (if (__rbnode-is-external node)
				   node
				   (if (funcall cmp val (__rbnode-value node))
					   (imp (__rbnode-left node))
					   (if (funcall cmp (__rbnode-value node) val)
						   (imp (__rbnode-right node))
						   node)))))
	  (imp (__rbnode-left (__rbtree-top-node tree))))))

(defun __rbtree-add-external-nodes (node)
  (let ((left  (make-rbnode))
		(right (make-rbnode)))
	(setf (__rbnode-left   node)  left)
	(setf (__rbnode-right  node)  right)
	(setf (__rbnode-parent left)  node)
	(setf (__rbnode-parent right) node)))


(labels ((add-node (tree node val need-copy)
		   (if need-copy                       ; 指定されたノードに値を設定
			   (_= (__rbnode-value node) val)
			   (setf (__rbnode-value node) val))
		   (setf (__rbnode-rank  node)      1) ; ランクとして1を設定
		   (__rbtree-add-external-nodes  node) ; 配下に外部ノードを追加
		   (__rbtree-rebalance-on-insert node) ; バランシング処理
		   (incf (__rbtree-cur-size tree))     ; サイズキャッシュをインクリメント
		   node))

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
	;; 要素の重複が許可されるかどうかで分岐
	(if (not allow-duplicate)
		;; 許可されない場合、findで指定値のノードを検索
		(let ((node (__rbtree-find tree val)))
		  ;; 見つかったのが外部ノードかどうか検査
		  (if (__rbnode-is-external node)
			  ;; 外部ノードの場合、そのノードに値を設定して返却
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
		  (values (add-node tree node val need-copy) t)))))


(defun __rbtree-delete-node (tree node)
  ; 対象のノードが外点でない場合のみ処理
  (unless (__rbnode-is-external node)
	(let ((left-node nil))
	  (if (not (__rbnode-has-left-node node))
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

(defun __rbtree-for-each (tree uf)
  ;MEMO : uf is always lambda function ( see stl:for ). 
  (let ((node1 (__rbtree-get-first tree))
		(node2 (__rbtree-get-end   tree)))
	(when (< 0 (__rbtree-cur-size tree))
	  (setf node2 (__rbnode-next node2)))
	(do ()
		((eq node1 node2) uf)
	  (funcall uf (__rbnode-value node1))
	  (setf node1 (__rbnode-next node1)))))




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
	  (imp (__rbnode-left (__rbtree-top-node tree)) stream 0))))

#+cl-stl-debug
(defun __rbtree-check-integrity (tree stream)
  (let ((err-count  0)
		(node-count 0)
		(cur-size (__rbtree-cur-size tree))
		(the-top  (__rbnode-left (__rbtree-top-node tree))))
	; ノード数の取得
	(setf node-count (__rbnode-size the-top))
	; 深度の取得
	(multiple-value-bind (min-depth max-depth) (__rbnode-depth the-top)
	  ; レポートを出力
	  (progn
		(format stream "  cur-size cache : ~A~%" cur-size)
		(format stream "  count of node  : ~A~%" node-count)
		(format stream "  min depth      : ~A~%" min-depth)
		(format stream "  max depth      : ~A~%" max-depth))
	  ; ノード数キャッシュの整合性チェック
	  (format stream "testing node count cache ... ")
	  (if (= node-count cur-size)
		  (format stream "OK.~%")
		  (progn
			(incf err-count)
			(format stream "NG.~%")))
	  ; 到達可能性チェック ( first -> last )
	  (format stream "testing forward traversal integrity ... ")
	  (let ((node1 (__rbtree-get-first tree))
			(node2 (__rbtree-get-end   tree)))
		(unless (__rbnode-is-external node2)
		  (setf node2 (__rbnode-next node2)))
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
	  ; 到達可能性チェック ( last -> first )
	  (format stream "testing backward traversal integrity ... ")
	  (let ((node1 (__rbtree-get-first tree))
			(node2 (__rbtree-get-end   tree)))
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
	  ; 深度チェック
	  (format stream "testing tree depth integrity ... ")
	  (if (<= max-depth (* 2 min-depth))
		  (format stream "OK.~%")
		  (progn
			(incf err-count)
			(format stream "NG.~%")))
	  ; 順序チェック
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

