
(in-package :cl-stl)

(declaim (inline prev
				 next
				 back-inserter
				 front-inserter
				 inserter
#-cl-stl-noextra stream-writer
#-cl-stl-noextra stream-reader))


;;------------------------------------------------------------------------------
;;
;; utility implementation
;;
;;------------------------------------------------------------------------------
; input-iterator or forward-iterator
(locally (declare (optimize speed))
  (defmethod advance ((itr input-iterator) (n integer))
	(declare (type fixnum n))
	(__error-unless-non-negative-fixnum advance n)
	(with-operators
		(let ((i 0))
		  (declare (type fixnum i))
		  (for (nil (< i n) (incf i))
			++itr)))))

; bidirectional-iterator
(locally (declare (optimize speed))
  (defmethod advance ((itr bidirectional-iterator) (n integer))
	(declare (type fixnum n))
	(let ((i 0))
	  (declare (type fixnum i))
	  (with-operators
		  (if (<= 0 n)
			  (for (nil (< i n) (incf i))
				++itr)
			  (for (nil (< n i) (decf i))
				--itr))))))
		  
; randomaccess-iterator
(locally (declare (optimize speed))
  (defmethod advance ((itr randomaccess-iterator) (n integer))
	(declare (type fixnum n))
	(_+= itr n)
	nil))
		  
; input-iterator or forward-iterator or bidirectional-iterator
(locally (declare (optimize speed))
  (defmethod distance ((itr1 input-iterator) (itr2 input-iterator))
	(let ((cnt 0))
	  (declare (type fixnum cnt))
	  (with-operators
		  (for (((itr @~itr1)) (_/= itr itr2) ++itr :returns cnt)
			(incf cnt))))))

; randomaccess-iterator
(locally (declare (optimize speed))
  (defmethod distance ((itr1 randomaccess-iterator) (itr2 randomaccess-iterator))
	(_- itr2 itr1)))



;; MEMO : prev is exported only 0x11 or later...
(defun prev (itr &optional (n 1))
  ;; MEMO : itr needs support for operator_clone & advance.
  "Not yet documented."    ;; ToDo : document it...
  (let ((cpy (clone itr)))
	(advance cpy (- n))
	cpy))

;; MEMO : next is exported only 0x11 or later...
(defun next (itr &optional (n 1))
  ;; MEMO : itr needs support for operator_clone & advance.
  "Not yet documented."    ;; ToDo : document it...
  (let ((cpy (clone itr)))
	(advance cpy n)
	cpy))


;;------------------------------------------------------------------------------
;;
;; back-insert-iterator
;;
;;------------------------------------------------------------------------------
(defclass back-insert-iterator (output-iterator)
  ((target :type     :pushable-back-container
		   :initform nil
		   :initarg  :target
		   :accessor __back-ins-itr-target)))

(defmethod operator_= ((itr1 back-insert-iterator) (itr2 back-insert-iterator))
  (setf (__back-ins-itr-target itr1) (__back-ins-itr-target itr2))
  itr1)

(defmethod operator_clone ((itr back-insert-iterator))
  (make-instance 'back-insert-iterator
				 :target (__back-ins-itr-target itr)))

(defmethod (setf operator_*) (new-val (itr back-insert-iterator))
  (push-back (__back-ins-itr-target itr) new-val)
  new-val)

(defmethod operator_++ ((itr back-insert-iterator))
  itr)

(defun back-inserter (cont)
  (make-instance 'back-insert-iterator :target cont))


;;------------------------------------------------------------------------------
;;
;; front-insert-iterator
;;
;;------------------------------------------------------------------------------
(defclass front-insert-iterator (output-iterator)
  ((target :type     :pushable-front-container
		   :initform nil
		   :initarg  :target
		   :accessor __front-ins-itr-target)))

(defmethod operator_= ((itr1 front-insert-iterator) (itr2 front-insert-iterator))
  (setf (__front-ins-itr-target itr1) (__front-ins-itr-target itr2))
  itr1)

(defmethod operator_clone ((itr front-insert-iterator))
  (make-instance 'front-insert-iterator
				 :target (__front-ins-itr-target itr)))

(defmethod (setf operator_*) (new-val (itr front-insert-iterator))
  (push-front (__front-ins-itr-target itr) new-val)
  new-val)

(defmethod operator_++ ((itr front-insert-iterator))
  itr)

(defun front-inserter (cont)
  (make-instance 'front-insert-iterator :target cont))



;;------------------------------------------------------------------------------
;;
;; insert-iterator
;;
;;------------------------------------------------------------------------------
(defclass insert-iterator (output-iterator)
  ((target	 :initform nil
			 :initarg  :target
			 :accessor __insert-itr-target)
   (iterator :initform nil
			 :initarg  :iterator
			 :accessor __insert-itr-iterator)))

(defmethod operator_= ((itr1 insert-iterator) (itr2 insert-iterator))
  (setf (__insert-itr-target   itr1) (__insert-itr-target   itr2))
  (setf (__insert-itr-iterator itr1) (__insert-itr-iterator itr2))
  itr1)

(defmethod operator_clone ((itr insert-iterator))
  (make-instance 'insert-iterator
				 :target   (__insert-itr-target   itr)
				 :iterator (__insert-itr-iterator itr)))

(defmethod (setf operator_*) (new-val (itr insert-iterator))
  (let ((ret (insert (__insert-itr-target   itr)
					 (__insert-itr-iterator itr) new-val)))
	(_++ ret)
	(setf (__insert-itr-iterator itr) ret)
	new-val))

(defmethod operator_++ ((itr insert-iterator))
  itr)

(defun inserter (cont itr)
  (make-instance 'insert-iterator :target cont :iterator itr))



;;------------------------------------------------------------------------------
;;
;; reverse-bidirectional-iterator
;;
;;------------------------------------------------------------------------------
(defclass reverse-bidirectional-iterator (bidirectional-iterator)
  ((current :initform nil
			:initarg  :current
			:accessor rev-bid-itr-current)))

(define-constructor reverse-iterator ((itr bidirectional-iterator))
  (make-instance 'reverse-bidirectional-iterator :current (clone itr)))

(define-constructor reverse-iterator ((itr reverse-bidirectional-iterator))
  (clone (rev-bid-itr-current itr)))

;;------------------------------------------------------------------------------
;; methods
;;------------------------------------------------------------------------------
(defmethod operator_= ((itr1 reverse-bidirectional-iterator)
					  (itr2 reverse-bidirectional-iterator))
  (_= (rev-bid-itr-current itr1) (rev-bid-itr-current itr2))
  itr1)

(defmethod operator_clone ((itr reverse-bidirectional-iterator))
  (make-instance 'reverse-bidirectional-iterator
				 :current (clone (rev-bid-itr-current itr))))

(defmethod operator_== ((itr1 reverse-bidirectional-iterator)
				  (itr2 reverse-bidirectional-iterator))
  (_== (rev-bid-itr-current itr1) (rev-bid-itr-current itr2)))

(defmethod operator_/= ((itr1 reverse-bidirectional-iterator)
				   (itr2 reverse-bidirectional-iterator))
  (_/= (rev-bid-itr-current itr1) (rev-bid-itr-current itr2)))

(defmethod operator_* ((itr reverse-bidirectional-iterator))
  (let ((tmp (clone (rev-bid-itr-current itr))))
	(_-- tmp)
	(_* tmp)))

(defmethod (setf operator_*) (new-val (itr reverse-bidirectional-iterator))
  (let ((tmp (clone (rev-bid-itr-current itr))))
	(_-- tmp)
	(setf (_* tmp) new-val)
	new-val))

(defmethod operator_++ ((itr reverse-bidirectional-iterator))
  (setf (rev-bid-itr-current itr)
		(operator_-- (rev-bid-itr-current itr)))
  itr)

(defmethod operator_-- ((itr reverse-bidirectional-iterator))
  (setf (rev-bid-itr-current itr)
		(operator_++ (rev-bid-itr-current itr)))
  itr)

(defmethod advance ((itr reverse-bidirectional-iterator) (n integer))
  (advance (rev-bid-itr-current itr) (* n -1))
  nil)

(defmethod distance ((itr1 reverse-bidirectional-iterator)
					 (itr2 reverse-bidirectional-iterator))
  (distance (rev-bid-itr-current itr2) (rev-bid-itr-current itr1)))

(defmethod base ((rev-itr reverse-bidirectional-iterator))
  (clone (rev-bid-itr-current rev-itr)))




;;------------------------------------------------------------------------------
;;
;; reverse-randomaccess-iterator
;;
;;------------------------------------------------------------------------------
(defclass reverse-randomaccess-iterator (randomaccess-iterator)
  ((current :initform nil
			:initarg  :current
			:accessor rev-ra-itr-current)))

(define-constructor reverse-iterator ((itr randomaccess-iterator))
  (make-instance 'reverse-randomaccess-iterator :current (clone itr)))

(define-constructor reverse-iterator ((itr reverse-randomaccess-iterator))
  (clone (rev-ra-itr-current itr)))


;;------------------------------------------------------------------------------
;; methods
;;------------------------------------------------------------------------------
(defmethod operator_= ((itr1 reverse-randomaccess-iterator)
					  (itr2 reverse-randomaccess-iterator))
  (_= (rev-ra-itr-current itr1) (rev-ra-itr-current itr2))
  itr1)

(defmethod operator_clone ((itr reverse-randomaccess-iterator))
  (make-instance 'reverse-randomaccess-iterator
				 :current (clone (rev-ra-itr-current itr))))

(defmethod operator_== ((itr1 reverse-randomaccess-iterator)
				   (itr2 reverse-randomaccess-iterator))
  (_== (rev-ra-itr-current itr1) (rev-ra-itr-current itr2)))

(defmethod operator_/= ((itr1 reverse-randomaccess-iterator)
				   (itr2 reverse-randomaccess-iterator))
  (_/= (rev-ra-itr-current itr1) (rev-ra-itr-current itr2)))

(defmethod operator_* ((itr reverse-randomaccess-iterator))
  (_[] (rev-ra-itr-current itr) -1))

(defmethod (setf operator_*) (new-val (itr reverse-randomaccess-iterator))
  (setf (_[] (rev-ra-itr-current itr) -1) new-val)
  new-val)

(defmethod operator_++ ((itr reverse-randomaccess-iterator))
  (setf (rev-ra-itr-current itr)
		(operator_-- (rev-ra-itr-current itr)))
  itr)

(defmethod operator_-- ((itr reverse-randomaccess-iterator))
  (setf (rev-ra-itr-current itr)
		(operator_++ (rev-ra-itr-current itr)))
  itr)

(defmethod operator_< ((itr1 reverse-randomaccess-iterator)
				  (itr2 reverse-randomaccess-iterator))
  (_< (rev-ra-itr-current itr2) (rev-ra-itr-current itr1)))

(defmethod operator_<= ((itr1 reverse-randomaccess-iterator)
				   (itr2 reverse-randomaccess-iterator))
  (_<= (rev-ra-itr-current itr2) (rev-ra-itr-current itr1)))

(defmethod operator_> ((itr1 reverse-randomaccess-iterator)
				  (itr2 reverse-randomaccess-iterator))
  (_> (rev-ra-itr-current itr2) (rev-ra-itr-current itr1)))

(defmethod operator_>= ((itr1 reverse-randomaccess-iterator)
				   (itr2 reverse-randomaccess-iterator))
  (_>= (rev-ra-itr-current itr2) (rev-ra-itr-current itr1)))

(defmethod operator_+ ((itr reverse-randomaccess-iterator) (n integer))
  (let ((tmp (clone itr)))
	(_+= tmp n)
	tmp))

(defmethod operator_+= ((itr reverse-randomaccess-iterator) (n integer))
  (_-= (rev-ra-itr-current itr) n)
  itr)

(defmethod operator_- ((itr reverse-randomaccess-iterator) (n integer))
  (let ((tmp (clone itr)))
	(_-= tmp n)
	tmp))

(defmethod operator_- ((itr1 reverse-randomaccess-iterator)
				 (itr2 reverse-randomaccess-iterator))
  (_- (rev-ra-itr-current itr2) (rev-ra-itr-current itr1)))

(defmethod operator_-= ((itr reverse-randomaccess-iterator) (n integer))
  (_+= (rev-ra-itr-current itr) n)
  itr)

(defmethod operator_[] ((itr reverse-randomaccess-iterator) (idx integer))
  (_[] (rev-ra-itr-current itr) (1- (* -1 idx))))

(defmethod (setf operator_[]) (new-val (itr reverse-randomaccess-iterator) (idx integer))
  (setf (_[] (rev-ra-itr-current itr) (1- (* -1 idx))) new-val)
  new-val)

(defmethod advance ((itr reverse-randomaccess-iterator) (n integer))
  (advance (rev-ra-itr-current itr) (* n -1))
  nil)

(defmethod distance ((itr1 reverse-randomaccess-iterator)
					 (itr2 reverse-randomaccess-iterator))
  (distance (rev-ra-itr-current itr2) (rev-ra-itr-current itr1)))

(defmethod base ((rev-itr reverse-randomaccess-iterator))
  (clone (rev-ra-itr-current rev-itr)))




;;------------------------------------------------------------------------------
;;
;; stream-write-iterator
;;
;;------------------------------------------------------------------------------
#-cl-stl-noextra
(defclass stream-write-iterator (output-iterator)
  ((stream :initform nil
		   :initarg  :stream
		   :accessor __stream-wrt-itr-stream)))

#-cl-stl-noextra
(defmethod operator_= ((itr1 stream-write-iterator) (itr2 stream-write-iterator))
  (setf (__stream-wrt-itr-stream itr1) (__stream-wrt-itr-stream itr2))
  itr1)

#-cl-stl-noextra
(defmethod operator_clone ((itr stream-write-iterator))
  (make-instance 'stream-write-iterator
				 :stream (__stream-wrt-itr-stream itr)))

#-cl-stl-noextra
(defmethod (setf operator_*) (new-val (itr stream-write-iterator))
  (format (__stream-wrt-itr-stream itr) "~A" new-val)
  new-val)

#-cl-stl-noextra
(defmethod operator_++ ((itr stream-write-iterator))
  (format (slot-value itr 'stream) "~%")
  itr)

#-cl-stl-noextra
(defun stream-writer (stream)
  (make-instance 'stream-write-iterator :stream stream))

;;------------------------------------------------------------------------------
;;
;; stream-read-iterator
;;
;;------------------------------------------------------------------------------
#-cl-stl-noextra
(defclass stream-read-iterator (input-iterator)
  (m-stream
   m-linenum
   m-linebuf))

#-cl-stl-noextra
(defmethod operator_= ((itr1 stream-read-iterator) (itr2 stream-read-iterator))
  (setf (slot-value itr1 'm-stream)  (slot-value itr2 'm-stream))
  (setf (slot-value itr1 'm-linenum) (slot-value itr2 'm-linenum))
  (setf (slot-value itr1 'm-linebuf) (slot-value itr2 'm-linebuf))
  itr1)

#-cl-stl-noextra
(defmethod operator_clone ((itr stream-read-iterator))
  (let ((oitr (make-instance 'stream-read-iterator)))
	(_= oitr itr)
	oitr))

#-cl-stl-noextra
(defmethod operator_== ((itr1 stream-read-iterator) (itr2 stream-read-iterator))
	(if (and (eq (slot-value itr1 'm-linebuf) :eof)
			 (eq (slot-value itr2 'm-linebuf) :eof))
		t
		(if (= (slot-value itr1 'm-linenum)
			   (slot-value itr2 'm-linenum))
			t
			nil)))

#-cl-stl-noextra
(defmethod operator_/= ((itr1 stream-read-iterator) (itr2 stream-read-iterator))
	(not (_== itr1 itr2)))

#-cl-stl-noextra
(defmethod operator_* ((itr stream-read-iterator))
  (slot-value itr 'm-linebuf))

#-cl-stl-noextra
(defmethod operator_++ ((itr stream-read-iterator))
  (with-slots (m-stream m-linenum m-linebuf) itr
	(unless (eq m-linebuf :eof)
	  (setf m-linebuf (read-line m-stream nil :eof))
	  (cl:incf m-linenum)))
  itr)

#-cl-stl-noextra
(defun stream-reader (&optional (stream nil))
  (let ((linenum -1)
		(linebuf :eof)
		(itr (make-instance 'stream-read-iterator)))
	(when stream
	  (setf linebuf (read-line stream nil :eof))
	  (cl:incf linenum))
	(setf (slot-value itr 'm-stream)  stream)
	(setf (slot-value itr 'm-linenum) linenum)
	(setf (slot-value itr 'm-linebuf) linebuf)
	itr))

;;------------------------------------------------------------------------------
;;
;; with-* macros
;;
;;------------------------------------------------------------------------------
#-cl-stl-noextra
(defmacro with-sequence ((itr1 itr2) container &rest body)
  (let ((g-cont (gensym)))
	`(let* ((,g-cont ,container)
			(,itr1 (begin ,g-cont))
			(,itr2 (end   ,g-cont)))
	   ,@body)))

#-cl-stl-noextra
(defmacro with-stream-reader ((itr1 itr2) stream &rest body)
  `(let ((,itr1 (stream-reader ,stream))
		 (,itr2 (stream-reader     nil)))
	 ,@body))

#-cl-stl-noextra
(defmacro with-stream-writer (itr stream &rest body)
  `(let ((,itr (stream-writer ,stream)))
	 ,@body))

#-cl-stl-noextra
(defmacro with-file-reader ((itr1 itr2) file-name &rest body)
  (let ((stream (gensym)))
	`(with-open-file (,stream ,file-name :direction :input)
	   (let ((,itr1 (stream-reader ,stream))
			 (,itr2 (stream-reader     nil)))
		 ,@body))))

#-cl-stl-noextra
(defmacro with-file-writer (itr file-name &rest body)
  (let ((stream (gensym)))
	`(with-open-file (,stream ,file-name :direction :output :if-exists :supersede)
	   (let ((,itr (stream-writer ,stream)))
		 ,@body))))

#-cl-stl-noextra
(defmacro with-buffer-reader ((itr1 itr2) buffer &rest body)
  (let ((stream (gensym)))
	`(with-input-from-string (,stream ,buffer)
	   (let ((,itr1 (stream-reader ,stream))
			 (,itr2 (stream-reader     nil)))
		 ,@body))))

#-cl-stl-noextra
(defmacro with-buffer-writer (itr &rest body)
  (let ((stream (gensym)))
	`(with-output-to-string (,stream)
	   (let ((,itr (stream-writer ,stream)))
		 ,@body))))



