
; a 3d point
(defclass v3 ()
		((x :initarg :x)
		 (y :initarg :y)
		 (z :initarg :z)))

; face
(defclass face ()
	((vertex :initarg :vertex)))


(defun make-v3 (&key (x 0) (y 0) (z 0))
	(the number x)
	(the number y)
	(the number z)
	(make-instance 'v3 :x x :y y :z z))


(defun make-face (&rest vertex)
	(when (null vertex) 
		(error "Is empty"))
	(when (< (length vertex) 3) 
		(error "Not enough vertex"))
	(when (find-if-not (lambda (n) (typep n 'v3)) vertex)
		(error "Not a list of v3"))
	(make-instance 'face :vertex vertex))


(set-dispatch-macro-character #\# #\v (lambda (stream char ig)
																				(declare (ignore char ig))

																				(let ((frm (read stream))
																							(to-call nil))

																					(when (> (length frm) 0)
																						(setf to-call (cons :x to-call))
																						(setf to-call (cons (car frm) to-call))
																						(setf frm (cdr frm)))

																					(when (> (length frm) 0)
																						(setf to-call (cons :y to-call))
																						(setf to-call (cons (car frm) to-call))
																						(setf frm (cdr frm)))

																					(when (> (length frm) 0)
																						(setf to-call (cons :z to-call))
																						(setf to-call (cons (car frm) to-call))
																						(setf frm (cdr frm)))

																					(cons 'make-v3 (reverse to-call))

																					)))

(defgeneric scale! (v a)
	(:documentation "Scale a vector <v> by <a> (either a number or another vector)"))

(defgeneric scale! (v a)
	(:documentation "Translate a vector <v> by <a> (either a number or another vector)"))

(defmethod scale! ((this v3) (n number))
	(with-slots (x y z) this
		(setf x (* n x))
		(setf y (* n y))
		(setf z (* n z))
		this))

(defmethod scale! ((this v3) (v v3))
	(with-slots (x y z) this
		(setf x (* (slot-value v 'x) x))
		(setf y (* (slot-value v 'y) y))
		(setf z (* (slot-value v 'z) z))
		this))

(defmethod translate! ((this v3) (n number))
	(with-slots (x y z) this
		(setf x (+ x n))
		(setf y (+ y n))
		(setf z (+ z n))
		this))

(defmethod translate! ((this v3) (v v3))
	(with-slots (x y z) this
		(setf x (+ (slot-value v 'x) x))
		(setf y (+ (slot-value v 'y) y))
		(setf z (+ (slot-value v 'z) z))
		this))

(defmethod print-object ((v v3) stream)
	(with-slots (x y z) v
		(format stream "#v(~a ~a ~a)" x y z)))

(defmethod print-object ((f face) stream)
	(with-slots (vertex) f
		(format stream "#face(~{~a~^ ~})" vertex)))

(defun dup (v)
	(the v3 v)
	(with-slots (x y z) v
		(make-v3 :x x :y y :z z)))
