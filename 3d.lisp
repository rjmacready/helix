(defpackage :3d
	(:use :cl)
	(:export
	 ; constants
	 :epsilon
	 ; special vars
	 :*selected*
	 :*v3*
	 :*face*
	 :*mesh*
	 ; helper macros
	 :with-mesh
	 :with-face
	 :with-v3
	 ; classes
	 :v3
	 :face
	 :mesh
	 ; accessors
	 :v3-x
	 :v3-y
	 :v3-z
	 :mesh-faces
	 :mesh-vertices
	 :face-vertices
	 ; helper construction functions
	 :make-v3
	 :make-face
	 :make-mesh
	 ; helper functions
	 :*add-faces
	 ; functions
	 :extrude
	 :dup
	 :translate!
	 :scale!
	 ))

(in-package :3d)

(defconstant EPSILON 0.000001 
	"Math on common lisp looks horrible ... sqrt(2) has only 7 decimal places!")

(defvar *selected* nil 
	"The 'selected' v3/face/mesh. Useful as default for the generic functions.")
(defvar *v3* nil
	"The 'selected' v3.")
(defvar *face* nil
	"The 'selected' face.")
(defvar *mesh* nil
	"The 'selected' mesh.")

(defmacro with-mesh (mesh &rest body)
	`(let* ((*mesh* ,mesh)
					(*selected* *mesh*))
		 ,@body))

(defmacro with-face (face &rest body)
	`(let* ((*face* ,face)
					(*selected* *face*))
		 ,@body))

(defmacro with-v3 (v3 &rest body)
	`(let* ((*v3* ,v3)
					(*selected* *v3*))
		 ,@body))

; on hold!
;(defmacro with-vertices (defs &rest body)
;	`(let (,@(mapcar (lambda (n)
;										 (let ((hd (car n))
;													 (tl (cdr n)))
;											 (list hd nil))) defs))
;		 ,@body))

; a 3d point
(defclass v3 ()
		((x :initarg :x :reader v3-x)
		 (y :initarg :y :reader v3-y)
		 (z :initarg :z :reader v3-z)))

; face
(defclass face ()
	((vertex :initarg :vertex :reader face-vertices)))

; a collection of faces
(defclass mesh ()
	((faces :initarg :faces :reader mesh-faces)))

(defun float= (f1 f2)
	(< (abs (- f1 f2)) EPSILON))

(defun *add-faces (&rest faces-to-add)
	(the list faces-to-add)
	(with-slots (faces) *mesh*
		(setf faces (append faces faces-to-add))))

(defun *add-faces-l (faces-to-add)
	(the list faces-to-add)
	(with-slots (faces) *mesh*
		(setf faces (append faces faces-to-add))))

(defun make-v3 (&key (x 0) (y 0) (z 0))
	(the number x)
	(the number y)
	(the number z)
	(make-instance 'v3 :x x :y y :z z))

(defun make-face (&rest vertex)
	"Make a face. Each face as a separate argument."
	(when (null vertex) 
		(error "Is empty"))
	(when (< (length vertex) 3) 
		(error "Not enough vertex"))
	(when (find-if-not (lambda (n) (typep n 'v3)) vertex)
		(error "Not a list of v3"))
	(make-instance 'face :vertex vertex))

(defun make-face-l (vertex)
	"Make a face from a list of vertices"
	(the list vertex)

	(when (null vertex) 
		(error "Is empty"))
	(when (< (length vertex) 3) 
		(error "Not enough vertex"))
	(when (find-if-not (lambda (n) (typep n 'v3)) vertex)
		(error "Not a list of v3"))
	(make-instance 'face :vertex vertex))

(defun make-mesh (&rest faces)
	"Make a mesh. Each face as a separate argument."
	(when (or (not (listp faces)) (find-if-not (lambda (n) (typep n 'face)) faces))
		(error "Not a list of face"))
	(make-instance 'mesh :faces faces))


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

(defgeneric mesh-vertices (m)
	(:documentation "All distinct vertices in a mesh."))

(defmethod mesh-vertices ((m mesh))
	"All distinct vertices in a mesh. The test for distinctness is componentwise: meaning
that if we create manually two instances of #v(), those will be the same vertice."
	(let ((ls nil))
		(with-slots (faces) m
			(loop for f in faces do
					 (with-slots (vertex) f
						 (loop for v in vertex do 
									(when (not (member v ls :test #'equal-v3))
										(setf ls (append ls (list v)))))))
			ls)))

(defgeneric scale! (v a)
	(:documentation "Scale a vector <v> by <a> (either a number or another vector)"))

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

(defun equal-v3 (a b)
	(the v3 a)
	(the v3 b)
	(and (equal (slot-value a 'x) (slot-value b 'x))
			 (equal (slot-value a 'y) (slot-value b 'y))
			 (equal (slot-value a 'z) (slot-value b 'z))))

(defgeneric translate! (v a)
	(:documentation "Translate a vector <v> by <a> (either a number or another vector)"))

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

(defmethod translate! ((this face) (v v3))
	(with-slots (vertex) this
		(mapc (lambda (vert) (translate! vert v)) vertex)))

(defmethod print-object ((v v3) stream)
	(with-slots (x y z) v
		(format stream "#v(~a ~a ~a)" x y z)))

(defmethod print-object ((f face) stream)
	(with-slots (vertex) f
		(format stream "#face(~{~a~^ ~})" vertex)))

(defmethod print-object ((m mesh) stream)
	(with-slots (faces) m
		(format stream "#mesh(~{~a~^ ~})" faces)))

(defun dot-product (v1 v2)
	(the v3 v1)
	(the v3 v2)
	(with-slots (x y z) v1
		(let ((x2 (slot-value v2 'x))
					(y2 (slot-value v2 'y))
					(z2 (slot-value v2 'z)))
			(+ (* x x2) (* y y2) (* z z2)))))

(defun cross-product (v1 v2)
	(the v3 v1)
	(the v3 v2)
	(with-slots (x y z) v1
		(let ((x2 (slot-value v2 'x))
					(y2 (slot-value v2 'y))
					(z2 (slot-value v2 'z)))
			(make-v3 
			 :x (- (* y z2) (* z y2))
			 :y (- (* z x2) (* x z2)) 
			 :z (- (* x y2) (* y x2))))))

(defun magnitude (v)
	(the v3 v)
	(sqrt (dot-product v v)))

(defun magnitude1p (p)
	(the v3 p)
	(float= 1 (magnitude p)))

(defgeneric dup (s)
	(:documentation "Duplicate some <s>. No references reused."))

(defun *dup ()
	"Duplicate the selected object"
	(dup *selected*))

(defmethod dup ((v v3))
	(with-slots (x y z) v
		(make-v3 :x x :y y :z z)))

(defmethod dup ((f face))
	(with-slots (vertex) f
		(make-face-l (mapcar #'dup vertex))))

; create unit vector from ... a vector
(defun normalize! (vec)
	(the v3 vec)
	(scale! vec (/ 1 (magnitude vec))))

; TODO 
; extrude a face, with the "dupped" vertices connected. 
; this will be akin to extruding a face on blender.
; return a list with:
; - created opposing face
; + a face for each pair of vertices
(defmethod extrude ((original face))
	(let ((opposed (dup original)))
		; TODO - rotate opposed so that it points outwards.
		; TODO - or flip normal. maybe we need to attach normals to faces.
		(let ((v-or (slot-value original 'vertex))
					(v-op (slot-value opposed 'vertex)))

			; 0-1
			; 1-2
			; ...
			; n-0
			(let ((v-or-2 (append v-or (list (car v-or))))
						(v-op-2 (append v-op (list (car v-op)))))

				(cons 
				 opposed
				 (loop 
						for ctrl in (range 0 (length v-or))
						for x on v-or-2
						for y on v-op-2

							; FIXME
							; swap the order of the y-face vertices.
							; maybe will be dropped after we flip the face?
						collect (make-face-l (list (first x) (second x) (second y) (first y)))
							))))))

; TODO
; from an origin and a "walker", make a regular polygon.
; for maximum flexibility, wither the number of sides (ie, 4) 
; or the inner angle (ie, Pi/2) can be given
; the "walker" must be a unit vector that must be used to calculate
; the next vertex. a unit vector of (0 1 0) (y axis) to create a square from (0 0 0)
; will return the following vertex: (0 0 0) (0 1 0) (-1 1 0) (0 -1 0)
; as the "walker" should move counter-clockwise, 
; it will turn -Pi/2 degrees (or 3Pi/2 degrees) for a square
(defun regular-polygon ()
	nil)


; TODO 
; take a mathematical function, 
; and sample it on a set of values
(defun sample-fun (the-fun inputs)
	(the function the-fun)
	(loop for x in inputs collect (funcall the-fun x)))

; TODO
; look out for imprecisions on floats.
; ideally, we need to calculate the number of items, and then calculate them,
; and not accumulating them increasily.
(defun range (start exclusive-end &optional (step 1))
	(if (>= start exclusive-end)
			nil
			(cons start (range (+ start step) exclusive-end step))))

; TODO
; given two faces, get a series of transformations (translate, scale, rotate) so
; they can "fit". its possible that there's no solution.
(defun merge-faces ()
	nil)
