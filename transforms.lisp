;(defpackage tranforms)

; each axis a separate type so
; we can use types to distinguish between 
; x/y/z axis. Only export the constants!

(defclass axis () 
	((canonical :initarg :canonical)))

(defclass z-axis (axis) 
	((canonical :initform #v(0 0 1))))

(defclass y-axis (axis) 
	((canonical :initform #v(0 1))))

(defclass x-axis (axis) 
	((canonical :initform #v(1))))

(defconstant z-a (make-instance 'z-axis))
(defconstant y-a (make-instance 'y-axis))
(defconstant x-a (make-instance 'x-axis))

(defgeneric rotate (a rot v)
	(:documentation ""))

(defmethod rotate ((a z-axis) (rot number) (v 3d:v3))
	(let ((x (3d:v3-x v))
				(y (3d:v3-y v))
				(z (3d:v3-z v)))
		(let ((x1 (- (* x (cos rot)) (* y (sin rot))))
					(y1 (+ (* x (sin rot)) (* y (cos rot)))))
			#v(x1 y1 z))))

(rotate z-a PI #v(10 10 10))

; to define a separate coordinate system we only need 
; to define a set of translations, rotations and scales to get there.
; we should curry all transforming functions so that they 
; only take one parameter - the point
