(defpackage :3d.spherical
	(:use :cl :3d)
	(:export
	 ; classes
	 :sph-pt
	 ; helper
;	 :to-v3
	 :dup
	 :inc-phi*
	 ; helper construction functions
	 :make-circle
	 ))

(in-package :3d.spherical)

; stuff using spherical coordinates
; angles must be in rads

(defclass sph-pt ()
	((r :initarg :r) 
	 (theta :initarg :theta)
	 (phi :initarg :phi)))


(defmethod to-v3 ((p sph-pt))
	(with-slots (r theta phi) p
;		(print `(r ,r))
;		(print `(theta ,theta ,(sin theta) ,(cos theta)))
;		(print `(phi ,phi ,(sin phi) ,(cos phi)))
;		(print 
;		 `(x ,(* r (sin theta) (cos phi)) 
;				 y ,(* r (sin theta) (sin phi)) 
;				 z ,(* r (cos theta))))
		(make-v3 
		 :x (* r (sin theta) (cos phi)) 
		 :y (* r (sin theta) (sin phi)) 
		 :z (* r (cos theta)))))

(defmethod dup ((p sph-pt))
	(with-slots (r theta phi) p
		(make-instance 'sph-pt :r r :theta theta :phi phi)))

(defun inc-phi! (i inc)
	(with-slots (phi) i
		(setf phi (+ phi inc))
		i))

(defmethod print-object ((p sph-pt) stream)
	(with-slots (r theta phi) p
		(format stream "#sph-pt(r=~a theta=~a phi=~a)" r theta phi)))

(defun make-circle (src segments)
	(unless (>= segments 3)
		(error "give at least 3 segments!"))
	(let ((inc (/ (* PI 2) segments)))
		(loop for x from 1 upto (1- segments)
				 collect (inc-phi! (dup src) (* x inc)))))

