(defpackage :3d.spherical
	(:use :cl :3d)
	(:export
	 ; classes
	 :sph-pt
	 ; helper
;	 :to-v3
	 :dup
	 :inc-phi!
	 :inc-theta!
	 ; helper construction functions
	 :make-circle
	 ))

(in-package :3d.spherical)

; stuff using spherical coordinates
; angles must be in rads

(defclass sph-pt (3d:pt)
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

(defun inc-theta! (i inc)
	(with-slots (theta) i
		(setf theta (+ theta inc))
		i))

(defmethod print-object ((p sph-pt) stream)
	(with-slots (r theta phi) p
		(format stream "#sph-pt(r=~a theta=~a phi=~a)" r theta phi)))

; it should return a collection of edges.
; this collection should be "closed" (as in, the last 
; generated vertice should connect with <src>)
(defun make-circle (src segments)
	(unless (>= segments 3)
		(error "give at least 3 segments!"))

	(let* ((inc (/ (* PI 2) segments))
				 (generated (loop for x from 1 upto (1- segments)
											 collect (inc-phi! (dup src) (* x inc))))
				 (edges (loop for sublist-gen on generated
									 collect (make-instance 
														'3d:edge 
														:src (first sublist-gen) 
														:dest (let ((sec (second sublist-gen)))
																		(if (null sec)
																				src
																				sec)))
										 )))
		
		(make-instance '3d:edge-loop :ls edges)))
