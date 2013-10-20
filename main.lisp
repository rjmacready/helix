
;(load "3d.lisp")


;(defparameter *v1* #v())
;(defparameter *v2* (translate! (dup *v1*) #v(1)))
;(defparameter *v3* (translate! (dup *v1*) #v(0 1)))

(defparameter *X* #v(1 0 0))
(defparameter *Y* #v(0 1 0))
(defparameter *Z* #v(0 0 1))

;(print *v1*)
;(print *v2*)
;(print *v3*)
;(defparameter *face* (make-face *v1* *v2* *v3*))
;(print *face*)
;(defparameter *new-faces* (extrude *face*))
;(print *new-faces*)
;(translate! (car *new-faces*) (scale! (dup *z*) 2))
;(print (cons *face* *new-faces*))

;(with-v3 (make-v3 :x 1 :y 1)
;	(normalize! *v3*)
;	(print *v3*)
;	(print (magnitude1p *v3*)))

;(format T "~70$" (sqrt 2))

(3d:with-mesh (3d:make-mesh)
	(let ((a #v())				
				(b #v(1))
				(c #v(0 1)))
		
		(3d:with-face (3d:make-face a b c)
			(3d:*add-faces 3d:*face*)

			(destructuring-bind (extruded fa fb fc) (3d:extrude 3d:*face*)
				(3d:translate! extruded (3d:scale! (3d:dup *Z*) 1.5))
				(3d:*add-faces extruded fa fb fc)))

		;(print 3d:*mesh*)
		;(print 'vertices)
		;(print (3d:mesh-vertices 3d:*mesh*))
		 (export-scene 3d:*mesh*)
		;(print (3d:v3-x b))
		))
