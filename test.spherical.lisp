
(defparameter *center* 
	(make-instance '3d.spherical:sph-pt :r 0 :theta (/ PI 2) :phi 0))
(defparameter *start* 
	(make-instance '3d.spherical:sph-pt :r 10 :theta (/ PI 2) :phi 0))

(defun make-circle-and-jump ()
	(let* ((new-verts (3d.spherical:make-circle *start* 60))
				 (r (cons *start* new-verts)))
		(setf *start* 
					(3d.spherical:inc-theta! 
					 (3d:dup *start*) 
					 (/ PI -8)))
		r))

(3d:with-mesh (3d:make-mesh)

	(dotimes (v 2)
		; make verts
		(let ((tmp-verts (make-circle-and-jump)))
			; make faces
			(let ((verts (append tmp-verts (list (first tmp-verts)))))
				(loop for
						 ctl in (3d:range 0 (length tmp-verts))
						 for pair on verts
						 collect 
						 (3d:*add-faces 
							(3d:make-face
							 (3d:to-v3 *center*)
							 (3d:to-v3 (first pair))
							 (3d:to-v3 (second pair))))))
			))

	
	;(let ((tmp-v (append *new-verts* (list (car *new-verts*)))))
	;	(loop 
	;		 for ctl in (3d:range 0 (length *new-verts*))
	;		 for c_v on tmp-v
	;			 collect 
	;			 (progn
	;				 (3d:*add-faces (3d:make-face 
	;												(3d:to-v3 *center*)
	;												(3d:to-v3 (first c_v))
	;												(3d:to-v3 (second c_v)))))
	;			 ))
	
	;(print 3d:*mesh*)
	(export-scene 3d:*mesh*)
	)
