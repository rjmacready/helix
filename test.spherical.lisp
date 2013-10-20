
(defparameter *center* (make-instance '3d.spherical:sph-pt :r 0 :theta (/ PI 2) :phi 0))
(defparameter *start* (make-instance '3d.spherical:sph-pt :r 10 :theta (/ PI 2) :phi 0))

(defparameter *new-verts* (3d.spherical:make-circle *start* 60))

(setf *new-verts* (cons *start* *new-verts*))

;(print *new-verts*)
;(print (mapcar #'3d:to-v3 *new-verts*))
;(3d:to-v3 (car *new-verts*))

(3d:with-mesh (3d:make-mesh)
	
	(let ((tmp-v (append *new-verts* (list (car *new-verts*)))))
		(loop 
			 for ctl in (3d:range 0 (length *new-verts*))
			 for c_v on tmp-v
				 collect 
				 (progn
					 (3d:*add-faces (3d:make-face 
													(3d:to-v3 *center*)
													(3d:to-v3 (first c_v))
													(3d:to-v3 (second c_v)))))
				 ))
	
	;(print 3d:*mesh*)
	(export-scene 3d:*mesh*)
	)
