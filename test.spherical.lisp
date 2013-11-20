
(defparameter *center* 
	(make-instance '3d.spherical:sph-pt :r 0 :theta (/ PI 2) :phi 0))
(defparameter *start* 
	(make-instance '3d.spherical:sph-pt :r 10 :theta (/ PI 2) :phi 0))

(defun make-stuff ()
	(3d:with-mesh (3d:make-mesh)
		(let* ((base (3d.spherical:make-circle *start* 60))
					 (base-c base))
			
			(dotimes (v 30)
				(multiple-value-bind (extruded new-verts new-edge-loop) (3d:extrude base-c)
					
					(3d:*add-faces-l extruded)
					(mapc (lambda (f)
									(3d.spherical:inc-theta! f (/ PI -30))) new-verts)
					(setf base-c new-edge-loop)))
			
			(dotimes (v 30)
				(multiple-value-bind (extruded new-verts new-edge-loop) (3d:extrude base)
					
					(3d:*add-faces-l extruded)
					(mapc (lambda (f)
									(3d.spherical:inc-theta! f (/ PI 30))) new-verts)
					(setf base new-edge-loop)))

			)
		
;			(print 3d:*mesh*)
		(export-scene 3d:*mesh*)
		
		nil))

(make-stuff)