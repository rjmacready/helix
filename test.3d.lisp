
(defparameter *X* #v(1 0 0))
(defparameter *Y* #v(0 1 0))
(defparameter *Z* #v(0 0 1))

(3d:with-mesh (3d:make-mesh)
	(let ((a #v())				
				(b #v(1))
				(c #v(0 1)))
		
		(3d:with-face (3d:make-face a b c)
			(3d:*add-faces 3d:*face*)

			(multiple-value-bind (faces extruded facing) (3d:extrude 3d:*face*)
				(declare (ignore extruded))
				(3d:translate! facing (3d:scale! (3d:dup *Z*) 1.5))
				(3d:*add-faces-l faces)
				(3d:*add-faces facing))

		(export-scene 3d:*mesh*)
		)))
