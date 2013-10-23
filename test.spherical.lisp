
(defparameter *center* 
	(make-instance '3d.spherical:sph-pt :r 0 :theta (/ PI 2) :phi 0))
(defparameter *start* 
	(make-instance '3d.spherical:sph-pt :r 10 :theta (/ PI 2) :phi 0))

(defun make-circle ()
	(cons *start* (3d.spherical:make-circle *start* 60)))

(3d:with-mesh (3d:make-mesh)
   ; make verts
	(let ((tmp-verts (make-circle)))
		; trick to close the edge loop
		(let ((verts (append tmp-verts (list (first tmp-verts)))))
			; duplicate to create the upper edge loop
			(let ((upper (mapcar #'3d:dup verts)))
				(mapc (lambda (n) (3d.spherical:inc-theta! n (/ PI -30))) upper)
				
				; create faces!
				(3d:*add-faces-l
				 (loop for ctl in (3d:range 0 (length tmp-verts))
						for l_c on verts
						for u_c on upper
						collect (3d:make-face-l 
										 (list 
											(3d:to-v3 (first l_c))
											(3d:to-v3 (second l_c))
											(3d:to-v3 (second u_c))
											(3d:to-v3 (first u_c)))))
				 )
				)
			
			)
		)
	


;			(let ((verts (append tmp-verts (list (first tmp-verts)))))
;				(loop for
;						 ctl in (3d:range 0 (length tmp-verts))
;						 for pair on verts
;						 collect 
;						 (3d:*add-faces 
;							(3d:make-face
;							 (3d:to-v3 *center*)
;							 (3d:to-v3 (first pair))
;							 (3d:to-v3 (second pair))))))

	
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
