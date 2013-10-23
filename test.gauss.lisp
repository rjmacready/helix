; create a regular mesh of square, then translate vertices to
; make a "plot" for a Gaussian function. Or whatever.

(defun gaussian (a)
	(exp (* 1/2 (3d:magnitude a))))

(3d:with-mesh (3d:make-mesh)
	(let ((a #v())
				(b #v(1 0))
				(c #v(1 1))
				(d #v(0 1)))
		
		(3d:with-face (3d:make-face a b c d)
			(3d:*add-faces 3d:*face*))

		(let ((b1 b)
					(c1 c)
					(b2 b)
					(c2 c))
			
			(dotimes (r 100)
				(setf b2 (3d:translate! (3d:dup b2) #v(1)))
				(setf c2 (3d:translate! (3d:dup c2) #v(1)))
				
				; make and add face
				(3d:*add-faces (3d:make-face b1 b2 c2 c1))
				
				(setf b1 b2)
				(setf c1 c2)
				
				))

		(mapc (lambda (v)
						(3d:translate! v #v(0 0 (gaussian v))))
					(3d:mesh-vertices 3d:*mesh*))
		
		;(print 3d:*mesh*)
		(export-scene 3d:*mesh*)
		))
