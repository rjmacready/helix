
(3d:with-mesh (3d:make-mesh)
	(let ((a #v())				
				(b #v(1))				
				(c #v(1 1))
				(d #v(0 1)))
		
		(3d:with-face (3d:make-face a b c d)
			(3d:*add-faces 3d:*face*))
		
		

		
		(print 3d:*mesh*)
		))

