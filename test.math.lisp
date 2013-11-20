
;(make-instance 'grid:matrix 
;							 :specification '((grid:foreign-array 2 2) double-float) 
;							 :initial-contents (list 1 0 0 1))

(let* ((a (grid:make-grid 
					'((grid:foreign-array 3 3) double-float) 
					:initial-contents '((2 6 2) (-3 -8 0) (4 9 2))))
			 (rhs (grid:make-grid 
					'((grid:foreign-array 3) double-float) 
					:initial-contents '(2 2 3))))
	(multiple-value-bind (upper permutation signum)
			(gsll:LU-decomposition (gsll:copy a))
		(print upper)
		(let ((initial-solution (gsll:LU-solve upper rhs permutation T)))
			(print initial-solution)
			(print (gsll:LU-refine a upper permutation rhs initial-solution)))))

