
(defun v3-angle (v1 v2)
	"The angle between two vectors"
	(the 3d:v3 v1)
	(the 3d:v3 v2)
	(let ((answer (acos (/ (3d:dot-product v1 v2) 
												 (* (3d:magnitude v1) (3d:magnitude v2))))))
				answer))

(v3-angle #v(1) #v(0 1))
(v3-angle #v(1) #v(1 1))

(defclass plane ()
	((a :initarg :a) (b :initarg :b) (c :initarg :c) (d :initarg :d)))

(defun find-intersection (p1 p2)
		(with-slots (a b c d) p1
			(let ((lhs-1 (list a b c))
						(rhs-1 (* -1 d)))				
				(with-slots (a b c d) p2
					(let ((lhs-2 (list a b c))
								(rhs-2 (* -1 d)))						
						(let ((lhs (list lhs-1 lhs-2))
									(rhs (list rhs-1 rhs-2)))

									(let* ((a (grid:make-grid 
														 '((grid:foreign-array 3 2) double-float) 
														 :initial-contents lhs))
												 (rhs ;(grid:transpose 
															 (grid:make-grid
																'((grid:foreign-array 2) double-float) 
																:initial-contents rhs)));)


										#+nil(print `(a ,a))
										(multiple-value-bind (a) (gsll:householder-solve a rhs)
											(print 'asdasdqw)
											(print a))

										#+nil(print `(rhs ,rhs))
											#+nil(multiple-value-bind (QR tau) (gsll:QR-decomposition (gsll:copy a))
										(print a)
										(print '---)
										(print QR)
												 (multiple-value-bind (Q R) (gsll:QR-unpack QR tau)
													 (print '---)
													 (print Q)
													 (print '---)
													 (print R)
													 #+nil(gsll:QR-QRsolve Q R rhs)
													 (print '---)
													 (print (gsll:matrix-product Q R))
													 (print '---)
;													 (print (gsll:matrix-exponential a -1))
													 ))										
										)))))))

(let ((p1 (make-instance 'plane :a 3 :b 2 :c -4 :d -6))
			(p2 (make-instance 'plane :a 1 :b -3 :c -2 :d -4)))
	(find-intersection p1 p2))
