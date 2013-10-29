(defpackage :3d.space
	(:use :cl))

(defmacro with-space ((base transforms) &body body)	
	`(print (list ',base ',transforms ',body)))

(defclass space () ())

(defclass canonical-space (space)
	())


(defclass transform () 
	())

(defclass rotate (transform)
	())

(defclass translate (transform)
	())

(defclass scale (transform)
	())
