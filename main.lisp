
(load "3d.lisp")

(defparameter *v1* #v())
(defparameter *v2* (translate! (dup *v1*) #v(1)))
(defparameter *v3* (translate! (dup *v1*) #v(0 1)))

(defparameter *z* #v(0 0 1))

(print *v1*)
(print *v2*)
(print *v3*)

(defparameter *face* (make-face *v1* *v2* *v3*))

(print *face*)

(defparameter *new-faces* (extrude *face*))

(print *new-faces*)

(translate! (car *new-faces*) (scale! (dup *z*) 2))

(print (cons *face* *new-faces*))
