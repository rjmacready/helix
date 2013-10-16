
(load "3d.lisp")

(defparameter *v1* #v())
(defparameter *v2* (translate! (dup *v1*) #v(1)))
(defparameter *v3* (translate! (dup *v1*) #v(0 1)))

(print *v1*)
(print *v2*)
(print *v3*)

(defparameter *e1* (make-edge *v1* *v2*))
(defparameter *e2* (make-edge *v2* *v3*))
(defparameter *e3* (make-edge *v3* *v1*))

(print *e1*)
(print *e2*)
(print *e3*)

(defparameter *face* (make-face *v1* *v2* *v3*))

(print *face*)

