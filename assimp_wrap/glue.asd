(defsystem "glue"
  :components ((:file "glue")
							 (:file "3d")
							 (:file "spherical" :depends-on ("3d"))))