(defun create-grid ()
  (make-array '(9 9)))


(defun affiche-grille (grid)
  (princ "   | A B C | D E F | G H I |" ) (terpri)
  (princ "**********************************")(terpri)
  (dotimes (i (car (array-dimensions grid)))
	    (princ i))
)
