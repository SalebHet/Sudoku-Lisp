(defun create_grid ()
  (make-array '(9 9)))


(defun affiche-grille (grid)
  (princ "   | A B C | D E F | G H I |" ) (terpri)
  (princ "**********************************")(terpri)
   (dotimes i 1 (length grid)
	    (princ i))
)
