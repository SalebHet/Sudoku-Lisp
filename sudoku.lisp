(defun create-grid ()
  (make-array '(9 9)))


(defun affiche-grille (grid)
  (format t "   | A B C | D E F | G H I |~%" )
  (dotimes (i (array-dimension grid 0));(car (array-dimensions grid)))
    (progn
      (if (= (mod i 3) 0)
	  (format t "****************************~%"))
      (format t "   ")
      (dotimes (j (array-dimension grid 1))
	(progn
	  (if (= (mod j 3) 0)
	      (format t "| "))
	  (format t "~d " (aref grid i j))))
      (format t "|~%"))))
