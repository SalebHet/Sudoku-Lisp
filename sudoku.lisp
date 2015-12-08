(defun create-grid ()
  (make-array '(9 9)))

(defun create-grid-full ()
  (make-array '(9 9) :initial-element 9))




(defun show-grid (grid)
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

(defun add-number (grid x y numb)
  (if (valid-position grid x y numb)
      (setf (aref grid y x) numb)))

(defun valid-position (grid x y numb)
  (if (/= (aref grid y x) 0)
      (return-from valid-position NIL))
  (dotimes (i 9) (if (= numb (aref grid y i))
		     (return-from valid-position NIL)))
  (dotimes (i 9) (if (= numb (aref grid i x))
		     (return-from valid-position NIL)))
  (dotimes (i 3)
    (dotimes (j 3)
      (if (= numb (aref grid (+ (* 3 (multiple-value-bind (q r) (floor y 3) q)) i) (+ (* 3 (multiple-value-bind (q r) (floor x 3) q)) j)))
	  (return-from valid-position NIL))))
  T)

(defun game-over (grid)
  (dotimes (i 9)
    (dotimes (j 9)
      (if (= 0 (aref grid j i))
	  (return-from game-over NIL))))
  (return-from game-over T))
