

(defun copy-grid (grid)
  (let ((grid2 (make-array (array-dimensions grid))))
    (dotimes (i (array-dimension grid 0))
      (dotimes (j (array-dimension grid 1))
        (setf (aref grid2 i j) (aref grid i j))))
    (return-from copy-grid grid2)))        



(defun add-number (grid grid2 x y numb)
  (if (valid-position grid2 x y numb)
      (setf (aref grid y x) numb)))

(defun valid-position (grid x y numb)
  (format t "Je suis entrain de tester ~a ~a ~a ~%"x y numb)
  (if (/= (aref grid y x) 0)
      (return-from valid-position NIL))
  (dotimes (i 9) (if (= numb (aref grid y i))
		     (return-from valid-position NIL)))
  (dotimes (i 9) (if (= numb (aref grid i x))
		     (return-from valid-position NIL)))
  (dotimes (i 3)
    (dotimes (j 3)
      (if (= numb (aref grid (+ (* 3 (floor y 3)) i) (+ (* 3 (floor x 3)) j)))
	  (return-from valid-position NIL))))
  T)



(defvar *grid*)

(defun init-standalone (grid)
  (setf *grid* (copy-grid grid)))

(defun main-standalone ()
  (loop while T do
       (let ((c (random 8)) (l (random 8)) (v (random 9)))
	  (format t " Je teste un triplé de valeur ~a ~a ~a !~%" c l v)
	 (if (valid-position  *grid* c l v)
	     (progn
	        (format t " j'ai trouvé un triplet !~%")
		(add-number *grid* *grid* c l v)
		(show-grid +my-grid+ +my-grid+)
		(return-from main-standalone (values c l v)))))))
	     
  
