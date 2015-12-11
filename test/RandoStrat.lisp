(defconstant +my-grid+ #2A((0 2 8 0 0 0 0 6 0)
			   (0 0 0 0 0 8 5 0 4)
			   (0 3 9 0 6 0 7 0 0)
			   (3 0 0 2 0 1 0 0 0)
			   (0 0 5 0 0 0 4 0 0)
			   (0 0 0 5 0 3 0 0 6)
			   (0 0 7 0 2 0 3 5 0)
			   (2 0 3 9 0 0 0 0 0)
			   (0 9 0 0 0 0 2 4 0)))

(defun create-grid ()
  (make-array '(9 9)))

(defun create-grid-full ()
  (make-array '(9 9) :initial-element 9))

(defun copy-grid (grid)
  (let ((grid2 (make-array (array-dimensions grid))))
    (dotimes (i (array-dimension grid 0))
      (dotimes (j (array-dimension grid 1))
        (setf (aref grid2 i j) (aref grid i j))))
    (return-from copy-grid grid2)))        

(defun show-grid (grid grid2)
  (format t "   | A B C | D E F | G H I |~%" )
  (dotimes (i (array-dimension grid 0));(car (array-dimensions grid)))
    (progn
      (if (= (mod i 3) 0)
	  (format t "****************************~%"))
      (format t " ~d " (+ 1 i))
      (dotimes (j (array-dimension grid 1))
	(progn
	  (if (= (mod j 3) 0)
	      (format t "| "))
	  (if (zerop (aref grid i j))
	      (format t "  ")
	      (if (zerop (aref grid2 i j))
		(format t "~d " (aref grid i j))
		(format t "~c[31m~d~c[0m " #\ESC (aref grid i j) #\ESC)))))
      (format t "|~%")))
  (format t "****************************~%"))

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

(defun game-over (grid)
  (dotimes (i 9)
    (dotimes (j 9)
      (if (= 0 (aref grid j i))
	  (return-from game-over NIL))))
  (return-from game-over T))

(defun sudoku (grid)
  (let ((grid2 (copy-grid grid)))
    (loop while (not (game-over grid2)) do
       (show-grid grid2 grid)
       (format t " Ajouter un nombre : Colonne Ligne Nombre ~%")
       (if (not (add-number grid2 grid
			    (cdr (assoc (read) '((A . 0) (B . 1) (C . 2) (D . 3) (E . 4) (F . 5) (G . 6) (H . 7) (I . 8))))
			    (- (read) 1) (read)))
	   (format t " Position non valide !~%")))))



(defclass coup ()
  ((grid :initarg grid)))

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
	     
  
