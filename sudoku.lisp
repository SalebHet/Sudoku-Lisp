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

(defun init-standalone (grid)
  (defparameter *s2gsolved* (copy-grid grid))
  (defparameter *s2gpossibles* (make-array '(9 9 10) :initial-element T))
  (dotimes (x 9)
    (dotimes (y 9)
      (setf (aref *s2gpossibles* x y 0) 9))))

(defun main-standalone ()
  (dotimes (x 9)
    (dotimes (y 9)
      (if (zerop (aref *s2gsolved* x y))
        (progn
          (loop for i from x to 9 do
            (check x y i y))
          (loop for j from y to 9 do
            (check x y x j))
          (loop for b in (list-block *s2gpossibles* x y) do;FAIL, check too much
            (format t "")))))))

(defun check (x y i j);x,y is not resolved
  (if (aref *s2gpossibles* x y (aref *s2gsolved* i j))
    (progn
      (setf (aref *s2gpossibles* x y (aref *s2gsolved* i j)) nil)
      (setf (aref *s2gpossibles* x y 0) (- (aref *s2gpossibles* x y 0) 1));????????????
      (if (= (aref *s2gpossibles* x y 0) 1)
        (setf (aref *s2gsolved* x y) (only-possibility *s2gpossibles* x y))))));should return
  ;(if equal teste si les meme -> supprimer possibilites lignes/colonnes/block
  

(defun list-block (a x y)
  (let ((l ()))
    (dotimes (i 3)
      (dotimes (j 3)
        (cons (aref a (+ (* 3 (floor y 3)) i) (+ (* 3 (floor x 3)) j)) 
'l)))));;;;;;;;;;;;;;;;;;;;;;;;RETURN

(defun possibilities-number (p x y)
  (let ((c 0))
    (dotimes (i 9)
      (if (eq (aref p x y i) T)
        (setf c (+ c 1))))));;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;RETURN C

(defun only-possibility (p x y)
  (loop for i from 1 to 9 do ;OK??????
    (if (aref p x y i)
      (return-from only-possibility i))))

;(sudoku +my-grid+)
(init-standalone +my-grid+)
;(main-standalone)
