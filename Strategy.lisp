;(defconstant +my-grid+ #2A((0 2 8 0 0 0 0 6 0)
;			   (0 0 0 0 0 8 5 0 4)
;			   (0 3 9 0 6 0 7 0 0)
;			   (3 0 0 2 0 1 0 0 0)
;			   (0 0 5 0 0 0 4 0 0)
;			   (0 0 0 5 0 3 0 0 6)
;			   (0 0 7 0 2 0 3 5 0)
;			   (2 0 3 9 0 0 0 0 0)
;			   (0 9 0 0 0 0 2 4 0)))

(defconstant +my-grid+ #2A((1 0 0 0 0 4 0 0 5)
			   (0 0 0 9 5 0 0 8 0)
			   (0 0 0 0 0 3 0 9 0)
			   (0 0 5 0 0 2 0 0 4)
			   (0 0 1 0 6 0 7 0 0)
			   (7 0 0 3 0 0 2 0 0)
			   (0 6 0 5 0 0 0 0 0)
			   (0 8 0 0 1 6 0 0 0)
			   (5 0 0 2 0 0 0 0 7)))

(defparameter *s2gsolved* ())
(defparameter *s2gpossibles* ())

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
  (dotimes (i (array-dimension grid 0))
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
  (setf *s2gsolved* (copy-grid grid))
  (setf *s2gpossibles* (make-array '(9 9 10) :initial-element T))
  (dotimes (x 9)
    (dotimes (y 9)
      (setf (aref *s2gpossibles* x y 0) 9))))

(defun main-standalone ()
  (dotimes (x 9)
    (dotimes (y 9)
      (if (zerop (aref *s2gsolved* y x))
        (progn
          (loop for i from 0 to 8 if (/= i x) do
            (if (check x y i y)
              (return-from main-standalone "BLUBLUBLUBBBB1")))
          (loop for j from 0 to 8 if (/= j y) do
	    (if (check x y x j)
              (return-from main-standalone "BLUBLUBLUBBBB2")))
	  (let ((l (list-block x y)))
	    (loop for b in l do
              (if (not (eq (cons x y) l))
	        (if (check x y (car b) (cdr b))
                  (return-from main-standalone "BLUBLUBLUBBBB3")))))
          (loop for i from 1 to 9 do (progn
            (if (only-in-line x y i)(progn
	      (setf (aref *s2gsolved* y x) i)
              (return-from main-standalone "BLUBLUBLUBBBB4")))
            (if (only-in-column x y i)(progn
	      (setf (aref *s2gsolved* y x) i)
              (return-from main-standalone "BLUBLUBLUBBBB5")))
            (if (only-in-block x y i)(progn
	      (setf (aref *s2gsolved* y x) i)
              (return-from main-standalone "BLUBLUBLUBBBB6"))))))))))

(defun check (x y i j);x,y is not resolved
  (if (not (zerop (aref *s2gsolved* j i)))
    (if (aref *s2gpossibles* x y (aref *s2gsolved* j i))
      (progn
        (setf (aref *s2gpossibles* x y (aref *s2gsolved* j i)) nil)
        (setf (aref *s2gpossibles* x y 0) (- (aref *s2gpossibles* x y 0) 1))
        (if (= (aref *s2gpossibles* x y 0) 1)
	  (progn
            (setf (aref *s2gsolved* y x) (only-possibility *s2gpossibles* x y))
            (return-from check "BLUBLUB"))))))
  nil);should return
  ;(if equal teste si les meme -> supprimer possibilites lignes/colonnes/block
  
;(defun list-block (x y l)
;  (if (zerop (mod (+ 1 x) 3))
;    (if (zerop (mod (+ 1 y) 3))
;      (cons (cons x y) l)
;      (cons (cons x y) (list-block (- x 2) (+ 1 y) l)))
;    (cons (cons x y) (list-block (+ 1 x) y l)))) 

(defun list-block-t (x y l)
  (if (zerop (mod (+ 1 x) 3))
    (if (zerop (mod (+ 1 y) 3))
      (cons (cons x y) l)
      (cons (cons x y) (list-block-t (- x 2) (+ 1 y) l)))
    (cons (cons x y) (list-block-t (+ 1 x) y l))))

(defun list-block (x y)
  (list-block-t (* 3 (floor x 3)) (* 3 (floor y 3)) '()))

;(defun possibilities-number (p x y)
;  (let ((c 0))
;    (dotimes (i 9)
;      (if (eq (aref p x y i) T)
;        (setf c (+ c 1))))));;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;RETURN C

(defun only-possibility (p x y)
  (loop for i from 1 to 9 do
    (if (aref p x y i)
      (return-from only-possibility i))))

(defun only-in-line (x y p)
  (loop for i from 0 to 8 do
    (if (= (aref *s2gsolved* y i) p)
      (return-from only-in-line nil)
      (if (aref *s2gpossibles* i y p)
        (if (/= x i)
          (return-from only-in-line nil)))))
  T)

(defun only-in-column (x y p)
  (loop for j from 0 to 8 do
    (if (= (aref *s2gsolved* j x) p)
      (return-from only-in-column nil)
      (if (aref *s2gpossibles* x j p)
        (if (/= j y)
          (return-from only-in-column nil)))))
  T)

(defun only-in-block (x y p)
  (let ((l (list-block x y)))
    (loop for b in l do
      (if (not (eq (cons x y) l))
        (if (= (aref *s2gsolved* (cdr b) (car b)) p)
          (return-from only-in-block nil)
          (if (zerop (aref *s2gsolved* (cdr b) (car b)))
            (if (aref *s2gpossibles* (car b) (cdr b) p)
              (return-from only-in-block nil)))))))
  T)

(defun resolve(grid)
  (init-standalone grid)
  ;(loop while (not (game-over grid)) do (progn
  (dotimes (i 200)
       (main-standalone)))
       ;(show-grid *s2gsolved* grid))))
;(sudoku +my-grid+)

;(format t "~a~%" (main-standalone))

(resolve +my-grid+)
;(show-grid *s2gsolved* +my-grid+)
