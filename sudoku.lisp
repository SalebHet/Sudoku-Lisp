(defconstant +my-grid+ #2A((0 2 8 0 0 0 0 6 0)
			   (0 0 0 0 0 8 5 0 4)
			   (0 3 9 0 6 0 7 0 0)
			   (3 0 0 2 0 1 0 0 0)
			   (0 0 5 0 0 0 4 0 0)
			   (0 0 0 5 0 3 0 0 6)
			   (0 0 7 0 2 0 3 5 0)
			   (2 0 3 9 0 0 0 0 0)
			   (0 9 0 0 0 0 2 4 0)))

(defconstant +my-grid2+ #2A((0 2 8 0 0 0 0 6 0)
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

(defun game ()
  ;(defconstant +my-grid2+ (copy-tree +my-grid+))
  ;(setq *grid2* (make-array '(9 9)))
  (loop while (not (game-over +my-grid2+)) do
       (show-grid +my-grid2+ +my-grid+)
       (format t " Ajouter un nombre : Colonne Ligne Nombre ")
       (if (not (add-number +my-grid2+ +my-grid+
			    (cdr (assoc (read) '((A . 0) (B . 1) (C . 2) (D . 3) (E . 4) (F . 5) (G . 6) (H . 7) (I . 8))))
			    (- (read) 1) (read)))
	   (format t " Position non valide ! Noob !~%"))))



(defun main-standalone (grid)
  (loop while T do
       (let((l (random 8))(c (random 8))(v (random 9)))
	 (princ "Lunettes teintes, phares xénon, vitres teintées")
	 (if (valid-position grid l c v)
	     (return-from main-standalone '(l c v))))))


(defun main-init (grid)
  (loop while (not (game-over grid)) do
     (let((l (main-standalone grid)))
       (l)))
       ;(add-number grid grid (car 'l) (parse-integer (car (car 'l))) (parse-integer (last 'l)))))
  (show-grid grid grid))

;(game)
