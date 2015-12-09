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

(defun show-grid (grid)
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
	      (format t "~d " (aref grid i j)))))
      (format t "|~%")))
  (format t "****************************~%"))

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

(defun game ()
  (loop while (not (game-over +my-grid+)) do
       (show-grid +my-grid+)
       (format t " Ajouter un nombre : Colonne Ligne Nombre ")
       (if (not (add-number +my-grid+
			    (cdr (assoc (read) '((A . 0) (B . 1) (C . 2) (D . 3) (E . 4) (F . 5) (G . 6) (H . 7) (F . 8))))
			    (- (read) 1) (read)))
	   (format t " Position non valide ! Noob !~%"))))



(defun main-standalone (grid)
  (loop while T do
       (let((l (random 8))(c (random 8))(v (random 9)))
	 (if (valid-position grid l c v)
	     (return-from main-standalone '(l c v))))))


(defun main-init (grid)
  (loop while (not (game-over grid)) do
       (add-number grid (parse-integer (car (list(main-standalone grid)))) (parse-integer (car (car (list(main-standalone grid))))) (parse-integer (last (list(main-standalone grid)))))
       (show-grid grid)))
