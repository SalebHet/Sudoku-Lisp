
;Définition des grilles
;Décommentez là et commentez l'autre pour jouer avec l'autre grille puis recompilez

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

;#########################################################################################

(defun copy-grid (grid) ;Retourne la copie de la grille donnée en paramètre
  (let ((grid2 (make-array (array-dimensions grid))))
    (dotimes (i (array-dimension grid 0))
      (dotimes (j (array-dimension grid 1))
        (setf (aref grid2 i j) (aref grid i j))))
    (return-from copy-grid grid2)))        

(defun show-grid (grid grid2) ;;Affiche la grille dans le terminal en mettant en rouge les valeurs initial de la grille
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

(defun add-number (grid grid2 x y numb) ;Ajoute la valeur numb à la position xy en vérifiant si la position est valide sur la grille originel
  (if (valid-position grid2 x y numb)
      (setf (aref grid y x) numb)))

(defun valid-position (grid x y numb);Verifie si un coup se joue à une case non occupée et si il respecte les règles du Sudoku
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

(defun game-over (grid);Retourne T si toutes les cases du Sudoku sont occupés
  (dotimes (i 9)
    (dotimes (j 9)
      (if (= 0 (aref grid j i))
	  (return-from game-over NIL))))
  (return-from game-over T))

(defun sudoku (grid);Fonction principale appelé pour jouer 
  (let ((grid2 (copy-grid grid)))
    (loop while (not (game-over grid2)) do ;Boucle infinie tant que la grille n'est pas remplie 
       (show-grid grid2 grid)
       (format t " Ajouter un nombre : Colonne Ligne Nombre ~%")
       (if (not (add-number grid2 grid
			    (cdr (assoc (read) '((A . 0) (B . 1) (C . 2) (D . 3) (E . 4) (F . 5) (G . 6) (H . 7) (I . 8))))  ;Joue le coup donné par le joueur 
			    (- (read) 1) (read)))
	   (format t " Position non valide !~%")))))  ; Signale au joueur si son coup n'est pas valide


(sudoku +my-grid+)
