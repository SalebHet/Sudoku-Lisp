

(defun copy-grid (grid) ;Fonction qui copie la grille donnée en paramètre
  (let ((grid2 (make-array (array-dimensions grid))))
    (dotimes (i (array-dimension grid 0))
      (dotimes (j (array-dimension grid 1))
        (setf (aref grid2 i j) (aref grid i j))))
    (return-from copy-grid grid2)))        



(defun add-number (grid x y numb) ;ajoute la valeur dans la grille à l'emplacement x y 
  (if (valid-position grid x y numb)
      (setf (aref grid y x) numb)))

(defun valid-position (grid x y numb) ;Verifie si le coup proposer est valide (case vide et respectant les règles d'un sudoku)
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



(defvar *grid*);grille utilisé par la stratégie 

(defun init-standalone (grid);initialise la stratégie en copiant la grille originale
  (setf *grid* (copy-grid grid)))

(defun main-standalone ();stratégie aléatoire
  (loop while T do
       (let ((c (random 8)) (l (random 8)) (v (random 9)))
	 (if (valid-position  *grid* c l v);génère un coup aléatoire jusqu'à que le coup soit jouable
	     (progn
	       (add-number *grid* c l v) ;met à jour la grille pour le prochain coup
	       (return-from main-standalone (values c l v)))))));retourne le coup

  
