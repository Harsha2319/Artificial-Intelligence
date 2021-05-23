(defvar *infinity* 99999) ; maximum value of a board
(defvar *nodes-generated*) ; number of nodes looked at by the game-tree search algorithm
(defparameter *straights* '((1 2 3) (4 5 6) (7 8 9) (1 4 7) (2 5 8) (3 6 9) (1 5 9) (3 5 7)))
(defparameter *current-player* 'x)

(defparameter *xamount* 0)
(defparameter *oamount* 0)
(defparameter *pot* 0)

(defun generate-board ()
  (loop repeat 9 collect nil))

(defun display-board (board)
  (loop for i downfrom 2 to 0
     do (loop for j from 1 to 3
	   initially (format t "|")
	   do (format t "~a|" (or (get-board-elt (+ (* 3 i) j) board) (+ (* 3 i) j)))
	   finally (format t "~&"))))

(defun get-board-elt (n board)
  (nth (1- n) board))

;;; checks whether the board represents a draw
(defun is-draw (board)    ; the game is a draw when the board is full
  (cond
    ((is-win board 'X) nil) ; unless it is a win for X
    ((is-win board 'O) nil) ; or it is a win for O
    (t (not (member nil board)))))

;;; checks whether the board represents a win for the given player
(defun is-win (board player)
  (let ((wins '((0 1 2) (3 4 5) (6 7 8) ; rows
                (0 3 6) (1 4 7) (2 5 8) ; columns
                (0 4 8) (2 4 6)))) ; diagonals
    (check-for-wins wins board player)))

;;; checks whether the boards represents a game that is finished
(defun game-over (board)
  (or (is-win board 'X) (is-win board 'O) (is-draw board)))

;;; used by is-win
(defun check-for-wins (wins board player)
  (cond
    ((null wins) nil) ; got to the end of list and there were no wins
    ((is-a-win board player (car wins)) t)
    (t (check-for-wins (cdr wins) board player))))

;;; used by check-for-wins
(defun is-a-win (board player positions)
  (and (equal (nth (nth 0 positions) board) player)
       (equal (nth (nth 1 positions) board) player)
       (equal (nth (nth 2 positions) board) player)))

;;; converts 'O to 'X and vice versa
(defun other-player (player)
  (case player
    (X 'O)
    (O 'X)))
 
;;; returns a list of next moves for a given board (or nil if there are none)
(defun next-moves (board player)
  (if (is-win board (other-player player))
      nil
      (next-moves-aux nil board player)))

;;; constructs a list where there is a board corresponding to each nil 
;;; slot in the original board.  
(defun next-moves-aux (slots-checked slots-to-check player)
  (cond ((null slots-to-check) nil)
	((null (car slots-to-check)) 
	     (cons (append slots-checked (list player) (cdr slots-to-check))
		   (next-moves-aux (append slots-checked '(()))
				   (cdr slots-to-check) 
				   player)))
        (t (next-moves-aux (append slots-checked (list (car slots-to-check)))
				   (cdr slots-to-check) 
				   player))))

;;; invokes minimax-search with alpha-beta pruning
(defun get-machine-move (board player max-search)
  (setf *nodes-generated* 0)
  (let ((board (minimax-alpha-beta board 0 max-search player 
			   *infinity* (- *infinity*))))
  (format t "Number of nodes generated: ~S~%" *nodes-generated*)
  board))

;;; performs minimax-search with alpha-beta pruning (see pages 317-318 of
;;; Rich and Knight's textbook)
;;; This version differs slightly in that the return value is the value
;;; of the node only, except at the top level where it is the resulting board
;;; only. Rich and Knight return a value and a path.
(defun minimax-alpha-beta (board depth max-depth player use-thresh pass-thresh)
  (if (equal depth max-depth)
   ;; then
     (heuristic board player)
      
   ;; else
      (let ((successors (next-moves board player)))
	(setf *nodes-generated* (+ *nodes-generated* (length successors)))
    	(if (null successors)
         ;; then
      (heuristic board player)
	 ;; else
	    (do ((new-value nil)
		 (best-move (car successors)))
		;; when no more successors return pass-thresh or
		;; best-move if at top level
		((null successors) (if (= depth 0)
				       best-move
				       pass-thresh))
	       (setf new-value 
		 (- (minimax-alpha-beta 
			(car successors)
			(+ 1 depth) 
			max-depth 
		 	(other-player player)
			(- pass-thresh)
			(- use-thresh))))
		(when (> new-value pass-thresh) 
			      (setf pass-thresh new-value)
			      (setf best-move (car successors)))
		(if (>= pass-thresh use-thresh) 
                 ;; then
		    (setf successors nil)  ; terminate the loop
		 ;; else
		    (setf successors (cdr successors))))))))


;;; this function needs work
;;; determines the heuristic value of a given board for a given player
;;; (= evaluation function)
(defun heuristic (board player)
  (cond 
    ((is-win board player) *infinity*)
    ((is-win board (other-player player)) (- *infinity*))
    ((is-draw board) 0)
    (t (+ (* 10 (count-pairs board player))
	  (*  3 (count-corners board player))
	  (check-center board player)))
))

;;; counts the number of adjacent pairs player has on the board
(defun count-pairs (board player)
  (let ((pairs '((0 1) (1 2) (3 4) (4 5) (6 7) (7 8)  ; rows
		 (0 3) (3 6) (1 4) (4 7) (2 5) (5 8)  ; columns
		 (0 4) (4 8) (2 4) (4 6))))           ; diagonals
    (add-pairs pairs board player)))

;;; adds the number of pairs player has on the board
(defun add-pairs (pairs board player)
  (cond
   ((null pairs) 0)  ; end of pair list
   (t (+ (is-a-pair (car pairs) board player)
	 (add-pairs (cdr pairs) board player)))))

;;; checks if the pair is on the board
(defun is-a-pair (pair board player)
  (cond
   ((and (equal (nth (nth 0 pair) board) player)
	 (equal (nth (nth 1 pair) board) player)) 1)
   (t 0)))

;;; counts the number of corners player has on the board
(defun count-corners (board player)
  (let ((corners '(0 2 6 8)))
    (add-corners corners board player)))

;;; adds the number of corners
(defun add-corners (corners board player)
  (cond
   ((null corners) 0)
   (t (+ (is-a-corner (car corners) board player)
	 (add-corners (cdr corners) board player)))))

;;; checks if the corner position is on the board
(defun is-a-corner (corner board player)
  (cond
   ((equal (nth corner board) player) 1)
   (t 0)))

;;; checks if player has the board center
(defun check-center (board player)
  (cond
   ((equal (nth 4 board) player) 1)
   (t 0)))

;;; *EOF*

(defun computer-move-p (current-player autoplay-x-p autoplay-o-p)
  (if (eq current-player 'x)
      autoplay-x-p
      autoplay-o-p))

(defun perform-turn (current-player board autoplay-x-p autoplay-o-p)
  (if (computer-move-p current-player autoplay-x-p autoplay-o-p)
      (setq board (get-machine-move board current-player 9))
      (setq board (get-machine-move board current-player 9))))

(defun switch-player ()
  (if (eq *current-player* 'x) 
      (setf *current-player* 'o)
      (setf *current-player* 'x)))

(defun set-player (mark)
t)

;;; plays tic-tac-toe
(defun play ()
  (setf *current-player* 'x)
  (let ((board (generate-board))
	(autoplay-x-p (set-player 'x))
	(autoplay-o-p (set-player 'o)))
    (setq randx 0)
    (setq rando 0)
    (do ()   ; (no do variables)
        ((game-over board) ; until game over
         (cond ((is-draw board)	(progn (princ "draw") (setq *xamount* (- *xamount* randx)) (setq *oamount* (- *oamount* rando)) (setq *pot* (+ *pot* randx rando))))
	       ((is-win board player) (progn (princ "x") (setq *xamount* (+ *xamount* rando)) (setq *oamount* (- *oamount* rando))))
	       (t (progn (princ "o") (setq *xamount* (- *xamount* randx)) (setq *oamount* (+ *oamount* randx))))))
           (setq randx1 (random 6))
           (setq randx (+ randx randx1))
           (setq rando1 (random 6))
           (setq rando (+ rando rando1))
           (format t "Randx :  ~S~%" randx1)
           (format t "Rando :  ~S~%" rando1)
           (setq board (perform-turn *current-player* board autoplay-x-p autoplay-o-p))
	      (switch-player)
	      (display-board board))))

(defun tttXyAb ()
    (setq i 0)
    (format t "Set initial amount for x and o : ")
    (setq amount (read))
    (setq *xamount* amount)
    (setq *oamount* amount)
    (do ()
    ((or (<= *xamount* 0) (<= *oamount* 0)))
    (play)
    (princ "output after each game")
    (format t "output after each game~%")
    (format t "Amount left with X :  ~S~%" *xamount*)
    (format t "Amount left with o :  ~S~%" *oamount*)
    (format t "Amount in the pot :  ~S~%" *pot*)
    (setq i (+ i 1))
)
(format t "Amount with x or o is less than 0. So, program terminated.~%")
(format t "Number of iteration before termination ~a" i))