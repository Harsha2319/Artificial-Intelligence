(defvar *infinity* 99999) ; maximum value of a board
(defvar *nodes-generated*) ; number of nodes looked at by the game-tree search algorithm 
(defparameter *straights* '((1 2 3) (4 5 6) (7 8 9) (1 4 7) (2 5 8) (3 6 9) (1 5 9) (3 5 7)))
(defparameter *current-player* 'x)

(defparameter *x* 0)
(defparameter *o* 0)
(defparameter *tie* 0)
 
(defun generate-board ()
  (loop repeat 9 collect nil))

(defun get-board-elt (n board)
  (nth (1- n) board))
 
(defun legal-p (n board)
  (null (get-board-elt n board)))
 
(defun set-board-elt (n board symbol)
  (if (legal-p n board)
      (setf (nth (1- n) board) symbol)
      (progn (format t "Illegal move. Try again.~&")
	     (set-board-elt (read) board symbol))))
 
(defun list-legal-moves (board)
  (loop for i from 1 to (length board)
     when (legal-p i board)
     collect i))

(defun get-random-element (lst)
  (nth (random (length lst)) lst))
 
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
 
(defun set-player (mark)
  t)

(defun find_elt_for_pairs (board player)
(let ((pairs '((1 2) (2 3) (1 3) (4 5) (5 6) (4 6) (7 8) (8 9) (7 9) 
(1 4) (4 7) (1 7) (2 5) (5 8) (2 8) (3 6) (6 9) (3 9) 
(1 5) (5 9) (1 9) (3 5) (5 7) (3 7)))            ; Diagonal
(element '(3 1 2 6 4 5 9 7 8 7 1 4 8 2 5 9 3 6 9 1 5 7 3 5)))

(find_pair_rec pairs element board player)))
      

(defun find_pair_rec (pairs element board player) 
(cond
      ((null pairs) nil)  ; end of pair list
      (t (if (find-pair (car pairs) (car element) board player)
          (car element)
          (find_pair_rec (cdr pairs) (cdr element) board player)))))

(defun find-pair (pair element board player)
  (cond
   ((and (equal (nth (nth 0 pair) board) player)
	 (equal (nth (nth 1 pair) board) player)) 1)
   (t 0)))

(defun computer-move2 (board symbol)
  (setq elemento (find_elt_for_pairs board 'o))
  (setq elementx (find_elt_for_pairs board 'x))
  (cond
  ((null (get-board-elt elementx board)) (progn (setq move elementx)(set-board-elt elementx board symbol)))
  ((null (get-board-elt elemento board)) (progn (setq move elemento)(set-board-elt elemento board symbol)))
  ((null (get-board-elt 5 board)) (progn (setq move 5)(set-board-elt 5 board symbol) ))
  ((null (get-board-elt 1 board)) (progn (setq move 1)(set-board-elt 1 board symbol) ))
  ((null (get-board-elt 3 board)) (progn (setq move 3)(set-board-elt 3 board symbol) ))
  ((null (get-board-elt 7 board)) (progn (setq move 7)(set-board-elt 7 board symbol) ))
  ((null (get-board-elt 9 board)) (progn (setq move 9)(set-board-elt 9 board symbol) ))
  (t (let ((move (get-random-element (list-legal-moves board))))
    (set-board-elt move board symbol))))
  (format t "~%computer selects ~a~%~%" move) board)
 
(defun perform-turn (current-player board autoplay-x-p autoplay-o-p)
  (if (eq current-player 'x)
      (progn (format t "Minimax : ~a~%" current-player)
      (setq board (get-machine-move board current-player 9)))
      (progn (format t "Heuristic : ~a~%" current-player)
      (setq board (computer-move2 board current-player)))))
 
(defun switch-player ()
  (if (eq *current-player* 'x) 
      (setf *current-player* 'o)
      (setf *current-player* 'x)))
 
(defun display-board (board)
  (loop for i downfrom 2 to 0
     do (loop for j from 1 to 3
	   initially (format t "|")
	   do (format t "~a|" (or (get-board-elt (+ (* 3 i) j) board) (+ (* 3 i) j)))
	   finally (format t "~&"))))
 
(defun ttt3x3nAbH ()
  (format t "~%Enter no of games : ")
  (setq n (read))
  (defparameter *x* 0)
  (defparameter *o* 0)
  (defparameter *tie* 0)
  (setq i 1)
  (setf *current-player* 'x)
  (loop while (<= i n) do
    (format t "~%Game: ")
    (write i)
    (let ((board (generate-board))
	(autoplay-x-p (set-player 'x))
	(autoplay-o-p (set-player 'o)))
    (do ()   ; (no do variables)
        ((game-over board) ; until game over
         (cond ((is-draw board)	(progn (format t "~%It's a tie")(setq *tie* (+ *tie* 1))))
	       ((is-win board 'x) (progn (format t "~%Winner is x")(setq *x* (+ *x* 1))))
	       (t (progn (format t "~%Winner is o")(setq *o* (+ *o* 1))))))
        (format t "~%Current player : ~a~%" *current-player*)
        (setq board (perform-turn *current-player* board autoplay-x-p autoplay-o-p))
        (switch-player)
        (display-board board)))
        (setf i (+ i 1)))
        (Format t "~%No of wins by x : ~a" *x*)
        (Format t "~%No of wins by o : ~a" *o*)
        (Format t "~%No of tie : ~a" *tie*))

