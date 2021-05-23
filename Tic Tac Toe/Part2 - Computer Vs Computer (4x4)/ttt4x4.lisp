(defun generate-board ()
  (loop repeat 16 collect nil))
 
(defparameter *straights* '((1 2 3) (2 3 4) (5 6 7) (6 7 8) (9 10 11) (10 11 12) (13 14 15) (14 15 16) (1 5 9) (5 9 13) (2 6 10) (6 10 14) (3 7 11) (7 11 15) (4 8 12) (8 12 16) (5 10 15) (2 7 12) (1 6 11) (6 11 16) (9 6 3) (14 11 8) (13 10 7) (10 7 4)))
(defparameter *current-player* 'x)
 
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
 
(defun multi-non-nil-eq (lst)
  (and (notany #'null lst)
       (notany #'null (mapcar #'(lambda (x) (eq (car lst) x)) lst))
       (car lst)))
 
(defun elements-of-straights (board)
  (loop for i in *straights*
     collect (loop for j from 0 to 2
	   collect (get-board-elt (nth j i) board))))
 
(defun find-winner (board)
  (car (remove-if #'null (mapcar #'multi-non-nil-eq (elements-of-straights board)))))
 
(defun set-player (mark)
  (format t "Shall a computer play as ~a? (y/n)~&" mark)
  (let ((response (read)))
    (cond ((equalp response 'y) t)
	  ((equalp response 'n) nil)
	  (t (format t "Come again?~&")
	     (set-player mark)))))
 
(defun player-move (board symbol)
  (format t "~%Player ~a, please input your move.~&" symbol)
  (set-board-elt (read) board symbol)
  (format t "~%"))
 
(defun computer-move (board symbol)
  (let ((move (get-random-element (list-legal-moves board))))
    (set-board-elt move board symbol)
    (format t "~%computer ~a selects ~a~%~%" symbol move)))
 
(defun computer-move-p (current-player autoplay-x-p autoplay-o-p)
  (if (eq current-player 'x)
      autoplay-x-p
      autoplay-o-p))
 
(defun perform-turn (current-player board autoplay-x-p autoplay-o-p)
  (if (computer-move-p current-player autoplay-x-p autoplay-o-p)
      (computer-move board current-player)
      (computer-move board current-player)))
 
(defun switch-player ()
  (if (eq *current-player* 'x) 
      (setf *current-player* 'o)
      (setf *current-player* 'x)))
 
(defun display-board (board)
  (loop for i downfrom 3 to 0
     do (loop for j from 1 to 4
	   initially (format t "|")
	   do (format t "~a|" (or (get-board-elt (+ (* 4 i) j) board) (+ (* 4 i) j)))
	   finally (format t "~&"))))
 
(defun ttt4x4 ()
  (print "Enter no of games")
  (setq n (read))
  (setq i 1)
  (setf *current-player* 'x)
  (loop while (<= i n) do
    (princ "Game: ")
    (write i)
    (let ((board (generate-board))
	  (autoplay-x-p t)
	  (autoplay-o-p nil))
    (format t "~%")
    (loop until (or (find-winner board) (null (list-legal-moves board)))
       do (display-board board)      
       do (perform-turn *current-player* board autoplay-x-p autoplay-o-p)
       do (switch-player)
       finally (if (find-winner board)
		   (format t "The winner of Game ~a is ~a!~%" i (find-winner board))
		   (format t "It's a tie.")))
       (setf i (+ i 1)))))