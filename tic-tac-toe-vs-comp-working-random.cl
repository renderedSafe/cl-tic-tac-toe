;;;; tictactoe.lisp
;;;; 
;;;; Andrew Levenson
;;;; 10/27/10
;;;;
;;;; Simple two player ASCII Tic Tac Toe Game

;;; First player is X, so initialize the marker to X
(setf *marker* :X)
(setf *player* "Player 1")



;;; Create the board in memory
(defun create-board ()
	(setf *board* (make-array 9 :initial-contents
		                          '(1 2 3 4 5 6 7 8 9))))

;;; Greet the player and display the board.
(defun welcome-player ()
	(format t "Welcome to TicTacToe!~%~%")
	(create-board)
	(play nil))

;;; Draw the board
;;; If numbersp is t, draw the numbers on the board
;;; to represent which number to enter to select that space
(defun draw-board ()
	(format t "     |     |     ~%")
	(format t "   ~a |   ~a |   ~a  ~%" (board-ref 1) (board-ref 2) (board-ref 3))
	(format t "     |     |     ~%")
	(format t "_________________~%")
	(format t "     |     |     ~%")
	(format t "   ~a |   ~a |   ~a  ~%" (board-ref 4) (board-ref 5) (board-ref 6))
	(format t "     |     |     ~%")
	(format t "_________________~%")
	(format t "     |     |     ~%")
	(format t "   ~a |   ~a |   ~a  ~%" (board-ref 7) (board-ref 8) (board-ref 9))
	(format t "     |     |     ~%~%"))

;;; Play the game
(defun play (&optional switch-p)
  (when switch-p (switch-player))
  (if (equal *marker* :X)
      (check-choice (read-choice))
      (computer-move))
  
  (when (and 
	  (not (check-for-win-p)) 
          (not (stalemate)))
     (play t))
  (when (check-for-win-p)
    (progn
      (draw-board)
	  (format t "~a has won! " *player*)
	  (force-output nil)
	  (if (y-or-n-p "Play again? ")
	    (play-again)
	    (quit))))
  (when (stalemate)
        (if (y-or-n-p "~%~%Stalemate! Play again? ")
	  (play-again)
	  (quit))))

(defun play-again ()
	(create-board)
	(switch-player)
	(format t "This game will be started by ~a.~%~%" *player*)
	(play))
	
				

(defun switch-player ()
	(if (equal *marker* :X)
		(setf *marker* :O)
		(setf *marker* :X))
	(if (equal *player* "Player 1")
		(setf *player* "Player 2")
		(setf *player* "Player 1")))

;;; Allow the player to choose a square
;;; Or, if they wish, display the board with
;;; the numbers on it.
(defun read-choice ()
	(draw-board)
	(format t "~a, select a number to choose a square.~%" *player*) 
	(force-output nil)
	(parse-integer (read-line *query-io*) :junk-allowed t))

(defun check-choice (choice)
	(if (and
			  (numberp choice)
			  (> choice 0)
			  (< choice 10))
		(select choice)
		(progn
			(format t "~%Invalid choice.~%")
			(check-choice (read-choice)))))

(defun select (choice)
	(if (numberp (aref *board* (- choice 1))) (setf (aref *board* (- choice 1)) *marker*)
			   	                         (invalid-selection)))

(defun invalid-selection ()
	(format t "That spot is taken. Please choose another spot.~%~%")
	(draw-board)
	(force-output nil)
	(check-choice (read-choice)))

(defun check-for-win-p ()
	(or (is-line-p 1 2 3)
		  (is-line-p 1 4 7)
		  (is-line-p 1 5 9)
		  (is-line-p 2 5 8)
		  (is-line-p 3 6 9)
		  (is-line-p 3 5 7)
		  (is-line-p 4 5 6)
		  (is-line-p 7 8 9)))

(defun is-line-p (a b c)
	(and
	  (eql
	    (board-ref a)
	    (board-ref b))
	  (eql
	    (board-ref a)
	    (board-ref c))))

(defun board-ref (cell)
  (aref *board* (- cell 1)))
  
			   
(defun stalemate ()
	(when
		(and
			(not (numberp (aref *board* 0)))
			(not (numberp (aref *board* 1)))
			(not (numberp (aref *board* 2)))
			(not (numberp (aref *board* 3)))
			(not (numberp (aref *board* 4)))
			(not (numberp (aref *board* 5)))
			(not (numberp (aref *board* 6)))
			(not (numberp (aref *board* 7)))
			(not (numberp (aref *board* 8))))
		t))

;;;Hey look,style to match the code that was already in place. Referencing a global variable inside a function..
(defun get-legal-moves ()
  (loop for x across (remove-if-not #'numberp *board*)
        collect x))

(defun computer-move ()
  (select (find-best-move)))

;;;checks he utility of every legal move and returns the cell with the highest utility
(defun find-best-move ()
  (let
      ((best-move-cell (elt (get-legal-moves) 0))
       (best-move-value 0)
       (legal-moves (get-legal-moves)))
    (loop for x in legal-moves
        if (> (find-move-utility x) best-move-value)
        do (progn (setf best-move-value (find-move-utility x))
                  (setf best-move-cell x)))
    best-move-cell))

;;;TODO: make this take the reference piece as argument, that way we can check opponent's
;;; moves too and minimize their score
;;;quantifies the utility of a move in a given cell based on the current state of the *board*
(defun find-move-utility (cell)
  (let
      ((member-lines-list (find-member-lines cell))
       (utility-value 0))
    (loop for x in member-lines-list
             if (or (eql (count :X x) 2) (eql (count :O x) 2))
                do (setf utility-value (+ utility-value 100))
              if (and (eql (count :O x) 1) (eql (count :X x) 1))
                do (setf utility-value (+ utility-value 0))
              if (and (eql (count :O x) 1) (eql (count :X x) 0))
                do (setf utility-value (+ utility-value 2))
              if (and (eql (count :O x) 0) (eql (count :X x) 1))
                do (setf utility-value (+ utility-value 1)))
    utility-value))
            
              
;;;returns the lines on the board that cell is a member of
(defun find-member-lines (cell)
  (let 
      ((possible-line-list (list 
                         '(1 2 3)
                         '(1 4 7)
                         '(1 5 9)
                         '(2 5 8)
                         '(3 6 9)
                         '(3 5 7)
                         '(4 5 6)
                         '(7 8 9))))
    (loop for x in possible-line-list
        if (contains cell x)
          collect (board-line x))))

(defun contains (item sequence) 
  (if (member item sequence) T NIL))

;;;used for getting the actual moves in a line on the board from cell numbers
(defun board-line (line)
  (list (board-ref (first line))
        (board-ref (second line))
        (board-ref (third line))))