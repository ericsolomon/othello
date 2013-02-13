;;; Eric Solomon
;;; Kevin Kuhl
;;; CS 4346 Final Project
;;; Last Modified: 12-10-07

;;; The Othello Project
;;; FINAL VERSION

;;; README: Due to the nature of our algorithm and design, almost every variable
;;;         in the program is global.
;;;         Since all our variables are global, also every function has several side effects,
;;;         so to make things simpler, we generalized what each functions side-effects were. It's 
;;;         easy enough to look through the functions and see what exactly is getting changed.
;;;
;;; CHANGE LOG: We didn't keep up with who did what modifications for the majority of 
;;;             the code because we had written a majority before receiving the documentation
;;;             template. The change logs in our documentation is purely what we could remember
;;;             to the best of our ability about who did what and when. 

;;; CODE START--------------------------------------------------

;;;; Function Name: play
;;;
;;;  Purpose: It's the engine for our game
;;;  Returns: null (values)
;;;  Arguments: none
;;;  Keywords: n/a
;;;  Side Effects: Prints to screen, changes variables and arrays
;;;  Error Handling: Yes, handles user input
;;;  Calls: generate-board, generate-weight-board, get-play-choice, human-move,
;;;         random-ai, move-avail, clear-plusses, flip-ai, pick-move-flip-ai,
;;;         play-ai-piece, weight-ai, flip-weight-ai 
;;;  Called By: quit-game, the user
;;;
;;;  Change Log:
;;;
;;;  1. Eric Solomon  12-09-07
;;;     Kevin Kuhl
;;;     - added hybrid ai
;;;  2. Eric Solomon  12-01-07
;;;     - added weighted ai
;;;  2. Kevin Kuhl    11-28-07
;;;     - added flips ai  
;;;  3. Eric Solomon  11-15-07
;;;     - Put in error handling
;;;  4. Eric Solomon  11-05-07
;;;      - wrote function

(defun play ()
  (format t "~%THE OTHELLO PROJECT")
  (format t "~%Written by Eric Solomon and Kevin Kuhl~%~%")

  ; init 
  
  ; FLAGS
  (setf bflag nil)
  (setf wflag nil)
  (setf bflag2 nil)
  (setf wflag2 nil)
  (setf cvflag nil)
  (setf choice nil)
  (setf humanturn nil)
  (setf moveflag nil)
  (setf vflag nil)

 
  (setf i 1)
  (setf inputs nil)
  (generate-board)
  (generate-weight-board)
  (get-play-choice)

  ; game loop
  (loop until (eql i 0) do
	(if (eql choice 1)
	    (human-move)
	  (if (eql choice 2)
	      (if (null humanturn)
		  (let () 
		    (if (eql 1 aichoice) 
			(random-ai)
		      (if (eql 2 aichoice) 
			  (let ()
			    (setf heur_lst nil)
			    (setf min_lst nil)	;List of losses for a particular lookahead of move
			    (setf flip-amount -100)  ;Set flip-amount to absolute minimum number
			    (move-avail a1)
			    (clear-plusses a1)
			    (flip-ai pair_lst)
			    (setf best-move (second (first heur_lst))) ;Default: best move for AI is the first one
			    (pick-move-flip-ai heur_lst)	;Will reset the best move should another be better (i.e. Max)
			    (format t "~%Computer places a piece at ")
			    (princ best-move)
			    (format t "~%")
			    (play-ai-piece best-move a1 playermove)
			    (setf humanturn t)
			    (setf playermove 'black))
			(if (eql 3 aichoice)
			    (let ()
			      (weight-ai)
			      (format t "~%Computer places a piece at ")
			      (princ best-weight-move)
			      (format t "~%"))
			  (if (eql 4 aichoice)
			      (let ()
				(setf heur_lst nil)
				(setf min_lst nil)    
				(setf flip-amount -100)  
				(move-avail a1)
				(clear-plusses a1)
				(flip-weight-ai pair_lst)
				(setf best-move (second (first heur_lst))) 
				(pick-move-flip-ai heur_lst)
				(format t "~%Computer places a piece at ")
				(princ best-move)
				(format t "~%")
				(play-ai-piece best-move a1 playermove)
				(setf humanturn t)
				(setf playermove 'black)))))))
		(if (eql humanturn t) 
		    (human-move))))))
  (values))


;;;; Function Name: get-play-choice
;;;
;;;  Purpose: To get whether the user wants to play against an AI or human,
;;;           and what AI they want to play against if they choose that. 
;;;  Returns: null
;;;  Arguments: none
;;;  Keywords: n/a
;;;  Side Effects: Prints to screen and changes variables
;;;  Error Handling: Yes, handles user input
;;;  Calls: string-to-list, get-play-choice
;;;  Called By: play
;;;
;;;  Change Log:
;;;
;;;  1. Eric Solomon     12-09-07
;;;     - added hybrid ai as a choice
;;;  2. Eric Solomon     12-01-07
;;;     - added weighted ai as a choice
;;;  3. Kevin Kuhl       11-28-07
;;;     - added flips ai as a choice
;;;  4. Eric Solomon     11-16-07     
;;;     - wrote function

(defun get-play-choice ()
  (if (eql choice nil)
      (let ()
	(format t "Do you want to play against (1) Human (2) Computer : ")
	(setf choice (first (string-to-list (read-line))))))
  (if (eql choice 1)
      (setf playermove 'black)
    (if (eql choice 2) 
	(let () 
	  (format t "~%What AI do you want to play against?~%")
	  (format t "(1) Random Move (2) Flips Min-Max (3) Weighted (4) Flips + Weighted Min-Max: ")
	  (setf aichoice (first (string-to-list (read-line))))
	  (if (or (eql 1 aichoice)
		  (eql 2 aichoice)
		  (eql 3 aichoice)
		  (eql 4 aichoice))
	      (let ()
		(setf humanturn t)
		(setf playermove 'black))
	    (let ()
	      (format t "~%Invalid AI selection. Please try again.~%")
	      (get-play-choice))))
      (let ()
	(format t "~%Invalid choice. Please try again.~%~%")
	(setf choice nil)
	(get-play-choice)))))


;;;; Function Name: human-move
;;;
;;;  Purpose: Provides interface and necessary processing for human play
;;;  Returns: null
;;;  Arguments: none
;;;  Keywords: n/a
;;;  Side Effects: Prints to screen and changes variables
;;;  Error Handling: Yes, handles user input.
;;;  Calls: move-avail, clear-plusses, string-to-list, get-winner, print-score,
;;;         quit-game, play-piece
;;;  Called By: play
;;;
;;;  Change Log:
;;;
;;;  1. Eric Solomon  11-06-07
;;;     - wrote function

(defun human-move ()
  (if (move-avail a1)
      (let()
	(print a1)
	(clear-plusses a1)
	(format t "~%~%")
	(princ playermove)
	(format t "'s turn. Enter Row and Column ('q' to quit, 's' for score): ")
	(setf inputs (string-to-list (read-line)))
	(if (and (eql 'q (first inputs))
		 (null (second inputs)))      
	    (let () (setf i 0)
		 (get-winner)
		 (print-score)
		 (quit-game))
	  (if (and (eql 's (first inputs))
		   (null (second inputs)))
	      (print-score)
	    (play-piece inputs a1))))))


;;;; Function Name: move-avail
;;;
;;;  Purpose: Determines whether the current player has any available
;;;           moves on the board
;;;  Returns: moveflag
;;;  Arguments: The array that is to be searched for available moves
;;;  Keywords:
;;;  Side Effects: Prints to screen and changes variables
;;;  Error Handling: n/a
;;;  Calls: check-valid, get-winner, print-score, quit-game, 
;;;  Called By: play, flip-ai, weight-ai, check-min, check-min-flip-weight-ai,
;;;             human-move, flip-weight-ai
;;;
;;;  Change Log:
;;;   
;;;  1. Eric Solomon   11-28-07
;;;     - added humanturn and flags for ai
;;;  2. Kevin Kuhl     11-13-07
;;;     - added parameters  
;;;  3. Eric Solomon   11-06-07
;;;     - wrote function

(defun move-avail (array)
  (setf moveflag nil)
  (setf pair_lst nil)
  (if (eql playermove 'black)
      (let ()(setf bflag2 nil)
	   (if (null (check-valid array))
	       (if (null wflag2)
		   (let () (format t "~%No more valid moves.~%")
			(print a1)
			(format t "~%")
			(get-winner)
			(print-score)
			(quit-game))
		 (let ()(format t "~%No valid moves for black. White's turn.~%")
		      (setf playermove 'white)(setf humanturn nil)))
	     (setf moveflag t)))
    (if (eql playermove 'white)
	(let () (setf wflag2 nil)
	     (if (null (check-valid array))
		 (if (null bflag2)
		     (let () (format t "~%No more valid moves.~%")
			  (print a1)
			  (format t "~%")
			  (get-winner)
			  (print-score)
			  (quit-game))
		   (let () (format t "~%No valid moves for white. Black's turn.~%")
			(setf playermove 'black)(setf humanturn t)))
	       (setf moveflag t)))))
  moveflag)


;;;; Function Name: generate-board
;;;
;;;  Purpose: Creates the initial game board
;;;  Returns: null
;;;  Arguments: none
;;;  Keywords: n/a
;;;  Side Effects: Creates an array
;;;  Error Handling: n/a
;;;  Calls: none
;;;  Called By: play
;;;
;;;  Change Log:
;;;
;;;  1. Eric Solomon  11-02-07
;;;     - wrote function

(defun generate-board ()
  (setf a1 (make-array '(9 9) :initial-element '-))
  (setf (aref a1 5 5) '0
	(aref a1 4 5) '@
	(aref a1 5 4) '@
	(aref a1 4 4) '0
	(aref a1 0 1) '1
	(aref a1 0 2) '2
	(aref a1 0 3) '3
	(aref a1 0 4) '4
	(aref a1 0 5) '5
	(aref a1 0 6) '6
	(aref a1 0 7) '7
	(aref a1 0 8) '8
	(aref a1 1 0) '1
	(aref a1 2 0) '2
	(aref a1 3 0) '3
	(aref a1 4 0) '4
	(aref a1 5 0) '5
	(aref a1 6 0) '6
	(aref a1 7 0) '7
	(aref a1 8 0) '8
	(aref a1 0 0) '+))


;;;; Function Name: quit-game
;;;
;;;  Purpose: Checks if the user wants to quit and reacts accordingly
;;;  Returns: null
;;;  Arguments: none
;;;  Keywords: n/a
;;;  Side Effects: Prints to screen and changes variables
;;;  Error Handling: Yes, handles user input
;;;  Calls: quit-game, play, string-to-list
;;;  Called By: play, human-move, move-avail
;;;
;;;  Change Log:
;;;
;;;  1. Eric Solomon   11-23-07
;;;     - added error handling
;;;  2. Eric Solomon   11-10-07
;;;     - wrote function

(defun quit-game ()
  (format t "~%Would you like to play again ('y' or 'n')? ")
  (setf play-choice (first (string-to-list (read-line))))
  (if (or (eql 'y play-choice)
	  (eql 'n play-choice))
      (cond ((eql 'y play-choice)(play))
	    ((eql 'n play-choice)(format t "~%Thanks for playing!~%")(setf i 0)))
    (let ()
      (format t "~%Invalid choice. Please try again.~%")
      (quit-game))))


;;;; Function Name: get-winner
;;;
;;;  Purpose: Prints the winner of the game
;;;  Returns: null
;;;  Arguments: none
;;;  Keywords: n/a
;;;  Side Effects: Prints to screen
;;;  Error Handling: n/a
;;;  Calls: count-black, count-white
;;;  Called By: human-move, move-avail
;;;
;;;  Change Log:
;;;
;;;  1. Eric Solomon   11-06-07
;;;     - wrote function

(defun get-winner ()
  (if (< (count-black)(count-white a1))
      (format t "~%White wins!~%")
    (if (> (count-black) (count-white a1))
	(format t "~%Black wins!~%")
      (if (eql (count-black)(count-white a1))
	  (format t "~%Tie Game!~%")))))


;;;; Function Name: play-piece
;;;
;;;  Purpose: Plays the piece for the human
;;;  Returns: null (values)
;;;  Arguments: The inputs of where to play and the array on which to play
;;;  Keywords: n/a
;;;  Side Effects: Prints to screen and changes variables
;;;  Error Handling: Yes, handles location validity
;;;  Calls: validate-move
;;;  Called By: human-move
;;;
;;;  Change Log:
;;;
;;;  1. Kevin Kuhl   11-26-07     
;;;     - added parameters
;;;  2. Eric Solomon 11-25-07
;;;     - added humanturn stuff for ai
;;;  3. Eric Solomn  11-08-07
;;;     - wrote function

(defun play-piece (inputs array)
  (if (validate-move inputs array)     
      (if (eql playermove 'black)
	  (let () (setf (aref a1 (first inputs) (second inputs)) '@)
	       (setf playermove 'white)
	       (setf humanturn nil))
	(if (eql playermove 'white)
	    (let ()(setf (aref a1 (first inputs) (second inputs)) '0)
		 (setf playermove 'black)
		 (setf humanturn t))))
    (format t "~%Inputs invalid or move is not in bounds. Please try again.~%"))(values))


;;;; Function Name: string-to-list
;;;
;;;  Purpose: Converts a string to a list
;;;  Returns: null
;;;  Arguments: The string 
;;;  Keywords: n/a
;;;  Side Effects: creates a list
;;;  Error Handling: n/a
;;;  Calls: none
;;;  Called By: get-play-choice, human-move, quit-game
;;;
;;;  Change Log:
;;;
;;;  1. Eric Solomon  11-03-07
;;;     - copied function from notes

(defun string-to-list (string)
  (with-input-from-string (stream string)
			  (do ((word (read stream nil 'end-of-string)
				     (read stream nil 'end-of-string))
			       (string-list))
			      ((eq word 'end-of-string)
			       (nreverse string-list))
			      (push word string-list))))


;;;; Function Name: count-black
;;;
;;;  Purpose: Counts the black pieces on the board
;;;  Returns: bcount
;;;  Arguments: none
;;;  Keywords: n/a
;;;  Side Effects: changes bcount
;;;  Error Handling: n/a
;;;  Calls: none
;;;  Called By: print-score, get-winner
;;;
;;;  Change Log:
;;;
;;;  1. Eric Solomon     11-10-07
;;;     - wrote function

(defun count-black ()
  (setf bcount 0)
  (loop for j from 1 to 8
	do (loop for i from 1 to 8
		 do (if (eql (aref a1 j i) '@)
			(setf bcount (+ 1 bcount)))))bcount)


;;;; Function Name: count-white
;;;
;;;  Purpose: Counts the white pieces on the board
;;;  Returns: wcount
;;;  Arguments: The array on which to count
;;;  Keywords: n/a
;;;  Side Effects: changes wcount
;;;  Error Handling: n/a
;;;  Calls: none
;;;  Called By: print-score, check-min, flip-ai, flip-weight-ai,
;;;             check-min-flip-weight-ai, get-winner
;;;
;;;  Change Log:
;;;
;;;  1. Kevin Kuhl     11-25-07
;;;     - added parameters for ai purposes
;;;  2. Eric Solomon   11-10-07
;;;     - wrote function

(defun count-white (array)
  (setf wcount 0)
  (loop for j from 1 to 8
	do (loop for i from 1 to 8
		 do (if (eql (aref array j i) '0)
			(setf wcount (+ 1 wcount)))))wcount)


;;;; Function Name: print-score
;;;
;;;  Purpose: Prints the current score
;;;  Returns: null (values)
;;;  Arguments: none
;;;  Keywords: n/a
;;;  Side Effects: Prints to screen
;;;  Error Handling: n/a
;;;  Calls: count-black, count-white
;;;  Called By: human-move, move-avail
;;;
;;;  Change Log:
;;;
;;;  1. Eric Solomon    11-10-07
;;;     - wrote function

(defun print-score ()
  (format t "~%Black: ")(princ (count-black))
  (format t "~%White: ")(princ (count-white a1))
  (format t "~%")
  (values))


;;;; Function Name: validate-move
;;;
;;;  Purpose: Checks all location's neighbors for valid moves and flips pieces
;;;           on the path of any found valid move
;;;  Returns: vflag
;;;  Arguments: The location to check and the array to check on
;;;  Keywords: n/a
;;;  Side Effects: changes variables and arrays
;;;  Error Handling: n/a
;;;  Calls: check-bounds, check-path, flip-pieces
;;;  Called By: play-piece, play-ai-piece
;;;
;;;  Change Log:
;;;
;;;  1. Kevin Kuhl   11-25-07
;;;     - added parameters for ai purposes
;;;  2. Eric Solomom 11-07-07
;;;     - wrote function

(defun validate-move (inputs array)
  (setf vflag nil)
  (if (and (numberp (first inputs)) ; checking for valid grid location and inputs are numbers
	   (numberp (second inputs))
           (< (first inputs) 9)
           (< (second inputs) 9)
           (> (first inputs) 0)
           (> (second inputs) 0)
	   (or (eql (aref array (first inputs) (second inputs)) '-)
	       (eql (aref array (first inputs) (second inputS)) '+)))
      
      ; checking for flip of opponent
      (if (eql playermove 'black)
	  (let ()
	    (loop for i from -1 to 1
		  do (loop for j from -1 to 1
			   do (if (and (check-bounds (+ i (first inputs))(+ j (second inputs)))
				       (eql (aref array (+ i (first inputs)) (+ j (second inputs))) '0))
				  (if (check-path (+ i (first inputs)) (+ j (second inputs)) i j array)
				      (let () (setf vflag t)
					   (flip-pieces (+ i (first inputs))(+ j (second inputs)) i j array)))))))
	(if (eql playermove 'white)
	    (loop for i from -1 to 1
		  do (loop for j from -1 to 1
			   do (if (and (check-bounds (+ i (first inputs))(+ j (second inputs)))
				       (eql (aref array (+ i (first inputs)) (+ j (second inputs))) '@))
				  (if (check-path (+ i (first inputs)) (+ j (second inputs)) i j array)
				      (let () (setf vflag t)
					   (flip-pieces (+ i (first inputs))(+ j (second inputs)) i j array)))))))))
  vflag)


;;;; Function Name: check-bounds
;;;
;;;  Purpose: Check to see if a move is on the board
;;;  Returns: t or nil
;;;  Arguments: row and column of the location to check
;;;  Keywords: n/a
;;;  Side Effects: none
;;;  Error Handling: n/a
;;;  Calls: none
;;;  Called By: validate-move, validate-weight-move, black-valid, white-valid
;;;
;;;  Change Log:
;;;
;;;  1. Eric Solomon   11-07-07
;;;     - wrote function

(defun check-bounds (row col)
  (if (and (< row 9)
	   (< col 9)
	   (> row 0)
	   (> col 0)
	   (null (eql col nil)))
      t
    nil))


;;;; Function Name: check-path
;;;
;;;  Purpose: Checks path of a possible valid move for another piece
;;;           of the same color as the current player. No - or + can be in
;;;           the path.
;;;  Returns: null
;;;  Arguments: row, column , row increment, column increment, array
;;;  Keywords: n/a
;;;  Side Effects: none
;;;  Error Handling: n/a
;;;  Calls: check-path
;;;  Called By: black-valid, white-valid, validate-move, validate-weight-move
;;;
;;;  Change Log:
;;;
;;;  1. Kevin Kuhl     11-25-07
;;;     - added array parameter for ai purposes
;;;  2. Eric Solomon   11-07-07
;;;     - wrote function

(defun check-path (row col rinc cinc array)
  (if (or (eql row 0)
	  (eql row 9)
	  (eql col 0)
	  (eql col 9)
	  (eql (aref array row col) '+)
	  (eql (aref array row col) '-))
      nil
    (if (and (eql playermove 'black)
	     (eql (aref array row col) '@))
	t
      (if (and (eql playermove 'white)
	       (eql (aref array row col) '0))
	  t
	(check-path (+ rinc row)(+ cinc col) rinc cinc array)))))


;;;; Function Name: flip-pieces
;;;
;;;  Purpose: Flips pieces on a path to current player's color
;;;           until a piece the same color as the player is found.
;;;  Returns: null
;;;  Arguments: row, column, row increment, column increment, array
;;;  Keywords: n/a
;;;  Side Effects: Changes arrays
;;;  Error Handling: n/a
;;;  Calls: flip-pieces
;;;  Called By: validate-move
;;;
;;;  Change Log:
;;;
;;;  1. Kevin Kuhl   11-25-07
;;;     - added array parameter for ai purposes
;;;  2. Eric Solomon 11-07-07
;;;     - wrote function

(defun flip-pieces (row col rinc cinc array)
  (if (or (and (eql playermove 'black)
	       (eql (aref array row col) '@))
	  (and (eql playermove 'white)
	       (eql (aref array row col) '0)))
      nil
    (if (eql playermove 'black)
	(let () (setf (aref array row col) '@)
	     (flip-pieces (+ rinc row) (+ cinc col) rinc cinc array))
      (if (eql playermove 'white)
	  (let () (setf (aref array row col) '0)
	       (flip-pieces (+ rinc row) (+ cinc col) rinc cinc array))))))


;;;; Function Name: check-valid
;;;
;;;  Purpose: Searches board for valid moves and returns true if at least one
;;;           is found. Also sets 'x'flag2 to true to signal that the player was
;;;           able to move on his previous turn.
;;;  Returns: cvflag
;;;  Arguments: array
;;;  Keywords: n/a  
;;;  Side Effects: Creates a pair_lst, changes variables and arrays (adds +'s)
;;;  Error Handling: n/a
;;;  Calls: black-valid, white-valid
;;;  Called By: move-avail
;;;
;;;  Change Log:
;;;
;;;  1. Kevin Kuhl   11-26-07
;;;     - added pair_lst creation for ai purposes
;;;  2. Eric Solomon 11-09-07
;;;     - wrote function

(defun check-valid (array)
  (setf cvflag nil)
  (if (eql playermove 'black)
      (loop for i from 1 to 8 do
	    (loop for j from 1 to 8
		  do
		  (if (black-valid (list i j) array)
		      (let () (push (list i j) pair_lst)
			   (setf (aref array i j) '+)(setf cvflag t)(setf bflag2 t)))))
    
    (if (eql playermove 'white)
	(loop for k from 1 to 8 do
	      (loop for m from 1 to 8
		    do (if (white-valid (list k m) array)
			   (let () (setf (aref array k m) '+)(setf cvflag t)(setf wflag2 t)
				(push (list k m) pair_lst)))))))cvflag)


;;;; Function Name: black-valid
;;;
;;;  Purpose: Checks a given location to see if a black piece
;;;           can be legally played there.
;;;  Returns: bflag
;;;  Arguments: the location and the array
;;;  Keywords: n/a
;;;  Side Effects: Changes variables
;;;  Error Handling: n/a
;;;  Calls: check-bounds, check-path
;;;  Called By: check-valid
;;;
;;;  Change Log:
;;;
;;;  1. Eric Solomon   11-10-07   
;;;     -wrote function

(defun black-valid (inputs array)
  (setf bflag nil)
  (if (null (eql (aref array (first inputs) (second inputs)) '-))
      ()
    (loop for i from -1 to 1
	  do (loop for j from -1 to 1
		   do (if (and (check-bounds (+ i (first inputs))(+ j (second inputs)))
			       (eql (aref array (+ i (first inputs)) (+ j (second inputs))) '0))
			  (if (check-path (+ i (first inputs)) (+ j (second inputs)) i j array)
			      (setf bflag t))))))bflag)


;;;; Function Name: white-valid
;;;
;;;  Purpose: Checks a given location to see if a white piece
;;;           can be legally played there.
;;;  Returns: wflag
;;;  Arguments: the location and the array
;;;  Keywords: n/a
;;;  Side Effects: Changes variables
;;;  Error Handling: n/a
;;;  Calls: check-bounds, check-path
;;;  Called By: check-valid
;;;
;;;  Change Log:
;;;
;;;  1. Eric Solomon   11-10-07
;;;     -wrote function

(defun white-valid (inputs array)
  (setf wflag nil)
  (if (null (eql (aref array (first inputs) (second inputs)) '-))
      ()
    (loop for i from -1 to 1
	  do (loop for j from -1 to 1
		   do (if (and (check-bounds (+ i (first inputs))(+ j (second inputs)))
			       (eql (aref array (+ i (first inputs)) (+ j (second inputs))) '@))
			  (if (check-path (+ i (first inputs)) (+ j (second inputs)) i j array)
			      (setf wflag t))))))wflag)


;;;; Function Name: clear plusses
;;;
;;;  Purpose: Clears all the +'s from an array
;;;  Returns: null
;;;  Arguments: The array
;;;  Keywords: n/a
;;;  Side Effects: Changes arrays
;;;  Error Handling: n/a
;;;  Calls: none
;;;  Called By: random-ai, check-min, flip-ai, weight-ai,
;;;             flip-weight-ai, check-min-flip-weight-ai,
;;;             play, humanmove
;;;
;;;  Change Log:
;;;
;;;  1. Kevin Kuhl     11-26-07
;;;     - added array parameter for ai purposes
;;;  2. Eric Solomon   11-19-07
;;;     - wrote function
  
(defun clear-plusses (array)
  (loop for i from 1 to 8 do
	(loop for j from 1 to 8 do
	      (if (eql (aref array i j) '+)
		  (setf (aref array  i j) '-)))))


;;;; Function Name: copy-a1
;;;
;;;  Purpose: Copies a1 array to ai-array
;;;  Returns: null
;;;  Arguments: none
;;;  Keywords: n/a
;;;  Side Effects: Changes ai-array
;;;  Error Handling: n/a
;;;  Calls: none
;;;  Called By: flip-ai, flip-weight-ai
;;;
;;;  Change Log:
;;;
;;;  1. Eric Solomon    11-26-07
;;;     -wrote function

(defun copy-a1 ()
  (setf ai-array (make-array '(9 9) :initial-element '-))
  (loop for i from 0 to 8 do
	(loop for j from 0 to 8 do
	      (setf (aref ai-array i j) (aref a1 i j)))))


;;;; Function Name: copy-ai-arr
;;;
;;;  Purpose: Copies ai-array to ai-array2
;;;  Returns: null
;;;  Arguments: none
;;;  Keywords: n/a
;;;  Side Effects: Changes ai-array2
;;;  Error Handling: n/a
;;;  Calls: none
;;;  Called By: check-min, check-min-flip-weight-ai
;;;
;;;  Change Log:
;;;
;;;  1. Kevin Kuhl   11-28-07
;;;     -wrote function
	      
(defun copy-ai-arr ()
  (setf ai-array2 (make-array '(9 9) :initial-element '-))
  (loop for i from 0 to 8 do
	(loop for j from 0 to 8 do
	      (setf (aref ai-array2 i j) (aref ai-array i j)))))


; AI ONE - RANDOMLY CHOOSES -------------------------

;;;; Function Name: random-ai
;;;
;;;  Purpose: Plays a random available move
;;;  Returns: null
;;;  Arguments: none
;;;  Keywords: n/a
;;;  Side Effects: Prints to screen and changes variables and arrays
;;;  Error Handling: n/a
;;;  Calls: move-avail, clear-plusses, count-avail-moves, play-ai-piece
;;;  Called By: play
;;;
;;;  Change Log:
;;;
;;;  1. Eric Solomon   12-05-07
;;;     -added printout
;;;  2. Eric Solomon   11-25-07
;;;     - wrote function

(defun random-ai ()
  (if (move-avail a1)
      (let ()
	(setf movecount 0)
	(clear-plusses a1)		
	(count-avail-moves pair_lst)
	(setf playnum (random movecount (make-random-state t)))
	(print pair_lst)
	(format t "~%Computer places a piece at ")
	(princ (nth playnum pair_lst))
	(format t "~%")
	(play-ai-piece (nth playnum pair_lst) a1 playermove)
	(setf playermove 'black)
	(setf humanturn t))))
    

;;;; Function Name: play-ai-piece
;;;
;;;  Purpose: Plays a piece for an AI on a given array
;;;  Returns: null
;;;  Arguments: The location, array, and player color
;;;  Keywords: n/a
;;;  Side Effects: Changes arrays
;;;  Error Handling: n/a
;;;  Calls: validate-move 
;;;  Called By: play, check-min, flip-ai, weight-ai, flip-weight-ai, random-ai,
;;;             check-min-flip-weight-ai
;;;
;;;  Change Log:
;;;
;;;  1. Kevin Kuhl   11-27-07
;;;     - added parameters for ai purposes
;;;  2. Eric Solomon 11-25-07
;;;     - wrote function

(defun play-ai-piece (inputs array color) 
  (if (validate-move inputs array)
      (if (eql color 'white)
	  (setf (aref array (first inputs) (second inputs)) '0)
	;else
	(setf (aref array (first inputs) (second inputs)) '@))))


;;;; Function Name: count-avail-moves
;;;
;;;  Purpose: counts the number of available moves
;;;  Returns: nil
;;;  Arguments: list of available moves
;;;  Keywords: n/a
;;;  Side Effects: Changes movecount
;;;  Error Handling: n/a
;;;  Calls: count-avail-moves
;;;  Called By: random-ai
;;;
;;;  Change Log:
;;;
;;;  1. Eric Solomon   11-25-07
;;;    -wrote function

(defun count-avail-moves (lst)
  (if (null lst)
      ()
    (let ()
      (setf movecount (+ 1 movecount))
      (count-avail-moves (rest lst)))))
		   

; AI TWO - PLAYS MOST FLIPS -------------------------


;;;; Function Name: check-min
;;;
;;;  Purpose: Runs through min-max subtrees for one step and finds
;;;           the lowest min value of wcount for each subtree
;;;  Returns: null
;;;  Arguments: list of moves for black after white plays a moves
;;;  Keywords: n/a
;;;  Side Effects: Changes variables, lists, and arrays
;;;  Error Handling: n/a
;;;  Calls: copy-ai-arr, clear-plusses, check-min, move-avail,
;;;         find-min, count-white, play-ai-piece
;;;  Called By: flip-ai
;;;
;;;  Change Log:
;;;
;;;  1. Kevin Kuhl     12-01-07
;;;     -added 1-step lookahead
;;;  2. Kevin Kuhl     11-30-07
;;;     -wrote function

(defun check-min (&optional pairs)
  (if (eql first_min 't)
      (let ()
	(copy-ai-arr)
	(setf playermove 'black)
	(clear-plusses ai-array)
	(clear-plusses ai-array2)
	(move-avail ai-array2)
	(setf playermove 'white)
	(setf first_min nil)
	(check-min pair_lst))
         ;else
    (let ()
      (if (null pairs)
	  (find-min min_lst)
	;else
	(let ()
	  (copy-ai-arr);copies ai-array to ai-array2
	  (count-white ai-array2);count before the opponent move
	  (setf min-count1 wcount)
	  (clear-plusses ai-array2)
	  (setf playermove 'black) ;setting playermove to black for validation
	  (play-ai-piece (first pairs) ai-array2 playermove)
	  (setf playermove 'white) ;resetting playermove to ai's turn
	  (count-white ai-array2);count after the opponent move
	  (setf min-count2 wcount)
	  (push (- min-count2 min-count1) min_lst) ;Add the loss to the minimum list
	  (check-min (rest pairs)))))))


;;;; Function Name: flip-ai
;;;
;;;  Purpose: Plays move for white that will flip the most pieces
;;;  Returns: null
;;;  Arguments: list of possible moves
;;;  Keywords: n/a
;;;  Side Effects: Changes variables, arrays, and lists
;;;  Error Handling: n/a
;;;  Calls: move-avail, copy-a1, clear-plusses, count-white, play-ai-piece,
;;;         count-white, check-min, flip-ai
;;;  Called By: play
;;;
;;;  Change Log:
;;;
;;;  1. Kevin Kuhl     11-29-07
;;;     -wrote function

(defun flip-ai (pairs)
  (if (null pairs)
      ()
    (let ()
      (setf ai-count1 0)
      (setf ai-count2 0)
      (setf min-count1 0)
      (setf min-count2 0)
      (setf minimum 100)
      (if (move-avail a1)
          (let ()
            (copy-a1)
            (clear-plusses a1)
            (count-white ai-array)
            (setf ai-count1 wcount) ; total # of white pieces before playing a move

            ;Begin AI Moving Process
            (play-ai-piece (first pairs) ai-array playermove)
            (count-white ai-array) ; total # of white pieces after playing a move
            (setf ai-count2 wcount) ; number of newly flipped pieces (net gain)
            (setf first_min 't)
            (check-min)
            (push (cons (+ (- ai-count2 ai-count1) minimum) (list (first pairs))) heur_lst)
            (flip-ai (rest pairs)))))))


;;;; Function Name: find-min
;;;
;;;  Purpose: Finds the lowest minimum value in a list
;;;  Returns: null
;;;  Arguments: A list
;;;  Keywords: n/a
;;;  Side Effects: Changes minimum
;;;  Error Handling: n/a
;;;  Calls: find-min
;;;  Called By: check-min, check-min-flip-weight-ai
;;;
;;;  Change Log:
;;;
;;;  1. Kevin Kuhl   11-29-07
;;;     -wrote function

(defun find-min (lst)
  (if (null lst)
      ()
    ;else
    (let ()
      (if (< (first lst) minimum)
	  (setf minimum (first lst)))
      (find-min (rest lst)))))	


;;;; Function Name: pick-move-flip-ai
;;;
;;;  Purpose: Picks the best move based on flip amount
;;;  Returns: null
;;;  Arguments: A list of moves and flip amounts
;;;  Keywords: n/a
;;;  Side Effects: Changes best-move and flip-amount
;;;  Error Handling: n/a
;;;  Calls: pick-move-flip-ai
;;;  Called By: play
;;;
;;;  Change Log:
;;;
;;;  1. Eric Solomon   11-27-07
;;;     -wrote function

(defun pick-move-flip-ai (lst)
  (if (null lst)
      ()
    (let ()
      (if (> (first (first lst)) flip-amount)
	  (let ()  
	    (setf best-move (second (first lst)))
	    (setf flip-amount (first (first lst)))))
      (pick-move-flip-ai (rest lst)))))


; AI THREE - WEIGHTED AI ------------------------------------------------


;;;; Function Name: generate-weight-board
;;;
;;;  Purpose: creates the weighted board for the weight ai
;;;  Returns: null
;;;  Arguments: none
;;;  Keywords: n/a
;;;  Side Effects: Creates the weight-array
;;;  Error Handling: n/a
;;;  Calls: none
;;;  Called By: play
;;;
;;;  Change Log:
;;;
;;;  1. Eric Solomon   12-02-07
;;;     -updated weights
;;;  2. Eric Solomon   12-01-07
;;;     -wrote function

(defun generate-weight-board ()
  (setf weight-array (make-array '(9 9) :initial-element 1))
  (setf (aref weight-array 1 1) 50
	(aref weight-array 1 8) 50
	(aref weight-array 8 1) 50
	(aref weight-array 8 8) 50
	(aref weight-array 1 2) -5
	(aref weight-array 2 1) -5
	(aref weight-array 1 7) -5
	(aref weight-array 2 8) -5
	(aref weight-array 7 1) -5
	(aref weight-array 8 2) -5
	(aref weight-array 7 8) -5
	(aref weight-array 8 7) -5
	(aref weight-array 2 2) -10
	(aref weight-array 2 7) -10
	(aref weight-array 7 2) -10
	(aref weight-array 7 7) -10
	(aref weight-array 1 3) 5
	(aref weight-array 3 1) 5
	(aref weight-array 1 6) 5
	(aref weight-array 3 8) 5
	(aref weight-array 6 1) 5
	(aref weight-array 8 3) 5
	(aref weight-array 6 8) 5
	(aref weight-array 8 6) 5
	(aref weight-array 1 4) 3
	(aref weight-array 1 5) 3
	(aref weight-array 4 1) 3
        (aref weight-array 5 1) 3
        (aref weight-array 4 8) 3
        (aref weight-array 5 8) 3
        (aref weight-array 8 4) 3
        (aref weight-array 8 5) 3))


;;;; Function Name: weight-ai
;;;
;;;  Purpose: Plays a white piece based on weights of available moves
;;;  Returns: null
;;;  Arguments: none
;;;  Keywords: n/a
;;;  Side Effects: Changes variables, arrays
;;;  Error Handling: n/a
;;;  Calls: move-avail, clear-plusses, play-ai-piece
;;;  Called By: play
;;;
;;;  Change Log:
;;;
;;;  1. Eric Solomon   12-01-07
;;;     -wrote function

(defun weight-ai ()
  (setf best-weight 0)
  (setf temp-weight 0)
  (move-avail a1)
  (clear-plusses a1)
  (setf best-weight-move (first pair_lst)) 
  (get-best-weight-move pair_lst a1)
  (play-ai-piece best-weight-move a1 playermove)
  (setf playermove 'black)
  (setf humanturn t))


;;;; Function Name: get-best-weight-move
;;;
;;;  Purpose: Gets the best move based on weights
;;;  Returns: null
;;;  Arguments: list of moves and the array
;;;  Keywords: n/a
;;;  Side Effects: Changes temp-weight
;;;  Error Handling: n/a
;;;  Calls: validate-weight-move, get-best-weight-move
;;;  Called By: weight-ai, flip-weight-ai, check-min-flip-weight-ai
;;;
;;;  Change Log:
;;;
;;;  1. Kevin Kuhl     12-05-07
;;;     -added array parameter for hybrid
;;;  2. Eric Solomon   12-01-07
;;;     -wrote function

(defun get-best-weight-move (lst arr)
  (if (null lst)
      ()
    (let ()
      (validate-weight-move (first lst) arr)
      (setf temp-weight 0)
      (get-best-weight-move (rest lst) arr))))


;;;; Function Name: get-location weight
;;;
;;;  Purposes: Returns the weight of a given location
;;;  Returns: location weight
;;;  Arguments: row and column of location
;;;  Keywords: n/a
;;;  Side Effects: none
;;;  Error Handling: n/a
;;;  Calls: none
;;;  Called By: validate-weight-move, check-path-weight 
;;;
;;;  Change Log:
;;;
;;;  1. Eric Solomon   12-01-07
;;;     -wrote function

(defun get-location-weight (row col)
  (aref weight-array row col))


;;;; Function Name: validate-weight-move
;;;
;;;  Purpose: Checks neighbors of location and if valid path is found, calls
;;;           check-path-weight to add up all the weights for the pieces that will
;;;           be flipped on that path.
;;;  Returns: vflag
;;;  Arguments: The location and array
;;;  Keywords: n/a
;;;  Side Effects: Changes variables
;;;  Error Handling: n/a
;;;  Calls: check-bounds, check-path, check-path-weight
;;;  Called By: get-best-weight-move
;;;
;;;  Change Log:
;;;
;;;  1. Kevin Kuhl     12-05-07
;;;     -added array for hybrid
;;;  2. Eric Solomon   12-01-07
;;;     -wrote function

(defun validate-weight-move (inputs array)
  (setf vflag nil)
  (if (and (numberp (first inputs)) ; checking for valid grid location and inputs are numbers
	   (numberp (second inputs))
           (< (first inputs) 9)
           (< (second inputs) 9)
           (> (first inputs) 0)
           (> (second inputs) 0)
           (or (eql (aref array (first inputs) (second inputs)) '-)
               (eql (aref array (first inputs) (second inputS)) '+)))
      
      (loop for i from -1 to 1
	    do (loop for j from -1 to 1
		     do (if (or (and (eql playermove 'white)
				     (check-bounds (+ i (first inputs))(+ j (second inputs)))
				     (eql (aref array (+ i (first inputs)) (+ j (second inputs))) '@))
				(and (eql playermove 'black)
				     (check-bounds (+ i (first inputs))(+ j (second inputs))) 
				     (eql (aref array (+ i (first inputs)) (+ j (second inputs))) '0)))
			    
			    (if (check-path (+ i (first inputs)) (+ j (second inputs)) i j array)
				(let ()
				  (if (null (eql temp-weight 0))
				      (setf temp-weight (- temp-weight (get-location-weight (first inputs) (second inputs)))))
				  (check-path-weight (first inputs)(second inputs) i j array)
				  (if (> temp-weight best-weight)
				      (let ()
					(setf best-weight temp-weight)
					(setf best-weight-move inputs)
					(setf vflag t)))))))))
  vflag)


;;;; Function Name: check-path-weight
;;;
;;;  Purpose: Traverses a valid path and adds all the weights up
;;;  Returns: null
;;;  Arguments: row, column, row increment, column increment, array
;;;  Keywords: n/a
;;;  Side Effects: Changes temp-weight
;;;  Error Handling: n/a
;;;  Calls: get-location-weight, check-path-weight
;;;  Called By: validate-weight-move
;;;
;;;  Change Log:
;;;
;;;  1. Eric Solomon   12-07-07
;;;     -modified temp-weight calculation
;;;  2. Eric Solomon   12-02-07
;;;     -wrote function

(defun check-path-weight (row col rinc cinc array)
  (if (or (eql row 0)
          (eql row 9)
          (eql col 0)
          (eql col 9)
          (eql (aref array row col) '+))
      nil
    
    (if (or (and (eql playermove 'white)
		 (eql (aref array row col) '0))  
	    (and (eql playermove 'black)
	         (eql (aref array row col) '@)))
	t
      (let ()
	(setf temp-weight (+ temp-weight (get-location-weight row col)))
	(check-path-weight (+ rinc row)(+ cinc col) rinc cinc array)))))


; FLIP + WEIGHT AI WITH ONE STEP LOOKAHEAD --------------------------------------------------


;;;; Function Name: flip-weight-ai
;;;
;;;  Purpose: Plays white piece based on amount of flips gained and total
;;;           weight gained, using 1-step lookahead.
;;;  Returns: null
;;;  Arguments: A list of moves
;;;  Keywords: n/a
;;;  Side Effects: Changes variables, arrays, and lists
;;;  Error Handling: n/a
;;;  Calls: move-avail, copy-a1, clear-plusses, count-white, play-ai-piece
;;;         check-min-flip-weight-ai, get-best-weight-move, flip-weight-ai
;;;  Called By: play
;;;
;;;  Change Log:
;;;
;;;  1. Eric Solomon  12-06-07
;;;     Kevin Kuhl     
;;;     - modified flip-ai into this function

(defun flip-weight-ai (pairs)
  (if (null pairs)
      ()
    (let ()
      (setf ai-count1 0)
      (setf ai-count2 0)
      (setf min-count1 0)
      (setf min-count2 0)
      (setf minimum 100)
      (setf prevmove nil)
      (if (move-avail a1)
          (let ()
            (copy-a1)
            (clear-plusses a1)
            (count-white ai-array)
            (setf ai-count1 wcount) ; total # of white pieces before playing a move
	    
            ;Begin AI Moving Process
            (play-ai-piece (first pairs) ai-array playermove)
            (count-white ai-array) ; total # of white pieces after playing a move
            (setf ai-count2 wcount) ; number of newly flipped pieces (net gain)
            (setf first_min 't)
            (check-min-flip-weight-ai)
            (setf best-weight 0)
            (setf temp-weight 0)
            (get-best-weight-move (list (first pairs)) a1)
            (push (cons (+ (+ (- ai-count2 ai-count1) minimum) best-weight) (list (first pairs))) heur_lst)
            (flip-weight-ai (rest pairs)))))))


;;;; Function Name: check-min-flip-weight-ai
;;;
;;;  Purpose: Traverses through subtrees for each move in the list and
;;;           finds the greatest loss for the ai when black plays for the flips
;;;           and adds it to the sum of the total weights for the move.
;;;  Returns: null
;;;  Arguments: A list of moves
;;;  Keywords: n/a
;;;  Side Effects: Changes variables, arrays, and min_lst
;;;  Error Handling: n/a
;;;  Calls: copy-ai-arr, clear-plusses, move-avail, count-white,
;;;         check-min-flip-weight-ai, get-best-weight-move, play-ai-piece
;;;  Called By: flip-weight-ai
;;;
;;;  Change Log:
;;;
;;;  1. Eric Solomon     12-07-07
;;;     Kevin Kuhl
;;;     -modified check-min into this function

(defun check-min-flip-weight-ai (&optional pairs)
  (if (eql first_min 't)
      (let ()
	(copy-ai-arr)
	(setf playermove 'black)
	(clear-plusses ai-array)
	(clear-plusses ai-array2)
	(move-avail ai-array2)
	(setf playermove 'white)
	(setf first_min nil)
	(check-min-flip-weight-ai pair_lst))
    ;else
    (let ()
      (if (null pairs)
	  (find-min min_lst)
        ;else
        (let ()
	  (copy-ai-arr)   ;copies ai-array to ai-array2
	  (count-white ai-array2) ;count before the opponent move
	  (setf min-count1 wcount)
	  (clear-plusses ai-array2)
	  (setf playermove 'black) ;setting playermove to black for validation
	  (setf temp-weight 0)
	  (setf best-weight 0)
	  (get-best-weight-move (list (first pairs)) ai-array)
	  (play-ai-piece (first pairs) ai-array2 playermove)
	  (setf playermove 'white) ;resetting playermove to ai's turn
	  (count-white ai-array2) ;count after the opponent move
	  (setf min-count2 wcount)
	  (push (- (- min-count2 min-count1) best-weight) min_lst)  ;Add the loss to the minimum list
	  (check-min-flip-weight-ai (rest pairs)))))))
