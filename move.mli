open Board

type coordinate = char * int
type move = string * direction * coordinate


(* Removes the first occurance of a character element from the character list *)
val remove_elt : char list -> char -> char list

(* Takes the old board and the move (word to be played, the coordinates of
   the starting tile, and the direction) and returns the score to add to the
   player’s score *)
val word_score : game -> move -> int

(* Takes in current board and a proposed move to be played and returns whether
   the move is legal on the current game board *)
val valid_move : game -> move -> bool
(***TODO - fixed valid move***)

(* Takes in the the current board, and a proposed move to be played and
    returns true if the word being played, any words it may extend, and any
    perpendicular words it may create are valid words according to the Official
    Scrabble Player Dictionary *)
val valid_word : game -> move -> bool