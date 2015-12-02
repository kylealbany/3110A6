open Board

type coordinate = char * int
type move = string * direction * coordinate


(* Removes the first occurance of a character element from the character list *)
val remove_elt : char list -> char -> char list

(* Returns the tail of a list starting at the nth element *)
val nth_and_tail : 'a list -> int -> 'a list

(* Returns a sublist of the row/column on the board that the player starting
 * at the respective coordinate index *)
val get_subline : game -> coordinate -> direction -> cell list

(* Takes the old board and the move (word to be played, the coordinates of
   the starting tile, and the direction) and returns the score to add to the
   player’s score *)
val word_score : game -> move -> int

(* Returns true if the given move is a valid first move in the game *)
val valid_first : string -> direction -> coordinate -> bool


(* Takes in current board and a proposed move to be played and returns whether
   the move is legal on the current game board *)
val valid_move : game -> move -> bool

(* Returns true if all of the tiles the player uses that do not currently
 * exist on the board are from the player's rack *)
val valid_rack : game -> move -> char list -> bool

(* Takes in the the current board, and a proposed move to be played and
    returns true if the word being played, any words it may extend, and any
    perpendicular words it may create are valid words according to the Official
    Scrabble Player Dictionary *)
val valid_word : game -> move -> bool