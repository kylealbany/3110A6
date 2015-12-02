open Board
open Move

type player = {name: string; score: int; isCPU: bool; rack: char list}
(* type coordinate = char * int
type move = string * direction * coordinate *)
type command = Help | Quit | Pass | Shuffle | Score | Board | Play of move
    | Exchange of string | Unknown of string


(* Returns random tiles at the beginning of the game and every time a word
   is played *)
val gen_random_tiles : char list -> int -> char list * char list

(* Returns player that has the highest score *)
val get_winner :  player list -> player

(* Prints out the current players score *)
val get_score : player list -> unit

(* Takes the old board and the move (word to be played, the coordinates of
   the starting tile, and the direction) and returns the score to add to the
   player’s score *)
val word_score : game -> move -> int

(* Takes in current board and a proposed move to be played and returns whether
   the move is legal on the current game board *)
val valid_move : game -> move -> bool


(* Takes in the the current board, and a proposed move to be played and
    returns true if the word being played, any words it may extend, and any
    perpendicular words it may create are valid words according to the Official
    Scrabble Player Dictionary *)
val valid_word : game -> move -> bool

(* Exchanges tiles, which involves the player selecting any number of tiles
   on his/her rack to swap with random tiles in the bag*)
val exchange : char list -> string -> char list -> char list * char list

(* REPL function *)
val main : (game -> char list -> player list -> bool -> bool ->
            game * char list * player list)

