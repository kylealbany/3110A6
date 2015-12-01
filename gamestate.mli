type player = {name: string; mutable score: int; isCPU: bool; rack: char list}
type coordinate
type game
type move


(* Returns random tiles at the beginning of the game and every time a word
   is played *)
val gen_random_tiles : char list -> int -> char list * char list

(* Returns player that has the highest score *)
val get_winner :  player list -> player

(* Prints out all of the players scores *)
val get_scores : player list -> unit

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

(* Plays a word on the board. Assumes only legal words are played. Takes in
   a game, the move (word to be played, the coordinates of the starting tile,
   and the direction) and returns a game with the new board *)
val play_word : game -> move -> game

(* Exchanges tiles, which involves the player selecting any number of tiles
   on his/her rack to swap with random tiles in the bag*)
val exchange : game -> string -> game

(* REPL function *)
val main : game -> unit

