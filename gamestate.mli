type player
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

(* Plays a word on the board. Only allows legal words to be played. Takes in
   a game, the move (word to be played, the coordinates of the starting tile,
   and the direction) and returns a game with the updated score, rack, and
   board *)
val play_word : game -> move -> game

(* Exchanges tiles, which involves the player selecting any number of tiles
   on his/her rack to swap with random tiles in the bag*)
val exchange : game -> string -> game

(* REPL function *)
val main : game -> unit

