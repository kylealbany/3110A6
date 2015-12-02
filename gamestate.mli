open Board
open Move

type player = {name: string; score: int; isCPU: bool; rack: char list}
type command = Help | Quit | Pass | Shuffle | Score | Board | Play of move
    | Exchange of string | Unknown of string


(* Returns random tiles at the beginning of the game and every time a word
   is played *)
val gen_random_tiles : char list -> int -> char list * char list

(* Returns the player(s) that have the highest score *)
val get_winner :  player list -> player list

(* Exchanges tiles, which involves the player selecting any number of tiles
   on his/her rack to swap with random tiles in the bag*)
val exchange : char list -> string -> char list -> char list * char list

(* REPL function *)
val main : (game -> char list -> player list -> bool -> bool ->
            game * char list * player list)

