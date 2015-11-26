type rows
type columns
type cell
type grid
type direction


(* Creates board with cells that only contain respective multipliers *)
val init_board : unit ->  rows * columns

(* Updates the board to include the player's move of word string starting
 * at index (int, int) in direction string. Returns updated board option *)
val update_board : grid -> string -> char * int -> direction -> grid

(* Prints the current board *)
val print_board : grid -> unit


