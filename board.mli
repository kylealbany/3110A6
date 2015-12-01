
type cell = {letter_mult : int; word_mult : int; letter : char option}
type rows = cell list list
type columns = cell list list
type grid = rows * columns
type direction = Down | Across

(* Returns the head of a list if the list is nonempty and None otherwise *)
val safe_hd : 'a list -> 'a option

(* Returns the tail of a list if the list is nonempty and None otherwise *)
val safe_tl : 'a list -> 'a list option

(* Returns the transpose of an 'a list list *)
val transpose : 'a list list -> 'a list list

(* Returns a character list of a string *)
val to_char_list : string -> char list

(* Creates board with cells that only contain respective multipliers *)
val init_board : unit ->  rows * columns

(* Updates the board to include the player's move of word string starting
 * at index (int, int) in direction string. Returns updated board option *)
val update_board : grid -> string -> char * int -> direction -> grid

(* Prints the current board *)
val print_board : grid -> unit


