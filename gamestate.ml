open Board


type player = {name: string; mutable score: int}
type coordinate = char*int
type game = grid
type move = string*coordinate

let gen_random_tiles (clist: char list) (n: int) : char list option =
  failwith "HI PETER"

let get_winner (plist: player list) : player =
  failwith "megan is the winner"

let get_scores (plist: player list) : unit =
  failwith "megnas score > peters score"

let word_score (board: game) (turn: move) : int =
  failwith "plus is poopy"

let play_word (board: game) (turn: move) : game =
  failwith "no"

let exchange (board: game) (tiles: string) : game =
  failwith "no"

let main (board: game) : unit =
  failwith "no"
