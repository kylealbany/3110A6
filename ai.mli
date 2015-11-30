open Gamestate
open Board



  (* Generates a list of possible words based on the current board
     and the AI’s rack *)
  val gen_word_list : game -> move list

  (* Selects a word for the AI player to play *)
  val choose_word : move list -> move

  (* AI determines if it should exchange tiles *)
  val should_ex : move list -> bool

  (* returns string of tiles AI wants to exchange *)
  val exchange_tiles : game -> string



