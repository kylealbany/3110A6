
type play = Pass | Move of move

(* Selects a word for the AI player to play *)
val choose_word : game -> player -> dict -> play




