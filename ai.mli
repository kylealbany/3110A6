open Gamestate
open Dict
open Board

type ai_command = AI_Pass | AI_Exchange of string | AI_Play of move

(* Selects a word for the AI player to play *)
val choose_word : game -> char list -> dict -> char list -> bool -> ai_command




