type key = string
type value = unit
type dict

(* Return the empty dictionary *)
val empty : dict

(* Returns true if and only if the key is in the dictionary. *)
val member : dict -> key -> bool

(* Inserts a (key,value) pair into our dictionary. If the key is already
   in our dictionary, update the key to have the new value. *)
val insert : dict -> key -> dict

(* Builds a radix tree dictionary when given the file name of a dictionary file
   as input. The file should be formatted as one word per line with nothing else *)
val dict_init : string -> dict

(* Returns the Official Scrabble Player Dictionary of tye dict *)
val get_ospd : unit -> dict