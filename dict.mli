type key
type value
type dict

(* Return the empty dictionary *)
val empty : dict

(* Returns as an option the value associated with the provided key. If
   the key is not in the dictionary, return None. *)
val lookup : dict -> value -> string option

(* Returns true if and only if the key is in the dictionary. *)
val member : dict -> value -> bool

(* Inserts a (key,value) pair into our dictionary. If the key is already
   in our dictionary, update the key to have the new value. *)
val insert : dict -> key -> value -> dict

