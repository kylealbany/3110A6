type key
type value
type dict

(* Return the empty dictionary *)
val empty : dict

(* Returns true if and only if the key is in the dictionary. *)
val member : dict -> key -> bool

(* Inserts a (key,value) pair into our dictionary. If the key is already
   in our dictionary, update the key to have the new value. *)
val insert : dict -> key -> dict

