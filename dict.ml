open RadixTree

type key = string
type value = unit
type dict = unit PString.t

let empty = PString.empty

(* Returns true if and only if the key is in the dictionary. *)
let member dict key =
  try
    let _ = PString.find key dict in true
  with
      Not_found -> false

(* Inserts a (key,value) pair into our dictionary. If the key is already
   in our dictionary, update the key to have the new value. *)
let insert dict key  =
  let present = member dict key in
  match present with
  | false -> let new_map = PString.add key () dict in new_map
  | true ->  let new_map = PString.add key () dict in new_map

(* Builds a radix tree dictionary when given the file name of a dictionary file
   as input. The file should be formatted as one word per line with nothing else *)
let dict_init fname =
  let dict = ref empty in
  let file = open_in fname in
  try
    while true; do
      dict := insert !dict (input_line file)
    done; !dict
  with End_of_file ->
    close_in file;
    !dict

let ospd = dict_init "ospd.txt"

(* Returns the Official Scrabble Player Dictionary of tye dict *)
let get_ospd () : dict =
  ospd