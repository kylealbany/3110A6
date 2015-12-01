open RadixTree

type key = string
type value = unit
type dict = unit PString.t

let empty = PString.empty

let member dict key =
  try
    let _ = PString.find key dict in true
  with
      Not_found -> false

(* currently doesn't support overwriting an existing key's value *)
let insert dict key  =
  let present = member dict key in
  match present with
  | false -> let new_map = PString.add key () dict in new_map
  | true -> (* let _ = PString.remove key dict in *)
            let new_map = PString.add key () dict in new_map

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
