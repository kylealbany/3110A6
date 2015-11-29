open RadixTree

type key = string
type value = string
type dict = string PString.t

let empty = PString.empty

let lookup dict key =
  try
      Some (PString.find key dict)
  with
      Not_found -> None

let member dict value =
  try
    let _ = PString.find value dict in true
  with
      Not_found -> false

(* currently doesn't support overwriting an existing key's value *)
let insert dict key value =
  let present = member dict value in
  match present with
  | false -> let new_map = PString.add key value dict in new_map
  | true -> (* let _ = PString.remove key dict in *)
            let new_map = PString.add key value dict in new_map

