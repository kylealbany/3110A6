open RadixTree

type key = string
type value = string
type dict = PString


let empty = PString.empty
let mymap1 = PString.add "t1" "test1" empty
let mymap2 = PString.add "t2" "test2" mymap1
let mymap3 = PString.add "t3" "test3" mymap2

let t1_in = PString.find "t1" mymap1

(* should cause exception *)
(* let t1_in_any = PString.find "t4" mymap3 *)


let lookup dict key =
  try
      Some (PString.find key dict)
  with
      Not_found -> None


let member dict key =
  failwith "unimplemented"

let insert dict key value =
  failwith "unimplemented"