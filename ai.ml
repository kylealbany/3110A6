
(* distribute and permutation modified from
 * http://www.dietabaiamonte.info/79762.html#sthash.QgjGV9wd.dpuf
 * if we need lines we can rewrite ourselves
 *)
let distribute c l =
  let rec insert acc1 acc2 = function
    | [] -> acc2
    | hd::tl -> insert (hd::acc1)((List.rev_append acc1 (hd::c::tl))::acc2) tl
  in insert [] [c::l] l

(* accepts a string, returns a byte array *)
(* let to_array s  =
  let rec helper s a =
    match s with
    | "" -> []
    | x ->
      let l = (Bytes.length x) -1 in
      let ch = Bytes.get x l in
      let subs = Bytes.sub x 0 (l-1) in
      helper subs (ch::a)
  in helper s [] *)

(* convert a bytes list to a string *)
let blist_to_string b =
  let rec helper b str =
    match b with
    | [] -> str
    | hd::tl -> let new_s = String.make 1 hd in
                let new_str = String.concat "" (new_s::[str]) in
                  helper tl new_str
  in helper b ""

(* accepts a byte list and returns a string list containing the permutations *)
let permutation s =
  let rec perm s =
    match s with
    | [] -> [[]]
    | hd::tl -> List.fold_left (fun acc x -> List.rev_append (distribute hd x) acc)
      [] (perm tl)
  in
  let blist_perm = perm s in
    List.map blist_to_string blist_perm


let gen_word_list game = failwith "unimplimented"
(*   let tiles = game.tiles in
  let perms = permutation tiles in *)


let choose_word movelist =
  failwith "Kyle's a dick"

let should_ex movelist =
  failwith "Baaaaaaaaaaaaaaaaaaaales"

let exchange_tiles game =
  failwith "JK Kyle's actually a pretty cool guy"


