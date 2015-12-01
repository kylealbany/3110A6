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


let rec get_i_words dict strings (max : int) (i : int)  =
  if i = max then [] else
  match strings with
  | [] -> []
  | hd::tl -> if (member dict hd) then hd::(get_i_words dict tl max (i+1))
              else get_i_words dict tl max i

let get_valid_words dict chars =
  let strings = permutation chars in
  (* Change search_limit to increase AI difficulty*)
  let search_limit = 10 in
  get_i_words dict strings search_limit 0



(* return the char at letter at index [n] in [line] which is a list of tiles *)
let rec get_nth_letter line n =
  match n with
  | 0 -> (List.hd line).letter (* board is always 15*15 so List.hd is safe *)
  | _ -> begin match line with
         | [] -> None
         | hd::tl-> get_nth_letter tl (n-1)
         end

(* return the number of open tiles before first occupied tile in [line]*)
let space_above line =
  let rec helper l count =
    match l with
    | [] -> count
    | hd::tl -> if hd.letter = None then helper tl (count + 1) else count
  in helper line 0

(* return the number of open tiles after the last occupied tile in [line] *)
let space_below line =
  space_above (List.rev line)


(* given a tile list [tiles] return a list of each tiles letters as bytes *)
let filter_tiles tiles =
  let l = List.map (fun t -> t.letter) tiles in
  let no_none =  List.filter
  (fun t -> match t with | Some _ -> true | _ -> false) l in
   List.map
  (fun t -> match t with | Some x -> x | _ -> failwith "error filter") no_none

(* [tiles] is AI's current rack as byte list, [line] is one row or col in a grid
 * returns a list of playable words
 * assuming the last letter in line is isolated
 * i.e _ _ _ e _ would allow 4 letter words that end in e *)
let try_above dict tiles line =
  let max_len = space_above line in
  let last_letter = match (get_nth_letter line max_len) with
                      | Some x -> x
                      | None -> failwith "outside board"
    in
  let after_last_empty = match (get_nth_letter line (max_len + 1)) with
                      | Some x -> false
                      | None -> true
    in
  (* if space available above and last tile is isolated return words that fit*)
  if (after_last_empty && max_len > 0) then
    let choices = get_valid_words dict (last_letter::tiles) in
    List.filter
      (fun x -> (String.length x <= (max_len +1))
      && (x.[(String.length x) -1] = last_letter)) choices
  else []



(* [tiles] byte list, [line] cell list, returns a list of possible words
 * if space available below and last tile is isolated *)
let try_below dict tiles line =
  let line = List.rev line in
  let max_len = space_above line in
  let first_letter = match (get_nth_letter line max_len) with
                      | Some x -> x
                      | None -> failwith "outside board"
    in
  let before_first_empty = match (get_nth_letter line (max_len + 1)) with
                      | Some x -> false
                      | None -> true
    in
  (* if space available above and last tile is isolated return words that fit*)
  if (before_first_empty && max_len > 0) then
    let choices = get_valid_words dict (first_letter::(tiles)) in
    List.filter
      (fun x -> (String.length x <= (max_len +1))
      && (x.[0] = first_letter)) choices
  else []

(* let try_below dict tiles line =
  let max_len = space_below line in
  let index = (List.length line) - max_len -1 in
  let first_letter = match (get_nth_letter line index) with
                      | Some x -> x
                      | None -> failwith "outside board"
    in
  let before_first_empty = match (get_nth_letter line (index - 1)) with
                      | Some x -> false
                      | None -> true
    in
  (* if space available below and first tile is isolated return words that fit*)
  if (before_first_empty && max_len > 0) then
    let choices = get_valid_words dict (first_letter::(filter_tiles tiles)) in
    List.filter
      (fun x -> (String.length x <= (max_len+1))
      && (x.[0] = first_letter)) choices
  else [] *)




let gen_word_list game = failwith "unimplimented"
(*   let tiles = game.tiles in
  let perms = permutation tiles in *)

let choose_word movelist =
  failwith "Kyle's a dick"

let should_ex movelist =
  failwith "Baaaaaaaaaaaaaaaaaaaales"

let exchange_tiles game =
  failwith "JK Kyle's actually a pretty cool guy"


