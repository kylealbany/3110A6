

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

let string_compare_helper s1 s2 =
  if (s1 = s2) then 0
  else if (s1 < s2) then -1
  else 1

let get_valid_words dict chars =
  let strings = permutation chars in
  (* Change search_limit to increase AI difficulty*)
  let search_limit = 10 in
  let words = get_i_words dict strings search_limit 0 in
  List.sort_uniq string_compare_helper words

let rec get_i_words dict strings (max : int) (i : int)  =
  if i = max then [] else
  match strings with
  | [] -> []
  | hd::tl -> if (member dict hd) then hd::(get_i_words dict tl max (i+1))
              else get_i_words dict tl max i


(* Simple function to choose the best word to play. Uses the tile values to
   come up with a score for each playable word, and then will return the word
   with the highest score. Does not take into account board multipliers, wildcard
   tiles, or other connecting words. Can improve it to increase AI difficulty *)
let rec choose_best_word words =
  let get_word_score word =
    let score = ref 0 in
    let get_char_score character =
      match character with
      | 'a' -> score_ref := !score_ref + 1
      | 'b' -> score_ref := !score_ref + 3
      | 'c' -> score_ref := !score_ref + 3
      | 'd' -> score_ref := !score_ref + 2
      | 'e' -> score_ref := !score_ref + 1
      | 'f' -> score_ref := !score_ref + 4
      | 'g' -> score_ref := !score_ref + 2
      | 'h' -> score_ref := !score_ref + 4
      | 'i' -> score_ref := !score_ref + 1
      | 'j' -> score_ref := !score_ref + 8
      | 'k' -> score_ref := !score_ref + 5
      | 'l' -> score_ref := !score_ref + 1
      | 'm' -> score_ref := !score_ref + 3
      | 'n' -> score_ref := !score_ref + 1
      | 'o' -> score_ref := !score_ref + 1
      | 'p' -> score_ref := !score_ref + 3
      | 'q' -> score_ref := !score_ref + 10
      | 'r' -> score_ref := !score_ref + 1
      | 's' -> score_ref := !score_ref + 1
      | 't' -> score_ref := !score_ref + 1
      | 'u' -> score_ref := !score_ref + 1
      | 'v' -> score_ref := !score_ref + 4
      | 'w' -> score_ref := !score_ref + 4
      | 'x' -> score_ref := !score_ref + 8
      | 'y' -> score_ref := !score_ref + 4
      | 'z' -> score_ref := !score_ref + 10
      | _ -> failwith "Invalid character encountered in Ai.choose_best_word"
    in
    (String.iter get_char_score word); !score_ref
  in
  let max_score = ref 0 in
  let best_word = ref "" in
  let rec find_best words =
    match words with
    | [] -> !best_word
    | hd::tl -> let score = (get_word_score hd) in
                if score < !max_score then find_best tl
                else ((max_score := score); (best_word := hd); (find_best tl))
    in
  find_best words

let gen_word_list game = failwith "unimplimented"
(*   let tiles = game.tiles in
  let perms = permutation tiles in *)

let choose_word movelist =
  failwith "Kyle's a dick"

let should_ex movelist =
  failwith "Baaaaaaaaaaaaaaaaaaaales"

let exchange_tiles game =
  failwith "JK Kyle's actually a pretty cool guy"


