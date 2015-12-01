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

(* Takes in bytes list s, and integer n. Removes random elements from s until
   it only contains n elements.*)
let rec remove_chars s n =
  let l = List.length s in
  if l <= n then s
  else let index = Random.int l in
  let rec remove_char s index i =
    match s with
    | [] -> failwith "reached end in remove_chars without reaching index"
    | hd::tl -> if i = index then tl else hd::(remove_char tl index (i+1))
  in
  remove_chars (remove_char s index 0) n

(* Replace wildcards in char list chars with character *)
let rec replace_wildcards chars character =
  match chars with
  | [] -> []
  | hd::tl -> if hd = '*' then character::(replace_wildcards tl character)
              else hd::(replace_wildcards tl character)

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
(* n is the length of permutations to find. Select n random characters from s and
   return all permutations of them of length n *)
let permutation s n =
  let rec perm s =
    match s with
    | [] -> [[]]
    | hd::tl -> List.fold_left (fun acc x -> List.rev_append (distribute hd x) acc)
      [] (perm tl)
  in

  let s = remove_chars s n in

  let blist_perm = perm s in
    List.map blist_to_string blist_perm


let string_compare_helper s1 s2 =
  if (s1 = s2) then 0
  else if (s1 < s2) then -1
  else 1

(* Given a list of tiles, finds a subset of length n and finds valid words from
  all of the permutations of that subset*)
let get_valid_words dict chars n =
  let strings = permutation chars n in
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



(* Gets the score of an individual word, not taking the board of wildcareds into
   account at all. *)
let get_word_score word =
    let score_ref = ref 0 in
    (score_ref := 0);
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

let compare_score word1 word2 =
  let score1 = get_word_score word1 in
  let score2 = get_word_score word2 in
  if score1 >= score2 then
    if score1=score2 then 0 else -1
  else 1

(* Simple function to choose the best word to play. Uses the tile values to
   come up with a score for each playable word, and then will return the word
   with the highest score. Does not take into account board multipliers, wildcard
   tiles, or other connecting words. Can improve it to increase AI difficulty *)
let rec choose_best_word words =
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

(* Given available chars, try many different subsets of them of different lengths
  and accumulate all of the playable words that are found.
 Increasing the upper limits of x for the loops can increase difficulty*)
let try_tile_subsets dict chars=
  let possible_words = ref [] in
  (* Max length is 8, higher gives stack overflow finding permutations *)
  for x = 0 to 2 do
    (possible_words := !possible_words @ (get_valid_words dict chars 8))
  done;
  for x = 0 to 4 do
    (possible_words := !possible_words @ (get_valid_words dict chars 7))
  done;
  for x = 0 to 8 do
    (possible_words := !possible_words @ (get_valid_words dict chars 6))
  done;
  for x = 0 to 16 do
    (possible_words := !possible_words @ (get_valid_words dict chars 5))
  done;
  for x = 0 to 32 do
    (possible_words := !possible_words @ (get_valid_words dict chars 4))
  done;
  for x = 0 to 64 do
    (possible_words := !possible_words @ (get_valid_words dict chars 3))
  done;
  for x = 0 to 128 do
    (possible_words := !possible_words @ (get_valid_words dict chars 2))
  done;
  List.sort_uniq compare_score !possible_words



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
let try_above dict tiles line n =
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
    let choices = get_valid_words dict (last_letter::tiles) n in
    List.filter
      (fun x -> (String.length x <= (max_len +1))
      && (x.[(String.length x) -1] = last_letter)) choices
  else []



(* [tiles] byte list, [line] cell list, returns a list of possible words
 * if space available below and last tile is isolated *)
let try_below dict tiles line n =
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
    let choices = get_valid_words dict (first_letter::(tiles)) n in
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


