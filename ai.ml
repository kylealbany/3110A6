(* distribute and permutation modified from
 * http://www.dietabaiamonte.info/79762.html#sthash.QgjGV9wd.dpuf
 * if we need lines we can rewrite ourselves
 *)
let distribute c l =
  let rec insert acc1 acc2 = function
    | [] -> acc2
    | hd::tl -> insert (hd::acc1)((List.rev_append acc1 (hd::c::tl))::acc2) tl
  in insert [] [c::l] l

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
    let choices = try_tile_subsets dict (last_letter::tiles)  in
    let filtered = List.filter
      (fun x -> (String.length x <= (max_len +1))
      && (x.[(String.length x)-1] = last_letter)) choices
    in (filtered,max_len)
  else ([], -1)



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
    let choices = try_tile_subsets dict (first_letter::tiles) in
    let filtered = List.filter
      (fun x -> (String.length x <= (max_len +1))
      && (x.[0] = first_letter)) choices
     in (filtered,max_len)
  else ([],-1)

let gen_down_move words row_index dir =
  let word_list = fst words in
  let col_letter = char_of_int (snd words + 65) in
  let cord = (col_letter,row_index) in
  let f x = (x,dir,cord) in
    List.map f word_list

let gen_above_move words row_index dir =
  let word_list = fst words in
  let col_letter = char_of_int (snd words + 65) in
  let f x = let cord = (col_letter,row_index - ((String.length x) -1)) in (x,dir,cord) in
    List.map f word_list

(* [ai] is a player
 * [game] is a grid of cols and rows
 * iterate through each column and add playable words using [ai] rack
 * to a word list- returns word list
 * note the words are not necessarily playable, just valid within a column *)

let gen_move_list game ai dict =
  (* n can be changed as needed *)
  let tiles = ai.rack in
  let rows = fst game in
  let cols = snd game in
  let num_col = List.length cols in
  let words = ref [] in

 (* collect possible words in each column *)
  for col_index = 0 to (num_col -1) do
    let col = List.nth cols col_index in
    let above_words = try_above dict tiles col  in
    let above_moves = gen_above_move above_words col_index Down in
    let below_words = try_below dict tiles col  in
    let below_moves = gen_down_move below_words col_index Down in
    words := (!words)@above_moves;
    words := (!words)@below_moves;
  done;

  (* collect possible words in each row *)
  let num_rows = (List.length rows -1) in
  for row_index = 0 to (num_rows -1) do
    let row = List.nth rows row_index in
    let left_words = try_above dict tiles row in
    let left_moves = gen_above_move left_words row_index Across in
    let right_words = try_below dict tiles row in
    let right_moves = gen_down_move right_words row_index Across in
    words := (!words)@left_moves;
    words := (!words)@right_moves;
  done;
  (!words)


let compare_scores move1 move2 game =
  let score1 = word_score game move1 in
  let score2 = word_score game move2 in
  if score1 > score2 then -1
  else if score1 = score2 then 0
  else 1



let choose_word game player dict =
  let potential_moves = gen_move_list game player dict in
  let f = fun x -> valid_move game x in
  let f2 = fun x y -> compare_scores x y game in
  let playable_moves = List.filter f potential_moves in
  let sorted_moves = List.sort_uniq f2 playable_moves in

  match sorted_moves with
  | [] -> failwith "TODO"
  | hd::tl -> hd

