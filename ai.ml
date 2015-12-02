
type ai_command = AI_Pass | AI_Exchange of string | AI_Play of move

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


let rec get_i_words dict strings (max : int) (i : int)  =
  if i = max then [] else
  match strings with
  | [] -> []
  | hd::tl -> if (member dict hd) then hd::(get_i_words dict tl max (i+1))
              else get_i_words dict tl max i

(* Given a list of tiles, finds a subset of length n and finds valid words from
  all of the permutations of that subset*)
let get_valid_words dict chars n =
  let strings = permutation chars n in
  (* Change search_limit to increase AI difficulty*)
  let search_limit = 10 in
  let words = get_i_words dict strings search_limit 0 in
  List.sort_uniq string_compare_helper words



(* Gets the score of an individual word, not taking the board of wildcareds into
   account at all. *)
let get_word_score word =
    let score_ref = ref 0 in
    (score_ref := 0);
    let get_char_score character =
      match character with
      | 'A' -> score_ref := !score_ref + 1
      | 'B' -> score_ref := !score_ref + 3
      | 'C' -> score_ref := !score_ref + 3
      | 'D' -> score_ref := !score_ref + 2
      | 'E' -> score_ref := !score_ref + 1
      | 'F' -> score_ref := !score_ref + 4
      | 'G' -> score_ref := !score_ref + 2
      | 'H' -> score_ref := !score_ref + 4
      | 'I' -> score_ref := !score_ref + 1
      | 'J' -> score_ref := !score_ref + 8
      | 'K' -> score_ref := !score_ref + 5
      | 'L' -> score_ref := !score_ref + 1
      | 'M' -> score_ref := !score_ref + 3
      | 'N' -> score_ref := !score_ref + 1
      | 'O' -> score_ref := !score_ref + 1
      | 'P' -> score_ref := !score_ref + 3
      | 'Q' -> score_ref := !score_ref + 10
      | 'R' -> score_ref := !score_ref + 1
      | 'S' -> score_ref := !score_ref + 1
      | 'T' -> score_ref := !score_ref + 1
      | 'U' -> score_ref := !score_ref + 1
      | 'V' -> score_ref := !score_ref + 4
      | 'W' -> score_ref := !score_ref + 4
      | 'X' -> score_ref := !score_ref + 8
      | 'Y' -> score_ref := !score_ref + 4
      | 'Z' -> score_ref := !score_ref + 10
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
 (*  for x = 0 to 1 do
    (possible_words := !possible_words @ (get_valid_words dict chars 8))
  done;
  for x = 0 to 2 do
    (possible_words := !possible_words @ (get_valid_words dict chars 7))
  done; *)
  for x = 0 to 4 do
    (possible_words := !possible_words @ (get_valid_words dict chars 6))
  done;
  for x = 0 to 8 do
    (possible_words := !possible_words @ (get_valid_words dict chars 5))
  done;
  for x = 0 to 16 do
    (possible_words := !possible_words @ (get_valid_words dict chars 4))
  done;
  for x = 0 to 32 do
    (possible_words := !possible_words @ (get_valid_words dict chars 3))
  done;
  for x = 0 to 64 do
    (possible_words := !possible_words @ (get_valid_words dict chars 2))
  done;
  List.sort_uniq compare_score !possible_words


(* return the number of open tiles before first occupied tile in [line] *)
let space_above line =
  let rec helper l count =
    match l with
    | [] -> count
    | hd::tl -> if hd.letter = None then helper tl (count + 1) else count
  in
  let space = helper line 0 in if space = 15 then -1 else space

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


let is_tile_empty tile =
  match tile.letter with
  | Some x -> false
  | None -> true

(* returns letter in a [tile]
 * tile letter must not be None *)
let extract_letter tile =
  match tile.letter with
  | Some x -> x
  | _ -> failwith "invariant violated"

(* [tiles] is AI's current rack as byte list, [line] is one row or col in a grid
 * returns a list of playable words
 * assuming the last letter in line is isolated
 * i.e _ _ _ e _ would allow 4 letter words that end in e *)
let try_above dict tiles line =
    let max_len = (space_above line) in
    if max_len = -1 then ([],-1)
    else
      let last_tile = List.nth line max_len in
      (* entire row empty no chance of playing tile*)
      if (is_tile_empty last_tile || max_len = 0) then
        ([],-1)
      else
        let last_letter = extract_letter last_tile in
        (* must check that tile after is empty *)
        if ((max_len + 1) < 15 ) then
          let after_last_tile = (List.nth line (max_len + 1)) in
          let after_last_empty = is_tile_empty after_last_tile in
          if after_last_empty then
            let choices = try_tile_subsets dict (last_letter::tiles)  in
            let filtered = List.filter
              (fun x -> (String.length x <= (max_len +1))
              && (x.[(String.length x)-1] = last_letter)) choices
            in (filtered,max_len+1)
          else ([], -1)
        (* last tile on 15th cell, no need to check below it *)
        else
          let choices = try_tile_subsets dict (last_letter::tiles)  in
          let filtered = List.filter
            (fun x -> (String.length x <= (max_len +1))
            && (x.[(String.length x)-1] = last_letter)) choices
          in (filtered,max_len+1)

(* [tiles] byte list, [line] cell list, returns a list of possible words
 * if space available below and last tile is isolated *)
let try_below dict tiles line =
  let max_len = space_below line in
  if max_len = -1 then ([],-1)

  else
    let first_tile_index = (List.length line) - max_len -1 in
    let first_tile = List.nth line first_tile_index in

    (* entire row empty no chance of playing tile*)
    if (is_tile_empty first_tile || max_len = 0) then ([],-1)

    else
      let first_letter = extract_letter first_tile in
      if (first_tile_index-1) > 0 then
        let before_first_tile = (List.nth line (first_tile_index - 1)) in
        let before_first_empty = is_tile_empty  before_first_tile in
        (* must check that tile after is empty *)
        if before_first_empty then
          let choices = try_tile_subsets dict (first_letter::tiles) in
          let filtered = List.filter
            (fun x -> (String.length x <= (max_len +1))
            && (x.[0] = first_letter)) choices
           in (filtered,first_tile_index)
        else ([],-1)
      (* last tile on 15th cell, no need to check below it *)
      else
        let choices = try_tile_subsets dict (first_letter::tiles) in
        let filtered = List.filter
          (fun x -> (String.length x <= (max_len +1))
          && (x.[0] = first_letter)) choices
         in (filtered,first_tile_index)

let gen_down_move words col_index dir =
  let word_list = fst words in
  let col_letter = char_of_int (col_index + 65) in
  let cord = (col_letter,(snd words) + 1) in (* off by one i.e. add one more*)
  let f x = (x,dir,cord) in
    List.map f word_list

let gen_above_move words col_index dir =
  let word_list = fst words in
  let f x =
    let col_letter = char_of_int (col_index + 65) in
    let cord = (col_letter,(snd words) - ((String.length x) -1)) in
      (x,dir,cord) in
  List.map f word_list

let gen_left_move words row_index dir =
  let word_list = fst words in
  let f x =
    let col_letter = char_of_int ((snd words) - (String.length x) + 65) in
    let cord = (col_letter,row_index + 1) in
    (x,dir,cord) in
  List.map f word_list

let gen_right_move words row_index dir =
  let word_list = fst words in
  let col_letter = char_of_int ((snd words) + 65) in
  let cord = (col_letter,row_index + 1) in
  let f x = (x,dir,cord) in
    List.map f word_list

(* [ai] is a player
 * [game] is a grid of cols and rows
 * iterate through each column and add playable words using [ai] rack
 * to a word list- returns word list
 * note the words are not necessarily playable, just valid within a column *)
let gen_move_list game rack dict =
  (* n can be changed as needed *)
  let tiles = replace_wildcards rack 'E' in
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
    let left_moves = gen_left_move left_words row_index Across in
    let right_words = try_below dict tiles row in
    let right_moves = gen_right_move right_words row_index Across in
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


let get_char_score character =
  match character with
  | 'A' -> 1
  | 'B' -> 3
  | 'C' -> 3
  | 'D' -> 2
  | 'E' -> 1
  | 'F' -> 4
  | 'G' -> 2
  | 'H' -> 4
  | 'I' -> 1
  | 'J' -> 8
  | 'K' -> 5
  | 'L' -> 1
  | 'M' -> 3
  | 'N' -> 1
  | 'O' -> 1
  | 'P' -> 3
  | 'Q' -> 10
  | 'R' -> 1
  | 'S' -> 1
  | 'T' -> 1
  | 'U' -> 1
  | 'V' -> 4
  | 'W' -> 4
  | 'X' -> 8
  | 'Y' -> 4
  | 'Z' -> 10
  | '*' -> 0
  | _ -> failwith "Invalid character encountered in Ai.get_char_score"

let compare_char_score char1 char2 =
  let score1 = get_char_score char1 in
  let score2 = get_char_score char2 in
  if score1>score2 then -1
  else if score1=score2 then 0
  else 1

let rec get_duplicates chars =
  match chars with
  | [] -> []
  | hd::tl -> if List.mem hd tl then hd :: get_duplicates tl
              else get_duplicates tl

(* Exchange any duplicate tiles first, and then highest valued tiles, because
   they are the hardest to find valid words with. Want 3 total tiles to swap  *)
let exchange_tiles tiles =
  let duplicates = get_duplicates tiles in
  if (List.length duplicates) >= 3 then duplicates
  else let num_to_remove = 3 - (List.length duplicates) in
  let remaining_tiles = List.sort_uniq compare_char_score tiles in
  let rec remove_n_highest tiles n i =
    if i = n then []
    else match tiles with
    | [] -> []
    | hd::tl -> hd::(remove_n_highest tl n (i+1))
  in
  duplicates @ (remove_n_highest remaining_tiles num_to_remove 0)

let choose_word game rack dict bag first_move =
  let potential_moves = gen_move_list game rack dict in
  if first_move then
      let choices = try_tile_subsets dict (replace_wildcards rack 'E') in
        match choices with
        | [] -> (* Pass *) "Pass "
        | hd::tl -> (* Play (hd,Across,('H',8)) *) "Play " ^ hd ^ "Across " ^ "(H,8)"
  else
    let f = fun x -> valid_move game x in
    let f2 = fun x y -> compare_scores x y game in
    let playable_moves = List.filter f potential_moves in
    let sorted_moves = List.sort_uniq f2 playable_moves in
      match sorted_moves with
      | [] ->
        let tiles_to_exchange = blist_to_string (exchange_tiles rack) in
        let num_tiles = String.length tiles_to_exchange in
        let remaining_tiles = List.length bag in
        if num_tiles < remaining_tiles && remaining_tiles >= 7
          then (* Exchange tiles_to_exchange *) "Exchange " ^ "tiles_to_exchange"
        else(*  Pass *) "Pass "
      | hd::tl -> (* Play hd *)
        match hd with
        | (word,dir,(ch,i)) ->
          "Play " ^ word ^ " " ^ dir ^ " " ^ (String.make 1 ch) ^ " " (string_of_int i)
        | _ -> "Pass" (*failed*)
