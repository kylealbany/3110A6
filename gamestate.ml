open String
open Board
open Dict
(* open Ai *)

type player = {name: string; score: int; isCPU: bool; rack: char list}
type coordinate = char * int
type game = grid
type move = string * direction * coordinate
type command = Help | Quit | Pass | Shuffle | Score | Board | Play of move
    | Exchange of string | Unknown of string
type mode = Single | Multi | Err

(* see gamestate.mli *)
let ospd = dict_init "ospd.txt"


(* Initializes all the tiles of an official scrabble game *)
let init_tiles () : char list =
  let count = [9;2;2;4;12;2;3;2;9;1;1;4;2;6;8;2;1;6;4;6;4;2;2;1;2;1] in
  let alpha = ['A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';
  'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z'] in
  let rec repeat n x =
    (if n = 0 then [] else x::(repeat (n-1) x)) in
  let rec iter c a =
    (match c, a with
         | x::xs, y::ys -> (repeat x y) @ (iter xs ys)
         | _, _ -> []) in
  iter count alpha

(* Removes the first occurance of a character element from the character list
 *    -[c] is the character we want to remove
 *    -[clist] is the list from which we want to remove a character
 *)
let rec remove_elt (clist: char list) (c: char) : char list =
  match clist with
  | [] -> failwith "Element not found"
  | x::xs -> if x = c then xs else x::(remove_elt xs c)

(* See gamestate.mli *)
let rec gen_random_tiles (clist: char list) (n: int) : (char list) * (char list)
=
  match clist with
  | [] -> ([],[])
  | x ->
      let rand = Random.int (List.length x) in
      let rand_elt = List.nth x rand in
      if n = 1 then (remove_elt x rand_elt, [rand_elt])
      else
        let new_clist = remove_elt x rand_elt in
        let (chars,tiles) = (gen_random_tiles new_clist (n-1)) in
        (chars, rand_elt::tiles)

(* Initializes players for each name in the string list. Players are initalized
 * with a score of zero. They are each given 7 random tiles from the games tile
 * pool. Returns a list of initialized players and the remaining tile pool
 *    -[names] list if strings of players names
 *    -[all_tiles] character list of all of the remaining tiles in the game
 *)
let rec init_players (names: string list) (all_tiles: char list)
: player list * char list =
  match names with
  | [] -> ([], all_tiles)
  | x::xs ->
      let (game_tiles, player_tiles) = gen_random_tiles all_tiles 7 in
      let (plist, tlist) = init_players xs game_tiles in
      ({name = x; score = 0;isCPU = false;rack = player_tiles} :: plist, tlist)

(* Creates a string of the player's tile to be printed out
 *    -[playr] is the player whose tiles are to be displayed (whose turn it is)
 *)
let print_player_tiles (playr: player) : string =
  let clist = playr.rack in
  let n = List.length clist in
  let rec create_border (n: int) (s1: string) (s2: string) =
    if n = 0 then s2 else s1 ^ (create_border (n-1) s1 s2) in
  let rec create_tiles (lst: char list) =
    match lst with
    | [] -> "|\n"
    | x::xs -> "|\027[48;5;179m\027[38;5;0m " ^ (Char.escaped x) ^ " \027[0m" ^ (create_tiles xs) in
  let border = create_border n "+---" "+\n" in
  (">> " ^ playr.name ^ "'s tiles are: \n" ^ "   " ^ border ^ "   " ^
    (create_tiles clist) ^ "   " ^ border)

(* Returns a list of the player's tiles in a randomized order
 *    -[playr] is the player whose tiles are to be shuffled
 *)
let shuffle_player_tiles (playr: player) : char list =
  let clist = playr.rack in
  let rec shuffle_help (lst: char list) =
    match lst with
    | [] -> []
    | x ->
      let rand = Random.int (List.length x) in
      let rand_elt = List.nth x rand in
      rand_elt :: shuffle_help (remove_elt x rand_elt) in
  shuffle_help clist

(* let get_winner (plist: player list) : player =
  failwith "megan is the winner" *)

let get_score (plist: player list) : unit =
  failwith "megnas score > peters score"



(******************************************************************************)
(* WORD SCORE HELPERS *********************************************************)
(******************************************************************************)

(* Returns the offical tile score of the character
 *    -[c] is a capitalized char of the standard alphabet or '*' if it
 *    represents a blank tile
 *)
let get_char_score c =
  match c with
  | 'A' | 'E' | 'I' | 'O' | 'U' | 'L' | 'N' | 'S' | 'T' | 'R' -> 1
  | 'D' | 'G' -> 2
  | 'B' | 'C' | 'M' | 'P' -> 3
  | 'F' | 'H' | 'V' | 'W' | 'Y' -> 4
  | 'K' -> 5
  | 'J' | 'X' -> 8
  | 'Q' | 'Z' -> 10
  | _ -> 0


(* Set ith element of list lst to element e
 *    -[i] is an int repesenting the index of the list
 *    -[e] is the 'a element we wish to replace the ith element with
 *    -[lst] is the 'a list we wish to update
 *)
let rec set (e: 'a) (i: int) (lst: 'a list) : 'a list =
  match lst with
  | [] -> []
  | h::t -> if i = 0 then e::t else h::(set e (i-1) t)


(* Creates a sublist of lst from index i to index j. Includes all elements up to
 * and including j.
 * http://stackoverflow.com/questions/2710233/how-to-get-a-sub-list-from-a-list-
 * in-ocaml
 *     -[list] is an 'a list
 *     -[i] is the starting index of the sublist
 *     -[j] is the last index of the sublist
 *)
let rec sub_list (i: int) (j: int) (lst: 'a list) : 'a list =
  match lst with
  | [] -> []
  | h::t -> let tail = if j=0 then [] else sub_list (i-1) (j-1) t in
            if i>0 then tail else h :: tail


(* Returns the tail of a list starting at the nth element
 *    -[lst] is the 'a we wish to truncate
 *    -[index] is the starting index of the truncation
 *)
let nth_and_tail (lst: 'a list) (index: int) : 'a list =
  let rec helper (l: 'a list) (x: int) =
    match l with
    | [] -> []
    | h::t -> if x = 0 then l else helper t (x-1)
  in helper lst index


(* Returns the first index of a word in a cell list (row/column) on the board
 * given the coordinate of any letter in that word
 *    -[lst] is a cell list repesenting a row or column on the board
 *    -[index] an int representing a known idex of the word in a row/column
 *)
let find_assoc_index (lst: cell list) (index: int) : int =
  let rec helper (wrd: cell list) (i: int) =
    match wrd with
    | [] -> 0
    | h::t -> if h.letter = None then (15-i) else helper t (i+1)
  (* Run helped on sublist of cell list *)
  in helper (nth_and_tail (List.rev lst) (15-index)) (15-index)


(* Returns the last index of a word in a cell list (row/column) on the board
 * given the coordinate of any letter in that word
 *    -[lst] is a cell list repesenting a row or column on the board
 *    -[index] an int representing a known idex of the word in a row/column
 *)
let find_assoc_rindex (lst: cell list) (index: int) : int =
  let rec helper (wrd: cell list) (i: int) =
    match wrd with
    | [] -> 14
    | h::t -> if h.letter = None then i else helper t (i+1)
  in (helper (nth_and_tail lst index) 0)+index-1


(* Given the first a full row/column of the board and the index of the first
 * index of the word, returns a char list of the entire word in that row/col
 * startinf at that index
 *    -[cell_list] is a cell list representing a row/column on the board
 *    -[index] is an in represing the starting index of the word in that
 *    row/column
 *)
let find_assoc_clist (cell_list: cell list) (index: int) : char list =
  let sub = nth_and_tail cell_list index in
  let rec helper (lst: cell list) =
    match lst with
    | [] -> []
    | x::xs ->
        (match x.letter with
                | None -> []
                | Some a -> a::(helper xs)) in
  helper sub


(* Scores the main word formed by the tiles the player played. Takes in char
 * list of the word played and the subline of the row/column. Returns the score
 * and word multiplier pair
 *    -[wlist] is a char list of the word being played
 *    -[subline] is a sublist of the row/column the word is being played on
 *     starting at the index the word is being inserted at
 *)
let score_main (wlist: char list) (subline: cell list): int * int =
  let wmult = ref 1 in
  let rec helper wrd sub1 score =
    match wrd, sub1 with
    | [], _ -> (score,(!wmult))
    | h::t, hl::tl -> wmult := (!wmult*hl.word_mult);
        helper t tl ((hl.letter_mult * (get_char_score h))+score)
    | _,_ -> failwith "invalid word"
  in helper wlist subline 0


(* Retuns a list of rows/columns parallel to the row or column being played.
 *    -[lines] is a cell list representing a row/column
 *    -[index] is the index of that row/column
 *)
let get_adj (lines: cell list list) (index: int) : cell list list =
  let rec helper (l: cell list list) (x: int) (acc: cell list list) =
    match l with
    | [] -> acc
    | h::t -> if ((x = (index -1)) || (x = (index + 1)))
              then helper t (x+1) (h::acc) else helper t (x+1) acc
  in helper lines 0 []


(* Returns a sublist of the row/column on the board that the player starting
 * at the respective coordinate index
 *    -[board] is the current board for the game in (row major, col major) form
 *    -[coordiantes] is the starting coordinates of the word to be played
 *    -[dir] is the direction the word is to be played in
 *)
let get_subline (board: game) (coordinates: coordinate) (dir: direction)
: cell list =
  match dir,coordinates,board with
  | Down, (x,y), (r,c) -> nth_and_tail (List.nth c ((int_of_char x) -65)) (y-1)
  | Across, (x,y), (r,c) -> nth_and_tail (List.nth r (y-1)) ((int_of_char x)-65)


(* Returns the entire row/column of the board that that word is to be played on
 *    -[board] is the current board for the game in (row major, col major) form
 *    -[coordiantes] is the starting coordinates of the word to be played
 *    -[dir] is the direction the word is to be played in
 *)
let get_line (board: game) (coordinates: coordinate) (dir: direction)
: cell list =
   match dir,coordinates,board with
  | Down, (x,y), (r,c) -> (List.nth c ((int_of_char x) -65))
  | Across, (x,y), (r,c) -> (List.nth r (y-1))


(* Updates the row/col cell list to include the word repsented by the char list
 *    -[clist] is the char list representing the word to be played
 *    -[subl] is the entire row/column beginning where the word is
 *    to be inserted
 *    -[n] is the starting index of the word
 *)
let rec update_cell_list (clist: char list) (subl: cell list) (n: int)
: cell list =
  match clist, subl with
  | [], x -> x
  | c::cs, s::ss ->
      if n = 0 then
      let new_s = {s with letter = Some c} in
      new_s:: update_cell_list cs ss n
      else s:: update_cell_list (c::cs) ss (n-1)
  | _, [] -> failwith "Invalid character insertion"


let rec update_for_perp (clist: char list) (subl: cell list) (n: int) : cell list =
  match clist, subl with
  | [], x -> x
  | c::cs, s::ss ->
    if n = 0 then
    (match s.letter with
        | None -> print_string "matched to none\n"; {s with letter = Some c}::update_for_perp cs ss n
        | Some x -> print_string "matched to a letter\n"; {s with word_mult = 0}::update_for_perp cs ss n)
    else (print_string "in else\n"; s::(update_for_perp (c::cs) ss (n-1)))
  | _,_ -> failwith "Invalid character insertion"



(* Check associated list with a letter in the word being played,
 * if the length of that associated list is > 1, then score it and add it to the
 * total score
 *   -[n] is the index of the cell list
 *   -[score] is accumlated associated word score
 *)
let get_assoc_score (clist: cell list) (n: int) : int =
  let assoc_start = find_assoc_index clist n in
  let assoc = find_assoc_clist clist assoc_start in
  if List.length assoc > 1
    then let word_score, word_mult =
      score_main assoc (nth_and_tail clist assoc_start) in
      print_string ((string_of_int word_mult)^"\n");
      word_score * word_mult
    else 0


(* Returns true if the player gets a bingo by playing all 7 of the tiles on
 * his/her rack
 *    -[char_lst] character list of the word bieng played
 *    -[cell_list] is the sub list of therow/columnn the word is being played in
 *    starting at the starting index of the word
 *    -[n] is the length of the word
 *)
let bingo (char_lst: char list) (cell_lst: cell list) (n: int): bool =
  let rec count_tiles (clist: cell list) (i: int) =
    match clist with
    | [] -> 0
    | x::xs ->
        (match x.letter with
                 | None | Some '@' -> if i = 0 then 0 else count_tiles xs (i-1)
                 | Some a -> if i = 0 then 0 else (1 + (count_tiles xs (i-1))))
  in
  ((List.length char_lst) - (count_tiles cell_lst n)) = 7


(* See gamestate.mli *)
let word_score (board: game) (turn: move) : int =
  let word, dir, coordinates = turn in
  (* get sublist of row/col starting at respective coordinate *)
  let subl = get_subline board coordinates dir in
  (* create char list from word *)
  let wlist = to_char_list word in
  let rows, columns = board in
  let start_index, line_num =
    if dir = Down then (snd coordinates)-1, (int_of_char (fst coordinates))-65
    else (int_of_char (fst coordinates))-65, (snd coordinates)-1 in
  let played_score = score_main wlist (get_subline board coordinates dir) in
  (* Check if word played is a prefix or suffix of existing word *)
  let assoc_cell_list =
    update_cell_list wlist (get_line board coordinates dir) start_index in
  (* Set played score to computed score *)
  let new_played_score =
    if (find_assoc_index assoc_cell_list start_index) <> start_index ||
    (find_assoc_rindex assoc_cell_list start_index) <>
    (start_index + (List.length wlist) -1)
  then
    let exten_start = find_assoc_index assoc_cell_list start_index in
    let (exten_total, exten_mult) =
      score_main (find_assoc_clist assoc_cell_list exten_start) subl in
    exten_total * exten_mult
  else fst played_score * snd played_score in
  (* Adjacent score calculating logic *)
  let rec helper line_list n =
    match line_list with
    | [] -> 0
    | h::t -> get_assoc_score h n + helper t n
  in
  (* collect words the run perpedicular to the word being played *)
  let assoc_perp_list = update_for_perp wlist (get_line board coordinates dir) start_index in
  let perp_lines =
  (if dir = Down then
      let new_cols = set assoc_perp_list line_num columns in
      sub_list start_index (start_index + (List.length wlist)-1) (transpose new_cols)
    else
      let new_rows = set assoc_perp_list line_num rows in
      print_string ((string_of_int (List.length new_rows))^"\n");
      sub_list start_index (start_index + (List.length wlist)-1) (transpose new_rows))
  in
  let adj_score = helper perp_lines line_num in
  (* add 50 if all 7 of the player's tiles being used in the word *)
  let bingo_bonus = if bingo wlist subl (List.length wlist) then 50 else 0 in
  new_played_score + adj_score + bingo_bonus


(******************************************************************************)
(* WORD VALIDATE HELPERS ******************************************************)
(******************************************************************************)

(* Validates placement *)

(* Returns true if proposed word is within the bounds of the board
 *    -[word] char list of the word to be played
 *    -[dir] the direction of the word
 *    -[coord] coordinates of the starting character
 *)
let check_within_board (word: char list) (dir: direction) (coord:coordinate)
: bool =
  match dir,coord with
  | Down, (x,y) -> (y-1)+(List.length word) <= 15
  | Across, (x,y) -> ((int_of_char x) -65)+(List.length word) <=15


(* Returns true if proposed word is connected in some way to tiles already on
 * the board
 *    -[board] the current game board
 *    -[line] the line the proposed word will be played on
 *    -[char_list] char list of the word to be played
 *    -[dir] the direction of the word
 *    -[coord] coordinates of the starting character
 *)
let check_is_connected (board: game) (line: cell list) (char_list: char list)
(dir: direction) (coord: coordinate) : bool =
  let rec char_exists (subline : cell list) =
    match subline with
    | [] -> false
    | h::t -> match h.letter with
        | None | Some '@' -> false || char_exists t
        | Some a -> true
  in
  match dir, coord with
  | Down, (x,y) ->
        let adj_lines = get_adj (snd board) ((int_of_char x) -65) in
        let ext_thru = char_exists (sub_list (max (y-2) 0)
          (min (y + List.length char_list -2) 15) line) in
        let para = (List.map (fun l ->
          char_exists (sub_list (y-1) (y + List.length char_list -1) l)) adj_lines)
        in
        ext_thru || (List.fold_right (fun x acc -> x || acc) para false)
  | Across, (x,y) ->
        let adj_lines = get_adj (fst board) (y-1) in
        let ext_thru = char_exists (sub_list (max ((int_of_char x) -66) 0)
          (min ((int_of_char x) + List.length char_list -66) 15) line) in
        let para = (List.map (fun l ->
          char_exists (sub_list ((int_of_char x) -65)
          ((int_of_char x) + List.length char_list -65) l)) adj_lines) in
        ext_thru || (List.fold_right (fun x acc -> x || acc) para false)


(* Returns true if the proposed word does not overwrite existing tiles
 *    -[subline] the board line to be played on, cut at the word's
 *    starting index
 *    -[char_list] char list of the word to be played
 *)
let check_no_overwrite subline char_list =
  let rec helper sub word =
  match sub, word with
  | _, [] -> true
  | [], _ -> false
  | h::t, hc::tc -> (match h.letter,hc with
            | None, a | Some '@', a -> true && (helper t tc)
            | Some x, y -> (x = y) && (helper t tc))
  in
  helper subline char_list


(* Returns the string version of a character list
 *    -[clist] is a list of characters
 *)
let rec char_list_to_string (clist: char list) : string =
  match clist with
  | [] -> ""
  | x::xs -> (Char.escaped x) ^ (char_list_to_string xs)


(* Returns true if the word string can be found in the OSPD
 *    -[dict] is the OSPD radix tress dictionary
 *    -[turn] is the word being played, the starting coordinates, and the
 *    direction being played
 *)
let valid_main_word (dict: dict) (turn: move) : bool =
  let word, _, _ = turn in
  member dict word


(* Returns true if the entire word the players move is extending is a valid
 * word in the OSPD. Returns true if the word does not extend a word.
 *    -[dict] is the OSPD radix tress dictionary
 *    -[turn] is the word being played, the starting coordinates, and the
 *    direction being played
 *    -[board] is the current board the word is being inserted into stored
 *    in row major, col major form
 *)
let valid_extension (dict: dict) (board: game) (turn: move) : bool =
  let word, dir, coord = turn in
  let wlist = to_char_list word in
  let line = get_line board coord dir in
  let start_index = (if dir = Down then (snd coord)-1
      else (int_of_char (fst coord))-65) in
  let assoc_cell_list =
    update_cell_list wlist (get_line board coord dir) start_index in
  let new_start = find_assoc_index assoc_cell_list start_index in
  let new_end = find_assoc_rindex assoc_cell_list start_index in
  if new_start <> start_index || new_end <> (start_index + (List.length wlist)-1)
  then let exten_clist = find_assoc_clist line new_start in
    let exten_word = char_list_to_string exten_clist in
    member dict exten_word
  else true


(* Returns true if all of the new words perpendicular to the word being inserted
 * the are created with the insertion are valid words according to the OSPD
 *    -[dict] is the OSPD radix tress dictionary
 *    -[turn] is the word being played, the starting coordinates, and the
 *    direction being played
 *    -[board] is the current board the word is being inserted into stored
 *    in row major, col major form
 *)
let valid_parallels (dict: dict) (board: game) (turn: move) : bool =
  let word, dir, coord = turn in
  let rows, cols = board in
  let wlist = to_char_list word in
  let start_index, line_num =
    if dir = Down then (snd coord)-1, (int_of_char (fst coord))-65
    else (int_of_char (fst coord))-65, (snd coord)-1 in
  let assoc_cell_list =
    update_cell_list wlist (get_line board coord dir) start_index in
  let perp_lines =
  (if dir = Down then
      let new_cols = set assoc_cell_list line_num cols in
      sub_list start_index (start_index + (List.length wlist)-1) (transpose new_cols)
  else
      let new_rows = set assoc_cell_list line_num rows in
      sub_list start_index (start_index + (List.length wlist)-1) (transpose new_rows))
  in
  let valid_each (clist: cell list) (acc: bool) =
    let perp_start = find_assoc_index clist line_num in
    let char_list = find_assoc_clist clist perp_start in
    if (List.length char_list) > 1 then
      let word_string = char_list_to_string char_list in
      print_string (word_string ^ "\n");
      print_string (string_of_bool (member dict word_string) ^ "\n");
      acc && (member dict word_string)
    else acc in
  List.fold_right valid_each perp_lines true


(* Returns true if the given move is a valid first move in the game
 *    -[word] a string of the word to be played
 *    -[dir] the direction of the word
 *    -[coord] the coordinates of the first character in the word
 *)
let valid_first (word: string) (dir: direction) (coord: coordinate) : bool =
  match dir, coord with
  | Down, (x,y) -> (((int_of_char x) -65) = 7) &&
                   ((y + length word - 1) >= 7)
  | Across, (x,y) -> ((y-1) = 7) &&
                     ((((int_of_char x) -65) + length word) >= 7)


(* See gamestate.mli *)
let valid_move (board : game) (turn : move) : bool =
  let word, dir, coord = turn in
  let wlist = to_char_list word in
  (check_within_board wlist dir coord) &&
  (check_is_connected board (get_line board coord dir) wlist dir coord) &&
  (check_no_overwrite (get_subline board coord dir) wlist)


(* See gamestate.mli *)
let valid_word (board: game) (turn: move) : bool =
  (valid_main_word ospd turn) &&
  (valid_extension ospd board turn) &&
  (valid_parallels ospd board turn)

(* Returns true if all of the tiles the player uses that do not currently
 * exist on the board are from the player's rack
 *    -[board] is the game board in row major, col major form
 *    -[turn] is the word, direction, and coordinates of the move
 *    -[rack] is the player's rack
 *)
let valid_rack (board : game) (turn : move) (rack : char list) : bool =
  let (word, dir, coord) = turn in
  let wlist = to_char_list word in
  let subl = get_subline board coord dir in
  let rec valid_rack_tiles (wlst: char list) (clst: cell list) (rck: char list) (n: int) =
    match wlst, clst with
    | [], _ -> true
    | w::ws , c::cs -> if n = 0 then false else
          (match c.letter with
          | Some a -> valid_rack_tiles ws cs rck n
          | None ->
              if (List.mem w rack) then
              let remove_w = remove_elt rck w in
              valid_rack_tiles ws cs remove_w (n-1)
              else false)
    | _, [] -> failwith "Invalid move - out of bounds"
  in valid_rack_tiles wlist subl rack 7

(******************************************************************************)
(* MAIN HELPERS ***************************************************************)
(******************************************************************************)

(* Filters the game mode to return Single if its single player mode (1 player
 * v. the AI), Multi if its multiplayer mode (no AI), or Other if the command
 * is not recognized
 *)
let filer_mode (s: string) : mode =
  let filtered = trim (uppercase s) in
  if filtered = "SPM" then Single else
  if filtered = "MPM" then Multi else Err

(* Reads the user input to get the number of multiplayers.
 * postcondition: output must be a int between 2 and 4
 *)
let rec get_player_num () : int =
  let input_command = read_line () in
  let filtered = trim input_command in
  let n = (try int_of_string filtered with
    int_of_string -> print_string ("\n>> Invalid player number.\n>> Enter" ^
          " a number between 2 and 4:  "); get_player_num ()) in
  if (n < 2 || n > 4) then (print_string ("\n>> Invalid player number.\n>>" ^
      " Enter a number between 2 and 4:  "); get_player_num ())
  else n

(* Returns a list of the names of the players
 *    -[n] is the number of multiplayers (between 2 and 4)
 *    -[m] is 0 if the game is in single-player mode and > 0 otherwise. Used to
 *      indicate which players turn it is to enter their name
 *)
let rec get_names (n: int) (m: int): string list =
  if n = 0 then []
  else let input_command  =
      (if m = 0 then (print_string "\n>> Enter your name:  "; read_line ()) else
      (print_string ("\n>> Enter Player " ^ (string_of_int m) ^ "\'s name:  ");
      read_line ())) in
  let filter_name = trim input_command in
  print_string (">> Hi " ^ filter_name);
  filter_name :: (get_names (n-1) (m+1))

(* See gamestate.ml *)
let get_winner (plist: player list) : player =
  match safe_hd plist with
  | None -> failwith "No players initialized"
  | Some p ->
    List.fold_right (fun x acc -> if max x.score acc.score = x.score then x else acc) plist p


let find_from lst e n =
  let rec helper l elt i =
  match l with
  | [] -> -1
  | h::t -> if h = elt then i else helper t elt (i+1)
  in
  if (helper (nth_and_tail lst n) e 0) = -1 then -1 else (helper (nth_and_tail lst n) e 0) + n


let play_cmd (str: string) : command =
  let str = trim str in
  let end_word = find_from (to_char_list str) ' ' 0 in
  if end_word = -1 then Unknown ("Invalid word in Play command. Enter Help to review.\n") else
  let word = uppercase (sub str 0 end_word) in
  let new_str = trim (sub str end_word (length str - end_word)) in
  let end_dir = find_from (to_char_list new_str) ' ' 0 in
  if end_dir = -1 then Unknown ("Invalid direction in Play command. Enter Help to review\n") else
  let dir = capitalize (sub new_str 0 end_dir) in
  let coord_str = trim (sub (trim new_str) (length (trim new_str) - 4) 4) in
  let col,row = (Char.uppercase (get coord_str 0)), ((Char.code (get coord_str (length coord_str -1)) -48) + if length coord_str = 4 then 10 else 0) in
  (* let col,row = (Char.uppercase (get str (length str - 3))),(Char.code (get str (length str -1)) -48) in *)
  if not (contains "ABCDEFGHIJKLMNO" col) then Unknown ("Invalid column index\n") else
  if not (row >= 1 && row <=15) then Unknown ("Invalid row index\n") else
  match dir with
  | "Down" -> Play (word,Down,(col,row))
  | "Across" -> Play (word, Across, (col,row))
  | _ -> Unknown ("Invalid direction in Play command. Enter Help to review\n")


let exchange_cmd (str: string) : command =
  let str = trim str in
  if find_from (to_char_list str) ' ' 0 <> -1
  then Unknown ("Invalid Exchange argument, spaces not recognized\n") else
  if length str > 7 then Unknown ("Can only exchange up to 7 tiles\n") else
  if length str = 0 then Unknown ("Need to exchange at least 1 tile\n") else
  Exchange(str)


let parse_string (arg: string) : command =
  let arg = trim arg in
  let space_index = find_from (to_char_list arg) ' ' 0 in
  match if space_index = -1 then uppercase arg
        else uppercase (sub arg 0 space_index) with
  | "HELP" -> if length (sub arg 4 (length arg - 4)) > 0 then
              Unknown ("Command not recognized. Did you mean Help?\n")
              else Help
  | "BOARD" -> if length (sub arg 5 (length arg - 5)) > 0 then
              Unknown ("Command not recognized. Did you mean Board?\n")
              else Board
  | "SCORE" -> if length (sub arg 5 (length arg - 5)) > 0 then
              Unknown ("Command not recognized. Did you mean Score?\n")
              else Score
  | "PASS" -> if length (sub arg 4 (length arg - 4)) > 0 then
              Unknown ("Command not recognized. Did you mean Pass?\n")
              else Pass
  | "SHUFFLE" -> if length (sub arg 7 (length arg - 7)) > 0 then
              Unknown ("Command not recognized. Did you mean Shuffle?\n")
              else Shuffle
  | "QUIT" -> if length (sub arg 4 (length arg - 4)) > 0 then
              Unknown ("Command not recognized. Did you mean Quit?\n")
              else Quit
  | "PLAY" -> play_cmd (sub arg 4 (length arg - 4))
  | "EXCHANGE" -> exchange_cmd (sub (trim arg) 8 (length (trim arg) - 8))
  | _ -> Unknown ("Command not recognized, enter Help to review commands\n")


(* See gamestate.mli *)
let exchange (rack: char list) (tiles: string) (bag: char list)
: char list * char list =
  let tile_list = to_char_list (uppercase tiles) in
  let num_tiles = List.length tile_list in
  let rec remove_from_rack (r: char list) (t: char list) =
    match t with
    | [] -> r
    | x::xs -> remove_from_rack (remove_elt r x) xs in
  let new_rack = remove_from_rack rack tile_list in
  let (new_bag, new_tiles) = gen_random_tiles (bag @ tile_list) num_tiles in
  (new_rack @ new_tiles, new_bag)

let valid_exchange (rack: char list) (tiles: string) (bag: char list)
: string =
  let t_list = to_char_list (uppercase tiles) in
  let on_rack = not (List.exists (fun c -> find_from rack c 0 = -1) t_list) in
  let valid_bag = List.length bag >= 7 in
  if not on_rack then "Some tiles are not in your rack and cannot be exchanged"
  else if not valid_bag then "Bag has fewer than 7 tiles, cannot exchange"
  else ""


let find_remaining_rack (board: game) (turn: move) (rack: char list)
: char list =
  let (word, dir, coord) = turn in
  let subline = get_subline board coord dir in
  let rec remove_used_char (w: char list) (c: cell list ) (r: char list) =
    match w, c with
    | [], _ -> r
    | x::xs, c::cs -> (match c.letter with
              | Some a -> remove_used_char xs cs r
              | None -> if (List.mem x r) then
                    remove_used_char xs cs (remove_elt r x)
                    else failwith "Letter not on rack")
    | x::xs, _ -> failwith "Invalid move - out of bounds"
  in remove_used_char (to_char_list word) subline rack


let find_hd_n_tail (lst:'a list) =
  match lst with
  | [] -> None
  | x::xs -> Some (x,xs)


let get_hd_n_tail (lst: 'a list) =
  match find_hd_n_tail lst with
  | None -> failwith "Player list no initialized"
  | Some x -> x


let rec game_is_over (plist: player list) =
  match plist with
  | [] -> false
  | x::xs -> (x.rack = []) || game_is_over xs


(* Determines the mode the player wishes to use and generates a list of players
 *)
let rec init_game_players (bag: char list) : player list * char list =
  let input_command = read_line () in
  match filer_mode input_command with
  | Err -> print_string "\n>> Command not recognized. Please enter SPM or MP:  ";
        init_game_players bag
  | Single -> print_string "\n>> Single player mode.";
        let (player_tiles, new_bag) = gen_random_tiles bag 7 in
        let (other_players, rest_bag) = init_players (get_names 1 0) new_bag in
        ( List.rev ({name = "CPU1"; score = 0; isCPU = true; rack = player_tiles}
               :: other_players), rest_bag)
  | Multi -> print_string ("\n>> Multiplayer mode.\n>> Multiplayer mode" ^
        " can be played with 2 to 4 players.\n>> Enter # of players:  ");
        let num_players = get_player_num () in
        init_players (get_names num_players 1) bag


let help_string =
"We hope you enjoy playing our Scrabble game!
Our game recognizes the following commands:
>> Help - Opens the help menu (this thing)
>> Board - Displays the current game board
>> Score - Displays your score so far
>> Shuffle - Shuffles your current rack
>> Play word direction coordinates - Plays word in the given direction
   starting at the given coordinates
>> Exchange tiles - Exchanges the selected tiles from your rack
>> Pass - Passes your turn
>> Quit - Ends the game
When playing a word:
1 - Input your the desired word
2 - Specify the direction, either Across or Down
3 - Specify the coordinates, first the column and then the row
e.g. Play EXAMPLE Down H 8
Have fun!\n"


let rec first_move (board: game) (bag: char list) (plist: player list)
(show_board: bool) : game * char list * player list =
(*   failwith "first_move" *)
  let (playr, tl_plist) = get_hd_n_tail plist in
  (if show_board then (print_board board; print_string (print_player_tiles playr))
    else ());
  print_string ("\n>> Enter command: ");
  let input_command = read_line () in
  match parse_string input_command with
  | Quit ->
      let new_player = {name = playr.name; score = playr.score;
      isCPU = playr.isCPU; rack = []} in
      (board, bag, [new_player] @ tl_plist)
      (* quit out of loop *)
  | Help -> print_string help_string; first_move board bag plist false
  | Board -> first_move board bag plist true
  | Shuffle ->
      let new_player = {name = playr.name; score = playr.score;
      isCPU = playr.isCPU; rack = shuffle_player_tiles playr} in
      first_move board bag ([new_player] @ tl_plist) true
  | Score ->
      let msg = (">> " ^ playr.name ^ "'s score is: " ^
      (string_of_int playr.score) ^ "\n") in
      print_string msg; first_move board bag plist false
  | Pass -> let (nxt_playr, _) = get_hd_n_tail tl_plist in
      first_move board bag (tl_plist @ [playr]) (not nxt_playr.isCPU)
  | Exchange (tiles) ->
      let valid_msg = valid_exchange playr.rack tiles bag in
      if length valid_msg > 0 then
        (print_string valid_msg; first_move board bag plist false)
      else
      let (new_rack, new_bag) = exchange playr.rack tiles bag in
      let new_player = {name = playr.name; score = playr.score;
      isCPU = playr.isCPU; rack = new_rack} in
      let (nxt_playr, _) = get_hd_n_tail tl_plist in
      first_move board bag (tl_plist @ [new_player]) (not nxt_playr.isCPU)
  | Play (turn) ->
      let (word, dir, coord) = turn in
      if not (valid_rack board turn playr.rack) then
        (print_string (">> Move is not valid. Some tiles used are not on " ^
                  "your rack . Please try again.\n");
        first_move board bag plist false)
      else if not (valid_first word dir coord) then
        (print_string (">> Move is not valid. The first move must cover the" ^
                  " center of the board. Please try again.\n");
        first_move board bag plist false)
      else if not (valid_word board turn) then
        (print_string (">> At least one word formed is not a valid" ^
                   " word. Please try again.\n");
        first_move board bag plist false)
      else
        let wscore = word_score board turn in
        let new_board = update_board board word coord dir in
        let rest_rack = find_remaining_rack board turn playr.rack in
        let n_tiles_used = (List.length playr.rack) - (List.length rest_rack) in
        let (new_bag, new_tiles) = gen_random_tiles bag n_tiles_used in
        let new_rack = rest_rack @ new_tiles in
        let new_player = {name = playr.name; score = playr.score + wscore;
            isCPU = playr.isCPU; rack = new_rack} in
          print_string (">> " ^ playr.name ^ " played the word " ^ word ^
            " for " ^ (string_of_int wscore) ^ " points\n");
        (new_board, new_bag, tl_plist @ [new_player])
  | Unknown (message) -> print_string (">> " ^ message); first_move board bag plist false


(* See gamestate.mli *)
let rec main (board: game) (bag: char list) (plist: player list)
(show_board: bool) : game * char list * player list =
(*   failwith "main" *)
  (* check to see if game is over - player has empty rack *)
  if game_is_over plist then (board, bag, plist) else
  (* get the first player from the list *)
  let (playr, tl_plist) = get_hd_n_tail plist in
  (* determines if player is AI *)
  if playr.isCPU then
    (print_string ">> It is CPU1's turn";
    (*     choose_word board playr ospd *)
        (*TODO*)
        main board bag (tl_plist @ [playr])) true
  else
    ((if show_board then (print_board board;
                print_string (print_player_tiles playr)) else ());
        print_string ("\n>> Enter command: ");
        let input_command = read_line () in
        match parse_string input_command with
        | Quit -> (board, bag, plist)
        | Help -> print_string help_string; main board bag plist false
        | Board -> main board bag plist true
        | Shuffle ->
            let new_player = {name = playr.name; score = playr.score;
            isCPU = playr.isCPU; rack = shuffle_player_tiles playr} in
            main board bag ([new_player] @ tl_plist) true
        | Score ->
            let msg = (">> " ^ playr.name ^ "'s score is: " ^
            (string_of_int playr.score) ^ "\n") in
            print_string msg; main board bag plist false
        | Pass -> main board bag (tl_plist @ [playr]) true
        | Exchange (tiles) ->
          let valid_msg = valid_exchange playr.rack tiles bag in
          if length valid_msg > 0 then
            (print_string valid_msg; main board bag plist false)
          else
            let (new_rack, new_bag) = exchange playr.rack tiles bag in
            let new_player = {name = playr.name; score = playr.score;
            isCPU = playr.isCPU; rack = new_rack} in
            main board new_bag (tl_plist @ [new_player]) true
        | Play (turn) ->
            let (word, dir, coord) = turn in
            if not (valid_rack board turn playr.rack) then
                (print_string (">> Move is not valid. Some tiles used are not" ^
                  " on your rack . Please try again.\n");
                main board bag plist false)
            else if not (valid_move board turn) then
                (print_string ">> Move is not valid. Please try again.\n";
                main board bag plist false)
            else if not (valid_word board turn) then
                (print_string (">> At least one word formed is not a valid" ^
                    " word. Please try again.\n");
                main board bag plist false)
            else
              let wscore = word_score board turn in
              let new_board = update_board board word coord dir in
              let rest_rack = find_remaining_rack board turn playr.rack in
              let n_tiles_used = (List.length playr.rack) - (List.length rest_rack) in
              let (new_bag, new_tiles) = gen_random_tiles bag n_tiles_used in
              let new_rack = rest_rack @ new_tiles in
              let new_player = {name = playr.name; score = playr.score + wscore;
                  isCPU = playr.isCPU; rack = new_rack} in
              print_string (">> " ^ playr.name ^ " played the word " ^ word ^
                " for " ^ (string_of_int wscore) ^ " points\n");
              main new_board new_bag (tl_plist @ [new_player]) true
        | Unknown (message) -> print_string message; main board bag plist false)


let () =
  (* Random seed *)
  Random.self_init ();
  print_string ("\n>> Welcome to Scrabble!\n\n>> Would you like to play Single "
    ^ "Player Mode(SPM) or Multiplayer Mode (MPM)?\n>> Enter SPM or MPM:  ");
  let game_bag = init_tiles () in
  let (player_list, rest_bag) = init_game_players (game_bag) in
  let board = init_board () in
  let (fst_board, fst_bag, fst_plist) = first_move board rest_bag player_list true in
  let (_, _, final_plist) = main fst_board fst_bag fst_plist true in
  let winner = get_winner final_plist in
  print_string (">> " ^ winner.name ^ " wins! Congratulations!\n>> Thank you" ^
   " for playing!\n") (* () *)