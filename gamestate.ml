open String
(* open Board *)

type player = {name: string; mutable score: int; isCPU: bool; rack: char list}
type coordinate = char*int
type game = grid
type move = string*direction*coordinate
type mode = Single | Multi | Err


(* Initializes all the tiles of an official scrabble game *)
let init_tiles () : char list =
  let count = [9;2;2;4;12;2;3;2;9;1;1;4;2;6;8;2;1;6;4;6;4;2;2;1;2;1;2] in
  let alpha = ['A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';
  'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z';'*'] in
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
let rec init_players (names: string list) (all_tiles: char list) :
player list * char list =
  match names with
  | [] -> ([], all_tiles)
  | x::xs ->
      let (player_tiles, game_tiles) = gen_random_tiles all_tiles 7 in
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
    match clist with
    | [] -> "|\n"
    | x::xs -> "| " ^ (Char.escaped x) ^ " " ^ create_tiles xs in
  let border = create_border n "+---" "+\n" in
  border ^ (create_tiles clist) ^ border ^ (create_border n "===" "\n")

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

let get_winner (plist: player list) : player =
  failwith "megan is the winner"

let get_scores (plist: player list) : unit =
  failwith "megnas score > peters score"


let play_word (board: game) (turn: move) : game =
  failwith "no"

let exchange (board: game) (tiles: string) : game =
  failwith "no"

let rec main (board: game) : unit =
  failwith "no"


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
let rec sub (i: int) (j: int) (lst: 'a list) : 'a list =
  match lst with
  | [] -> []
  | h::t -> let tail = if j=0 then [] else sub (i-1) (j-1) t in
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
  print_string (string_of_int (count_tiles cell_lst n));
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
  (* collect words the run perpedicular to the word being played*)
  let perp_lines =
  (if dir = Down then
      let new_cols = set assoc_cell_list line_num columns in
      sub start_index (start_index + (List.length wlist)-1) (transpose new_cols)
    else
      let new_rows = set assoc_cell_list line_num rows in
      sub start_index (start_index + (List.length wlist)-1) (transpose new_rows))
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
(dir: direction) (coord: coordinates) : bool =
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
        let ext_thru = char_exists (sub (max (y-2) 0)
          (min (y + List.length char_list -2) 15) line) in
        let para = (List.map (fun l ->
          char_exists (sub (y-1) (y + List.length char_list -1) l)) adj_lines)
        in
        ext_thru || (List.fold_right (fun x acc -> x || acc) para false)
  | Across, (x,y) ->
        let adj_lines = get_adj (fst board) (y-1) in
        let ext_thru = char_exists (sub (max ((int_of_char x) -66) 0)
          (min ((int_of_char x) + List.length char_list -66) 15) line) in
        let para = (List.map (fun l ->
          char_exists (sub ((int_of_char x) -65)
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

(* See gamestate.mli *)
let valid_move (board : game) (turn : move) : bool =
  let word, dir, coord = turn in
  let wlist = to_char_list word in
  (check_within_board wlist dir coord) &&
  (check_is_connected board (get_line board coord dir) wlist dir coord) &&
  (check_no_overwrite (get_subline board coord dir) wlist)


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
  print_string ("Hi " ^ filter_name);
  filter_name :: (get_names (n-1) (m+1))

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
        (({name = "CPU1"; score = 0; isCPU = true; rack = player_tiles} ::
                  other_players), rest_bag)
  | Multi -> print_string ("\n>> Multiplayer mode.\n>> Multiplayer mode" ^
        " can be played with 2 to 4 players.\n>> Enter # of players:  ");
        let num_players = get_player_num () in
        init_players (get_names num_players 1) bag

let () =
  print_string ("\n>> Welcome to Scrabble!\n\n>> Would you like to play Single "
    ^ "Player Mode(SPM) or Multiplayer Mode (MPM)?\n>> Enter SPM or MPM:  ");
  (* let _ = init_game_players () in *) ()