open String
(* open Board *)

type player = {name: string; mutable score: int; isCPU: bool; rack: char list}
type coordinate = char*int
type game = grid
type move = string*direction*coordinate
type mode = Single | Multi | Err


(* (* Initializes players for each name in the string list. Players are initalized
 * with a score of zero.
 *    -[names] list if strings of players names
 *)
let rec init_players (names: string list) : player list =
  match names with
  | [] -> []
  | x::xs -> {name = x; score = 0; isCPU = false}::(init_players xs) *)

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
(*TODO*)let print_player_tiles (playr: player) : string =
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
(*TODO*)let shuffle_player_tiles (playr: player) : char list =
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


(** UPDATE BOARD HELPERS **)
(* Converts an input string to a charater list
 *    -[s] is a string
 *    -Code modeled after OCaml FAQ
 *     http://caml.inria.fr/pub/old_caml_site/FAQ/FAQ_EXPERT-eng.html#strings
 *)
let to_char_list (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

(* Transposes a cell matrix so that input rows are stored as columns and input
 * colums are stored as rows
 *   -[mat] is a cell matrix repesenting the row or the column format of the
 *    board
 *   -Code modeled after StackOverflow
 *)
(* let rec transpose (mat: 'a list list) : 'a list list =
  match mat with
  | [] -> []
  | [] :: xss -> transpose xss
  | (x::xs) :: xss ->
    let get_hd x = (match safe_hd x with
        | None -> failwith "empty list"
        | Some a -> a) in
    let get_tl x = (match safe_tl x with
        | None -> failwith "empty list"
        | Some a -> a) in
    (x::(List.map get_hd xss)) :: transpose (xs::(List.map get_tl xss)) *)
    (*  *)

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


(* Set ith element of list lst to element e *)
let rec set e i lst =
  match lst with
  | [] -> []
  | h::t -> if i = 0 then e::t else h::(set e (i-1) t)


(* http://stackoverflow.com/questions/2710233/how-to-get-a-sub-list-from-a-list-in-ocaml *)
let rec sub i j lst =
  match lst with
  | [] -> []
  | h::t -> let tail = if j=0 then [] else sub (i-1) (j-1) t in
            if i>0 then tail else h :: tail


let nth_and_tail lst index =
  let rec helper l x =
    match l with
    | [] -> []
    | h::t -> if x = 0 then l else helper t (x-1)
  in helper lst index


(* Returns the first index of a word given the coordinate of any letter in that word *)
let find_assoc_index lst index =
  let rec helper wrd i =
    match wrd with
    | [] -> 0
    | h::t -> if h.letter = None then (15-i) else helper t (i+1)
  in helper (nth_and_tail (List.rev lst) (15-index)) (15-index)


let find_assoc_rindex lst index =
  let rec helper wrd i =
    match wrd with
    | [] -> 14
    | h::t -> if h.letter = None then i else helper t (i+1)
  in (helper (nth_and_tail lst index) 0)+index-1
(*   14 - (find_assoc_index (List.rev lst) (15-index)) *)

(* Takes in full line and index of the first letter in the word *)
let find_assoc_clist cell_list index =
  let sub = nth_and_tail cell_list index in
  let rec helper lst =
    match lst with
    | [] -> []
    | x::xs ->
        (match x.letter with
                | None -> []
                | Some a -> a::(helper xs)) in
  helper sub


(* Scores the main word formed by the tiles the player played. Takes in char
   list of the word played and the subline of the row/column *)
let score_main (wlist: char list) (subline: cell list) =
  let wmult = ref 1 in
  let rec helper wrd sub1 score =
    match wrd, sub1 with
    | [], _ -> (score,(!wmult))
    | h::t, hl::tl -> wmult := (!wmult*hl.word_mult); helper t tl ((hl.letter_mult * (get_char_score h))+score)
    | _,_ -> failwith "invalid word"
  in helper wlist subline 0


(* Check if existing word above and below (can use f_a_w). If no -> multiply pair return from score_main, yes -> replace score with total word score (score + score_adj for remainder of word) times wmult *)

let get_adj lines index =
  let rec helper l x acc =
    match l with
    | [] -> acc
    | h::t -> if ((x = (index -1)) || (x = (index + 1)))
              then helper t (x+1) (h::acc) else helper t (x+1) acc
  in helper lines 0 []


let get_subline board coordinates dir =
  match dir,coordinates,board with
  | Down, (x,y), (r,c) -> nth_and_tail (List.nth c ((int_of_char x) -65)) (y-1)
  | Across, (x,y), (r,c) -> nth_and_tail (List.nth r (y-1)) ((int_of_char x)-65)


let get_line board coordinates dir =
   match dir,coordinates,board with
  | Down, (x,y), (r,c) -> (List.nth c ((int_of_char x) -65))
  | Across, (x,y), (r,c) -> (List.nth r (y-1))

(* let update_adj_lines adj_lines wlist coordinates dir =
  match dir,coordinates with
  | Down, (x,y) ->
  | Across, (x,y) ->
  | _, _ -> failwith "nope" *)

(* subl is entire row/column*)
let rec update_cell_list (clist: char list) (subl: cell list) (n: int) =
  match clist, subl with
  | [], x -> x
  | c::cs, s::ss ->
      if n = 0 then
      let new_s = {s with letter = Some c} in
      new_s:: update_cell_list cs ss n
      else s:: update_cell_list (c::cs) ss (n-1)
  | _, [] -> failwith "Invalid character insertion"


(* Check associated list with a letter in the word being played,
if the length of that associated list is > 1, then score it and add it to the total
score
    -[n] is the index of the cell list
    -[score] is accumlated associated word score*)
let get_assoc_score (clist: cell list) (n: int) =
  let assoc_start = find_assoc_index clist n in
  let assoc = find_assoc_clist clist assoc_start in
  if List.length assoc > 1
    then let word_score, word_mult = score_main assoc (nth_and_tail clist assoc_start) in
          word_score * word_mult
    else 0

let bingo (char_lst: char list) (cell_lst: cell list) (n: int): bool =
  let rec count_tiles (clist: cell list) (i: int) =
    match clist with
    | [] -> 0
    | x::xs ->
        (match x.letter with
                 | None | Some '@' -> if i = 0 then 0 else count_tiles xs (i-1)
                 | Some a -> if i = 0 then 0 else (1 + (count_tiles xs (i-1)))) in
  print_string (string_of_int (count_tiles cell_lst n));
  ((List.length char_lst) - (count_tiles cell_lst n)) = 7

(* let check_for_para para_i_list adj_lines *)

(*
XGet adjacent lines to the main line the word is played on
Cut those to only consider cells actually adjacent to the word played
Check each of those sublines for letters
  -> no letters -> return score
If there are letters, store index in which they're found
If there's two lists of indexes, flatten them and eliminate duplicates
    (flatten |> no_dups)
Take parallel indexes and get corresponding perpendicular lines
find_assoc_word |> find_assoc_clist to get the word that is perpendicular to played word
score that word and add it to overall word score
 *)

let word_score (board: game) (turn: move) : int =
  let word, dir, coordinates = turn in
  let subl = get_subline board coordinates dir in
  let wlist = to_char_list word in
  let rows, columns = board in
(*   let para_lines = if dir = Down then columns else rows in *)
  let start_index, line_num = if dir = Down then (snd coordinates)-1, (int_of_char (fst coordinates))-65
  else (int_of_char (fst coordinates))-65, (snd coordinates)-1 in
(*   let adj_lines = if dir = Down then rows else columns in *)
  let played_score = score_main wlist (get_subline board coordinates dir) in
  (* Check if word played is a prefix or suffix of existing word *)
  let assoc_cell_list = update_cell_list wlist (get_line board coordinates dir) start_index in
  (* Set played score to computed score *)
  let new_played_score =
    if (find_assoc_index assoc_cell_list start_index) <> start_index ||
    (find_assoc_rindex assoc_cell_list start_index) <> (start_index + (List.length wlist) -1)
  then
    let exten_start = find_assoc_index assoc_cell_list start_index in
    let (exten_total, exten_mult) = score_main (find_assoc_clist assoc_cell_list exten_start) subl in
    exten_total * exten_mult
  else fst played_score * snd played_score in
  (* Adjacent score calculating logic *)
  (* let adj_lines = if dir = Down then get_adj columns line_num else get_adj rows line_num in*)
  let rec helper line_list n =
  match line_list with
  | [] -> 0
  | h::t -> get_assoc_score h n + helper t n
  in
  let perp_lines =
  (if dir = Down then
      let new_cols = set assoc_cell_list line_num columns in
      sub start_index (start_index + (List.length wlist)-1) (transpose new_cols)
    else
      let new_rows = set assoc_cell_list line_num rows in
      sub start_index (start_index + (List.length wlist)-1) (transpose new_rows))
  in
  let adj_score = helper perp_lines line_num in
  let bingo_bonus = if bingo wlist subl (List.length wlist) then 50 else 0 in
  new_played_score + adj_score + bingo_bonus


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
  | Err -> print_string "\n>> Command not recognized. Please enter SPM or MP:  "; init_game_players bag
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