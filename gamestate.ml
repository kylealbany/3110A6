open String
open Board
open Move
open Ai

type player = {name: string; score: int; isCPU: bool; rack: char list}
type command = Help | Quit | Pass | Shuffle | Score | Board | Play of move
    | Exchange of string | Unknown of string
type mode = Single | Multi | Err


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
(*
Removes the first occurance of a character element from the character list
 *    -[c] is the character we want to remove
 *    -[clist] is the list from which we want to remove a character

let rec remove_elt (clist: char list) (c: char) : char list =
  match clist with
  | [] -> failwith "Element not found"
  | x::xs -> if x = c then xs else x::(remove_elt xs c) *)

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
    | x::xs -> ("|\027[48;5;179m\027[38;5;0m " ^ (Char.escaped x) ^
      " \027[0m" ^ (create_tiles xs)) in
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
let get_winner (plist: player list) : player list =
  match safe_hd plist with
  | None -> failwith "No players initialized"
  | Some p ->
(*     List.fold_right
      (fun x acc -> if max x.score acc.score = x.score then x else acc) plist p *)
    List.fold_left (fun acc x -> if x.score = (List.hd acc).score then acc@[x]
                    else if x.score > (List.hd acc).score then [x]
                    else acc) [p] plist


let find_from lst e n =
  let rec helper l elt i =
  match l with
  | [] -> -1
  | h::t -> if h = elt then i else helper t elt (i+1)
  in
  if (helper (nth_and_tail lst n) e 0) = -1 then -1
  else (helper (nth_and_tail lst n) e 0) + n


let rec no_dup (lst: char list) (c: char) : char list =
  match lst with
  | [] -> []
  | x::[] -> [x]
  | a::b::t -> if (a = c && b = c) then (no_dup (b::t) c) else a::(no_dup (b::t) c)


let play_cmd (str: string) : command =
  let str = char_list_to_string (no_dup (to_char_list (trim str)) ' ') in
  if (List.fold_right (fun x acc -> acc + (if x = ' ' then 1 else 0)) (to_char_list str) 0) <> 3
    then Unknown ("Invalid Play command. Enter Help to review.\n")
  else
    let end_word = find_from (to_char_list str) ' ' 0 in
    let word = uppercase (sub str 0 end_word) in
    let new_str = trim (sub str end_word (length str - end_word)) in
    let end_dir = find_from (to_char_list new_str) ' ' 0 in
    let dir = capitalize (sub new_str 0 end_dir) in
    let coord_str = trim (sub new_str (length new_str - 4) 4) in
    let col,row = (Char.uppercase (get coord_str 0)),
      ((Char.code (get coord_str (length coord_str -1)) -48) +
      if length coord_str = 4 then 10 else 0) in
    if not (contains "ABCDEFGHIJKLMNO" col)
      then Unknown ("Invalid column index\n")
      else
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


(* Returns a string containing an error message if the tiles the player is
 * trying to exchange is not valid and and empty string otherwise. The
 * player can only exchange tiles that exist on their rack. Tiles cannot be
 * exchanged if the bag has less than 7 tiles.
 *    -[rack] is the players current rack
 *    -[tiles] is a string of the tiles the player wants to exchange
 *    -[bag] is the games current bag
 *)
let valid_exchange (rack: char list) (tiles: string) (bag: char list)
: string =
  let t_list = to_char_list (uppercase tiles) in
  let on_rack = not (List.exists (fun c -> find_from rack c 0 = -1) t_list) in
  let valid_bag = List.length bag >= 7 in
  if not on_rack then "Some tiles are not in your rack and cannot be exchanged"
  else if not valid_bag then "Bag has fewer than 7 tiles, cannot exchange"
  else ""


(* Returns the remaining tiles on the player's rack after a word has been
 * played. If a tile is being written over an exisitng tile on the board, then
 * it remains on the player's rack
 *    -[board] is the current baord of the game
 *    -[turn] is the word beign played, the direction, and coordinate
 *    -[rack] is the player's current rack
 *)
let find_remaining_rack (board: game) (turn: move) (rack: char list)
: char list =
  let (word, dir, coord) = turn in
  let subline = get_subline board coord dir in
  let rec remove_used_char (w: char list) (c: cell list ) (r: char list) =
    match w, c with
    | [], _ -> r
    | x::xs, c::cs -> (match c.letter with
              | None | Some '@' -> if (List.mem x r) then
                   ( print_string ("Removing " ^ (Char.escaped x) ^ " \n");
                                       remove_used_char xs cs (remove_elt r x))
                    else failwith "Letter not on rack"
              | Some a -> remove_used_char xs cs r)
    | x::xs, _ -> failwith "Invalid move - out of bounds"
  in remove_used_char (to_char_list word) subline rack


(* Returns a head tail pair  otion of a list if it is nonempty and none
 * otherwise
 *    -[lst] list to be deconsed
 *)
let find_hd_n_tail (lst:'a list) =
  match lst with
  | [] -> None
  | x::xs -> Some (x,xs)


(* Returns a head tail pair of a list and fails if the list passed in is empty
 *    -[lst] list to be deconsed
 *)
let get_hd_n_tail (lst: 'a list) =
  match find_hd_n_tail lst with
  | None -> failwith "Player list no initialized"
  | Some x -> x


(* Returns true if a player has an empty rack
 *    -[plist] is the list of players in the game
 *)
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
        let (new_bag, player_tiles) = gen_random_tiles bag 7 in
        let (other_players, rest_bag) = init_players (get_names 1 0) new_bag in
        (List.rev ({name = "CPU1"; score = 0; isCPU = true; rack = player_tiles}
               :: other_players), rest_bag)
  | Multi -> print_string ("\n>> Multiplayer mode.\n>> Multiplayer mode" ^
        " can be played with 2 to 4 players.\n>> Enter # of players:  ");
        let num_players = get_player_num () in
        init_players (get_names num_players 1) bag

(* Helper string to be printed with help command *)
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


let pass_count = ref 0


let rec first_move (board: game) (bag: char list) (plist: player list)
(show_board: bool) : game * char list * player list * bool =
(*   failwith "first_move" *)
  let (playr, tl_plist) = get_hd_n_tail plist in
  if !pass_count >= 6 then
      let new_player = {name = playr.name; score = playr.score;
      isCPU = playr.isCPU; rack = []} in
      (board, bag, [new_player] @ tl_plist, false)
  else
  ((if show_board then
      (print_board board; print_string (print_player_tiles playr))
      else ());
    print_string ("\n>> Enter command: ");
    let input_command = read_line () in
    match parse_string input_command with
    | Quit ->
        let new_player = {name = playr.name; score = playr.score;
        isCPU = playr.isCPU; rack = []} in
        (board, bag, [new_player] @ tl_plist, false)
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
        pass_count := (!pass_count) + 1;
        if not nxt_playr.isCPU then
          first_move board bag (tl_plist @ [playr]) (not nxt_playr.isCPU)
        else (board, bag, tl_plist @ [playr], true)
    | Exchange (tiles) ->
        let valid_msg = valid_exchange playr.rack tiles bag in
        if length valid_msg > 0 then
          (print_string valid_msg; first_move board bag plist false)
        else
          (pass_count := 0;
          let (new_rack, new_bag) = exchange playr.rack tiles bag in
          let new_player = {name = playr.name; score = playr.score;
          isCPU = playr.isCPU; rack = new_rack} in
          let (nxt_playr, _) = get_hd_n_tail tl_plist in
          if not nxt_playr.isCPU then
            first_move board new_bag (tl_plist @ [new_player]) (not nxt_playr.isCPU)
          else (board, new_bag, tl_plist @ [new_player], true))
    | Play (turn) ->
        let (word, dir, coord) = turn in
        if not (valid_first word dir coord) then
          (print_string (">> Move is not valid. The first move must cover the" ^
                    " center of the board. Please try again.\n");
          first_move board bag plist false)
        else if not (valid_rack board turn playr.rack) then
          (print_string (">> Move is not valid. Some tiles used are not on " ^
                    "your rack . Please try again.\n");
          first_move board bag plist false)
        else if not (valid_word board turn) then
          (print_string (">> At least one word formed is not a valid" ^
                     " word. Please try again.\n");
          first_move board bag plist false)
        else
          (pass_count := 0;
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
          (new_board, new_bag, (tl_plist @ [new_player]), false))
    | Unknown (message) -> print_string (">> " ^ message);
        first_move board bag plist false)


(* See gamestate.mli *)
let rec main (board: game) (bag: char list) (plist: player list)
(show_board: bool) (ai_flag: bool): game * char list * player list =
(*   failwith "main" *)
  (* check to see if game is over - player has empty rack *)
  if (!pass_count >= 6 || game_is_over plist) then (board, bag, plist) else
  (* get the first player from the list *)
  let (playr, tl_plist) = get_hd_n_tail plist in
  (* determines if player is AI *)
  if playr.isCPU then
        (* note this will have to be passed in from somewhere *)
        (let is_first_move = ai_flag in
                print_string ">> It is CPU1's turn\n";
                let ai_move = choose_word board playr.rack bag is_first_move in
                match parse_string ai_move with
                | Pass ->
                    pass_count := (!pass_count) + 1;
                    main board bag (tl_plist @ [playr]) true ai_flag
                | Play x ->
                    pass_count := 0;
                    let word, dir, coord = x in
                    let wscore = word_score board x in
                    let new_board = update_board board word coord dir in
                    let rest_rack = find_remaining_rack board x playr.rack in
                    let n_tiles_used = ((List.length playr.rack) -
                      (List.length rest_rack)) in
                    let (new_bag, new_tiles) =
                      gen_random_tiles bag n_tiles_used in
                    let new_rack = rest_rack @ new_tiles in
                    let new_player =
                        {name = playr.name; score = playr.score + wscore;
                        isCPU = playr.isCPU; rack = new_rack} in
                    print_string (">> " ^ playr.name ^ " played the word " ^
                        word ^ " for " ^ (string_of_int wscore) ^ " points\n");
                    main new_board new_bag (tl_plist @ [new_player]) true false
                | Exchange s ->
                    pass_count := 0;
                    let (new_rack, new_bag) = exchange playr.rack s bag in
                    let new_player = {name = playr.name; score = playr.score;
                    isCPU = playr.isCPU; rack = new_rack} in
                    main board new_bag (tl_plist @ [new_player]) true ai_flag
                | _ -> failwith "Not a valid AI response")
  else
    ((if show_board then (print_board board;
                print_string (print_player_tiles playr)) else ());
        print_string ("\n>> Enter command: ");
        let input_command = read_line () in
        match parse_string input_command with
        | Quit -> (board, bag, plist)
        | Help -> print_string help_string; main board bag plist false ai_flag
        | Board -> main board bag plist true ai_flag
        | Shuffle ->
            let new_player = {name = playr.name; score = playr.score;
            isCPU = playr.isCPU; rack = shuffle_player_tiles playr} in
            main board bag ([new_player] @ tl_plist) true ai_flag
        | Score ->
            let msg = (">> " ^ playr.name ^ "'s score is: " ^
            (string_of_int playr.score) ^ "\n") in
            print_string msg; main board bag plist false ai_flag
        | Pass ->
            pass_count := (!pass_count) + 1;
            main board bag (tl_plist @ [playr]) true ai_flag
        | Exchange (tiles) ->
          let valid_msg = valid_exchange playr.rack tiles bag in
          if length valid_msg > 0 then
            (print_string valid_msg; main board bag plist false ai_flag)
          else
            (pass_count := 0;
            let (new_rack, new_bag) = exchange playr.rack tiles bag in
            let new_player = {name = playr.name; score = playr.score;
            isCPU = playr.isCPU; rack = new_rack} in
            main board new_bag (tl_plist @ [new_player]) true ai_flag)
        | Play (turn) ->
            let (word, dir, coord) = turn in
            if not (valid_move board turn) then
                (print_string ">> Move is not valid. Please try again.\n";
                main board bag plist false ai_flag)
            else if not (valid_rack board turn playr.rack) then
                (print_string (">> Move is not valid. Some tiles used are not" ^
                  " on your rack . Please try again.\n");
                main board bag plist false ai_flag)
            else if not (valid_word board turn) then
                (print_string (">> At least one word formed is not a valid" ^
                    " word. Please try again.\n");
                main board bag plist false ai_flag)
            else
              (pass_count := 0;
              let wscore = word_score board turn in
              let new_board = update_board board word coord dir in
              let rest_rack = find_remaining_rack board turn playr.rack in
              let n_tiles_used =
                (List.length playr.rack) - (List.length rest_rack) in
              let (new_bag, new_tiles) = gen_random_tiles bag n_tiles_used in
              let new_rack = rest_rack @ new_tiles in
              let new_player = {name = playr.name; score = playr.score + wscore;
                  isCPU = playr.isCPU; rack = new_rack} in
              print_string (">> " ^ playr.name ^ " played the word " ^ word ^
                " for " ^ (string_of_int wscore) ^ " points\n");
              main new_board new_bag (tl_plist @ [new_player]) true false)
        | Unknown (message) -> print_string message;
            main board bag plist false ai_flag)


let () =
  (* Random seed *)
  Random.self_init ();
  print_string ("\n>> Welcome to Scrabble!\n\n>> Would you like to play Single "
    ^ "Player Mode(SPM) or Multiplayer Mode (MPM)?\n>> Enter SPM or MPM:  ");
  let game_bag = init_tiles () in
  let (player_list, rest_bag) = init_game_players (game_bag) in
  let board = init_board () in
  let (fst_board, fst_bag, fst_plist, is_fst_move) =
    first_move board rest_bag player_list true in
  let (final_board, _, final_plist) =
    main fst_board fst_bag fst_plist true is_fst_move in
  let winner = get_winner final_plist in
  match winner with
  | [] -> print_string "oh no"
  | hd::[] -> print_string (">> " ^ hd.name ^ " wins! Congratulations!\n>> Thank you" ^
   " for playing!\n") (* () *)
  | hd::hd'::tl -> (print_string ">> Congratulations players \n   ");
              (List.iter (fun x -> (print_string (x.name ^ " \n   "))) (hd'::tl));
              (print_string "tied for first!\n")

