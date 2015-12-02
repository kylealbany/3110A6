
type cell = {letter_mult : int; word_mult : int; letter : char option}
type rows = cell list list
type columns = cell list list
type game = rows * columns
type direction = Down | Across


(** INIT BOARD HELPERS **)
(* Returns None is lst is empty and Some head element if lst is non-empty *)
let safe_hd (lst: 'a list) : 'a option =
  match lst with
  | [] -> None
  | x::xs -> Some x

(* Returns None is lst is empty and Some tail of the list if lst is non-empty *)
let safe_tl (lst: 'a list) : 'a list option =
  match lst with
  | [] -> None
  | x::xs -> Some xs

(* Transposes a cell matrix so that input rows are stored as columns and input
 * colums are stored as rows
 *   -[mat] is a cell matrix repesenting the row or the column format of the
 *    board
 *   -Code modeled after StackOverflow
 *)
let rec transpose (mat: 'a list list) : 'a list list =
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
    (x::(List.map get_hd xss)) :: transpose (xs::(List.map get_tl xss))

let init_board () =
  let tw = {letter_mult = 1; word_mult = 3; letter = None} in
  let dw = {letter_mult = 1; word_mult = 2; letter = None} in
  let tl = {letter_mult = 3; word_mult = 1; letter = None} in
  let dl = {letter_mult = 2; word_mult = 1; letter = None} in
  let ct = {letter_mult = 1; word_mult = 2; letter = Some '@'} in
  let xx = {letter_mult = 1; word_mult = 1; letter = None} in
  let row1 = [tw;xx;xx;dl;xx;xx;xx;tw;xx;xx;xx;dl;xx;xx;tw] in
  let row2 = [xx;dw;xx;xx;xx;tl;xx;xx;xx;tl;xx;xx;xx;dw;xx] in
  let row3 = [xx;xx;dw;xx;xx;xx;dl;xx;dl;xx;xx;xx;dw;xx;xx] in
  let row4 = [dl;xx;xx;dw;xx;xx;xx;dl;xx;xx;xx;dw;xx;xx;dl] in
  let row5 = [xx;xx;xx;xx;dw;xx;xx;xx;xx;xx;dw;xx;xx;xx;xx] in
  let row6 = [xx;tl;xx;xx;xx;tl;xx;xx;xx;tl;xx;xx;xx;tl;xx] in
  let row7 = [xx;xx;dl;xx;xx;xx;dl;xx;dl;xx;xx;xx;dl;xx;xx] in
  let row8 = [tw;xx;xx;dl;xx;xx;xx;ct;xx;xx;xx;dl;xx;xx;tw] in
  let rows =
    [row1;row2;row3;row4;row5;row6;row7;row8;row7;row6;row5;row4;row3;row2;row1]
  in
  (rows, transpose rows)

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

(* Updates a row or column to include the character list, word, starting at
 * index n
 *    -[n] is the starting index of the word to insert. Must be between 0 and
 *      the length of lst
 *    -[lst] is the original row/column of the board
 *    -[word] is the character list of the word to be inserted
 *)
let rec edit_list (lst:cell list) (n: int) (word: char list) : cell list =
  match lst with
  | [] -> []
  | x::xs -> if n = 0 then
    (match safe_hd word, safe_tl word  with
        | None, _ -> x::edit_list xs (n-1) word
        | _, None -> failwith "empty wordlist"
        | Some hd , Some tl ->
            let new_x = {letter_mult = 1; word_mult = 1; letter = Some hd} in
              new_x::edit_list xs n tl)
      else x::(edit_list xs (n-1) word)

(* Updates the board grid matrix to include the new word at coordinates (m,n)
 *    -[lst] is the board grid matrix
 *    -[n] is the row/column where the word should be inserted
 *    -[m] id the offset within the nth row/column
 *    -[word] is the char list of the word string to be inserted
 *)
let rec edit_index (lst: cell list list) (m: int) (n: int) (word: char list) :
cell list list =
  match lst with
  | [] -> []
  | x::xs ->
      (if n = 0 then edit_list x m word else x)::edit_index xs m (n-1) word

let update_board grid word coordinates direction =
  let (rows, cols) = grid in
  let (a,b) = coordinates in
  let (a',b') = ((int_of_char a)-65, b-1) in
  let wlist = to_char_list word in
  match direction with
  | Down -> let new_cols = edit_index cols b' a' wlist in
        (transpose new_cols, new_cols)
  | Across -> let new_rows = edit_index rows a' b' wlist in
        (new_rows, transpose new_rows)


(* Returns the type of the cell to be printed on the board. TW represents triple
 * word tiles. DW represent double word tiles. TL represent triple letter tiles.
 * DL represented double letter tiles.
 *    -[c] is the cell we wish to get the type of
 *)
let get_type (c: cell) : string =
  match c.letter_mult, c.word_mult, c.letter with
  | 1, 3, None -> "|\027[48;5;208m\027[38;5;0m TW \027[0m"
  | 1, 2, None -> "|\027[48;5;218m\027[38;5;0m DW \027[0m"
  | 3, 1, None -> "|\027[48;5;27m\027[38;5;0m TL \027[0m"
  | 2, 1, None -> "|\027[48;5;69m\027[38;5;0m DL \027[0m"
  | 1, 1, None -> "|\027[48;5;254m    \027[0m"
  | 1, 2, Some a -> ("|\027[48;5;219m\027[38;5;0m " ^ (Char.escaped a) ^
        "  \027[0m")
  | 1, 1, Some a -> ("|\027[48;5;179m\027[38;5;0m "^ (Char.escaped a) ^
       "  \027[0m")
  | _, _, _ -> failwith "Not valid tile"

(* Builds the rows of the board with the respective multipliers and characters
 *    -[row] is the current row of the board we are generating
 *)
let rec build_row (row: cell list) : string =
  match row with
  | [] -> "|"
  | x::xs -> (get_type x) ^ (build_row xs)

(* Generates the entire board by concatenating each row's values and their
 * borders
 *    -[rows] the row major form of the board grid
 *    -[n] is the current row number
 *)
let rec by_row (rows: cell list list) (n:int) : string =
  let underline = ("\n    +----+----+----+----+----+----+----+----+----+----+" ^
    "----+----+----+----+----+\n") in
  let endline = ("\n    +====+====+====+====+====+====+====+====+====+====+" ^
      "====+====+====+====+====+\n") in
  match rows with
  | [] -> ""
  | x::[] -> " " ^ (string_of_int n) ^ (if n < 10 then " " else "") ^  " " ^
      (build_row x) ^ endline
  | x::xs -> " " ^ (string_of_int n) ^ (if n < 10 then " " else "") ^ " " ^
      (build_row x) ^ underline ^ (by_row xs (n+1))


let print_board grid =
  let border = ("\n      A    B    C    D    E    F    G    H    I    J    K" ^
    "    L    M    N    O\n    +====+====+====+====+====+====+====+====+====+" ^
    "====+====+====+====+====+====+\n") in
  let (rows,cols) = grid in
  let scrabble = by_row rows 1 in
  print_string (border ^ scrabble)