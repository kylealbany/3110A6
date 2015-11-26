
type cell = {letter_mult : int; word_mult : int; letter : char option}

type rows = cell list list

type columns = cell list list

type grid = rows * columns


let safe_hd (lst: 'a list) : 'a option =
  match lst with
  | [] -> None
  | x::xs -> Some x

let safe_tl (lst: 'a list) : 'a list option =
  match lst with
  | [] -> None
  | x::xs -> Some xs

(* Transposes a cell matrix so that input rows are stored as columns and input
 * colums are stored as rows
 *   -[mat] is a cell matrix repesenting the row or the column format of the
 *    board
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
  let xx = {letter_mult = 1; word_mult = 1; letter = None} in
  let row1 = [tw;xx;xx;dl;xx;xx;xx;tw;xx;xx;xx;dl;xx;xx;tw] in
  let row2 = [xx;dw;xx;xx;xx;tl;xx;xx;xx;tl;xx;xx;xx;dl;xx] in
  let row3 = [xx;xx;dw;xx;xx;xx;dl;xx;dl;xx;xx;xx;dw;xx;xx] in
  let row4 = [dl;xx;xx;dw;xx;xx;xx;dl;xx;xx;xx;dw;xx;xx;dl] in
  let row5 = [xx;xx;xx;xx;dw;xx;xx;xx;xx;xx;dw;xx;xx;xx;xx] in
  let row6 = [xx;tl;xx;xx;xx;tl;xx;xx;xx;tl;xx;xx;xx;tl;xx] in
  let row7 = [xx;xx;dl;xx;xx;xx;dl;xx;dl;xx;xx;xx;dl;xx;xx] in
  let row8 = [tw;xx;xx;dl;xx;xx;xx;xx;xx;xx;xx;dl;xx;xx;tw] in
  let rows =
    [row1;row2;row3;row4;row5;row6;row7;row8;row7;row6;row5;row4;row3;row2;row1]
  in
  (rows, transpose rows)
(*   failwith "*drake and josh voice* Megan..." *)

let update_board grid word coordinates direction =
  failwith "todo"

let print_board grid =
  failwith "Bales is too quiet"






