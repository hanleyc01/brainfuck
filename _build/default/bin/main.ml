(* *)
type expr =
  | INCP
  | DECP
  | INCB
  | DECB
  | PRINT
  | WRITE
  | LOOP of expr list
type prgrm = expr list 

type token = 
  | FORWARD
  | BACK
  | PLUS
  | MINUS
  | PERIOD
  | COMMA
  | LBRACK
  | RBRACK
  | NONE

let rec expr_to_string ex =
  match ex with
  | INCP -> "INCP"
  | DECP -> "DECP"
  | INCB -> "INCB"
  | DECB -> "DECB"
  | PRINT -> "PRINT"
  | WRITE -> "WRITE"
  | LOOP [] -> "LOOP []"
  | LOOP xs -> 
    let body = List.map expr_to_string xs |> String.concat " , " in
    "LOOP [ " ^ body ^ " ]"
  
let token_to_str ty = 
  match ty with
  | FORWARD -> "FORWARD"
  | BACK -> "BACK"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | PERIOD -> "PERIOD"
  | COMMA -> "COMMA"
  | LBRACK -> "LBRACK"
  | RBRACK -> "RBRACK"
  | NONE -> "NONE"

type ptr = int
type arr = int array

(** Function which interprets a Brainfuck program *)
let interp (p : prgrm) : unit =
  let rec interp_exp (e : expr) (a : arr) (n : ptr) : arr * ptr =
    match e with
    | INCP -> (a, n + 1)
    | DECP -> (a, n - 1)
    | INCB -> (Array.set a n ((Array.get a n) + 1);
      (a, n))
    | DECB -> (Array.set a n ((Array.get a n) - 1); (a, n))
    | PRINT -> (print_endline (Int.to_string (Array.get a n)); (a, n))
    | WRITE -> 
      let x = read_int () in
      (Array.set a n x; (a, n))
    | LOOP xs -> loop_exp xs a n
  and interp_prgrm (p : prgrm) (a : arr) (n : ptr) : arr * ptr =
    match p with
    | [] -> (a, n)
    | (x :: xs) ->
      let (a0, p1) = interp_exp x a n 
      in interp_prgrm xs a0 p1
  and loop_exp (p : prgrm) (a : arr) (n : ptr) : arr * ptr =
      if a.(n) != 0 then
        let (a0, n0) = interp_prgrm p a n in
        loop_exp p a0 n0
      else (a, n)
  in let a = Array.make 300 0 in
let _ = interp_prgrm p a 0 in
print_endline "program finished executing"

(* [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ...]
   [1 0  0 0 0 ...] INCB
   [2 ...] INCB
   [3 ....] INCB
    ^ 
   [3 0 ...] INCP
      ^
   [3 1 ...] INCB
   > 1
   [3 1 1 1 1 1 1 1 ...] (WRONG)
   
   [3 1 ...] DECP
    ^
    
   [2 1 ...] DECB
   
   if 2 != 0 then
    repeat instructions above
   otherwise
    finish instructions*)

(* +++[>+.<-] *)

(**From https://caml.inria.fr/pub/old_caml_site/FAQ/FAQ_EXPERT-eng.html#strings*)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let rec lex (conts : string) : token list =
  let tokenize (ch : char) : token =
    match ch with
    | '+' -> PLUS
    | '-' -> MINUS
    | '>' -> FORWARD
    | '<' -> BACK
    | ',' -> COMMA
    | '.' -> PERIOD
    | '[' -> LBRACK
    | ']' -> RBRACK
    | _ -> NONE in
  let tokens = explode conts |> List.map tokenize in
  let eq_none x = x != NONE in
  List.filter eq_none tokens

let rec parse (tokens : token list) : prgrm  =
  let rec parse_loop (ts : token list) (acc : expr list) : expr list * token list =

    match ts with
    | [] -> (acc, [])
    | (x :: xs) -> 
      match x with
      | FORWARD -> parse_loop xs (acc @ [INCP])
      | BACK -> parse_loop xs (acc @ [DECP])
      | PLUS -> parse_loop xs (acc @ [INCB])
      | MINUS -> parse_loop xs (acc @ [DECB])
      | PERIOD -> parse_loop xs (acc @ [PRINT])
      | COMMA -> parse_loop xs (acc @ [WRITE])
      | NONE -> parse_loop xs acc
      | RBRACK ->
        (acc, xs) 
      | LBRACK ->
        let (es, ts) = parse_loop xs [] in
        parse_loop ts (acc @ [LOOP es])
  in 
  match tokens with
  | [] -> []
  | (x :: xs) -> 
    match x with
    | FORWARD -> INCP :: parse xs
    | BACK -> DECP :: parse xs
    | PLUS -> INCB :: parse xs
    | MINUS -> DECB :: parse xs
    | PERIOD -> PRINT :: parse xs
    | COMMA -> WRITE :: parse xs
    | NONE -> parse xs
    | LBRACK ->
      let (head, tail) = parse_loop xs [] in
      LOOP head :: parse tail
    | RBRACK -> raise (Failure "Unexpected right bracket without loop body")

let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []
  
let _ =
  let argv = Sys.argv in
  let argc = Array.length argv in
  if argc = 2 then 
    let file_name = argv.(1) in
    let conts = String.concat " " (read_lines file_name) in
    lex conts |> parse |> interp  
  else
    print_endline "No file provided, exiting normally";
    exit 1
