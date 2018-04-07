open Format;;
open Lexing;;
open Lexer;;
open Parser;;
open Ast;;
open Interpretation;;
open Typing;;

let file_test = ["ex1.miniml" ; "ex2.miniml" ]
;;

let evt = ref 0;;
(* ou p_def list * types list * values list *)
(* Semble plus simple à implémenter mais faire attention aux conneries *)
let () =
  Printf.printf "\t miniML version 0.1\n\n"
    
(** Create a lexer buffer which reads from the given string **)
let rec top () = 
  while  1 = 1 do
    try 
      let e = read_line () in (* TODO : mettre la fonction récupérant une entrée clavier *)
      if e = "exit"
      then exit 0
      else
	let lb = Lexing.from_string e in	
	(* dl : de type plets = liste de déclarations au toplevel = pdefs list *)
	let dl = Parser.lets Lexer.token lb in (* parser: pdef list *)
	let ld = ( (), dl) in
	
    with
      Lexical_error s -> ()
    | Parsing.Parse_error -> ()
  done;
  
;;

