open Format;;
open Lexing;;
open Lexer;;
open Parser;;
open Ast;;
open Interpretation;;
open Typing;;

let file_test = ["ex1.miniml" ; "ex2.miniml"; "ex3.miniml"]
;;

let file_descriptor_test = List.map (open_in) file_test
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
      List.iter
	(fun e ->
	  let lb = Lexing.from_channel e in
	  let dl = Parser.lets Lexer.token lb in (* parser: pdef list *)
          let plets_interpreted = plets_interp dl in
	  List.iter (fun x -> Printf.printf "%s\n" (str_value (x))) plets_interpreted
	)
	(file_descriptor_test )
    with
      Lexical_error s -> ()
    | Parsing.Parse_error -> ()
  done;  
;;

