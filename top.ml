open Format;;
open Interpretation;;
open Types;;

let evt = ref 0;;
(* ou p_def list * types list * values list *)
(* Semble plus simple à implémenter mais faire attention aux conneries *)


    
(** Create a lexer buffer which reads from the given string **)

let rec top () = 
  while true do
    Printf.printf "\t miniML version 0.1\n\n";
    let line = read_line () in
    let lb = Lexing.from_string line in
    let dl = Parser.lets Lexer.token lb in (* parser: pdef list *)
    let plets_interpreted = plets_interp dl in
    List.iter
      (fun x -> Printf.printf "%s\n" (str_value (x)))
      plets_interpreted
  done;;

top ();;
