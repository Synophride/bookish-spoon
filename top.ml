open Format
open Lexing
open Lexer
open Parser
open Ast
open Typing
;;

let evt = ref 0;;
(* ou p_def list * types list * values list *)
(* Semble plus simple à implémenter mais faire attention aux conneries *)
let () =
  Printf.printf "\t miniML version 0.1\n\n"

(** Create a lexer buffer which reads from the given string **)
let rec top () = 
  let e = "" in (* TODO : mettre la fonction récupérant une entrée clavier *)
  if e = "exit"
  then ()
  else    
    let lb = Lexing.from_string e in (* Lexing *)
    try
      (* dl : de type plets = liste de déclarations au toplevel = pdefs list *)
      let dl = Parser.lets Lexer.token lb in (* parser: pdef list *)
      let ld = ( (), dl) in
      fst ld
    (* pdef =
       desc=(
       isrec, -> true si c'est une fonction récursive
       identificateur, -> nom de la var/fonction -> Peut être défini dans un tuple
       expression -> expression associée 
       )
       + location 
    *)
    with
      Lexical_error s -> ()
    | Parsing.Parse_error -> ()
    | _ -> () 
;;

