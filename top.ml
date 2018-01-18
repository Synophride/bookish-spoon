open Format
open Lexing
open Lexer
open Parser
open Ast

let () =
  printf "\t miniML version 0.1\n\n"

(** Create a lexer buffer which reads from the given string **)
let ()= 
  let lb = Lexing.from_string e in
  try
    (* dl : de type plets = liste de déclarations au toplevel = pdefs list *)
    let dl = Parser.lets Lexer.token lb in
    let a :: suite = dl in
  (* pdef =
      desc=(
           isrec, -> true si c'est une fonction récursive
           identificateur, -> nom de la var/fonction -> Peut être défini dans un tuple
           expression -> 
      )
     + location 
  *)
  with
    Lexical_error s -> ()
  | Parsing.Parse_error -> ()
  | _ -> ()x


 

