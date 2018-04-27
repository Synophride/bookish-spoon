open Format;;
open Interpretation;;
open Typing;;

(** Create a lexer buffer which reads from the given string **)

let print_val_aj =
  List.iter (fun (identifiant, value) -> Printf.printf "%s %s\n" identifiant (str_value value))
;;

  
let rec top environnement = 
  let line = read_line () in
  let lb = Lexing.from_string line in
  let dl = Parser.lets Lexer.token lb in
  try 
    let typ = Typing.typage_plets dl in			     
    let new_evt, valeurs_interp = plets_interp dl environnement in
    (
      print_val_aj valeurs_interp;
      top new_evt
    )
  with
  | Failure(_) | Not_found
    -> (Printf.printf "fail \n"; top environnement)
;;

Printf.printf "\t miniML version 0.1\n\n";
top Str_map.empty;;
