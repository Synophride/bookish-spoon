open Format;;
open Interpretation;;
open Typing;;

(** Create a lexer buffer which reads from the given string **)

let print_val_aj =
  List.iter (fun (identifiant, value) ->
    Printf.printf "%s = %s\n" identifiant (str_value value) )
;;

  
let rec top environnement ev_t = 
  let line = read_line () in
  let lb = Lexing.from_string line in
  let dl = Parser.lets Lexer.token lb in
  try
    let ev_t' = typage_plets dl ev_t in 			     
    let new_evt, valeurs_interp = plets_interp dl environnement in
    (
      print_val_aj valeurs_interp;
      Printf.printf "# ";
      top new_evt ev_t'
    )      
  with
  | Failure(_)
  | Not_found
    -> (Printf.printf "Erreur dans le typage \n# ";
	top environnement ev_t)
;;

Printf.printf "\t miniML version 0.7\n\n# ";
top Str_map.empty Str_map.empty;;
