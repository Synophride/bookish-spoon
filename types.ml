(* ********************
 * TYPES.ML
 * V 0.0
 * 28/02
 * ********************)

open Ast
;;

type t =
  | Unit
  | Bool
  | Integer
  | Float
  | String
  | Char
  | Tuple of t list (* au moins deux éléments *)
  | List of t
  | Fun of (t * t) 
  | Id of int      
;;


exception Bad_type of location * t * t;;
exception WTFexception of string;;

(*
let rec str_of_t wat_to_do_with_ids =
  function
  | Unit -> "unit"
  | Bool -> "bool"
  | Integer -> "int"
  | Float -> "float"
  | String -> "string"
  | Char -> "char"
  | Tuple(t_lst)
    -> let head = List.hd t_lst in
       let suite = List.tl t_lst in
       "( " ^
	 (str_of_t wat_to_do_with_ids head) ^
	 List.fold_left
	 (fun acc elt -> acc ^ " ** " ^ str_of_t wat_to_do_with_ids elt)
	 ""
	 suite
	 
       ^ " )"
	 
  | Fun(a, b)
    -> let head =  a in
       let suite = b in
       "( " ^  str_of_t wat_to_do_with_ids head ^
       ^ " -> " ^ (str_of_t wat_to_do_with_ids suite)
       ^ " )"
  | List(t) ->
     str_of_t wat_to_do_with_ids t ^ " list"
  | Id(i) -> (wat_to_do_with_ids i)
;;
*)
 
