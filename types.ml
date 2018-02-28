(* ********************
 * TYPES.ML
 * V 0.0
 * 28/02
 *********************)
open Ast
;;
exception Bad_type of location * t * t;;

exception WTFexception of string;;

type t =
  | Unit
  | Bool
  | Integer
  | Float
  | String
  | Char
  | Tuple of t list
  | List of t
  | Fun of (t list)
  | Id of ident
;;
