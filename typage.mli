open Ast
;;

exception Bad_type
;;


exception Not_implemented;;

type types =
    Unit
  | Alpha
  | Float
  | Integer
  | Bool
  | String
  | Unit
  | Pair of types * types
  | Fun of types list
;;


