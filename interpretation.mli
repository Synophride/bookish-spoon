type value =
    Val_unit
  | Val_bool of bool
  | Val_int of int
  | Val_float of float
  | Val_str of string
  | Val_fun of Ast.p_patt * Ast.p_expr
  | Val_lst of value list
  | Val_tuple of value list
module Str_map :
  sig
    type key = String.t
    type 'a t = 'a Map.Make(String).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end
type environnement = value Str_map.t
val contains : Ast.p_patt -> Ast.p_patt -> bool
val contains_pdesc : Ast.p_patt_desc -> Ast.p_patt -> bool
val substitution : Ast.p_expr -> Ast.p_patt -> Ast.p_expr_desc -> Ast.p_expr
val subs_pexprdesc :
  Ast.p_expr_desc -> Ast.p_patt -> Ast.p_expr_desc -> Ast.p_expr_desc
val application_subst : Ast.p_expr -> Ast.p_patt -> Ast.p_expr -> Ast.p_expr
val valuation_cte : Ast.constant -> value
val apply_unop : Ast.unop -> value -> value
val apply_binop : Ast.binop -> value -> value -> value
val application_fun : Ast.p_patt -> Ast.p_expr -> Ast.p_expr -> Ast.p_expr
val interpretation : Ast.p_expr -> value Str_map.t -> value
val pdef_interp : Ast.p_def -> value
val plets_interp : Ast.p_def list -> value list
val str_pattern : Ast.p_patt -> Ast.ident
val str_of_binop : 'a -> string
val str_expr : Ast.p_expr -> Ast.ident
val str_value : value -> string
