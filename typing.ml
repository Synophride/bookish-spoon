(* ***********
 * TYPAGE.ML
 * V 0.?
 * 28/02
 * **********)

open Ast;;
open Types;;
open Environnement;;


(* *****************
 *
 * EXCEPTIONS 
 * 
 *******************)

exception Bad_type of t * t * location;;

(* Note : pê prendre en compte les questions de variables de types *)
let typage_cte =
  function
  | Cunit -> Unit
  | Cbool(_) -> Bool
  | Cint(_) -> Integer
  | Cfloat(_) -> Float
  | Cstring(_) -> String
;;

let typage_attendu_unop =
  function
  | Unot -> Bool
  | Uminus -> Integer
  | Uminus_f -> Float
;;

let typage_attendu_binop =
  function
  (* ces opérateurs sont polymorphes *)
  | Beq | Bneq | Blt | Ble | Bgt | Bge
    -> None
     
  | Badd | Bsub | Bmul | Bdiv
    -> Some(Integer)
     
  | Badd_f | Bsub_f | Bmul_f | Bdiv_f 
    -> Some(Float)
     
  | Band | Bor
    -> Some(Bool)
;;

let typage plets =
  let rec typage_pexp pexpr evt =
    try
      typage_pdesc (pexpr.pexpr_desc) evt
    with
      Non_unifiable(t1, t2) ->
	raise (Bad_type(t1, t2, pexpr.pexpr_loc))	  
  and typage_pdesc pexpr_desc evt =
    match pexpr_desc with
    | PE_cte(c) -> typage_cte c
    | PE_ident(i) -> find i evt
    | PE_unop(operateur, pexpr)
      -> let typ_unop = typage_attendu_unop operateur in
	 let typ_expr = typage_pexp pexpr in
	 unification typ_unop typ_expr evt (* on peut pê simplifier ça *)
    | PE_binop(op, pexpr1, pexpr2)
      ->
       let typ1, typ2 = (typage_pexp pexpr1, typage_pexp pexpr2) in
       let type_attendu = typage_attendu_binop op in
       (
	 match type_attendu with
	 | None -> (unification typ1 typ2 evt) (* changer quand on a la généralisation *)
	 | Some(typ_attendu)
	   -> unification (unification typ1 typ2 evt) typ_attendu evt
       )
    (* 1. Créer des variables de type pour le pattern *) 

    | PE_tuple(pexp_list) -> Tuple(List.map (fun x -> typage_pexp x evt) pexp_list)

    | PE_if(bool_exp, e1, e2) ->
       ( unification bool_exp Bool evt;
	 unification e1 e2 evt
       )

    | PE_fun(pattern, expr)
      -> let (new_typ, new_evt) = add_pattern_to_evt evt pattern in
	 let typ_exp = typage_pexp expr new_evt in 
	 Fun(new_typ :: [typ_exp]) (* pas forcément legit, à voir *)

    | PE_app(exp1, exp2)
      -> (
	let t1, t2 = typage_pexp exp1 evt, typage_pexp exp2 evt in
	match t1 with
	| Fun(x :: s) ->
	   (unification x t2 evt;
	    match s with
	    | [x] -> x
	    | x :: l -> Fun(s)
	    | _ -> raise (Non_unifiable(t1, t2)) 
	   )
	| _ -> raise (Non_unifiable(t1, t2))
      )
    | PE_let(is_rec, pattern, e1, e2)
      -> let t_e1 = typage_pexp e1 in
	 (* 
	    i) typer le pattern + ajouter les vartypes
	    ii) unifier type du pettern + t_e1
	    fixme
	 *) 
	 typage_pexp e2 evt

    (* reste à implémenter les listes *)
    | PE_nil -> List( Id(next_val () ) )
    | PE_cons(exp1, exp2) -> let t1, t2 = typage_pexp exp1 evt, typage_pexp exp2 evt in
			     (
			       match t1, t2 with
			       | _, List(t) -> unification t1 t evt
			       | _ -> raise (Non_unifiable(t1, t2))
			     )
    | PE_match(exp1, exp2, (pattern1, pattern2, exp3)) -> failwith ""
    | _ -> (failwith "not implemented")      
  in
  ()
;;
