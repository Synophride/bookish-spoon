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

(* But : pouvoir changer des types a posteriori 
   1. Fonction récupérant des substitutions + les renvoyant
   2. Fonction donnant un type après unification 
*)

type id = int;;

(* un module map 
   + str -> id
   + id -> type
*)
module Str_map =
  Map.Make(String)
;; 

module Int_map =
  Map.Make(
    struct
      type t = int
      let compare = compare
    end
  )
;;

type evt_ex =
  id Str_map.t
;;

type evt_in =
  Types.t Int_map.t
;;

(* **************
 * Gérer unification,
 * généralisation des types, 
 * variables de type,
 * Variables de type : utilisation de substitutions ou des types mutables ?
 *  substitution : type evt_in -> type evt_in
 *  mutable type evt_in ->  t option ref
 *  + 
 *  -> il faudra récupérer le bon environnement "au-dessus"
 *************** *)







(* ******* ************ *)

































(******** ****************)
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


let typage plets evt=
  let rec typage_pexp pexpr evt =
    try
      typage_pdesc (pexpr.pexpr_desc) evt
    with
      Non_unifiable(t1, t2) ->
	raise (Bad_type(t1, t2, pexpr.pexpr_loc))	  
  and typage_pdesc pexpr_desc evt =
    match pexpr_desc with
    | PE_cte(c) -> typage_cte c
    | PE_ident(i) ->let vartyp = (find i evt) in
		    (
		      match vartyp.o_typ with
		      | None -> Id( vartyp.id)
		      | Some(t) -> t
		    )

    | PE_unop(operateur, pexpr)
      -> let typ_unop = typage_attendu_unop operateur in
	 let typ_expr = typage_pexp pexpr evt in
	 unification typ_unop typ_expr evt
	   
    | PE_binop(op, pexpr1, pexpr2)
      ->
       let typ1, typ2 = (typage_pexp pexpr1 evt, typage_pexp pexpr2 evt) in
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
       ( let t1, t2, t3 =
	   typage_pexp bool_exp evt,
	 typage_pexp e1 evt,
	 typage_pexp e2 evt 
	 in
	 unification t1 Bool evt;
	 unification t2 t3 evt
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
  let pdef = List.hd plets in
  let {pdef_desc ; pdef_loc} = pdef in
  let (isrec, pattern, exp) = pdef_desc in
  typage_pexp exp evt
;;

let typaj lets =
  let evt = {evt_in = []; evt_ex = []} in  
  Printf.printf "%s\n" (str_of_t (fun x -> " " ^ string_of_int x ^ " " )  (typage lets evt))
;;
