(* ***********
 * TYPAGE.ML
 * V 0.?
 * 28/02
 * **********)

open Ast;;
open Types;;
open Environnement;;


(* ******************
 * EXCEPTIONS 
 *******************)

exception Bad_type of t * t * location;;


let evt_general = ref [];;

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
    -> () (* ??? *) 
 
  | Badd | Bsub | Bmul | Bdiv
    -> Tuple([Integer; Integer])

  | Badd_f | Bsub_f | Bmul_f | Bdiv_f 
    -> Tuple([Float; Float])
     
  | Band | Bor
    -> Tuple ([Bool; Bool])
;;

let typage plets =
  let rec typage_pexp pexpr evt =
    try
      typage_pdesc (pexpr.pexpr_desc) evt
    with
    | Non_unifiable(t1, t2) -> raise Bad_type(t1, t2, location)
       
  and typage_pdesc pexpr_desc evt =
    match pexpr_desc with
    | PE_cte(c) -> typage_cte c
    | PE_ident(i) -> let typ = (find i evt) (* référence vers la vartype ou type présent de la variable ? (ou genre subst_todo) *)
    | PE_unop(operateur, pexpr)
      -> let typ_unop = typage_attendu_unop operateur in
	 
				   
    | PE_binop 
       
