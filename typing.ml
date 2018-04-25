(* ***********
 * TYPAGE.ML
 * V 0.?
 * 28/02
 * **********)

open Ast;;
open Types;;


(* *****************
 *
 * EXCEPTIONS 
 * 
 *******************)
exception Bad_type of t * t * location;;

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

type substitution =
  {elt_a_substituer : t; elt_substituant: t}
;;

(* liste de ces trucs pour
   un système d'équations 
   -> Premiere passe sur l'expression liant chaque 
   ident à une vartyp
   -> Seconde passe chiant toutes les équations
   -> Troisième passe substituant toutes les équation s
*)x

let get_cpt () =
  let a = ref 0 in
  let nextval () =
    let b = !a in (
      a := !a + 1;
      b)
  in nextval
;;



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



(* 
 * On part d'un environnement + d'une liste d'équastions
 * On veut construire un nouvel evt tel que, pour toute equation
 *  de la forme (t1 ?= t2) , on ait (evt.get t1 = evt.get t2)
 * ==> Unificateur le plus général de l'ensemble d'équations.

 * Lier une variable de type à chaque sous-expression : Map expr -> vartype
 *

 * L'unificateur principal est une liste de substitutions à effectuer 

 * Pê deux types de vt : un pour les ident, l'autre pour les expressions

 * substitution : sur les variables libres uniquement 

 *** unif ***
 * idées de variables et de termes
 * 
 * On part d'un evt + une expression : 
 * 
 * ?0? "Convertir" l'expression expressive en une expression de types
 * 
 * A. Association de chaque sous-expression, et chaque variable à 
 * une variable de type (renvoie un nouvel environnement)
 * 
 *  
 * B. Construction du système d'équations, comme dit dans le poly.
 * -> Renvoie une liste d'équations (var1 ?= var2)
 * 
 * 
 * C. Unification 
 * -> Renvoie la liste des substitutions la plus générale
 * 
 * D. Application des substitutions sur l'expression / l'expression typée
 * -> ? Pendant l'unification ?
 * 
 *)

let next_val = get_cpt ();;

(* termes, variables de type *)

module Exp =
struct
  type t = Ast.p_expr
  let compare = compare
end
;;

module Exp_evt = Map.Make(Exp);;

type exp_to_vartyp = int Exp_evt.t;;

let rec annotation_pattern pattern evt =
  failwith ""
;;



(**
   Annote une expression, en associant une variable (un int) à chaque sous-expression de pexpr

   @param pexpr 
   l'expression à annoter
   @param evt l'environnement 
*)
let rec annotation_pexp pexpr evt =
  annotation_pdesc (pexpr.pexpr_desc) (Exp_evt.add pexpr (next_val()) evt)
and annotation_pdesc pdesc evt =
  match pdesc with
    (* si on a déjà vu la même chose,  *)
  | PE_cte(c) ->   evt
  | PE_ident(i) -> evt
  | PE_unop(op, pexp) -> annotation_pexp pexp evt
     
  | PE_binop(op, pexp1, pexp2)
    -> let evt1 = annotation_pexp pexp1 evt in (* ii. ajout de la première expression dans l'evt *)
       annotation_pexp pexp2 evt1 (* puis la seconde exp *)
	 
  | PE_if(expb, e1, e2)
    -> let evt1 = annotation_pexp expb evt in 
       let evt2 = annotation_pexp e1 evt1 in
       annotation_pexp e2 evt2

  | PE_app (exp_fun, exp_value)
    -> let evt1 = annotation_pexp exp_fun evt in
       annotation_pexp exp_value evt1 

    (* typer le pattern ? *)
  | PE_fun (pattern, expr) ->
     let evt' = annotation_pattern pattern evt in
     annotation_pexp expr evt'

  | PE_tuple(exp_lst) ->
     List.fold_left
       (fun evt exp -> annotation_pexp exp evt)
       evt
       exp_lst

  | PE_let(isrec, pattern, exp1, exp2)
    -> let evt' = annotation_pattern pattern evt in
       let evt_e1 = annotation_pexp exp1 evt' in
       annotation_pexp exp2 evt_e1
	 
  | PE_match(exp0, exp_r1, (pattern, pattern', exp_r2) )
    -> failwith "flemme"
     
  | PE_nil -> evt
  | PE_cons(e1, e2)
    -> let evt' = annotation_pexp e1 evt in
       annotation_pexp e2 evt'
;;

type equation =
  { old_typ : Types.t;
    new_typ : Types.t}
;;

(* 
 * Comment gérer les identificateurs ?
 * a. deux types de vt ?
 * b. Même type de vt ?
 * Note : tous les appels récursifs ne sont pas faits
 *)

let rec ecriture_pdesc expr evt acc = 
  match expr.pexpdesc with
  | PE_cte(c)
    -> let type_attendu = typage_cte c in
       {old_typ = Id( Exp_evt.find expr evt); 
	new_typ = type_attendu} :: acc

  | PE_ident(i)
    -> failwith "todo"

  | PE_unop(op, exp)
    -> let type_attendu = type_attendu_unop op in
       let acc' = {old_typ = Id(Exp_evt.find expr evt);
	new_typ = type_attendu_unop} ::
	 {old_typ = Id(Exp_evt.find exp evt);
	  new_typ = type_attendu_unop} :: acc
       in ()
  | PE_binop(op, exp1, exp2)
    -> let type_attendu = type_attendu_binop op in
       let acc' = match type_attendu with
       (* I. Lier les deux types entre eux. 
	* II.Lier l'expression avec un booléen
	*)
       | None ->
	  {old_typ = Id(Exp_evt.find exp1 evt);
	   new_typ = Id(Exp_evt.find exp2 evt)}
	  :: { old_typ = Id(Exp_evt.find expr evt);
	       new_typ = Bool}
	  :: acc
       | Some(type_attendu) ->
	  { old_typ = Id(Exp_evt.find exp1 evt);
	    new_typ = type_attendu}
	  :: { old_typ = Id(Exp_evt.find exp2 evt);
	       new_typ = type_attendu }
	  :: { old_typ = Id(Exp_evt.find expr evt);
	       new_typ = type_attendu}
       in ()
  | PE_if(bool_exp, e1, e2)
    ->
     let acc' =
       {old_typ = Id(Exp_evt.find bool_exp evt);
	new_typ = Bool}
       :: {old_typ = Id(Exp_evt.find e1 evt);
	   new_typ = Id(Exp_evt.find e2 evt)}
       :: {old_typ = Id(Exp_evt.find e1 evt);
	   new_typ = Id(Exp_evt.find expr evt)}
       :: acc
     in
     ()
       
  | PE_app(efun, eval)
    -> let acc' =
	 { old_id = Id(Exp_evt.find efun evt);
	   new_id = Fun([Id(Exp_evt.find expr evt); Id(Exp_evt.find eval evt)])
	 } :: acc
       in () 

       

  | PE_fun(pattern, exp)
    -> failwith "todo"
  | PE_tuple(explist)
    -> failwith ""	
  
  | _ -> failwith ""
;;


let rec unification ens_equations evt = 
  match liste_equations with
  | [] -> expr
  | x :: suite
    ->
     (match (x.old_id, x.new_id) with
     | Id(old_), Id(new_)
       -> 
     )
let () = ();;

(* il faudrait créer une map id -> (variable de type/type) ? *)
and substitution_evt type_a_remplacer_type











(* ************************ 
 *
 * 
 **************************)


