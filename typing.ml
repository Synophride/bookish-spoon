(* ***********
 * TYPAGE.ML
 * V 0.?
 * 28/02
 * ***********)

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

module M =
struct
  type t = int
  let compare = compare
end
;;

module Int_map =
  Map.Make(M)
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
*)

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

(* ******** 
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

let next_val =
  get_cpt ()
;;

(* termes, variables de type *)

module Exp =
struct
  type t = Ast.p_expr
  let compare = compare
end
;;

module Exp_evt = Map.Make(Exp);;
module Ident_evt = Map.Make(String);;

type id_to_int = int Ident_evt.t
;;

type exp_to_vartyp = int Exp_evt.t
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
  (* si on a déjà vu la même chose*)
  | PE_cte(c)
    -> evt
     
  | PE_ident(i)
    -> evt
     
  | PE_unop(op, pexp)
    -> annotation_pexp pexp evt  
     
  | PE_binop(op, pexp1, pexp2)
    -> let evt1 = annotation_pexp pexp1 evt  in (* ii. ajout de la première expression dans l'evt *)
       annotation_pexp pexp2 evt1  (* puis la seconde exp *)
	 
  | PE_if(expb, e1, e2)
    -> let evt1 = annotation_pexp expb evt  in 
       let evt2 = annotation_pexp e1 evt1  in
       annotation_pexp e2 evt2  
	 
  | PE_app (exp_fun, exp_value)
    -> let evt1 = annotation_pexp exp_fun evt  in
       annotation_pexp exp_value evt1 
	 
  (* typer le pattern ? *)
  | PE_fun (pattern, exp)
    -> annotation_pexp exp evt
     
  | PE_tuple(exp_lst) ->
     List.fold_left
       (fun evt exp -> annotation_pexp exp evt) 
       evt
       exp_lst
       
  | PE_let(isrec, pattern, exp1, exp2)
    ->
     if isrec
     then failwith "mono"
     else
       let evt' = annotation_pexp exp1 evt in
       annotation_pexp exp2 evt' 
	 
  | PE_match(exp0, exp_r1, (pattern, pattern', exp_r2) )
    ->
     let evt' = annotation_pexp exp0 evt in
     let evt'' = annotation_pexp exp_r1 evt' in
     annotation_pexp exp_r2 evt''
     
  | PE_nil
    -> evt
     
  | PE_cons(e1, e2)
    -> let evt' = annotation_pexp e1 evt  in
       annotation_pexp e2 evt'  
;;

type equation =
  { old_typ : Types.t;
    new_typ : Types.t }
;;

let rec annotation_pattern pattern id_evt =
  annotation_patt_desc pattern.ppatt_desc id_evt
and annotation_patt_desc pattern id_evt =
  match pattern with
  | PP_any -> id_evt

  | PP_ident(id)
    -> Ident_evt.add (id) ( next_val () ) id_evt
     
  | PP_tuple(pattern_liste)
    ->
     List.fold_left
       (fun acc elt ->
	 annotation_pattern elt acc)
       id_evt
       pattern_liste
;;

let rec typage_pattern pattern id_evt=
  match pattern.ppatt_desc with
  | PP_any -> Id( next_val () )
  | PP_ident(idt) -> Id(Ident_evt.find (idt) (id_evt))
  | PP_tuple(pattern_lst)
    -> Tuple(List.map (fun x -> typage_pattern x id_evt) pattern_lst)
     
     
;;

(* ******* 
 * Comment gérer les identificateurs ?
 * a. deux types de vt ?
 * b. Même type de vt ?
 * 
 * Note : tous les appels récursifs ne sont pas faits
 * initialisation de l'environnement des identificaters ici
 *
 *******)
let rec ecriture_equat expr evt acc id_evt = 
  match expr.pexpr_desc with
  | PE_cte(c)
    -> let type_attendu = typage_cte c in
       { old_typ = Id( Exp_evt.find expr evt); 
	 new_typ = type_attendu} :: acc
	 
  | PE_ident(i)
    -> {old_typ = Id(Str_map.find i id_evt);
	new_typ = Id(Exp_evt.find expr evt)} :: acc
     
  | PE_unop(op, exp)
    -> let type_attendu = typage_attendu_unop op in
       let acc' =
	 {old_typ = Id(Exp_evt.find expr evt);
	  new_typ = type_attendu}
	 :: {old_typ = Id(Exp_evt.find exp evt);
	     new_typ = type_attendu}
	 :: acc
       in ecriture_equat exp evt acc' id_evt
       
  | PE_binop(op, exp1, exp2)
    -> let type_attendu = typage_attendu_binop op in
       let acc' = match type_attendu with
       | None ->
	  let acc' = {old_typ = Id(Exp_evt.find exp1 evt);
	   new_typ = Id(Exp_evt.find exp2 evt)}
	  :: { old_typ = Id(Exp_evt.find expr evt);
	       new_typ = Bool}
	    :: acc in
	  failwith "arrêt ici"
       | Some(type_attendu) ->
	  { old_typ = Id(Exp_evt.find exp1 evt);
	    new_typ = type_attendu}
	  :: { old_typ = Id(Exp_evt.find exp2 evt);
	       new_typ = type_attendu }
	  :: { old_typ = Id(Exp_evt.find expr evt);
	       new_typ = type_attendu} :: acc
       in
       let acc'' = ecriture_equat exp1 evt acc' id_evt in
       ecriture_equat exp2 evt acc'' id_evt 
	    
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
     let acc_b = ecriture_equat bool_exp evt acc' id_evt in
     let acc_e1 = ecriture_equat e1 evt acc_b id_evt in
     ecriture_equat e2 evt acc_e1 id_evt
       
  | PE_app(efun, eval)
    -> let acc' =
	 { old_typ = Id(Exp_evt.find efun evt);
	   new_typ = Fun( Id(Exp_evt.find expr evt), Id(Exp_evt.find eval evt) )
	 } :: acc
       in
       let acc_expf = ecriture_equat efun evt acc' id_evt in
       ecriture_equat eval evt acc_expf id_evt 

  | PE_fun(pattern, exp)
    -> let pattern_annote = annotation_pattern pattern id_evt in
       let type_pattern = typage_pattern pattern pattern_annote in
       let acc' = { old_typ = Id(Exp_evt.find expr evt);
		    new_typ = Fun(type_pattern, Id(Exp_evt.find exp evt))}
	 :: acc
       in ecriture_equat exp evt acc' pattern_annote
	   
  | PE_tuple(explist)
    -> let acc' =
	 { old_typ = Id(Exp_evt.find expr evt);
	   new_typ = Tuple(List.map (fun elt -> Id(Exp_evt.find elt evt)) explist)}
	 :: acc
       in
       List.fold_left
	 (fun lst_substs exp ->
	   ecriture_equat exp evt lst_substs id_evt)
	 acc'
	 explist

  | PE_let(isrec, pattern, e1, e2)
    ->
     if isrec then failwith "nonencoreimplémenteé"
     else
       let id_evt' = annotation_pattern pattern id_evt in
       let type_pattern = typage_pattern pattern id_evt' in
       let acc' = {old_typ = Id(Exp_evt.find e1 evt);
		   new_typ = type_pattern}
	 :: {old_typ = Id(Exp_evt.find expr evt);
	     new_typ = Id(Exp_evt.find e2 evt)}
	 :: acc
       in
       let acc'' = ecriture_equat e1 evt acc' id_evt' in
       ecriture_equat e2 evt acc'' id_evt'

  | PE_match(e1, r1, (p1, p2, r2))
    -> failwith "todo"
  | PE_nil -> failwith "todo"

  | PE_cons (e1, e2) ->
     let acc' = {old_typ = Id(Exp_evt.find e2 evt);
		 new_typ = List(Id(Exp_evt.find e1 evt))}
       :: {old_typ = Id(Exp_evt.find expr evt);
	   new_typ = Id(Exp_evt.find e2   evt)}
       :: acc in
     let acc'' = ecriture_equat e1 evt acc' id_evt in
     ecriture_equat e2 evt acc'' id_evt 

  | _ -> failwith ""
;;

