open Ast;;

type t =
  | Unit
  | Bool
  | Integer
  | Float
  | String
  | Char
  | Tuple of t list
  | List of t
  | Fun of (t list) (* Respectivement types des arguments en entrée, type de la sortie *)
  | Id of ident           (* Uniquement pour les types généraux *)
  | Var of t option ref ref (* Uniquement lors du typage (pas dans les types généralisés *)
;;

(** schémas **)
type scm =
    T of t
  | Forall of ident * scm
;;

type types =
    T of t
  | S of scm
;;

(* Endroit de l'erreur, type attendu, type vu *)
exception Bad_type of location * t * t;;
exception Non_unifiable of t * t;;
exception Non_unifiable_lst;;
exception Not_found;;
exception WTFexception of string;;





(* ********************************************
 *
 * UNIFICATION
 *
 **********************************************) 

(**
   Unification entre deux types t1 et t2.
   @param t1, t2 de type 't'. 
   @return unit
   @raise Non_unifiable (t1, t2) si l'unification est impossible. Les valeurs de l'exception sont celles passées en entrée.
   @version 0.1
   **** A TESTER ****
**)
let rec unification t1 t2 =
  match t1, t2 with
  (* 1 : Cas triviaux *) 
  | Bool, Bool
  | Unit, Unit
  | Integer, Integer
  | Float, Float
  | String, String
  | Char, Char -> ()

  (* 2 : Cas presque triviaux *)
  | Tuple(tl1), Tuple(tl2)
    -> (
      try
	unif_lst tl1 tl2;
      with
	Non_unifiable_lst -> (raise ( Non_unifiable (t1, t2)))
    )
     
  | List(t1), List(t2)
    -> unification t1 t2

  | Fun(l1), Fun(l2) 
    -> ( unif_lst l1 l2;
    )

  (* cas comportant des variables de type *)
  (* TODO : Petit paragraphe pour expliquer ce truc là *)
  | Var(op_ref_ref1), Var(op_ref_ref2) -> (
    let t_op1, t_op2 = ( ! !op_ref_ref1, ! !op_ref_ref2 ) in
    match t_op1, t_op2 with
    | None, None -> let op_ref2 = !op_ref_ref2 in
		    op_ref_ref1 := op_ref2
    | Some(t1) ,Some(t2) -> unification t1 t2
    | Some(t1), None -> let op_ref1 = !op_ref_ref1 in (* référence vers la valeur Some(t1) *) 
			op_ref_ref2 := op_ref1
    | None, Some(t2) -> let op_ref2 = !op_ref_ref2 in
			op_ref_ref1 := op_ref2
  )

  | _ , Var(op_ref_ref2) ->(
    match !(!op_ref_ref2) with
    | None -> !op_ref_ref2 := Some(t1)
    | Some(t) -> unification t1 t
  )
  | Var(op_ref_ref1), _ -> unification t2 t1
  | _ -> raise ( Non_unifiable (t1, t2) )
(* Fonction utilitaire pour l'unification entre liste (Pê qu'il y a un itérateur pour ça d'ailleurs *)
and unif_lst l1 l2 =
  match l1, l2 with
  | x :: s , y :: t -> (unification x y;
			unif_lst s t)
  | ([], []) -> ()
  | _ -> raise (Non_unifiable_lst)     
;;



(* *******************************************************************
 ***
 *** INSTANCIATION / GENERALISATION
 *** 
 ******************************************************************** *)

let rec ist evt_rr_lst i_type =
  match i_type with
  | Unit | Bool | Integer | Float | String | Char
    -> i_type

  | Tuple(t_lst) -> let tlst = List.map (ist evt_rr_lst) t_lst in
		    Tuple(tlst)
  | List(t) -> let instd_t = ist evt_rr_lst t in
	       List(instd_t)
  | Fun(t_lst) -> let tlst = List.map (ist evt_rr_lst) t_lst 
		  Fun(tlst)
  | Id(ident) ->
     let rec find lst id =
       match lst with
       | [] -> raise Not_found
       | (identificateur, ref_ref_none)::s
	 ->
	  if identificateur = id
	  then ref_ref_none
	  else find s id
     in Var( find evt_rr_lst ident )
  | Var(_) -> (
    raise (WTFexception("Instanciation lors du typage : Var(t option ref ref) inattendu")))
;;

(* écrit. à tester *)
(** @param g_type de tupe scm
*)
let rec instanciation g_type evt_refref_lst =
  match g_type with
  | T(x)
    -> ist evt_refref_lst x
  | Forall(ident, g_type2)
    -> let ref_ident = ref (ref None) in
       instanciation g_type2 ( (ident, ref_ident) :: evt_refref_lst)
;;

(* *******************
 * TODO 
 ******************* *)

(* Note : je laisse ça là au cas ou, mais a priori ce sera inutile  *)
let incr_char char =
  let i_char = int_of_char char in
  let new_i_char = i_char + 1 in
  let new_char = char_of_int new_i_char in
  if(new_char < 'a') then raise WTFException("'a' censé pas être là")
  else if (new_char > 'z') then 'a'
  else new_char
;;

let incr_str =
  let str = ref "a" in
  !str
;;

(* Type de type t *)
let generalisation type_t =
  (* remplace les ref ref None par des ref ref Some(id) + Renvoi de la liste des identificateurs *)
  (* Contient une liste de (ident) *)
  let ref_lst = ref [] in 
  let rec first_passage t =
    match t with
    (* incr_str : rend un nouvel identificateur *)
    | Unit
    | Bool
    | Integer
    | Float
    | String
    | Char -> t
       
    | List(t) -> List(first_passage t)
    | Tuple(t_lst)  -> Tuple(List.map first_passage t_lst)
    | Fun(t_lst) -> Fun(List.map first_passage t_lst)
    (* TODO : vérifier que le raisonnement est bon + test *) 
    | Var(ref (ref None) ) -> let new_id = incr_str () in
			      (
				ref_lst := new_id :: (!ref_lst);
				let Var(ref_ref_none) = t in
				(!ref_ref_none) := Some( Id(new_id) ) ;
				t
			      )
    | Var(_) -> t
  in
  
(* il reste a remplacer les ref ref Some(id) par id 
   + mettre les id en tête du scm *)
;;
