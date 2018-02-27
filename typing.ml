open Ast;;
(* TODO:
    - Unification 
   
       * unification
       * Instanciation
       * Généralisation
*)
type location = int*int;;

type ident = string
;;

(* Pour l'instant, une liste de couples 
(nom_de_la_var : ident , type: types) 
   -> Relativement dégueulasse du point de vue algorithmique, m'enfin à la limite on s'en fout pour l'instant
   -> Pê on pourrait utiliser une hashmap (voire devrait) 
   -> Note : C'est un environnement général. 
*)
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
  | Id of ident (* Uniquement pour les types généraux *)
  | Var of vartype (* Uniquement dans des types instanciés (pas dans les types généralisés) *)
and vartype = {
  ident : ident;
    (* Q: Comment 'unifier' deux identificateurs ? *)
    mutable typ of t option
  }
;;

(** schémas **)
type scm =
    T of t
  | Forall of ident * scm
;;

type types =
    Typ of t
  | S of scm (* note : utilisé seulement dans les let *)
;;

type i_evt =
  (ident * t) list
;;

type g_evt =
  (ident * scm) list
;;

type environnement =
  {ins_evt : i_evt;
   gen_evt : g_evt }
;;

(* *******************************************
 * 
 * EXCEPTIONS
 * 
 * ***************************************** *)

(* Endroit de l'erreur, type attendu, type vu *)
exception Bad_type of location * t * t;;
exception Non_unifiable of t * t;;
exception Non_unifiable_lst;;
exception Not_found;;
exception WTFexception of string;;

(* *********************************************
 *
 * FONCTIONS LIEES A L'EVT
 * 
 * ********************************************)
 
let find_in_evt ident evt =
  (* 1 - Recherche dans l'environnement des trucs instanciés *)
  let inst_evt = evt.inst_evt in
  try
    let i_elt = List.find (fun (lst_ident, i_type) -> lst_ident = ident) (inst_evt) in
    i_elt 
  with Not_Found
    -> let g_evt = List.find(fun (lst_ident, g_type) -> lst_ident = ident) (evt.gen_evt) in
       instanciation g_evt
;;							      



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
  | Char, Char
    -> ()

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
    -> (unif_lst l1 l2;
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
(* Fonction utilitaire pour l'unification entre liste (Pê qu'il y a un itérateur pour ça d'ailleurs) *)
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
  | Fun(t_lst) -> let tlst = List.map (ist evt_rr_lst) t_lst in
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

(* Note : je laisse ça là au cas ou, mais a priori ce sera inutile *)
let incr_char char =
  let i_char = int_of_char char in
  let new_i_char = i_char + 1 in
  let new_char = char_of_int new_i_char in
  if(new_char < 'a') then raise (WTFexception( "'a' censé pas être là" ))
  else if (new_char > 'z') then 'a'
  else new_char
;;

let next_str =
  let str = ref "a" in
  (fun () -> !str) 
;;

(* Type de type t 
   Renvoie une valeur de type scm
*)

(* refaire cette fonction *)
let generalisation type_t =
  (* remplace les ref ref None par des ref ref Some(id) + Renvoi de la liste des identificateurs *)
  (* Contient une liste de (ident) *)
  let ref_lst = ref [] in 

  let rec first_passage t =
    match t with
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
    | Var(ref_ref_none) -> let i = ! ! ref_ref_none in (
      match i with
      | None ->
	 let new_id = next_str () in
	 (
	   ref_lst := new_id :: (!ref_lst);
	   (!ref_ref_none) := Some( Id(new_id) ) ;
	   t
	 )
      | Some(t) ->
	 Var(ref (ref ( Some(first_passage t))))
    )		       
    | Id(ident) -> raise (WTFexception ("Generalisation : trouvé un identificateur "))
  in
  let rec second_passage semi_i_type =
    match semi_i_type with
    | Unit
    | Bool
    | Integer
    | Float
    | String
    | Char -> semi_i_type

    | List(t) -> List(second_passage t)
    | Tuple(t_lst) -> Tuple(List.map second_passage t_lst)
    | Fun(t_lst) -> Fun(List.map second_passage t_lst)
    | Id(ident) -> Id(ident)
    | Var(t_op_rr)
      ->
       (
	 match (! (!t_op_rr)) with
	 | None -> raise ( WTFexception("Généralisation - Il reste des ref ref None après le premier passage") );
	 | Some(typ) -> second_passage (typ)
       )
  in
  
  (******************************************************************************)
  let third_passage quart_i_type lst_id =
    let f id acc  =
      ( Forall(id, acc) ) in
    (List.fold_right (f)  (lst_id)  ( T(quart_i_type) ) ) 
      
  in
  third_passage (second_passage (first_passage type_t) ) ( !ref_lst )
;;



(* *******************************************************************************
 * 
 * ALGO DE TYPAGE
 * 
 ********************************************************************************)

let typage expr evt =
  let finder x evt_lst =
    List.exists ((=) x) evt_lst
  in
  let rec w_pexpr expr evt =
    let (p_desc, p_loc) = (expr.pexpr_desc, expr.pexpr_loc) in
    try
      w_pexpr_desc pdesc evt
    with
      Non_unifiable(attendu, vu) -> raise ( Bad_type(attendu, vu, pexpr_loc) );
  and w_pexpr_desc expr evt=
    match expr with
    | PE_cte(c) -> (
      match c with
       | Cunit -> Typ(Unit)
       | Cbool -> Typ(Bool)
       | Cint  -> Typ(Integer)
       | Cfloat ->Typ(Float)
       | Cstring->Typ(String) 
    )
    | PE_ident(id) -> finder
	  
  in
  ()
;;

      
