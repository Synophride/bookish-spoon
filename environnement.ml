(* ************
 * ENVIRONNEMENT.ML
 * Version 0.0
 * 28/02
 **************)
  
(* Q: Comment définir l'environnement ? *)

(* *** "Cahier de charges :"
 * On doit pouvoir manipuler :
 *    - schémas
 *    - types
 *    - Variables de type
 * 
 * Fonctions publiques :
 *  - Unification (type1, type2)
 *  - Ajout/Remplacement d'un type + ident
 * 
 * Fonctions cachées :
 *  - Instanciation / Généralisation
 *  
 * *****************************************
 * 
 * Types instanciés : 
 *    Comment faire s'il y a des variables de type dans le bordel ?
 *      -> est-ce qu'un type instancié devient (vartype) list * t
 *            => Du coup, quand on tombe sur un identificateur dans un type on va chercher dans la liste de vartypes pour déterminer la référence vers le type     
 * 
 * ***)

open Ast;;
open Types;;

(* ****************
 * TYPES 
 * ****************)

type vartype = {
  id : int;
  mutable r_typ : (t option) ref;
}
;;

type scm =
    T of t
  | Forall of (int * scm)
;;

type typ = vartype list * t
;;

(* les variables de types sont les identificateurs, qui se réfèrent à la vartype list *)
type types =
    Typ of typ
  | S of scm  (* note : utilisé seulement dans les let *)
;;

(* Environnement : Associe un type (qui peut être généralisé) à un identificateur *)
type environnement =
  (ident * types) list (* Pour l'instant, par une liste, ce qui est dégueulasse du point de vue algo, mais...*)
;;

(* ****************
 * EXCEPTIONS
 * ****************)
exception Non_unifiable of typ * typ;;

(* ********************
 * FONCTIONS "PRIVEES"
 * ********************)
let id_vt = ref 0
;;

let get_new_vartyp () =
  let new_vt = {id = !id_vt; r_typ = ref None } in
  (id_vt := !id_vt + 1;
   new_vt)
;;

let copy_varlist varlst =
  let rec cvl varlst acc =
    match varlst with
    | [] -> acc
    | elt :: s ->
       (* On regarde si la référence de l'identificateur a déjà été créée : 
	  Si oui, on continue sur la suite de la liste, sinon on ajoute la référence *)
       if ( List.exists
	      (fun x ->
		let x_id = x.id in
		x_id = elt.id
	      )
	      acc
       )
       then
	 cvl s acc
       else
	 (* 1. Création de la nouvelle référence -> de la vartype *)
	 let new_ref_vartyp = ref None in
	 let old_ref_vartyp = elt.r_typ in
	 (* 2. Regarder dans le reste de la liste pour voir s'il y a des variables polymorphes liées à l'identificateur présent, si oui on les ajoute à l'identificateur *)
	 (* Fold_left, testant le "==", retournant une liste *)
	 let acc' =
	   List.rev_append (
	     List.fold_left
	       (fun acc elt ->
		 if elt.r_typ == old_ref_vartyp
		 then
		   ({id = elt.id; r_typ = new_ref_vartyp} :: acc)
		 else
		   acc
	       )
	       []
	       varlst
	   )
	     acc
	 in
	 cvl varlst acc' 
  in cvl varlst []
;;

(** 

**)
let instanciation scm =
  let rec ist scm existing_vartypes_lst =
    match scm with
    | T(t) -> Typ(existing_vartypes_lst, t)
    | Forall(id, f_scm)
      -> let added_vartyp = {id; r_typ = ref None} in
	 ist f_scm (added_vartyp :: existing_vartypes_lst)
  in
  ist scm []
;;

(**
    Generalise un type passé en paramètre
    @param typ le 'typ' (instancié donc) à généraliser  
    @return le scm généralisant le paramètre 
**)
let generalisation typ =
  (* *** 1. ***
   * on part d'un type instancié (de type typ), on veut faire en sorte de
   * relier deux vartypes pointant vers le même ref None
   * à un même identificateur
   * Renvoie un t, semi-généralisé
   *)
  let rec gen vart_l t  =
    match t with
    | Unit
    | Bool
    | Integer
    | Float
    | String
    | Char -> t

    | Fun(t_lst) 
      -> Fun(List.map (gen vart_l) t_lst)
     
    | Tuple(t_lst)
      -> Tuple(List.map (gen vart_l) t_lst)
       
    | List(t) ->  List (gen vart_l t)
       
    | Id(id)
      -> try
	   let vart = List.find (fun vart -> vart.id = id) vart_l in
	   (
	     match !(vart.r_typ) with
	     | None ->
		(
		  vart.r_typ := Some( Id(id) );
		  Id(id)
		)
	     | Some( Id(id) ) -> Id (id)
	     | Some( new_t ) -> gen vart_l new_t
	   )
	with Not_found -> raise (WTFexception("généralisation : un identifiant inexistant a été trouvé"))
  in
  (* *** 2.
   * Maintenant qu'on a fait en sorte que les identificateurs représentent bien les variables polymorphes,
   * on fait la liste des variables de types pour pouvoir mettre les Forall
   *
   * ***)
  let get_lst_forall vartyp_lst =
    List.fold_left
      (fun acc elt ->
	let id_vartyp = elt.id in
	let typ_vt = !( elt.r_typ ) in
	match typ_vt with
	| Some( Id( x ) ) -> if x = id_vartyp then (x::acc) else acc
	| _ -> acc
      )
      []
      vartyp_lst
  in
  (* puis on ajoute les forall *)
  let put_forall lst_idents typ =
    List.fold_left (fun acc elt -> Forall(elt, acc)) typ lst_idents
  in
  let (vart_l, t) = typ in
  let new_t = gen vart_l t in
  let lst_of_id = get_lst_forall vart_l in
  put_forall lst_of_id ( T(new_t) )
;;


(* ********************
 * FONCTIONS PUBLIQUES
 * ********************)

(**
   Rend le type (instancié) associé à l'identificateur 
   @param ident l'indentificateur à associer 
   @param evt l'environnement dans lequel rechercher
   @return le type (instancié) associé à l'identificateur
**)
let find ident evt =
  let ret_type = List.find (fun (x, _) -> x = ident ) evt in
  match ret_type with
  | Typ(t) -> t
  | Scm(s) -> instanciation s
;;

(**
   Remplace l'identificateur dans l'environnement, par le type typ.
   
   Q : Est_ce qu'on met un booléen qui dit s'il faut le généraliser ?

   - On doit s'assurer qu'un seul couple ident/lst existe par environnement

   @param evt l'environnement auquel rajouter le couple identificateur*type
   @param typ le type qui sera associé à l'identificateur 
   @param generalize booléen indiquant s'il faut généraliser
   @return un environnement evt' , tel que evt' = evt + ident : typ 
**)
let add_or_replace evt ident typ generalize =
  if generalize
  then (ident, generalisation typ) :: evt
  else (ident, typ) :: evt
;;

(**
   Unifie deux types
   L'unification ne rend rien, elle est un effet de bord
   Si la fonction renvoie unit, les deux types sont bien unifiables (et unifiées, du coup)
   Si les types sont pas unifiables, la fonction lance une exception "Non_unifiable(t1, t2)" 
   
   @param t1 un type instancié
   @param t2 un autre type instancié
   @return unit
**)
let unification typ1 typ2 =
  
  let rec unf var_list t1 t2
    match t1, t2 with (* peut-on utiliser le = ? *) 
    | Unit, Unit
    | Bool, Bool
    | Integer, Integer
    | Char, Char
    | String, String
    | Float, Float -> t1

    | Tuple(t_lst1),Tuple(t_lst2) -> Tuple(List.map2 (fun e1 e2 -> unf e1 e2 ))
    | Fun(t_lst1), Fun(t_lst2) 
      -> List.iter2 (unf)  t_lst1 t_lst2) 

    | List(t), List(t2) -> unf (vt1, t1) (vt2, t2)

    | Id(i1), Id(i2)
      -> let vt1, vt2 = (List.find (fun x -> x.id = i1) vt1,
		       List.find (fun x -> x.id = i2) vt2)
	 in (* penser à penser à acheter du savon et du café *)
	 (
	   match (!vt1.r_typ), (!vt2.r_typ) with
	   | None, None
	     -> vt1.r_typ <- vt2.r_typ;
	   | None, Some(truc)
	     -> vt1.r_typ <- vt2.r_typ;
	   | Some(truc), None
	     -> vt2.r_typ <- vt1.r_typ;
	   | Some(truc1), Some(truc2)
	     ->
	      ( unf (vt1, truc1) (vt2, truc2);
		vt2.r_typ <- vt1.r_typ
	      )
	 )
    | Id(i1), _
      ->  let vl = List.find (fun x -> x.id = i1) vt1 in
	  vl.r_typ <- t2
    | _ -> raise ( Non_unifiable(typ1, typ2) )
 
  and unf_lst vt1 vt2 e1 e2 =
    unf (vt1, e1) (vt2, e2)
;;


(* ******************
 * FONCTIONS "PUBLIQUES"
 * *****************)

(* ************************* 
 *
 * FONCTIONS PRECENDENTES : probablement inutiles mais peuvent resservir 
 * 
 * ************************)
(* 


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
*)
