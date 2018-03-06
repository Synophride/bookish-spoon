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
    @param typ le 'typ' (instancié donc) à généraliser  
    @return le scm généralisant la
**)
let gnt typ =
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

(* Q : peut-on bousiller les vartypes ? 
   R : Généralisation une seule fois - dans le let: on peut bien bousiller les vartypes
   * NOTE : Ne JAMAIS reutiliser les vartypes
*)
let generalisation =
  function
  | Typ(v) -> v
  | S(_) -> raise (WTFexception("Généralisation d'un scm"))
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
  ()
;;

(** 
    Remplace l'identificateur dans l'environnement, par le type typ.

    Q : Est_ce qu'on met un booléen qui dit s'il faut le généraliser ?

    - On doit s'assurer qu'un seul couple ident/lst existe par environnement

    @param ident l'identificateur représentant le type à remplacer 
    @param typ le type qui sera associé à l'identificateur 

    @return un environnement evt' , tel que evt' = evt + ident : typ 
**)
let add_or_replace ident typ evt=
  ()
;;

(**
   Unifie deux types. 
   L'unification ne rend rien, elle est un effet de bord
   Si la fonction renvoie unit, les deux fonctions sont bien unifiables (et unifiées, du coup)
   Si les types sont pas unifiables, la fonctions lance une exception "Non_unifiable(t1, t2)
   
   @param t1 un type instancié
   @param t2 un autre type instancié
   @return unit
**)
let unification t1 t2 =
  ()
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
