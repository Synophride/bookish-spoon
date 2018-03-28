(* *****************
 * ENVIRONNEMENT.ML
 * Version 0.0
 * 28/02
 ******************)
  
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


type subst =
  {old_id : int; new_id : int}
;;

(* ****************
 * EXCEPTIONS
 * ****************)
exception Non_unifiable of typ * typ;;

(* ********************
 * FONCTIONS "PRIVEES"
 * ********************)

let get_sequence () =
  let a = ref 0 in
  let seq () =
    let b = !a in
    (a := !a + 1;
     b)
  in seq
;;

let new_id =
  get_sequence ()
;;

(* on laisse cette fonction là *)
let copy_varlist varlst =
  let rec cvl varlst acc =
    match varlst with
    | [] -> acc
    | elt :: s ->
       (* On regarde si la référence de l'identificateur a déjà été créée : 
	  Si oui, on continue sur la suite de la liste, sinon on ajoute la référence *)
       if (
	 List.exists
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
    ok.
**)
let instanciation scm =
  let rec get_all_vartyp_and_subst vartyp_lst subst_list scm =
    match scm with
    | T(t) -> (vartyp_lst, subst_list, t)
    | Forall(id, f_scm) 
      -> let subst = {old_id = id; new_id = new_id() } in
	 let new_vartyp = {id = subst.new_id; r_typ = ref None} in
	 get_all_vartyp_and_subst (new_vartyp :: vartyp_lst) (sust :: subst_list) f_scm
  in
  let rec substitute  subst_list typ =
    match typ with
    | Id(id) ->
       let new_subst = List.find (fun x -> x.old_id = id) subst_list in
       Id( new_subst.new_id )
    | List(typ) -> List( substitute subst_list typ)
    | Tuple(t_lst) -> Tuple( List.map  (substitute subst_list) t_lst )
    | Fun(t_lst) -> Fun( List.map  (substitute subst_list) t_lst)
    | _ -> typ (* pas d'identificateurs *) 
  in
  let (new_vartype_list, subst_list, t) = get_all_vart_and_subst [] [] scm in
  let new_type = substitute subst_list t in
  (new_vartype_list, new_type)
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


let get_id ident evt =
  let get_type =
    List.find (fun (x, y) -> ident = x) evt in
(* la liste de vartyp ne contient pas deux_mêmes vartyp *)
;;

(**
   Unifie deux types.
   Renvoie le type unifiant typ1 et typ2
   Si les types sont pas unifiables, la fonction lance une exception "Non_unifiable(t1, t2)", ou t1 et t2 sont de type t.
   
   @param t1 un type instancié
   @param t2 un autre type instancié
   @return unit
***)
let unification typ1 typ2 =
  let (vt_l1, t1), (vt_l2, t2) = typ1, typ2 in
  let vartyp_list = vt_l1 @ vt_l2 in (* Pour l'instant on le fait de manière bourrine*)  
  (* 
     Ne rend rien 
     Dans unf, on s'occupe uniquement des types, pas des variables de type 
     ** note : Vérifier les variables libres. **
  *)
  let rec unf t1 t2 =
    match (t1, t2) with (* peut-on utiliser le = ? *) 
    (* 1. : cas triviaux *)
    | Unit, Unit
    | Bool, Bool
    | Integer, Integer
    | Char, Char
    | String, String
    | Float, Float
      -> ()

    | Tuple(t_lst1),Tuple(t_lst2)
    | Fun(t_lst1), Fun(t_lst2) 
      -> (List.iter2 (unf) t_lst1 t_lst2) 
       
    | List(t), List(t2)
      -> unf (t1) (t2)

    | Id(i1), Id(i2)
      ->
       let vt1, vt2 = (List.find (fun x -> x.id = i1) vartyp_list,
		       List.find (fun x -> x.id = i2) vartyp_list)
       in (* penser à penser à acheter du savon et du café *)
       (
	 match (!vt1.r_typ), (!vt2.r_typ) with
	 | None, None (* on fait pointer les deux vartypes vers la même référence *)
	   -> vt1.r_typ <- vt2.r_typ;
	     
	 | None, Some(truc)
	   -> vt1.r_typ <- vt2.r_typ;
	 | Some(truc), None
	   -> vt2.r_typ <- vt1.r_typ;
	     
	 | Some(type1), Some(type2)
	   -> (* On pourrait pê tester les variables libres *)
	    (
	      unf type1 type2;
	      vt1.r_typ <- vt2.r_typ;
	      ()
	    )
       )
    | Id(i1), _
      ->  let vl = List.find (fun x -> x.id = i1) vt1 in
	  (unf (!(vl.r_typ)) t2;
	   vl.r_typ <- t2)

    | _, Id(i) -> unf t2 t1
       
    | _ -> raise ( Non_unifiable(typ1, typ2) )
  in
  ( vartyp_list, t1 )
;;


