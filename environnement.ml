(* *****************
 * ENVIRONNEMENT.ML
 * Version 0.0
 * 28/02
 * ****************)
  
(* Q: Comment définir l'environnement ? *)

(* *** "Cahier de charges :"
 *   
 *  Variables de type : Une pseudo-structure union-find-like;
 *  Chaque identifiant étant identifié par un chiffre;;
 * 
 ** ***)

open Ast;;
open Types;;

(* ****************
 * TYPES 
 * ****************)

type scm =
    T of t
  | Forall of (int * scm)
;;


(* les variables de types sont les identificateurs, qui se réfèrent à la vartype list *)
type types =
    Typ of t
  | S of scm  (* note : utilisé seulement dans les let *)
;;

type vartype = {
  id : int;
  mutable o_typ : (t option); (* = Id(id2) avec id2 < id; si unifié avec une autre vartype *)
}
;;

type environnement =
  {
    evt_ex : (ident * int) list;
    evt_in : vartype list; (* pê rajouter generalized_evt *) 
  } (* comment gérer la généralisation de tout ça ? *)
;;


(* ****************
 *
 * EXCEPTIONS
 * 
 * ****************)

(* Pê on devrait changer de exception *)
exception Non_unifiable of t * t;;

(* ********************
 *
 * TODO FUNS 
 * 
 * ********************)

let void_evt =
  {evt_in = []; evt_ex = []}
;; 

let get_id ident evt =
  0
;;

(* ********************
 * FONCTIONS "PRIVEES"
 * ********************)

let new_sequence () =
  let a = ref 0 in
  let seq () =
    let b = !a in
    (a := !a + 1;
     b)
  in seq
;;

let next_val =
  new_sequence ()
;;


(**
   Remplace l'identificateur dans l'environnement, par le type typ.
   
   Q : Est_ce qu'on met un booléen qui dit s'il faut le généraliser ?

   - On doit s'assurer qu'un seul couple ident/lst existe par environnement
   -> si remplacement, on refait deux nouvelles listes
   - une nouvelle liste evt_ex ou l'identificateur n'est pas lié au même entier
   - une nouvelle liste evt_in ou les entiers sont différents. 
   
   @param evt l'environnement auquel rajouter le couple identificateur*type
   @param typ le type qui sera associé à l'identificateur 
   @param generalize booléen indiquant s'il faut généraliser
   @return un tuple (typ, environnement)
**)
let add_or_replace evt ident typ =
  let new_id = next_val () in
  let old_id = ref (-1) in
  (* i. refaire la nouvelle liste *)
  let (lst_ident_id, was_seen) =
    List.fold_left
      (
	fun (lst_rendue, was_seen) (idt, id) ->
	  if ident = idt && (not was_seen) 
	  then
	    (old_id := id;
	     (((ident, new_id) :: lst_rendue), true))
	  else if ident = idt && (was_seen)
	  then failwith "add_or_replace : deux fois le même ident dans la liste"
	  else (lst_rendue, was_seen)
      )
      ([], false)
      (evt.evt_ex)
  in
  let new_lst_ident_id =
    if was_seen
    then lst_ident_id
    else (ident, new_id) :: lst_ident_id
  in
  let new_evt_in =
    {id = new_id; o_typ = if typ = Id(-1) then None else Some(typ) } :: evt.evt_in

  in
  (  Id(new_id), {evt_in =new_evt_in; evt_ex = new_lst_ident_id} ) 
;;

(*
  (*
  on laisse cette fonction là 
  FIXME
*)
  let copy_varlist varlst =aaaaaa
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
	   List.fold_left
	     (fun acc elt ->
	       if elt.r_typ == old_ref_vartyp
	       then
		 ({id = elt.id; r_typ = new_ref_vartyp} :: acc)
	       else
		 acc
	     )
	     acc
	     varlst
  in
  cvl varlst acc' 
  in cvl varlst []
  ;;
*)

(* ********************
 * FONCTIONS PUBLIQUES
 * ********************)

(* fixme *)
let get_typ_pattern patt =
  let rec get_type_pattern pattern evt =
    match pattern with
    | PP_any -> Id(next_val () )
    | PP_ident(str) -> Id(get_id str evt)
    | PP_tuple(pattern_lst) -> Id (0)
  in Id (0)
;;
  

(* *** 
 * fixme
 * rend un couple (Type, nouvel_environnement) 
 * ***)
let rec add_pattern_to_evt evt pattern =
  add_pattern_to_evt_d (pattern.ppatt_desc) evt
and add_pattern_to_evt_d pattern evt =
  match pattern with
  | PP_any
    -> let new_val = next_val () in 
       (Id(new_val),
	{evt_ex = evt.evt_ex;
	 evt_in =( {id = new_val; o_typ = None} :: evt.evt_in) })

  | PP_ident(ident)
    -> (add_or_replace evt ident ( Id(-1) ) )
  
  | PP_tuple( ppatt_lst )
    ->
     (* todo : vérifier que c'est dans le bon sens *)
     let (new_typ, new_evt) = List.fold_left
       (
	 fun (acc_typ, acc_evt) pattern ->
	   let (new_typ, new_evt) = add_pattern_to_evt acc_evt pattern in
	   ( (new_typ :: acc_typ ) , new_evt) 
       )
       ( [], evt ) 
       ppatt_lst
     in (Tuple(new_typ), new_evt)
;;


(**
   Rend le type (instancié) associé à l'identificateur 
   @param ident l'indentificateur à associer 
   @param evt l'environnement dans lequel rechercher
   @return la variable de type représentant l'identificateur passé en paramètre
   fixme
**)
let find ident evt =
  let rec find_representant vt =
    match vt.o_typ with
    | None -> vt
    | Some(Id(id)) -> if(id <> vt.id)
      then find_representant (List.find (fun f -> f.id = id) evt.evt_in)
      else vt
    | Some(t) -> vt
  in
  let (_, id) = List.find (fun (a, _) -> a = ident) (evt.evt_ex) in 
  let vartyp = List.find (fun vt -> vt.id = id) (evt.evt_in) in
  let representant = find_representant vartyp in
  let a = match representant.o_typ with
  | None -> Id(representant.id)
  | Some(truc) -> truc
  in  {id = 0; o_typ = None }
;;

(* fixme
   donne la variable de type représenant *)
let find_representant id evt =
  {id = 0; o_typ = Some(Id(0))}
;;

(* fixme *)
let rec unification t1 t2 evt =
  match t1, t2 with
  | Unit, Unit
  | Bool, Bool
  | Integer, Integer
  | Float, Float
  | String, String
  | Char, Char -> t1
  | Tuple(t_list1), Tuple(t_list2)
    ->
     (
       try
	 Tuple(List.map2 (fun x y -> unification x y evt) t_list1 t_list2)
       with
	 Invalid_argument(a) -> raise (Non_unifiable (t1, t2));
     )
  | Fun(t_list1), Fun(t_list2) ->
     (
       try
	 Fun(List.map2 (fun x y -> unification x y evt) t_list1 t_list2)
       with
       | (Invalid_argument(a)) -> raise (Non_unifiable (t1, t2));
     )
       
  (* c'est là que ça va chier *)
  | Id(id1), Id(id2) -> let representant1, representant2 =
			  find_representant id1 evt, find_representant id2 evt in
			(
			  match representant1.o_typ, representant2.o_typ with
			  | None, None ->
			     if id1 < id2
			     then (representant2.o_typ <- Some(Id(id1));
				   Id(id1))
			     else if id1 > id2 
			     then (representant1.o_typ <- Some(Id(id2));
				   Id(id2))
			     else Id(id1)
			  | Some(t1), None -> (* on sait que t1 pas un id(truc) car on part du principe que on a bien le bon *)
			     if(id1 < id2)
			     then (representant2.o_typ <- Some(Id(id1));
				   t1)
			     else if id2 < id1
			     then (
			       representant2.o_typ <- Some(t1);
			       representant1.o_typ <- Some(Id(id2));
			       t1
			     )
			     else failwith "typage : ";
			    
			  | None, Some(t2) ->
			     if(id1 < id2)
			     then (
			       representant1.o_typ <- Some(t2);
			       representant2.o_typ <- Some(Id(id1));
			       t2
			     )
			     else if id2 < id1
			     then (
			       representant1.o_typ <- Some(Id(id2));
			       t2
			     ) else failwith ""
			  | Some(t1), Some(t2) -> unification t1 t2 evt (* il faudrait pê prendre en compte le cas ou t1 est dans t2, ou inversement *) 
			)
  | _ -> raise (Non_unifiable(t1, t2))
;;



(* la liste de vartyp ne contient pas deux_mêmes vartyp *)

(*
  
(* **************
  *
  * To see later 
  *
  * **************)

(**
   Unifie deux types.
   Renvoie le type unifiant typ1 et typ2
   Si les types sont pas unifiables, la fonction lance une exception "Non_unifiable(t1, t2)", ou t1 et t2 sont de type t.
   
   @param t1 un type instancié
   @param t2 un autre type instancié
  @return unit
   
   (* FIXME *)
   Paramètres : Deux types + un environnement intérieur
                Les types peuvent être Id(truc), auquel cas on se réfère au référent de la vartype.
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
*)
