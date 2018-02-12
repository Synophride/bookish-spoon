open Ast;;
module SS = Set.Make(String);;

type t =
  | Unit
  | Bool
  | Integer
  | Float
  | String
  | Char
  | Tuple of t list
  | List of t
  | Fun of (t list * t) (* Respectivement types des arguments en entrée, type de la sortie *)
  | Id of ident           (* Uniquement pour les types généraux *)
  | Var of t option ref ref (* Uniquement lors du typage (pas dans les types généralisés *)
;;

(** Représente les types généralisés **)
type g_t =
    T of t
  | Forall of ident * g_t
;;

(* Endroit de l'erreur, type attendu, type vu *)
exception Bad_type of location * t * t;;
exception Non_unifiable of t * t;;
exception Non_unifiable_lst;;
exception Not_found;;
exception WTFexception of string



let type_cte =
  function
  | Cunit -> Unit
  | Cbool(b) -> Bool
  | Cfloat(_) -> Float
  | Cint(_) -> Integer
  | Cstring(_) -> String
;;

let rec ist evt_rr_lst i_type =
  match i_type with
  | Unit
  | Bool
  | Integer
  | Float
  | String
  | Char -> i_type
  | Tuple(t_lst) -> let tlst = List.map (ist evt_rr_lst) t_lst in
		    Tuple(tlst)
  | List(t) -> let instd_t = ist evt_rr_lst t in
	       List(instd_t)
  | Fun(t_lst, ret) -> let tlst = List.map (ist evt_rr_lst) t_lst in
		       let ret_t= ist evt_rr_lst ret in
		       Fun( tlst, ret_t )
  | Id(ident) ->
     let rec find lst id =
       match lst with
       | [] -> raise Not_found
       | (identificateur, ref_ref_none)::s
	 ->
	  if identificateur = id
	  then ref_ref_none
	  else find s id
     in Var(find evt_rr_lst ident)
  | Var(_) -> (raise (WTFexception("Instanciation lors du typage : Var(t option ref ref) inattendu") ))
;;

let rec instanciation g_type evt_refref_lst =
  match g_type with
  | T(x) -> 
     ist evt_refref_lst x
  | Forall(ident, g_type2)
    -> let ref_ident = ref (ref None) in
       instanciation g_type2 ( (ident, ref_ident) :: evt_refref_lst)
;;

let rec generalisation i_type lst = ()
(* WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK *)
;;

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

  | Fun(l1, s1), Fun(l2, s2) 
    -> ( unif_lst l1 l2;
	 unification s1 s2
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


(**
   Q : Rendre un type instancié ou généralisé ?
     -> a priori généralisé, mais il faut instancier au moment de l'unification.
   fonction de typage 
   
   @param g_evt l'environnement généralisé
   @param i_evt l'environnement instancié (Nécessaire quand on veut typer des params de fonction par exemple)
   @param p_expr l'expression à typer
**)
let rec typage_w g_evt i_evt p_expr =
  let (pexpr_desc, pexpr_loc) = (p_expr.pexpr_desc , p_expr.pexpr_loc) in
  try
    typage_w_desc g_evt i_evt pexpr_desc
  with
  | Non_unifiable(type_attendu, type_vu) -> raise ( Bad_type(pexpr_loc, type_attendu, type_vu))     
and typage_w_desc g_evt i_evt =
  function
  (* Note: Pour l'unif, instancier les types *)
  | PE_cte(ct) -> T( type_cte ct ) 
  | PE_unop(op, pexpr)
    -> let type_pexpr = typage_w pexpr in
       (
	 match op with
	 | Unot -> (unification type_pexpr Bool;
		    T(Bool))
	 | Uminus -> (unification type_pexpr Integer;
		      T(Integer))
	 | Uminus_f -> (unification type_pexpr Float;
			Float)
       )
  | PE_binop(op, pexpr1, pexpr2)
    -> let t1, t2 = (typage_w pexpr1, typage_w pexpr2) in
       (
	 match op with
	 | Beq | Bneq | Blt | Ble | Bgt | Bge ->
					 (* NOTE : JETER LES TYPES FONCTIONNELS PENDANT L'EVAL *)
	    (unification t1 t2;
	     t1
	    )
	 | Badd | Bsub | Bmul | Bdiv ->
	    (unification Integer t1;
	     unification Integer t2;
	     Integer)
	 | Badd_f | Bsub_f | Bmul_f | Bdiv_f -> 
	    (unification Float t1;
	     unification Float t2;
	     Float)
	 | Band | Bor ->
	    (unification Bool t1;
	     unification Bool t2;
	     Bool)			      
       )
  | PE_if(cond, e1, e2) ->
     let tcond, t1, t2 = (typage_w cond, typage_w t1, typage_w t2) in
     (
       unification tcond;
       unification t1, t2;
     )
  (* expression, expression *) 
  | PE_app(f, x) -> raise Not_implemented
  (* Instanciation de f +( unification du premier type attendu par f avec x) + renvoi du type de f généralisé (faire attention au cas ou le type de x définit le reste du type de f) *)
  (* ?renvoyer un truc instancié ou généralisé ?*)
  | PE_fun(pattern, expr) -> raise Not_implemented
     
  (* fun (pattern) -> expr
     -> Mettre la/les vars en variables instanciées
     -> Typer l'expression en rajoutant/remplaçant le truc associé dans le set_i 
     -> Note : Prendre en compte le cas chiant ou x est un tuple
     -> rendre type général 
  *)
  | _ -> raise Not_implemented
(*| PE_tuple(expr_lst) -> 
  | PE_let(isrec, pattern, e1, e2) ->
  (* | PE_match of p_expr * p_expr * (p_patt * p_patt * p_expr) *)
  | PE_nil 
  | PE_cons of p_expr * p_expr
*)  
;;

(*___________________________________________________________________________________________*)



(*
(** Rend un 'set' des identificateurs des variables libres dans expr **)
  let rec var_libres pexpr =
  vlib pexpr.pexpr_desc
  and vlib pexpr_desc = 
  match pexpr_desc with
  | PE_cte _ -> SS.empty
  | PE_ident(id) -> SS.singleton id
  | PE_unop(op, p_expr) -> var_libres p_expr
  | PE_binop(op, exp1, exp2) -> SS.union (var_libres exp1) (var_libres exp2)
  | PE_if(b_expr, e1, e2) -> SS.union (SS.union (var_libres e1) (var_libres e2)) (var_libres b_expr)
  | PE_let(isrec, pattern, e1, e2) -> SS.union (var_libres e1) (SS.remove pattern)
  | _ -> SS.empty
(* etc. *)

  (* WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK *)
  ;;


(** Rend le type inféré pour expr dans l'environnement  (cf. Poly) **)
  let rec w evt expr =
  match expr with
  | _ -> unit 
(* WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK WORK *)
  ;;
*)

