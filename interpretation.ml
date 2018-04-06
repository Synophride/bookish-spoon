open Ast;;

type value =
  | Val_unit 
  | Val_bool of bool
  | Val_int of int
  | Val_float of float
  | Val_str of string
  | Val_fun of p_patt * p_expr
  | Val_lst of value list
  | Val_tuple of value list
;;

module Str_map =
  Map.Make(String)
;;

type environnement =
  value Str_map.t
;;

(* pê faire plus fin *)
(* pê créer un type substitution *)
let rec contains pattern_conteneur pattern_contenu =
  contains_pdesc pattern_conteneur.ppatt_desc pattern_contenu
and contains_pdesc pdesc pattern_contenu =
  match pdesc with
  | PP_any
    -> false

  | PP_ident (str)
    -> pdesc = pattern_contenu.ppatt_desc
  
  | PP_tuple (pattern_lst)
    -> List.exists (fun pattern -> contains pattern pattern_contenu) pattern_lst
;;



(* element à substituer ?= pattern ? *) 
let rec substitution pexp element_a_substituer element_remplacant =
  let new_pexprdesc = subs_pexprdesc (pexp.pexpr_desc) element_a_substituer element_remplacant in
  {pexpr_loc = pexp.pexpr_loc; pexpr_desc = new_pexprdesc}
and subs_pexprdesc pexpdesc element_a_substituer element_remplacant =
  match pexpdesc with
  | PE_cte(_)
    -> pexpdesc
     
  | PE_ident(i) ->
     if (i = (
       match element_a_substituer.ppatt_desc with | PP_ident(e) -> e | _ -> failwith "erreur dans la substitution"))
     then element_remplacant
     else pexpdesc
       
  | PE_unop(op, b) ->
     PE_unop(op, substitution b element_a_substituer element_remplacant)

  | PE_binop(op, exp1, exp2) ->
     PE_binop( op,
	       substitution exp1 element_a_substituer element_remplacant,
	       substitution exp2 element_a_substituer element_remplacant)
       
  | PE_app (exp_fun, exp_val)
    -> PE_app( substitution exp_fun element_a_substituer element_remplacant,
	       substitution exp_val element_a_substituer element_remplacant)

  | PE_if(bool_exp, e1, e2) 
    -> PE_if( 
      substitution bool_exp element_a_substituer element_remplacant,
      substitution e1 element_a_substituer element_remplacant,
      substitution e2 element_a_substituer element_remplacant)

  | PE_tuple(explist)
    -> PE_tuple(
      List.map (fun exp -> substitution exp element_a_substituer element_remplacant) explist)
     
  | PE_fun(pattern, exp)
    ->
     if contains pattern element_a_substituer
      then pexpdesc
      else PE_fun(pattern, substitution exp element_a_substituer element_remplacant) 

  | PE_let(isrec, x, e1, e2)
    -> if contains x element_a_substituer && not isrec 
      then PE_let(false, x, substitution e1 element_a_substituer element_remplacant, e2)
      else if isrec
      then pexpdesc
      else PE_let(false, x, substitution e2 element_a_substituer element_remplacant, substitution e2 element_a_substituer element_remplacant)

  | PE_match(exp, rendu1, (pattern_elt, pattern_suite, rendu2)) ->
     PE_match(
       substitution exp element_a_substituer element_remplacant,
       substitution exp element_a_substituer element_remplacant,
       
       if contains pattern_elt element_a_substituer 
	 || contains pattern_suite element_a_substituer
       then (pattern_elt, pattern_suite, rendu2)
       else (pattern_elt, pattern_suite, substitution exp element_a_substituer element_remplacant)
     )

  | PE_nil -> pexpdesc

  | PE_cons(elt, suite)
    -> PE_cons( substitution elt element_a_substituer element_remplacant,
		substitution suite element_a_substituer element_remplacant)

  | _ -> failwith ""
;;


let application_subst expression_a_changer pattern expression =
  (* Associe une expr a un pattern *)
  let rec gas  pattern exp acc =
    gas_desc (pattern.ppatt_desc) exp acc
  and gas_desc pattern_desc exp acc =
    match pattern_desc with
    | PP_ident(i) -> (i, exp.pexpr_desc) :: acc
    | PP_any -> acc
    | PP_tuple( ppatt_list ) ->
       List.fold_left2

	 (fun lst_evt_acc pattern exp
	    -> (gas (pattern) exp lst_evt_acc))
	 acc
	 ppatt_list
	 (match exp.pexpr_desc with
	 | PE_tuple(tlist) -> tlist

	 |_ -> failwith "application subst : les types ne sont pas les mêmes")
  in
  let lst = gas pattern expression [] in
    List.fold_left
      (fun acc (ident, new_exp) -> substitution acc
	{ppatt_loc = pattern.ppatt_loc; ppatt_desc = (PP_ident(ident))}
	(new_exp))
      expression_a_changer
      lst
;;
      
(*****************************)

let valuation_cte =
  function
  | Cunit -> Val_unit
  | Cbool(b) -> Val_bool(b)
  | Cint(i) -> Val_int(i)
  | Cfloat(f) -> Val_float(f)
  | Cstring(str) -> Val_str(str)
;;

let apply_unop op value =
  match op with
  | Unot -> (match value with 
    | Val_bool(v) -> Val_bool(not v)
    | _ -> failwith "apply unop : pas un booléen")
  | Uminus -> (match value with
    | Val_int(i) -> Val_int( -i )
    | _ -> failwith "apply : mauvais typage")
  | Uminus_f -> (match value with
    | Val_float(i) -> Val_float( -.i )
    | _ -> failwith "apply : mauvais typage")
;;

let apply_binop op v1 v2 =
  match op with
  | Beq -> Val_bool( v1 =  v2) (* on est assuré par le typage que v1 v2 de même type *)
  | Bneq-> Val_bool( v1 <> v2)
  | Blt -> Val_bool( v1 <  v2) 
  | Ble -> Val_bool( v1 <= v2) 
  | Bgt -> Val_bool( v1 >  v2) 
  | Bge -> Val_bool( v1 >= v2) 

  | Badd-> Val_int(
    (match v1 with Val_int(i) -> i | _ -> failwith "appl binop : mauvais typage") +
      (match v2 with Val_int(i) -> i | _ -> failwith "appl binop : mauvais typage")
  )
  | Bsub -> Val_int(
    (match v1 with Val_int(i) -> i | _ -> failwith "appl binop : mauvais typage") -
      (match v2 with Val_int(i) -> i | _ -> failwith "appl binop : mauvais typage")
  )
  | Bmul-> Val_int(
    (match v1 with Val_int(i) -> i | _ -> failwith "appl binop : mauvais typage") *
      (match v2 with Val_int(i) -> i | _ -> failwith "appl binop : mauvais typage")
  )  
  | Bdiv -> Val_int(
    (match v1 with Val_int(i) -> i | _ -> failwith "appl binop : mauvais typage") /
      (match v2 with Val_int(i) -> i | _ -> failwith "appl binop : mauvais typage")
  )

     
  | Badd_f
    -> Val_float(
    (match v1 with Val_float(i) -> i | _ -> failwith "appl binop : mauvais typage") +.
      (match v2 with Val_float(i) -> i | _ -> failwith "appl binop : mauvais typage")
  )
  | Bsub_f
    -> Val_float(
      (match v1 with Val_float(i) -> i | _ -> failwith "appl binop : mauvais typage") -.
	(match v2 with Val_float(i) -> i | _ -> failwith "appl binop : mauvais typage")
    )
  | Bmul_f
      -> Val_float(
    (match v1 with Val_float(i) -> i | _ -> failwith "appl binop : mauvais typage") *.
      (match v2 with Val_float(i) -> i | _ -> failwith "appl binop : mauvais typage")
  )
  | Bdiv_f 
    -> Val_float(
    (match v1 with Val_float(i) -> i | _ -> failwith "appl binop : mauvais typage") /.
      (match v2 with Val_float(i) -> i | _ -> failwith "appl binop : mauvais typage")
    )
     
  | Band ->
     Val_bool(
       (match v1 with Val_bool(b) -> b | _ -> failwith "apply binop : mauvais typage") &&	
	 (match v2 with Val_bool(b) -> b | _ -> failwith "apply binop : mauvais typage")
     )
 
  | Bor ->
     Val_bool(
       (match v1 with Val_bool(b) -> b | _ -> failwith "apply binop : mauvais typage") || 
	 (match v2 with Val_bool(b) -> b | _ -> failwith "apply binop : mauvais typage")
     )
;;


(* censé attribuer une valeur avec un environnement donné *)
let interpretation expression environnement =
  let rec inter pexpr evt =
    inter_desc (pexpr.pexpr_desc) environnement
  and inter_desc pexp evt =
    match pexp with
    | PE_cte(c) -> valuation_cte c
    | PE_ident(i) -> Str_map.find i evt
    | PE_unop(op, exp)
      -> let value = inter exp evt in
	 apply_unop op value

    | PE_binop(op, e1, e2)
      -> let value_e1, value_e2 = inter e1 evt, inter e2 evt in
	 apply_binop op value_e1 value_e2

    | PE_if(bool_exp, e1, e2)
      -> let v_bool = inter bool_exp evt in
	 if (match v_bool with | Val_bool(v) -> v | _ -> failwith "") 
	 then  inter e1 evt
	 else  inter e2 evt
    |_ -> failwith " "
  in ()
;;
