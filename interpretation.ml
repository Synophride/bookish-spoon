type value =
  | Value_unit 
  | Val_bool of bool
  | Val_int of int
  | Val_float of float
  | Val_char of char
  | Val_str of string
  | Val_fun of pattern * expr
  | Val_lst of value list
;;

module Str_map =
  Map.make(String)
;;

type environnement =
  value Str_Map.t
;;

(* pê faire plus fin *)
(* pê créer un type substitution *)
let rec contains pattern_conteneur pattern_contenu =
  contains_pdesc pattern.ppatt_desc elt
and contains_pdesc pdesc pattern_contenu =
  match pdesc with
  | PP_any
    -> false
  | PP_ident (str)
    -> pdesc = pattern_contenu.ppatt_desc
  | PP_tuple (pattern_lst)
    -> List.exists (fun pattern -> contains pattern pattern_contenu) pattern_lst
;;

(* element à substituer ?= ident ou pattern ? *) 
let rec substitution pexp element_a_substituer element_remplacant =
  let new_pexprdesc = substitution pexp.ppatt_desc element_a_substituer element_remplacant in
  {pexp when pexp_desc = new_pexprdesc} (* todo change dis  *)
and subs_pexprdec pexpdesc element_a_substituer element_remplacant =
  match pexpdesc with
  | PE_cte(_)
    -> pexpdesc
     
  | PE_ident(i) ->
     if (ident = (match element_a_substituer with | PE_ident(e) then e | _ -> failwith "erreur dans la substitution"))
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
    -> if contains pattern element_a_substituer
      then pexpdesc
      else PE_fun(pattern, substitution exp element_a_substituer element_remplacant) 

  | PE_let(isrec, x, e1, e2)
    -> if contains x element_a_substituer && not isrec 
      then PE_let(x, substitution e1 element_a_substituer element_remplacant, e2)
      else if isrec
      then pexpdesc
      else PE_let(x, substitution e2 element_a_substituer element_remplacant, substitution e2 element_a_substituer element_remplacant)
  (* match exp with | [] -> rendu1 
     | exp_elt :: exp_suite -> rendu2 *)
  | PE_match(exp, rendu1, (pattern_elt, pattern_suite, rendu2)) ->
     PE_match(
       substitution exp element_a_substituer element_remplacant,
       substitution exp element_a_substituer element_remplacant,
       
       if contains exp_elt element_a_substituer 
	 || contains exp_suite element_a_substituer
       then (pattern_elt, pattern_suite, rendu2)
       else (pattern_elt, pattern_suite, substitution exp element_a_substituer element_remplacant)
     )

  | PE_nil -> pexpr

  | PE_cons(elt, suite)
    -> PE_cons( substitution elt element_a_substituer element_remplacant,
		substitution suite element_a_substituer element_remplacant)

  | _ -> failwith ""
;;


let remplacement_pattern_to_valeur pattern value expression environnement =
  (* récupération de la liste des valeurs à changer *)
  let rec recuperation_liste_substitutions pattern value evt =
    match pattern with
    | PP_ident ( ident ) -> str_map
    | PP_any -> []
    | PP_tuple(PP) ->
       List.fold_left (fun acc elt -> recuperation liste_substitution) 
