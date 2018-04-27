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


let value_to_expr value location =
  let rec v_exp value location= 
    {pexpr_loc = location;
     pexpr_desc = (v_expdesc value location) }
  and v_expdesc value location =
    match value with 
    | Val_unit
      -> PE_cte( Cunit )
    | Val_bool( b )
      -> PE_cte( Cbool(b) )
    | Val_int(i)
      -> PE_cte ( Cint(i) )
    | Val_float(f)
      -> PE_cte( Cfloat(f) )
    | Val_str(s)
      -> PE_cte( Cstring(s) )
       
    | Val_fun(p_patt , p_expr)
      -> PE_fun(p_patt, p_expr)

    | Val_lst(val_l)	
      ->(match val_l with
      | [] -> PE_nil
      (* trouver l'expression correspondant à une liste *)
      | x :: s -> PE_cons(v_exp x location,
			{pexpr_loc = location; pexpr_desc =  List.fold_right
			    (fun elt cons_expr ->
			      PE_cons((v_exp elt location), {pexpr_loc = location;
							     pexpr_desc = cons_expr})
			    )
			    s
			    PE_nil})
      
      
      )
  | Val_tuple(val_l)
    -> PE_tuple(List.map (fun x -> v_exp x location) val_l)
	 in v_exp value location
;;
(* **************** str shit ***********************)




(** Affiche un pattern sous forme de Strign **)

let rec str_pattern patt=
  match patt.ppatt_desc with 
  | PP_any -> "_"
  | PP_ident(str) -> str
  | PP_tuple(pattlst) -> "( " ^
     (List.fold_left
	(fun acc elt -> acc ^ " * " ^ str_pattern elt)
	""
	pattlst
     ) ^ " )"
;;

(** Affiche un opérateur binaire sous forme de String 
fixme
**)
let str_of_binop =
  function
  | Beq -> " = "
  | Bneq -> " != "
  | Blt -> " < "
  | Ble -> " <= "
  | Bgt -> " > "
  | Bge -> " >= "
  | Badd -> " + "
  | Badd_f -> " +. "
  | Bsub -> " - "
  | Bsub_f -> " -. "
  | Bmul -> " * "
  | Bmul_f -> " *. "
  | Bdiv -> " / "
  | Bdiv_f -> " /. "
  | Band -> " && "
  | Bor -> " || "
     
;;

(** Affiche une expression sous forme de Strign **)
let rec str_expr expr =
  str_expr_desc expr.pexpr_desc
and str_expr_desc expr = 
  match expr with
  | PE_cte(c) ->
     (match c with
     | Cunit -> "()"
     | Cbool(b) -> if b then "true" else "false"
     | Cint(i) -> string_of_int i
     | Cfloat(f) -> string_of_float f
     | Cstring(str) -> "\"" ^str^ "\""
     )
  | PE_ident(id) -> id
  | PE_unop(op, expr) ->
     (match op with | Unot -> "! "  | Uminus -> "é" | Uminus_f -> "~." )
     ^ "expr "
  | PE_binop(op, e1, e2) ->
     "(" ^ (str_expr e1) ^ (str_of_binop op) ^ (str_expr e2) ^ ")"
  | PE_if(b, th, els)
    -> "if " ^ (str_expr b) ^ "\nthen " ^ (str_expr th) ^ "\nelse " ^ (str_expr els)
  | PE_app(e1, e2) -> (str_expr e1) ^ " " ^ (str_expr e2)
  | PE_fun(pattern, expr) -> "fun " ^ (str_pattern pattern) ^" -> " ^ (str_expr expr)
  | PE_tuple(lst) -> "(" ^
     (List.fold_left
	(fun acc elt -> acc ^ "* " ^ str_expr elt)
	""
	lst
     ) ^ " )"
     
  | PE_let(isrec, pattern, e1, e2)
    ->
     "let " ^ (if isrec then "rec " else "") ^ (str_pattern pattern) ^ (str_expr e1) ^ " in\n" ^ (str_expr e2)
  | PE_match(exp1, retour1, (pattern_elt, pattern_suite, retour2)) ->
     "match " ^ (str_expr exp1) ^ " with \n | [] -> " ^
       (str_expr retour1) ^ " \n | " ^
       (str_pattern pattern_elt) ^ " :: " ^ (str_pattern pattern_suite) ^ " -> " ^
       (str_expr retour2)
  |PE_nil -> "[]"
  | PE_cons(expr_x, expr_s) -> (str_expr expr_x) ^ " :: " ^ (str_expr expr_s)
;;
       
(**  rend la valeur sous forme de chaîne **)
let rec str_value =
  function
  | Val_unit -> "()"
  | Val_bool(b) -> if b then "true" else "false"
  | Val_int(i) -> string_of_int i
  | Val_float(f) -> string_of_float f
  | Val_fun(pattern , pexpr) -> " fun " ^ (str_pattern pattern) ^ " -> " ^ (str_expr pexpr)
  | Val_lst(val_lst)
    -> " [" ^ (List.fold_left (fun acc elt -> acc ^ "; " ^ (str_value elt)) "" val_lst ) ^ "]"
  | Val_tuple(val_lst)
    -> " (" ^ (List.fold_left (fun acc elt -> acc ^", "^ (str_value elt)) "" val_lst) ^ ")"
  | Val_str(str) -> "\"" ^ str ^"\"" 
;;


let print_evt str =
  Str_map.iter (fun key value -> Printf.printf "( %s -> %s)" key (str_value value)) str;
  Printf.printf "\n"
;;




(** 
    Fonction déterminant si pattern_contenu est contenu dans pattern_conteneur
    @param pattern_contenu le pattern dont on teste s'il est contenu dans l'autre.
    Pour l'instant, pattern_contenu forcément de la forme PP_ident(i)
    @return booléen qui détermine si le pattern est contenu dans le pattern
**)
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

(**
   Renvoie une nouvelle expression, dans laquelle on a substitué element_a_remplacer par element_remplacant, dans l'expression pexp
**)
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
       else PE_let(false, x,
		  substitution e2 element_a_substituer element_remplacant,
		  substitution e2 element_a_substituer element_remplacant)
	
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

(** 
    Renvoie la liste des paires (identificateur, expression) associée à un pattern + une expression
**)
let rec gas  pattern exp acc =
  gas_desc (pattern.ppatt_desc) exp acc
and gas_desc pattern_desc exp acc =
  match pattern_desc with
  | PP_ident(i) -> (i, exp.pexpr_desc) :: acc
  | PP_any -> acc
  | PP_tuple( ppatt_list ) ->
     List.fold_left2
       (fun lst_evt_acc pattern exp ->
	 (gas (pattern) exp lst_evt_acc))
       acc
       ppatt_list
       (match exp.pexpr_desc with
       | PE_tuple(tlist) -> tlist
	  
       |_ -> failwith "application subst : les types ne sont pas les mêmes")
;;


(** 
    Applique une substitution, et ce sans restriction sur le pattern
**)
let application_subst expression_a_changer pattern expression =
  (* Associe une expr a un pattern *)

  let lst = gas pattern expression [] in
    List.fold_left
      (fun acc (ident, new_exp) -> substitution acc
	{ppatt_loc = pattern.ppatt_loc; ppatt_desc = (PP_ident(ident))}
	(new_exp))
      expression_a_changer
      lst
;;


(**********************************)

(** Évalue la valeur d'une constante **)
let valuation_cte =
  function
  | Cunit -> Val_unit
  | Cbool(b) -> Val_bool(b)
  | Cint(i) -> Val_int(i)
  | Cfloat(f) -> Val_float(f)
  | Cstring(str) -> Val_str(str)
;;

(** Evalue un opérateur unaire **)
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

(** Evalue une expression composée d'un opérateur binaire **)
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

(** Applique une fonction 
    fun pattern->expr_fun à une valeur value_exp **)
let application_fun pattern_fun expr_fun value_exp =
  application_subst expr_fun pattern_fun value_exp 
;;

(* str_map -> value *)
(* censé attribuer une valeur avec un environnement donné *)

(** 
    Interprète une expression en paramètre, en fonction de l'environnement passé en paramètre 
**)
let interpretation expression environnement =
  let rec inter pexpr evt =
    inter_desc (pexpr.pexpr_desc) evt
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

    (* 6/10 legit *)
    | PE_app(exp_fun, exp_val) ->
       let value_fun = inter exp_fun evt in
       let value = inter exp_val evt in (* a la valeur non-f *)
       let pattern_fun, exp_fun = (
	 match value_fun with
	 | Val_fun(pattern, pexp) -> pattern, pexp
	 | _ -> failwith ""
       )
       in
       let new_expr = application_fun pattern_fun exp_fun exp_val in
       inter new_expr evt
    (* substitution *)
    | PE_fun(param, exp) ->
       (* avec contains *)
       let substitution_lst = Str_map.bindings evt in
       let loc = param.ppatt_loc in
       let nouvelle_expression =
	 List.fold_left
	   (fun p_exp (ident, value) ->
	     let e = value_to_expr value loc in
	     let pattern = ({ppatt_loc = loc;
			     ppatt_desc = PP_ident(ident)})
	     in
	     if contains param pattern
	     then p_exp
	     else substitution p_exp pattern (e.pexpr_desc)
	   )
	   exp
	   substitution_lst
       in 
	   Val_fun(param, nouvelle_expression)

    | PE_tuple(pexp_lst) -> Val_tuple( List.map (fun exp -> inter exp evt) pexp_lst)

    | PE_let(isrec, pattern, e1, e2)
      -> if isrec
	then
	  failwith "fonctions récusives non encore implémentées"
	else
	  let id_exp_lst = gas pattern e1 [] in (

	    List.iter (fun (ident, expr) -> Printf.printf "%s %s\n" ident (str_expr_desc expr)) id_exp_lst ;

	    let evt_mis_a_jour =
	      List.fold_left
		(fun acc_evt (i,exp) ->
		  let interpreted_exp = inter_desc exp evt in 
		  Printf.printf "%s\n" (str_value interpreted_exp);
		  print_evt acc_evt;
		  Str_map.add (i) (interpreted_exp) acc_evt)
		evt 
		id_exp_lst
	    in
	    inter e2 evt_mis_a_jour
	  )
	      
    (* encore 60 % legit *)
    | PE_match(expr, rendu1, (e_elt, e_suite, r2))
      ->
       if (expr.pexpr_desc = PE_nil) (* ie si la liste est vide *)
       then inter rendu1 evt
       else
	 let value_lst = inter expr evt in 
	 let exp_elt, exp_suite =
	   (match expr.pexpr_desc with
	   | PE_cons(e1, e2) -> e1, e2
	   | _ -> failwith "match avec pas une liste"
	   ) in
	 let substituted_expr =
	   application_subst r2 e_elt exp_elt in
	 let substituted_expr'=
	   application_subst substituted_expr e_suite exp_suite in
	 inter substituted_expr' evt
	   

    | PE_nil -> Val_lst( [] )

    | PE_cons(x, s) ->
       let interp_x, interp_s =
	 inter x evt, inter s evt in
       let val_lst_s =
	 (match interp_s with
	 | Val_lst (vallst) -> vallst
	 | _ -> failwith ""
	 ) in
       Val_lst(interp_x :: val_lst_s)
	 
    | _ -> failwith "no implrementera"    
  in inter expression environnement
;;

let rec association_pattern_value pattern valeur acc =
  match pattern.ppatt_desc with
    | PP_any -> acc
    | PP_ident (id) -> (id, valeur) :: acc
    | PP_tuple(pattern_list)
      -> List.fold_left2
       (fun acc elt1 elt2 -> 
	 association_pattern_value elt1 elt2 acc
       )
       acc
       pattern_list
       (match valeur with
       | Val_tuple(lst) -> lst
       | _ -> failwith "" )
;;

(* renvoie un nouvel environnement de str.map *)
let pdef_interp pdef environnement =
  let (isrec, pattern, expression) = pdef.pdef_desc in
  let valeur = interpretation expression environnement in
  let lst_subst = association_pattern_value pattern valeur [] in
  (List.fold_left
     (fun (acc) (k, value) -> (Str_map.add k value environnement))
     environnement
     lst_subst
  ),
  lst_subst
;;


(* renvoie une liste de valeurs *)
let plets_interp lets environnement = 
  List.fold_left
    (fun (acc_evt, acc_lst) decl ->
      let (evt', lst_substs) = pdef_interp decl acc_evt in
      (evt' , lst_substs @ acc_lst))
    (environnement, [])
    lets
;;



