
(** 1 : Calcul du type des feuilles
    2 : Remonter **)


(** Première version (plus naive que 
   @param une expression à typer
   *)
let rec typage_monomorphe expr =
  match expr with

  (* Si l'expression est une constante, trivial *)
  | PE_cte( const ) -> (
    match const with
    | Cunit -> Unit
    | Cbool(b) -> Bool
    | Cint(i) -> Integer
    | Cstring(str) -> String
  )

  (* TODO *)
  | PE_ident(idt) -> Unit

  (* Opérateur unaire: On vérifie que l'expression est du bon type *)
  | PE_unop (op, expr) -> (
    match op with 
    | Unot ->
       let type_expr = typage_monomorphe expr in
       if  type_expr = Bool
       then Bool else raise Bad_type
    | Uminus ->
       let type_expr = typage_monomorphe expr in
       if type_expr = Integer
       then Integer else raise Bad_type
    | Uminus_f->
       let type_expr = typage_monomorphe expr in
       if type_expr = Float
       then Float else raise Bad_type
  )

  (* Opérateur binaire : Même chose que les opérateurs unaires, mais avec deux expressions *)
  | PE_binop(op, exp1, exp2) -> (
    let t1, t2 = ((typage_monomorphe exp1), (typage_monomorphe exp2)) in
    match op with
    | Beq 
    | Bneq ->
       if t1 = Bool && t2 = Bool
       then Bool
       else raise Bad_type
	 
    (* resp. <, <=, >=, > *)
    (* Pas de comparaison Float>Integer pour l'instant *)
    | Blt
    | Ble
    | Bgt 
    | Bge ->
       if t1 = t2 && t2 = Float then Float
       else if t1 = t2 && t2 = Integer
       then Bool
       else raise Bad_type
	 
    | Badd
    | Bsub
    | Bmul
    | Bdiv
      -> if  (t1 = Integer && t2 = Integer)
	then Integer
	else raise Bad_type
	  
    | Badd_f
    | Bsub_f
    | Bmul_f
    | Bdiv_f 
      -> if (t1 = Float && t2 = Float)
	then Float
	else raise Bad_type
	  
    | Band
    | Bor
      -> if t1 = Bool && t2 = Bool
	then Bool
	else raise Bad_type
  )
     
  | PE_if(cond, exp1, exp2) ->
     let t1 = typage_monomorphe exp1 in
     if (typage_monomorphe cond) = Bool && t1 = (typage_monomorphe exp2)
     then t1
     else raise Bad_type
       
  | PE_app(f, x)
  | PE_fun(patt, expr) 
  | PE_tuple(exp_lst) -> raise Not_implemented
  | PE_let (isrec, patt, e1, e2) -> (* TODO **) 
  | PE_match( e1, e2, (patt1, patt2, e3)) 
  | PE_nil 
  | PE_cons(exp1, exp2) -> raise Not_implemented
;;

