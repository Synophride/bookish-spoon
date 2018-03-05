(* ***********
 * TYPAGE.ML
 * V 0.?
 * 28/02
 * **********)

open Ast;;
open Types;;
open Environnement;;

exception Bad_type of t * t * location;;

(* Type de type t 
   Renvoie une valeur de type scm

let typage expr evt =
  let finder x evt_lst =
    List.exists ((=) x) evt_lst
  in
  let rec w_pexpr expr evt =
    let (p_desc, p_loc) = (expr.pexpr_desc, expr.pexpr_loc) in
    try
      w_pexpr_desc p_desc evt
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
*)
