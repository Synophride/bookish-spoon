open Ast
open Types
open Environnement
open Typing
;;

let rec scm_to_string =
  function
  | Forall(_,t) -> scm_to_string t
  | T(t) -> str_of_t (fun x -> "'" ^ string_of_int x) t
;;

let rec typ_to_string vart_lst =
  str_of_t
    (fun x ->
      let vartyp = List.find (fun y -> y.id = x) vart_lst in
      let ref_of_id = vartyp.r_typ in 
      match !ref_of_id with
      | Some(t) -> typ_to_string vart_lst t
      | None -> let get_first = List.find (fun y -> y.r_typ == ref_of_id) vart_lst in
		"'_" ^ string_of_int get_first.id
    )
;;


let typ_bool =
  [], Bool
;;

let typ_a =
  (
    [{id = 0; r_typ = ref None}],
    Id(0)
  )
;;

let typ_fun_a_b_a =
    [{id = 0; r_typ = ref None}; {id = 1; r_typ = ref None}],
  Fun([Id(0); Id(1); Id(0)])
;;

let typ_a_b =   
  (
   [{id = 0; r_typ = ref None}; {id = 1; r_typ = ref None}],  
   Tuple([Id(0); Id(1)])
  )
;;

let scm_a =
  Forall(1, T(Id(1)))
;;

let scm_bool =
  T(Bool)
;;

let scm_a_b =
  Forall(0,
	 Forall(1,
		T(
		  Tuple(
		    [Id(0);
		     Id(1)]
		  )
		)
	 )
  )
;;

