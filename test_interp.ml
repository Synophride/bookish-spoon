open Ast;;
open Interpretation;;
open Lexing;;

let lexing_pos1 =
  {
    pos_fname = "a";
    pos_lnum = 5;
    pos_bol = 4;
    pos_cnum = 7
  }
   
let loc1 = lexing_pos1, lexing_pos1;;

let p1 =
  {ppatt_loc = loc1;
   ppatt_desc= PP_ident("x")
  }
;;

let p2 =
  {ppatt_loc = loc1;
   ppatt_desc= PP_ident("y")
  }

;;
let p3 =
  {ppatt_loc = loc1;
   ppatt_desc= PP_ident("y")
  }
 ;;
  
let p5 = {ppatt_loc = loc1;
	  ppatt_desc= PP_tuple(
	  [ {ppatt_loc=loc1;
	     ppatt_desc=PP_any};
	    {ppatt_loc=loc1;
	     ppatt_desc=PP_ident("x")}
	  ])}
;;

let p6 =
  {ppatt_loc = loc1;
   ppatt_desc= PP_tuple(
     [ {ppatt_loc=loc1;
	ppatt_desc=PP_any};
       {ppatt_loc=loc1;
	ppatt_desc=PP_ident("x")};
       {ppatt_loc=loc1;
	ppatt_desc=PP_ident("x")};
       {ppatt_loc=loc1;
	ppatt_desc=PP_ident("x")}
     ])}
;;


let test_contains () =
  (assert (contains p2 p1));
(* ne fonctionne pas dans les deux sens *)
;;

let test_subs_pexprdesc
