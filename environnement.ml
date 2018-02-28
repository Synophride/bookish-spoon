(* ************
 * ENVIRONNEMENT.ML
 * Version 0.0
 * 28/02
 **************)

type vartype = {
  ident : ident;
  mutable typ of t option; (* None = n'importe quel type *)
}
type scm =
    T of t
  | Forall of (ident * t)
;;
;;
(* Q: Comment définir l'environnement ? *)

(* *** " Cahier de charges :
 * On doit pouvoir manipuler :
 *    - schémas
 *    - types
 *    - Variables de type
 *
 * Fonctions publiques
 *  - Unification
 *  - Ajout/Remplacement d'un type + ident
 * Fonctions cachées :
 *  - Instanciation / Généralisation
 * 
 * 
 * ***)

exception Non_unifiable of t * t;;
exception Non_unifiable_lst;;

type types =
    Typ of t
  | S of scm (* note : utilisé seulement dans les let *)
;;


(* *****************
 * FONCTIONS "PRIVEES"
 * ****************)

(** 
    Rend le type associé à l'identificateur 
    @param ident l'indentificateur à associer 
**)
let find ident =
  ()
;;

let add_or_replace ident typ =
  ()
;;

let unification t1 t2 =
  ()
;;
