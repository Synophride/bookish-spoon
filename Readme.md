
# Projet Mini-ML

##But : Construire un interpréteur Mini ml.
Id est faire la conversion String parsée -> Ocaml

-> N'accepte que les constructions let _ = ...;;

Donnés :

* Grammaire
* Arbre de syntaxe

A faire : 

* Typage/interp.
* Système de localisation des erreurs/gestion des erreurs


Fichiers demandés:
*donnés :
	* lexer.mll : le lexeur ocamllex (Donné)
	* ast.mli : le(s) type(s) des arbres de syntaxe abstraite (Donné)
	* parser.mly : le parseur ocamlyacc (Donné)

* à faire
	* typing.ml : l’algorithme de typage -> typage.ml
      	* interp.ml : l’interpréteur
  	* top.ml : le fichier principal

____________________

##Fichiers : 
###__typage.ml__:
A pour but de typer une expression expr.

TODO :
* __Définition claire des types__
* Tests unification
* Paragraphe explicatif
* Typage ( W )


### __interp.ml__ :
Evalue une expression expr déjà typée. (? dans quel sens les déclarations sont-elles données ?)
