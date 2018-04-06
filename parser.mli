
(* The type of tokens. *)

type token = 
  | WITH
  | UNDERSCORE
  | THEN
  | STAR_DOT
  | STAR
  | SLASH_DOT
  | SLASH
  | SEMI
  | RPAREN
  | REC
  | RBRACKET
  | PLUS_DOT
  | PLUS
  | OR
  | NOT
  | NEQ
  | MINUS_GT
  | MINUS_DOT
  | MINUS
  | MATCH
  | LPAREN
  | LET
  | LBRACKET
  | IN
  | IF
  | IDENT of (string)
  | FUNCTION
  | EQUAL
  | EOF
  | ELSE
  | CONST_STRING of (string)
  | CONST_INT of (int)
  | CONST_FLOAT of (float)
  | CONST_BOOL of (bool)
  | COMP of (Ast.binop)
  | COMMA
  | COLONCOLON
  | BAR
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val lets: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.plets)
