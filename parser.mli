type token =
  | AND
  | BAR
  | COLONCOLON
  | COMMA
  | COMP of (Ast.binop)
  | CONST_BOOL of (bool)
  | CONST_INT of (int)
  | CONST_FLOAT of (float)
  | CONST_STRING of (string)
  | ELSE
  | EOF
  | EQUAL
  | FUNCTION
  | IDENT of (string)
  | IF
  | IN
  | LBRACKET
  | LET
  | LPAREN
  | MATCH
  | MINUS
  | MINUS_DOT
  | MINUS_GT
  | NEQ
  | NOT
  | OR
  | PLUS
  | PLUS_DOT
  | RBRACKET
  | REC
  | RPAREN
  | SEMI
  | SLASH
  | SLASH_DOT
  | STAR
  | STAR_DOT
  | THEN
  | UNDERSCORE
  | WITH

val lets :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.plets
