exception Error

type token = 
  | UIDENT of (string)
  | TRUE
  | TIMES
  | THIS
  | STRING of (string)
  | SEMI
  | RPAREN
  | RBRACE
  | PLUS
  | OR
  | NULL
  | NOT
  | NEW
  | NEQ
  | MOD
  | MINUS
  | LT
  | LPAREN
  | LIDENT of (string)
  | LE
  | LBRACE
  | INT of (int)
  | INSTANCEOF
  | IN
  | IF
  | GT
  | GE
  | FALSE
  | EXTENDS
  | EQUAL
  | EQ
  | EOF
  | ELSE
  | DOT
  | DIV
  | COMMA
  | CLASS
  | AND


val start: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (AST.t)