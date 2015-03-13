exception Error

type token = 
  | VAR of (string)
  | TYPE of (string)
  | THIS
  | STRING of (string)
  | STATIC
  | SEMICOLON
  | RPAREN
  | RBRACE
  | PLUS
  | OR
  | NULL
  | NOT
  | NEW
  | NE
  | MULTI
  | MULLINECOMMENT of (string)
  | MOD
  | MINUS
  | LT
  | LPAREN
  | LE
  | LBRACE
  | INT of (int)
  | INSTANCEOF
  | INLINECOMMENT of (string)
  | IN
  | IF
  | GT
  | GE
  | EXTENDS
  | EQ
  | EOL
  | EOF
  | ELSE
  | DOT
  | DIV
  | COMMA
  | CLASS
  | BOOL of (bool)
  | ASSIGN
  | AND


val file_content: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ( Expression.class_or_expr list )