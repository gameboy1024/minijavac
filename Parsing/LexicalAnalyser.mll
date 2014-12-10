{
  type keyword =
    | CLASS
    | ELSE
    | EXTENDS
    | FALSE
    | IF
    | IN
    | INSTANCEOF
    | NEW
    | NULL
    | STATIC
    | THIS
    | TRUE

  type SYMBOLE =
    | EOF
    | LPAREN
    | RPAREN
    | LBRACE
    | RBRACE
    | SEMICOLON
    | ASSIGN
    | COMMA
    | PERIOD
    | INTERRO
    | MINUS
    | LT
    | GT
    | LE
    | GE
    | NE
    | EQ
    | PLUS
    | MULTI
    | DIV
    | MOD
    | AND
    | OR
    
  type DATA = 
    | BOOL of bool
    | INT of int
    | IDENT of string
    
  type COMMENT = 
    | INLINECOMMENT of string 
    | MULLINECOMMENT of string

  let print_element = function
    | EOF       -> print_string "EOF"
    | LPAREN    -> print_string "LPAREN"
    | RPAREN    -> print_string "RPAREN"
    | LBRACE    -> print_string "LBRACE"
    | RBRACE    -> print_string "RBRACE"
    | SEMICOLON -> print_string "SEMICOLON"
    | ASSIGN    -> print_string "ASSIGN"
    | COMMA     -> print_string "COMMA"
    | PERIOD    -> print_string "PERIOD"
    | INTERRO   -> print_string "INTERRO"
    | MINUS     -> print_string "MINUS"
    | LT        -> print_string "LT"
    | GT        -> print_string "GT"
    | LE        -> print_string "LE"
    | GE        -> print_string "TRUE"
    | NE        -> print_string "GE"
    | EQ        -> print_string "EQ"
    | PLUS      -> print_string "PLUS"
    | MULTI     -> print_string "MULTI"
    | DIV       -> print_string "DIV"
    | MOD       -> print_string "MOD"
    | AND       -> print_string "AND"
    | OR        -> print_string "OR"

    | FLOAT f   -> print_string "FLOAT("; print_float f; print_string ")"
    | IDENT s   -> print_string "IDENT("; print_string s; print_string ")"
    | CLASS     -> print_string "CLASS"
    | ELSE      -> print_string "ELSE"
    | EXTENDS   -> print_string "EXTENDS"
    | FALSE     -> print_string "FALSE"
    | IF        -> print_string "IF"
    | IN        -> print_string "IN"
    | INSTANCEOF-> print_string "INSTANCEOF"
    | NEW       -> print_string "NEW"
    | NULL      -> print_string "NULL"
    | STATIC    -> print_string "STATIC"
    | THIS      -> print_string "THIS"
    | TRUE      -> print_string "TRUE"
    
    | BOOL b    -> print_string "BOOL("; print_string string_of_bool b; print_string ")"
    | INT i     -> print_string "INT("; print_int i; print_string ")"
    | IDENT s   -> print_string "IDENT("; print_int s; print_string ")"
    
    | INLINECOMMENT ic -> print_string "INLINECOMMENT("; print_int ic; print_string ")"
    | MULLINECOMMENT mc-> print_string "MULLINECOMMENT("; print_int mc; print_string ")"

  open Lexing
  exception Eof

  type error =
    | Illegal_character of char
    | Illegal_float of string
  exception Error of error * position * position

  let raise_error err lexbuf =
    raise (Error(err, lexeme_start_p lexbuf, lexeme_end_p lexbuf))
    
  (* Les erreurs. *)
  let report_error = function
    | Illegal_character c ->
	print_string "Illegal character '";
	print_char c;
	print_string "' "
    | Illegal_float nb ->
	print_string "The float ";
	print_string nb;
	print_string " is illegal "

  let print_position debut fin =
    if (debut.pos_lnum = fin.pos_lnum) then
      begin
	print_string "line ";
	print_int debut.pos_lnum;
	print_string " characters ";
	print_int (debut.pos_cnum - debut.pos_bol);
	print_string "-";
	print_int (fin.pos_cnum - fin.pos_bol)
      end
    else
      begin
	print_string "from line ";
	print_int debut.pos_lnum;
	print_string " character ";
	print_int (debut.pos_cnum - debut.pos_bol);
	print_string " to line ";
	print_int fin.pos_lnum;
	print_string " character ";
	print_int (fin.pos_cnum - fin.pos_bol)
      end

  let incr_line lexbuf =
    let pos = lexbuf.lex_curr_p in
      lexbuf.lex_curr_p <- 
	{ 
	  pos with 
	    pos_lnum = pos.pos_lnum + 1; 
	    pos_bol = pos.pos_cnum;
	}

}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let real = digit* ('.' digit*)?
let ident = letter (letter | digit | '_')*
let newline = ('\010' | '\013' | "\013\010")
let blank = [' ' '\009']

rule nexttoken = parse
  | newline       { incr_line lexbuf; nexttoken lexbuf }
  | blank+        { nexttoken lexbuf }
  | eof           { EOF }
  | "+"           { PLUS } 
  | "-"           { MINUS } 
  | "/"           { DIV } 
  | "*"           { TIMES } 
  | real as nb    { try FLOAT (float_of_string nb) with Failure "float_of_string" -> raise_error (Illegal_float(nb)) lexbuf }
  | ident as str  { IDENT str }
  | _ as c        { raise_error (Illegal_character(c)) lexbuf }


{
  let rec examine_all lexbuf =
    let res = nexttoken lexbuf in
    print_lexeme res;
    print_string " ";
    match res with
    | EOF -> ()
    | _   -> examine_all lexbuf
  print_endline "end"
}
