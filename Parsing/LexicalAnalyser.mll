{
open SyntaxAnalyser

let print_element = function
    | EOF       -> print_string "EOF"
    | CLASS     -> print_string "CLASS"
    | LPAREN    -> print_string "LPAREN"
    | RPAREN    -> print_string "RPAREN"
    | LBRACE    -> print_string "LBRACE"
    | RBRACE    -> print_string "RBRACE"
    | SEMICOLON -> print_string "SEMICOLON"
    | ASSIGN    -> print_string "ASSIGN"
    | COMMA     -> print_string "COMMA"
    | DOT       -> print_string "DOT"
    | NOT   	  -> print_string "NOT"
    | MINUS     -> print_string "MINUS"
    | LT        -> print_string "LT"
    | GT        -> print_string "GT"
    | LE        -> print_string "LE"
    | GE        -> print_string "GE"
    | NE        -> print_string "NE"
    | EQ        -> print_string "EQ"
    | PLUS      -> print_string "PLUS"
    | MULTI     -> print_string "MULTI"
    | DIV       -> print_string "DIV"
    | MOD       -> print_string "MOD"
    | AND       -> print_string "AND"
    | OR        -> print_string "OR"
    
    | TYPE s    -> print_string "TYPE("; print_string s; print_string ")"
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
    
    | BOOL b    -> print_string "BOOL("; print_string (string_of_bool b); print_string ")"
    | INT i     -> print_string "INT("; print_int i; print_string ")"
    | STRING s  -> print_string "STRING("; print_string s; print_string ")"
    | VAR s     -> print_string "VAR("; print_string s; print_string ")"
    
    | INLINECOMMENT  ic -> print_string "INLINECOMMENT("; print_string ic; print_string ")"
    | MULLINECOMMENT mc -> print_string "MULLINECOMMENT("; print_string mc; print_string ")"

open Lexing
exception Eof

type error =
  | Illegal_character of char
  | Illegal_int of string
exception Error of error * position * position

let raise_error err lexbuf =
    raise (Error(err, lexeme_start_p lexbuf, lexeme_end_p lexbuf))
  (* Les erreurs. *)
  let report_error = function
    | Illegal_character c ->
      print_string "Illegal character '";
      print_char c;
      print_string "' "
    | Illegal_int nb ->
      print_string "The int ";
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

let l_letter = ['a'-'z']
let b_letter = ['A'-'Z']
let letter = l_letter | b_letter
let digit = ['0'-'9']
let integer = digit+
let boolean = "true" | "false"
let str = '"' ([^'"'] | '\"' )* '"'
let type_name = b_letter (letter)*
let var_name = l_letter (letter | digit | '_')*
let newline = ('\010' | '\013' | "\013\010")
let blank = [' ' '\009']

let inline_comment = "//" [^'\n']*
let mulline_comment = "/*" _* "*/"

rule nexttoken = parse
  | newline               { incr_line lexbuf; nexttoken lexbuf }
  | blank+                { nexttoken lexbuf }
  | eof                   { EOF }
  | inline_comment as c   { INLINECOMMENT c}
  | mulline_comment as c  { MULLINECOMMENT c}
  | str as s              { STRING s }
  | "class"               { CLASS }
  | "else"                { ELSE }
  | "extends"             { EXTENDS }
  | boolean as bl         { BOOL (bool_of_string bl)  }
  | "if"                  { IF }
  | "in"                  { IN }
  | "instanceof"          { INSTANCEOF }
  | "new"                 { NEW }
  | "null"                { NULL }
  | "static"              { STATIC }
  | "this"                { THIS }
  | "("                   { LPAREN }
  | ")"                   { RPAREN }
  | "{"                   { LBRACE }
  | "}"                   { RBRACE }
  | ";"                   { SEMICOLON }
  | "="                   { ASSIGN }
  | ","                   { COMMA }
  | "."                   { DOT }
  | "!"                   { NOT }
  | "<"                   { LT }
  | ">"                   { GT }
  | "<="                  { LE }
  | ">="                  { GE }
  | "!="                  { NE }
  | "=="                  { EQ }
  | "+"                   { PLUS }
  | "-"                   { MINUS }
  | "*"                   { MULTI }
  | "/"                   { DIV }
  | "%"                   { MOD }
  | "&&"                  { AND }
  | "||"                  { OR }
  | integer as nb         { try INT (int_of_string nb) with Failure "int_of_string" -> raise_error (Illegal_int(nb)) lexbuf }
  | type_name as str      { TYPE str }
  | var_name as str       { VAR str }
  | _ as c                { raise_error (Illegal_character(c)) lexbuf }

{
  let rec examine_all lexbuf =
    let res = nexttoken lexbuf in
      print_element res;
      print_string " ";
      match res with
        | EOF -> ()
        | _   -> examine_all lexbuf
}
