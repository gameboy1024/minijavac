{
open Parser
open Lexing
open Error

let keyword_table = Hashtbl.create 17
let _ =
  List.iter (fun (k,d) -> Hashtbl.add keyword_table k d) 
   [
     "class"	    , CLASS;
     "else"         , ELSE;
     "extends"	    , EXTENDS;
     "false"        , FALSE;
     "if"           , IF;
     "in"           , IN;
     "instanceof"   , INSTANCEOF;
     "new"	    , NEW;
     "null"	    , NULL;
     "this"         , THIS;
     "true"         , TRUE;
   ]

let ident_or_keyword id =
  try Hashtbl.find keyword_table id
  with Not_found -> LIDENT id

let buff = Buffer.create 256
let buffer_string str =	Buffer.add_string buff str
let buffer_char ch    =	Buffer.add_char buff ch

}

let not_newline_char = [^ '\n' '\r']
let newline = (['\n' '\r'] | "\r\n")
let blank = [' ' '\t']
let digit = ['0'-'9']+
let lower_ch = ['a'-'z']
let upper_ch = ['A'-'Z']
let id_ch = (lower_ch | upper_ch | digit | '_')
let esc_char = ['n' '\\' '"']

rule token = parse
  | newline                 { Location.incr_line lexbuf; token lexbuf }
  | blank +                 { token lexbuf }
  | "/*"                    { comment (Location.curr lexbuf) lexbuf; token lexbuf }
  | "//" not_newline_char*  { token lexbuf }
  | lower_ch id_ch * as id  { ident_or_keyword id }
  | upper_ch id_ch * as id  { UIDENT id }
  | digit+ as nb            { INT (int_of_string nb) }
  | "("                     { LPAREN }
  | ")"                     { RPAREN }
  | "{"                     { LBRACE }
  | "}"                     { RBRACE }
  | ";"                     { SEMI }
  | "."                     { DOT }
  | ","                     { COMMA }
  | "="                     { EQUAL }
  | '+'                     { PLUS }
  | '-'                     { MINUS }
  | '*'                     { TIMES }
  | '/'                     { DIV }
  | '%'                     { MOD }
  | "&&"                    { AND }
  | "||"                    { OR }
  | "!"                     { NOT }
  | ">"                     { GT }
  | ">="                    { GE }
  | "<"                     { LT }
  | "<="                    { LE }
  | "=="                    { EQ }
  | "!="                    { NEQ }
  | eof                     { EOF }
  | "\""
      { 
	Buffer.reset buff;
        let string_start = lexbuf.lex_start_p in
          string (Location.curr lexbuf) lexbuf;
          lexbuf.lex_start_p <- string_start;
          STRING (Buffer.contents buff) 
      }
  | _ as ch  { illegal_char ch (Location.curr lexbuf) }

and string start_loc = parse
  | '"'                  { () }
  | '\\' esc_char as ch  { buffer_string ch; string start_loc lexbuf }
  | '\\'                 { illegal_escape_char (Location.curr lexbuf) }
  | newline | eof        { unterminated_string start_loc }
  | _ as ch              { buffer_char ch; string start_loc lexbuf }

and comment start_loc = parse
  | "*/"    { () }
  | newline { Location.incr_line lexbuf; comment start_loc lexbuf }
  | _       { comment start_loc lexbuf }
  | eof     { unterminated_comment start_loc }
