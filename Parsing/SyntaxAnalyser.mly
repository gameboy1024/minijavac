%{
  (* Accessing the AST type *)
  open Expression
%}

/**************/
/* The tokens */
/**************/

/* Key words */
%token CLASS ELSE EXTENDS IF IN INSTANCEOF NEW NULL STATIC THIS

/* Separators */
%token EOF EOL LPAREN RPAREN LBRACE RBRACE SEMICOLON COMMA

/* Operators */
%token ASSIGN DOT 
%token PLUS MINUS MULTI DIV MOD
%token GT GE LT LE EQ NE
%token AND OR NOT

/* Literal values */
%token <int> INT
%token <bool> BOOL
%token <string> STRING
%token TRUE FALSE

/* Identifiers */
%token <string> VAR

		     
/* Declaration of variables */
%token <Expression.type_> TYPE

/* Comments */
%token <string> INLINECOMMENT
%token <string> MULLINECOMMENT

/********************************/
/* Priorities and associativity */
/********************************/
%nonassoc IN
%left OR
%left AND
%left EQ NE
%left GT GE LT LE
%left PLUS MINUS
%left MULTI DIV MOD
%right UMINUS NOT

/******************************/
/* Entry points of the parser */
/******************************/
%start file_content
%type < Expression.expression list > file_content


%%
/*************/
/* The rules */
/*************/

file_content:
 | e=class_or_expression EOL* EOF               { [e] }
 | e=class_or_expression EOL+ rest=file_content   { e::rest }

class_or_expression:
  (*| class_body { $1 }*)
  | expr { $1 }
  (*
class_body:
  | CLASS TYPE LBRACE (attribute_or_method)* RBRACE { $1} *)
  (*
attribute_or_method:
  | attribute 
      { $1 }
  | method_ 
      { $1 }
  
attribute:
  | STATIC? t=TYPE id=VAR (ASSIGN e=expr)? SEMICOLON 
      {Attribute(t, id, e)}

method_:
  | STATIC? t=TYPE id=VAR LPAREN p?=params RPAREN LBRACE e=expr RBRACE
      {Method(t, id, p, e)}
  *)
params:
  | t=TYPE id=VAR { [t, id] }
  | t=TYPE id=VAR COMMA rest=args { [t, id]::rest }

expr:
  | LPAREN e=expr RPAREN
      { e }
  | id=VAR
      { Var id }
  | i=INT
      { Int i }
  | s=STRING
      { String s }
  | NULL
      { Null }
  | b=BOOL
      { Bool b }
  | NOT e=expr
      { Unop(Unot,e) }
  | MINUS e=expr %prec UMINUS
      { Unop(Uminus,e) }
  | e1=expr o=bop e2=expr
      { Binop(o,e1,e2) }
  | id=VAR ASSIGN e=expr
      { Assign(id, e) }
  | t=TYPE id=VAR ASSIGN e1=expr IN e2=expr
      { Def(t, id, e1, e2) }
  | IF LPAREN cond=expr RPAREN LBRACE eif=expr RBRACE ELSE LBRACE eelse=expr RBRACE
      { Ifelse(cond, eif, eelse) }
  | e=expr DOT mthd=VAR LPAREN args=args RPAREN
      { Invoke(e, mthd, args) }
  | NEW t=TYPE
      { New(t) }
  | LPAREN t=TYPE RPAREN e=expr
      { Cast(t, e) }
  | e=expr INSTANCEOF t=TYPE
      { Instanceof(e, t) }

args:
  | e=expr { [e] }
  | e=expr COMMA rest=args { e::rest }


%inline bop:
  | PLUS      { Badd }
  | MINUS     { Bsub }
  | MULTI     { Bmul }
  | DIV       { Bdiv }
  | MOD       { Bmod }
  | OR        { Bor }
  | AND       { Band }
  | GT        { Bgt }
  | GE        { Bge }
  | LT        { Blt }
  | LE        { Ble }
  | EQ        { Beq }
  | NE        { Bneq }
  
%%
