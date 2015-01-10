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
%token TRUE FALSE

/* Identifiers */
%token <string> VAR

		     
/* Declaration of variables */
%token <string> TYPE

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
%start expressions
%type < Expression.expression list > expressions


%%
/*************/
/* The rules */
/*************/

expressions:
 | e=expr EOL* EOF               { [e] }
 | e=expr EOL+ rest=expressions   { e::rest }

expr:
  | LPAREN e=expr RPAREN
      { e }
  | NOT e=expr
      { Unop(Unot,e) }
  | MINUS e=expr %prec UMINUS
      { Unop(Uminus,e)}
  | e1=expr o=bop e2=expr
      { Binop(o,e1,e2)}
  | id=VAR
      { Var id }
  | i=INT
      { Int i }
  | TRUE
      { Bool true }
  | FALSE
      { Bool false }

%inline bop:
  | MINUS     { Bsub }
  | PLUS      { Badd }
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
  | NE       { Bneq }
  
%%
