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
%start file_content
%type < Expression.class_or_expr list > file_content


%%
/*************/
/* The rules */
/*************/

file_content:
  | EOF {print_endline("hahaha");[]}
  | e=class_or_expression rest=file_content EOF  { print_endline("olala");e::rest }

class_or_expression:
  | c=class_body { Class(c) }
  | e=expr { Expr(e) }

class_body:
  | CLASS t=TYPE LBRACE a=attributes_or_methods RBRACE 
      { Class_(t, a) }
  | CLASS t=TYPE EXTENDS st=TYPE LBRACE a=attributes_or_methods RBRACE 
      { ClassWithExtends(t, st, a) }

attributes_or_methods:
  | {[]}
  | a=attribute_or_method rest=attribute_or_method*
      { a :: rest }

attribute_or_method:    
  | a=attribute
      { Attr(a) }
  | m=method_
      { Meth(m) }

attribute:
  | static=STATIC? t=TYPE id=VAR SEMICOLON 
      {Attribute(static == None, t, id)}
  | static=STATIC? t=TYPE id=VAR ASSIGN e=expr SEMICOLON 
      {AttributeWithAssign(static == None, t, id, e)}

method_:
  | t=TYPE id=VAR LPAREN p=params RPAREN LBRACE e=expr RBRACE
      {Method(t, id, p, e)}
  | static=STATIC t=TYPE id=VAR LPAREN p=params RPAREN LBRACE e=expr RBRACE
      {MethodStatic(t, id, p, e)}

param:
  | t=TYPE id=VAR
      { Param(t,id) }
      
params:
  | {[]}	
  | t=TYPE id=VAR COMMA rest=param+
      { Param(t,id) :: rest }

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
  | e=expr DOT mthd=VAR LPAREN args_=args RPAREN
      { Invoke(e, mthd, args_) }
  | NEW t=TYPE
      { New(t) }
  | LPAREN t=TYPE RPAREN e=expr
      { Cast(t, e) }
  | e=expr INSTANCEOF t=TYPE
      { Instanceof(e, t) }

args:
  | e=expr { e }
  | e=expr COMMA rest=args { ExpList(e, rest) }


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
