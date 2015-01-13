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

%left SEMICOLON
%nonassoc IN
%right ASSIGN
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
  | comment* EOF {[]}
  | e=class_or_expression rest=file_content EOF { e::rest }
  
comment:
  | INLINECOMMENT {}
  | MULLINECOMMENT {}

class_or_expression:
  | comment* c=class_body { Class(c) }
  | comment* e=expr { Expr(e) }

class_body:
  // Class without extends
  | CLASS t=TYPE LBRACE a=attributes_or_methods RBRACE 
      { Class_(t, a) }
  // Class with extends
  | CLASS t=TYPE EXTENDS st=TYPE LBRACE a=attributes_or_methods RBRACE 
      { ClassWithExtends(t, st, a) }

attributes_or_methods:
  | comment*  { [] }
  | a=attribute_or_method  rest=attribute_or_method*
      { a :: rest }

attribute_or_method:    
  | comment* a=attribute SEMICOLON
      { Attr(a) }
  | comment* m=method_
      { Meth(m) }

attribute:
  | STATIC t=TYPE id=VAR
      { Attribute(true, t, id) }
  | t=TYPE id=VAR
      { Attribute(false, t, id) }
  | STATIC t=TYPE id=VAR ASSIGN e=expr
      { AttributeWithAssign(true, t, id, e) }
  | t=TYPE id=VAR ASSIGN e=expr
      { AttributeWithAssign(false, t, id, e) }

method_:
  // Four cases: static or not and with params or not
  | STATIC t=TYPE id=VAR LPAREN p=params RPAREN LBRACE e=expr RBRACE
      { Method(true, t, id, p, e) }
  | STATIC t=TYPE id=VAR LPAREN RPAREN LBRACE e=expr RBRACE
      { Method(true, t, id, [], e) }
  | t=TYPE id=VAR LPAREN p=params RPAREN LBRACE e=expr RBRACE
      { Method(false, t, id, p, e) }
  | t=TYPE id=VAR LPAREN RPAREN LBRACE e=expr RBRACE
      { Method(false, t, id, [], e) }

param:
  | t=TYPE id=VAR
      { Param(t,id) }
      
params:
  | { [] }
  | t=TYPE id=VAR
      { [Param(t,id)] }	
  | t=TYPE id=VAR COMMA rest=param+
      { Param(t,id) :: rest }
  
expr:
  | comment+ e=expr
      { e }
  | e=expr comment+
      { e }
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
  | THIS
      { This }
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
  | { [] }
  | e=expr { [e] }
  | e=expr COMMA rest=args { e :: rest }


%inline bop:
  | SEMICOLON { Bsemicolon }
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
  | NE        { Bne }
  
%%
