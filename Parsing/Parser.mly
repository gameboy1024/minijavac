%{
  open AST
  open Location

  type attOrMethod =
    | Att of astattribute
    | Meth of astmethod

  let mkexp d s e = 
    { 
      edesc = d;
      eloc  = symbol_loc s e;
      etype = None;
    }

  let mkval v s e = 
    { 
      edesc = Val v;
      eloc  = symbol_loc s e;
      etype = None;
    }

  let mkexploc d loc = 
    { 
      edesc = d;
      eloc  = loc;
      etype = None;
    }

  let mkclass n p a m s e = 
    { 
      cname = n;
      cparent = p;
      cattributes = a;
      cmethods = m;
      cloc  = symbol_loc s e;
    }

  let mkatt c n t d s e = 
    {
      astatic = c;
      aname = n;
      atype = t;
      adefault = d;
      aloc  = symbol_loc s e;
    }

  let mkmeth c n r a b s e =
    {
      mstatic = c;
      mname = n;
      mreturntype = r;
      margstype = a;
      mbody = b;
      mloc  = symbol_loc s e;
    }

  let mk_type ty s e =
    Located.mk_elem (Type.fromString ty) (symbol_loc s e)

%}

/**************/
/* The tokens */
/**************/
%token EOF

/* Literal values */
%token <int> INT
%token <string> STRING
%token TRUE FALSE NULL

/* Identifiers */
%token <string> LIDENT UIDENT

/* Separators */
%token LPAREN RPAREN LBRACE RBRACE

/* Keywords */		
%token CLASS ELSE EXTENDS IF IN INSTANCEOF NEW THIS STATIC

/* Operators */
%token SEMI DOT COMMA EQUAL
%token AND OR NOT 
%token GT GE LT LE EQ NEQ
%token PLUS MINUS TIMES DIV MOD

/********************************/
/* Priorities and associativity */
/********************************/
%right SEMI
%nonassoc IFP
%right EQUAL 
%left OR
%left AND
%left EQ NEQ
%left GT GE LT LE INSTANCEOF
%left PLUS MINUS
%left TIMES DIV MOD
%right UMINUS NOT CAST
%left DOT

/******************************/
/* Entry points of the parser */
/******************************/
%start start
%type <AST.t> start

%%
/*************/
/* The rules */
/*************/

start: cl=aclass* e=expression? EOF  { cl, e }

aclass:
  | CLASS name=UIDENT p=parent? LBRACE cb=classBody RBRACE
      { 
	let al,ml = cb in
	let ext = match p with
	  | Some t -> t 
	  | None -> Located.mk_elem (Type.fromString "Object") Location.none in
	mkclass name ext al ml $startpos $endpos
      }

parent:
  | EXTENDS UIDENT { mk_type $2 ($startpos($2)) ($endpos($2)) }

classBody: 
  | aOml=attributeOrMethod*
    {
      List.fold_left 
      (fun (al,ml) aOm -> match aOm with
    	  | Att a -> a::al,ml
    	  | Meth m -> al,m::ml) 
      ([],[]) 
      (List.rev aOml)
    }

attributeOrMethod:
  | static=static_opt typ=UIDENT name=LIDENT LPAREN args=separated_list(COMMA,argument) RPAREN LBRACE e=expression RBRACE
      {
	let typ = Located.mk_elem (Type.fromString typ) (symbol_loc ($startpos(typ)) ($endpos(typ))) in
	Meth(mkmeth static name typ args e $startpos $endpos )
      }
  | static=static_opt typ=UIDENT name=LIDENT initialization=preceded(EQUAL,expression)? SEMI
      {
	let typ = Located.mk_elem (Type.fromString typ) (symbol_loc ($startpos(typ)) ($endpos(typ))) in
	Att(mkatt static name typ initialization $startpos(typ) $endpos(name))
      }

static_opt:
    | STATIC
      { true }
  | /* nothing */
      { false }

argument: 
  | typ=UIDENT name=LIDENT 
    { 
      let typ = Located.mk_elem (Type.fromString typ) (symbol_loc ($startpos(typ)) ($endpos(typ))) in
      name,typ
    }

expression:
  | NOT expression
      { mkexp (Call($2,"not",[])) $startpos $endpos }
  | e1=expression op=binop e2=expression
      { mkexp (Call(e1,op,[e2])) $startpos $endpos }
  | expression SEMI expression
      { mkexp (Seq($1,$3)) $startpos $endpos }
  | MINUS expression %prec UMINUS
      { mkexp (Call($2,"neg",[])) $startpos $endpos }
  /* In version 2, the else can be omitted. */
  | IF LPAREN expression RPAREN LBRACE expression RBRACE else_cond %prec IFP
      { match $8 with
        | Some e -> mkexp (If($3, $6, Some e)) $startpos $endpos 
        | None -> mkexp (If($3, $6, None)) $startpos $endpos 
      }
  | LIDENT EQUAL expression
      { mkexp (Assign($1,$3)) $startpos $endpos }
  | UIDENT LIDENT EQUAL e1=expression IN LBRACE e2=expression RBRACE
      { 
	let typ = Located.mk_elem (Type.fromString $1) (symbol_loc ($startpos($1)) ($endpos($1))) in
	mkexp (Define($2,typ,e1,e2)) $startpos $endpos
      }
  | NEW UIDENT 
      { 
	let typ = Located.mk_elem (Type.fromString $2) (symbol_loc ($startpos($2)) ($endpos($2))) in
	mkexp (New typ) $startpos $endpos 
      }
  | o=expression DOT m=LIDENT LPAREN params=separated_list(COMMA,expression) RPAREN        
      { mkexp (Call(o,m,params)) $startpos $endpos }
  | LPAREN UIDENT RPAREN expression %prec CAST
      { 
	let typ = Located.mk_elem (Type.fromString $2) (symbol_loc ($startpos($2)) ($endpos($2))) in
	mkexp (Cast(typ,$4)) $startpos $endpos
      }
  | expression INSTANCEOF UIDENT
      { 
	let typ = Located.mk_elem (Type.fromString $3) (symbol_loc ($startpos($3)) ($endpos($3))) in
	mkexp (Instanceof($1,typ)) $startpos $endpos 
      }
  | LIDENT
      { mkexp (Var $1) $startpos $endpos }
  | THIS
      { mkexp (Var "this") $startpos $endpos}
  | value
      { mkval $1 $startpos $endpos }
  | LPAREN expression RPAREN
      { $2 }

else_cond:
  | ELSE LBRACE expression RBRACE
      { Some $3 }
  | /* nothing */
      { None }

%inline binop:
  | MINUS      { "sub" }
  | PLUS       { "add" }
  | TIMES      { "mul" }
  | DIV        { "div" }
  | MOD        { "mod" }
  | AND        { "and" }
  | OR         { "or" }
  | GT         { "gt" }
  | GE         { "ge" }
  | LT         { "lt" }
  | LE         { "le" }
  | EQ         { "eq" }
  | NEQ        { "neq" }

value:
  | INT    { Int $1 }
  | STRING { String $1 }
  | NULL   { Null }
  | TRUE   { Boolean true }
  | FALSE  { Boolean false }

