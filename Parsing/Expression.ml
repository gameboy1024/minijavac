type binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod
  | Band | Bor
  | Bgt | Bge | Blt | Ble | Beq | Bne
				    
type unop =
  | Unot | Uminus

type expression =
  | Int of int
  | Var of string
  | Null
  | Bool of bool
  | This
  | String of string
  | Def of string * string * expression * expression
  | Unop of unop * expression
  | Binop of binop * expression * expression  
  | Assign of string * expression
  | Ifelse of expression * expression * expression
  | New of string 
  | Instanceof of expression * string
  | Cast of string * expression
  | Invoke of expression * string * expression list

type param = 
  | Param of string * string
  
type attr =
  | Attribute of bool * string * string
  | AttributeWithAssign of bool * string * string * expression 

type mthd = 
  | Method of bool * string * string * (param list) * (expression list)
  
type attr_or_method = 
  | Attr of attr 
  | Meth of mthd

type class_ =
  | Class_ of string * attr_or_method list
  | ClassWithExtends of string * string * attr_or_method list
  
type class_or_expr = 
  | Class of class_ 
  | Expr of expression
  
type value =
  | Vbool of bool
  | Vint of int


exception Unbound_variable of string

let get_op_u op x = 
  match op, x with
  | Unot, Vbool b -> Vbool(not b)
  | Uminus, Vint x -> Vint (-x)
  | _ -> failwith "bug:type error not catched"

let get_op_b op x y =
  match op, x, y with
  | Badd, Vint x, Vint y -> Vint(x + y)
  | Bsub, Vint x, Vint y -> Vint(x - y)
  | Bmul, Vint x, Vint y -> Vint(x * y)
  | Bdiv, Vint x, Vint y -> Vint(x / y)
  | Bmod, Vint x, Vint y -> Vint(x mod y)
  | Bgt, Vint x, Vint y -> Vbool(x > y)
  | Bge, Vint x, Vint y -> Vbool(x >= y)
  | Blt, Vint x, Vint y -> Vbool(x < y)
  | Ble, Vint x, Vint y -> Vbool(x >= y)
  | Beq, Vint x, Vint y -> Vbool(x = y)
  | Bne, Vint x, Vint y -> Vbool(x != y)
  | Band, Vbool x, Vbool y -> Vbool(x && y)
  | Bor, Vbool x, Vbool y -> Vbool(x || y)
  | _ -> failwith "bug:type error not catched"

let string_type_of_value = function
  | Vbool _ -> "bool"
  | Vint _  -> "int"

let string_type_of_op_b = function
  | Badd | Bsub | Bmul | Bdiv | Bmod | Bgt
  | Bge | Blt | Ble | Beq | Bne -> "int"
  | Band | Bor  -> "bool"

let string_type_of_op_u = function
  | Uminus -> "int"
  | Unot   -> "bool"

let string_of_value = function
  | Vbool true -> "true"
  | Vbool false -> "false"
  | Vint i -> string_of_int i

let string_of_op_u = function
  | Unot -> "not "
  | Uminus -> "-"

let string_of_op_b = function
  | Badd -> " + "
  | Bsub -> " - "
  | Bmul -> " * "
  | Bdiv -> " / "
  | Bmod -> " % "
  | Band -> " && "
  | Bor  -> " || "
  | Bgt  -> " > "
  | Bge -> " >= "
  | Blt -> " < "
  | Ble -> " <= "
  | Beq -> " = "
  | Bne -> " != "

let rec string_of_expr exp = match exp with
  | Int i               ->  "Int("^string_of_int i^")"
  | Var v               ->  "Var("^v^")"
  | Null                ->  "Null()"
  | Bool true           ->  "Bool(true)"
  | Bool false          ->  "Bool(false)"
  | This                ->  "This(this)"
  | String s            ->  "String("^s^")"
  | Unop(op, e)         ->  "Unop("^(string_of_op_u op)^(string_of_expr e)^")"
  | Binop(op, e1, e2)   ->  "Binop("^(string_of_expr e1)^(string_of_op_b op)^(string_of_expr e2)^")"
  | Assign(s,e)         ->  "Assign("^s^"="^(string_of_expr e)^")"
  | Def(v1, v2, e1, e2) ->  "Def("^v1^v2^"="^(string_of_expr e1)^" in "^(string_of_expr e2)^")"
  | Ifelse(e1,e2,e3)    ->  "If("^(string_of_expr e1)^") { "^(string_of_expr e2 )^" } else { "^(string_of_expr e3)^" })"
  | Cast(t, v)          ->  "Cast(" ^ t ^ " " ^ string_of_expr v ^")"
  | Invoke(e, s, l)     ->  "Invoke("^(string_of_expr e)^"."^s^"("^(string_of_exprs l)^"))"
  | New s               ->  "New("^s^")" 
  | Instanceof(e, t)    ->  "Instanceof(" ^ string_of_expr e ^  " "^t^")"
      
and string_of_exprs = function
  | [] -> ""
  | e::l -> string_of_expr e ^ string_of_exprs l
    
let rec string_of_param p = match p with
	| Param(t,id) -> t^" "^id
  
let rec string_of_params params = match params with
  | [] -> ""
  | (p :: l) -> string_of_param(p) ^ ", " ^ string_of_params l
  
let string_of_static_bool = function
  | true -> "static"
  | false -> "non-static"
  
let string_of_attr = function
  | Attribute(static, t, id) ->"Attribute("^string_of_static_bool static ^ " " ^  t^" "^id^")"
  | AttributeWithAssign(static, t,id,e) -> "AttributeWithAssign("^string_of_static_bool static ^ " " ^t^" "^id^" = "^(string_of_expr e)^")"
  
let string_of_mthd = function
  | Method(static,s1,s2,p,e) -> "Method("^string_of_static_bool static^ " "  ^s1^" "^s2^"(Params("^(string_of_params p)^")) {"^(string_of_exprs e)^"})"

let string_of_attr_or_method = function
  | Attr a -> string_of_attr a
  | Meth m -> string_of_mthd m
  
let rec string_of_attrs_or_methods = function
  | [] -> ""
  | a::l -> (string_of_attr_or_method a) ^" "^ (string_of_attrs_or_methods l)
  
let string_of_class_ = function
  | Class_(s1, aom) -> "Class("^s1^" { "^(string_of_attrs_or_methods aom)^" })" 
  | ClassWithExtends(s1,s2, aom) -> "ClassWithExtends("^s1^" extends "^s2^" { "^(string_of_attrs_or_methods aom)^" })" 
  
let string_of_class_or_expr = function
  | Class c -> string_of_class_ c
  | Expr e -> string_of_expr e
