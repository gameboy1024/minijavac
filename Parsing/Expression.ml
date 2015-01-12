type binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod
  | Band | Bor
  | Bgt | Bge | Blt | Ble | Beq | Bneq
				    
type unop =
  | Unot | Uminus

type expression =
  | NoExp 
  | ExpList of expression * expression
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
  and arg =
    | Arg of expression
  and args =
    | Args of arg list
    
(*
type params = 
  | NoParam
  | Param of string * string
  | Params of params * params
 *)
 
type param = 
  | Param of string * string
  
type params =
  | Params of param list
  
type attr =
  | Attribute of bool * string * string
  | AttributeWithAssign of bool * string * string * expression 

type mthd = 
  | Method of bool * string * string * (param list) * expression
  
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
  | Bneq, Vint x, Vint y -> Vbool(x != y)
  | Band, Vbool x, Vbool y -> Vbool(x && y)
  | Bor, Vbool x, Vbool y -> Vbool(x || y)
  | _ -> failwith "bug:type error not catched"
  
(*
let rec eval env exp =
  match exp with
  | Bool b -> Vbool b
  | Int i -> Vint i
  | Def(v1, v2, e1,e2) -> eval ((v1, v2, eval env e1)::env) e2
  | Var v -> (try List.assoc v env with Not_found -> failwith "bug:type error not catched")
  | Binop(op,e1,e2) -> (get_op_b op) (eval env e1) (eval env e2)
  | Unop(op,e) -> (get_op_u op) (eval env e)
*)

let string_type_of_value = function
  | Vbool _ -> "bool"
  | Vint _  -> "int"

let string_type_of_op_b = function
  | Badd | Bsub | Bmul | Bdiv | Bmod | Bgt
  | Bge | Blt | Ble | Beq | Bneq -> "int"
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
  | Badd -> "+"
  | Bsub -> "-"
  | Bmul -> "*"
  | Bdiv -> "/"
  | Bmod -> "%"
  | Band -> "&&"
  | Bor  -> "||"
  | Bgt  -> ">"
  | Bge -> ">="
  | Blt -> "<"
  | Ble -> "<="
  | Beq -> "="
  | Bneq -> "!="

let rec string_of_expr exp =
  match exp with
  | NoExp -> ""
  | ExpList(e,l) -> string_of_expr e ; print_string "," ; string_of_expr l
  | Int i -> string_of_int i
  | Var v -> v
  | Null -> "null"
  | Bool true -> "Bool true"
  | Bool false -> "Bool false"
  | This -> "this"
  | String s -> s
  | Unop(op, e) -> "Unop("^(string_of_op_u op)^(string_of_expr e)^")"
  | Binop(op, e1, e2) -> 
      "("^(string_of_expr e1)^(string_of_op_b op)^(string_of_expr e2)^")"
  | Assign(s,e) -> s^"="^(string_of_expr e)
  | Def(v1, v2, e1, e2) -> " " ^v2^"="^(string_of_expr e1)^" in "^(string_of_expr e2)
  | Ifelse(e1,e2,e3) -> "if("^(string_of_expr e1)^") { "^(string_of_expr e2 )^" }else{ "^(string_of_expr e3)^"}"
  | Cast(t, v) -> "cast " ^ t ^ " " ^ string_of_expr v
  | Invoke(e, s, l) -> (string_of_expr e)^"."^s^"("^(string_of_args l)^")"
  | New s -> "new "^s 
  | Instanceof(e, t) ->"instanceof " ^ string_of_expr e ^  " "^t
  and string_of_args = function
      [] -> ""
      | a::l -> string_of_expr a ^ string_of_args l
      
  (*
let rec string_of_params = function
  | NoParam -> ""
  | Param(s1,s2) ->  s1^" "^s2
  | Params(p,l) -> string_of_params p; print_string "," ; string_of_params l
    *)
    
let rec string_of_param p = match p with
	| Param(t,id) -> t^" "^id
  
let rec string_of_params = function
  [] -> ""
  | (p :: l) -> string_of_param(p)^","^(string_of_params l)
  
let string_of_attr = function
  | Attribute(static, s1,s2) ->string_of_bool static ^ s1^" "^s2^";"
  | AttributeWithAssign(static, s1,s2,e) -> string_of_bool static ^s1^" "^s2^" ="^(string_of_expr e)^";"
  
let string_of_mthd = function
  | Method(b, s1,s2,p,e) -> string_of_bool b ^ s1^" "^s2^"("^(string_of_params p)^") {"^(string_of_expr e)^"}"
  (*
let string_of_attr_or_method = function 
  [] -> ""
  | Attr (a::l) -> (string_of_attr a) ^ (string_of_attr_or_method l)
  | Meth (m::l) -> string_of_mthd m ^ string_of_attr_or_method l
  *)
let string_of_attr_or_method = function
  | Attr a -> string_of_attr a
  | Meth m -> string_of_mthd m
  
let rec string_of_attrs_or_methods = function 
  [] -> ""
  | a::l -> (string_of_attr_or_method a) ^ (string_of_attrs_or_methods l)
  
let string_of_class_ = function
  | Class_(s1, aom) -> "class"^s1^" { "^(string_of_attrs_or_methods aom)^" }" 
  | ClassWithExtends(s1,s2, aom) -> "class"^s1^"(extends "^s2^") { "^(string_of_attrs_or_methods aom)^" }" 
  
let string_of_classorexpr = function
  | Class c -> string_of_class_ c
  | Expr e -> string_of_expr  e
      
type typ = Tbool | Tint
exception Wrong_types_bop of binop * typ * typ
exception Wrong_types_uop of unop * typ
								    
let get_op_b_type op x y =
  match op, x, y with
  | Badd, Tint, Tint
  | Bsub, Tint, Tint
  | Bmul, Tint, Tint
  | Bdiv, Tint, Tint
  | Bmod, Tint, Tint -> Tint
  | Bgt, Tint, Tint
  | Bge, Tint, Tint
  | Blt, Tint, Tint
  | Ble, Tint, Tint
  | Beq, Tint, Tint
  | Bneq, Tint, Tint
  | Band, Tbool, Tbool
  | Bor, Tbool, Tbool -> Tbool
  | _ -> raise(Wrong_types_bop(op,x,y))

let get_op_u_type op x = 
  match op, x with
  | Unot, Tbool -> Tbool
  | Uminus, Tint -> Tint
  | _ -> raise(Wrong_types_uop(op,x))

let string_of_type = function
  | Tbool -> "bool"
  | Tint  -> "int"

let rec typing env exp =
  match exp with
  | Bool _ -> Tbool
  | Int _ -> Tint
  | Var v -> (try List.assoc v env with Not_found -> raise(Unbound_variable v))
  | Def(v1, v2,e1,e2) ->
     let t1 = typing env e1 in
     typing ((v2,t1)::env) e2
  | Binop(op,e1,e2) ->
     let t1 = typing env e1 in
     let t2 = typing env e2 in
     get_op_b_type op t1 t2
  | Unop(op,e) -> 
     let t = typing env e in
     get_op_u_type op t
