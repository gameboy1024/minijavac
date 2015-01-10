type binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod
  | Band | Bor
  | Bgt | Bge | Blt | Ble | Beq | Bneq
				    
type unop =
  | Unot | Uminus

type expression =
  | Bool of bool
  | Int of int
  | Var of string
  | Def of string * expression * expression
  | Binop of binop * expression * expression
  | Unop of unop * expression

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

let rec eval env exp =
  match exp with
  | Bool b -> Vbool b
  | Int i -> Vint i
  | Def(v,e1,e2) -> eval ((v,eval env e1)::env) e2
  | Var v -> (try List.assoc v env with Not_found -> failwith "bug:type error not catched")
  | Binop(op,e1,e2) -> (get_op_b op) (eval env e1) (eval env e2)
  | Unop(op,e) -> (get_op_u op) (eval env e)

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
  | Bool true -> "true"
  | Bool false -> "false"
  | Int i -> string_of_int i
  | Var v -> v
  | Def(v, e1, e2) -> v^"="^(string_of_expr e1)^" in "^(string_of_expr e2)
  | Binop(op, e1, e2) -> 
      "("^(string_of_expr e1)^(string_of_op_b op)^(string_of_expr e2)^")"
  | Unop(op, e) -> "("^(string_of_op_u op)^(string_of_expr e)^")"

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
  | Def(v,e1,e2) ->
     let t1 = typing env e1 in
     typing ((v,t1)::env) e2
  | Binop(op,e1,e2) ->
     let t1 = typing env e1 in
     let t2 = typing env e2 in
     get_op_b_type op t1 t2
  | Unop(op,e) -> 
     let t = typing env e in
     get_op_u_type op t
