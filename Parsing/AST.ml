type argument = string * Type.t Located.t (* nom * type *)

type value = 
  | String of string
  | Int of int
  | Null
  | Boolean of bool

type expression_desc = 
  | New of Type.t Located.t
  | Seq of expression * expression
  | Call of expression * string * expression list
  | If of expression * expression * expression
  | Val of value
  | Var of string
  | Assign of string * expression
  | Define of string * Type.t Located.t * expression * expression
  | Cast of Type.t Located.t * expression
  | Instanceof of expression * Type.t Located.t

and expression = 
    {
      edesc : expression_desc;
      eloc : Location.t;
      mutable etype : Type.t option;
    }

type astattribute =
    {
      aname : string;
      atype : Type.t Located.t;
      adefault : expression option;
      aloc : Location.t;
    }

type astmethod =
    {
      mname : string;
      mreturntype : Type.t Located.t;
      margstype : argument list;
      mbody : expression;
      mloc : Location.t;
    }

type astclass =
    {
      cname : string;
      cparent : Type.t Located.t;
      cattributes : astattribute list;
      cmethods : astmethod list;
      cloc : Location.t;
    }

type t = astclass list * expression option

let string_of_value = function
  | String s -> "\""^s^"\""
  | Boolean b -> string_of_bool b
  | Int i -> string_of_int i
  | Null -> "null"

let rec string_of_expression_desc = function
  | New t -> 
      "new "^(Type.stringOf (Located.elem_of t))
  | Seq(e1,e2) -> 
      (string_of_expression e1)^" ; "^(string_of_expression e2)
  | If(c,e1,e2) -> 
      "if "^(string_of_expression c)^" { "^
      (string_of_expression e1)^" } else { "^(string_of_expression e2)^" }"
  | Call(r,m,al) -> 
      (string_of_expression r)^
      "."^m^"("^
      (String.concat "," (List.map string_of_expression al))^
      ")"
  | Val v -> string_of_value v
  | Var v -> v
  | Assign(s,e) ->
      s^":="^(string_of_expression e)
  | Define(n,t,e1,e2) ->
      (Type.stringOf (Located.elem_of t))^" "^n^" = "^(string_of_expression e1)^
	" in {"^(string_of_expression e2)^" }"
  | Cast(t,e) ->
      "("^(Type.stringOf (Located.elem_of t))^") "^(string_of_expression e)
  | Instanceof(e,t) ->
      (string_of_expression e)^" instanceof "^(Type.stringOf (Located.elem_of t))

and string_of_expression e = 
  let s = string_of_expression_desc e.edesc in
    match e.etype with
      | None -> s
      | Some t -> "("^s^" : "^(Type.stringOf t)^")"

let print_attribute a =
  print_string "  ";
  print_string ((Type.stringOf (Located.elem_of a.atype))^" "^a.aname);
  match a.adefault with
    | None -> print_newline ()
    | Some e -> print_endline(" = "^(string_of_expression e))

let print_method m =
  print_string "  ";
  print_string((Type.stringOf (Located.elem_of m.mreturntype))^" "^m.mname^"(");
  print_string(String.concat 
		  "," 
		  (List.map (fun (n,t) -> (Type.stringOf (Located.elem_of t))^" "^n) m.margstype)
	      );
  print_endline(") {");
  print_endline ("    "^string_of_expression m.mbody);
  print_endline("  }")

let print_class c =
  print_endline("class "^c.cname^" extends "^(Type.stringOf (Located.elem_of c.cparent))^" {");
  List.iter print_attribute c.cattributes;
  List.iter print_method c.cmethods;
  print_endline "}";
  print_newline()

let print_program (cl,e_op) =
  List.iter print_class cl;
  print_endline "main:";
  match e_op with
    | None -> print_endline "  nop"
    | Some e -> print_endline("  "^(string_of_expression e))
