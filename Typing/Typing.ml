open AST
open Error
open TypingError
open Type

(* Return true if one class is the subtype of another class *)
let rec isSubtypeOf env class_parent class_child =
  if (class_parent = class_child)
  then true
  else 
    begin
      if class_child = (fromString "Object")
      then false
      else
        let class_child_env = (findClass env (stringOf class_child)) in
          isSubtypeOf env class_parent (getParent class_child_env)
    end

(* Test if given values corresponds with argument list of a method *)
let rec match_arglist_args loc lex args env =
  match (lex, args) with
    | ([], []) -> ()
    | ([], l) -> not_enough_args loc
    | (l, []) -> too_much_args loc
    | (e::l1, (atype,_)::l2) -> 
      check_expr e env;
      match e.etype with
        | Some te ->
          if ( isSubtypeOf env atype te )
          then match_arglist_args loc l1 l2 env
          else not_subtype (stringOf te) (stringOf atype) loc
        | None -> typing_error loc

(* Test expressions *)
and check_expr e env = match e.edesc with 
    | New s -> 
      if (not (isClass env (Type.stringOf (Located.elem_of s))))
      then unknown_type (Type.stringOf (Located.elem_of s)) e.eloc;
      e.etype <- Some (Located.elem_of s)

    | Seq (e1, e2) -> 
      check_expr e1 env;
      check_expr e2 env;
      e.etype <- e2.etype

    | Call (e0, fname, args) -> 
      check_expr e0 env;
      begin match e0.etype with 
        | Some t -> 
          begin 
            try
              let f = (findMethod_rec env (stringOf t) fname) in
              let frt_n = (stringOf f.freturn) in
              if not (isClass env frt_n) 
              then unknown_type frt_n e.eloc
              else match_arglist_args e.eloc args f.fargs env;
              e.etype <- Some f.freturn
            with 
              Not_found -> unknown_meth fname (stringOf t) e.eloc
          end
        | None -> typing_error e.eloc
      end

    | If (e0, e1, e2) ->
      begin
        check_expr e0 env;
        begin match e0.etype with 
          | Some t -> if ( (stringOf t) <> "Boolean") 
                      then incorrect_type "Boolean" (stringOf t) e.eloc
          | None -> typing_error e.eloc
        end;
        check_expr e1 env;
        begin match e2 with
          | Some e2_ -> check_expr e2_ env;
          | None ->  ()
        end;
        e.etype <- e1.etype
      end

    | Val v -> 
      begin match v with
        | String s -> e.etype <- Some (fromString "String")
        | Int i -> e.etype <- Some (fromString "Int")
        | Boolean b -> e.etype <- Some (fromString "Boolean")
        | Null -> e.etype <- Some (fromString "Null")
      end

    | Var s -> 
      begin
        try e.etype <- Some (findVar env s)
        with Not_found -> unknown_var s e.eloc
      end

    (* Le type d'une assignation est Null *)
    | Assign (s,e0) -> 
      begin
        try 
          let var_type = (findVar env s) in
          check_expr e0 env;
          begin match e0.etype with
            | Some expr_type ->
              if (not (isSubtypeOf env var_type expr_type)) 
              then not_subtype (stringOf expr_type) (stringOf var_type) e.eloc
              else e.etype <- Some (fromString "Null")
            | None -> typing_error e.eloc
          end
        with Not_found -> unknown_var s e.eloc
      end

    | Define (var_name,var_type,e0,e1) ->
      let var_type_string = Type.stringOf (Located.elem_of var_type) in 
      if (not(isClass env var_type_string)) 
      then unknown_type var_type_string e.eloc;
      check_expr e0 env;
      begin match e0.etype with
        | Some expr_type ->
          if (not (isSubtypeOf env (fromString var_type_string) expr_type)) 
          then not_subtype (stringOf expr_type) var_type_string e.eloc
        | None -> typing_error e.eloc
      end;
      let new_env = addVar env var_name (Located.elem_of var_type) in
      check_expr e1 new_env;
      e.etype <- e1.etype

    | Cast (t,e0) -> 
      let t_string = Type.stringOf (Located.elem_of t) in
      if (not(isClass env t_string)) then unknown_type t_string e.eloc;
      let new_t = (fromString t_string) in
      check_expr e0 env;
      begin match e0.etype with
        | Some expr_type -> 
          if ((isSubtypeOf env new_t expr_type) || 
              (isSubtypeOf env expr_type new_t))
          then e.etype <- Some new_t
          else not_castable (stringOf expr_type) (stringOf new_t) e.eloc
        | None -> typing_error e.eloc
      end

    | Instanceof (e0,t) -> 
      let t_string = Type.stringOf (Located.elem_of t) in
      if (not(isClass env t_string)) then unknown_type t_string e.eloc
      check_expr e0 env;
      e.etype <- Some (fromString "Boolean")

(* Find all types *)
let rec find_types type_asts env = 
  match type_asts with
    | [] -> env
    | c1::others -> 
      try find_types others (addClass env c1.cname)
      with ClassAlreadyExists _ -> type_clash c1.cname c1.cloc

(* Check parent class *)
let check_parent c env =
  let cparent_string = Type.stringOf (Located.elem_of c.cparent) in
  let c_parenttype = (fromString cparent_string) in
  let c_type = (fromString c.cname) in
  if not (isClass env cparent_string)
  then unknown_type cparent_string c.cloc;
  if (isSubtypeOf env c_type c_parenttype)
  then inheritance_cycle (c.cname) cparent_string c.cloc;
  setParent env c.cname cparent_string

(* Check argument list *)
let rec check_args loc m args env =
  match args with
    | [] -> m
    | (aname, atype)::l -> 
      let atype_string = Type.stringOf (Located.elem_of atype) in
      if not (isClass env atype_string)
      then unknown_type atype_string loc;
      let this_arg = ((Located.elem_of atype), aname) in
      let new_args = this_arg::m.fargs in
      let new_f = { fargs = new_args ; freturn = m.freturn ; fstatic = m.fstatic} in
      check_args loc new_f l env

(* Compare two lists of argument *)
let rec compare_args args1 args2 =
  match (args1, args2) with
    | ([],[]) -> true
    | (_::q1, []) -> false
    | ([], _::q2) -> false
    | ((t1,_)::q1, (t2,_)::q2) -> (t1 = t2) && (compare_args q1 q2)

(* Step 1: Check method definitions and add them to env *)
let rec check_methods_def c methods env =
  match methods with 
    | [] -> env
    | m1::others -> 
      let mreturntype_string = Type.stringOf (Located.elem_of m1.mreturntype) in
      if not (isClass env mreturntype_string)
      then unknown_type mreturntype_string m1.mloc;
      let m = {fargs=[]; freturn = (fromString mreturntype_string); fstatic = m1.mstatic} in
      let f_wargs = check_args m1.mloc m m1.margstype env in
      try let new_env = addMethod env c m1.mname f_wargs in
        check_methods_def c others new_env
      with MethodAlreadyExists(s) -> method_clash m1.mname m1.mloc
  
(* Step 1: Check class definications and add them to env *)
let rec analyse_types type_asts env =
  match type_asts with
    | [] -> env
    | c1::others -> let parent_env = check_parent c1 env in
        let meth_env = check_methods_def c1.cname c1.cmethods parent_env in
        analyse_types others meth_env

(* Step 2: Check the attributes *)
let rec check_attributes c attrs env =
  match attrs with
    | [] -> env
    | a1::others -> 
      let atype_string = Type.stringOf (Located.elem_of a1.atype) in
      if not (isClass env atype_string)
      then unknown_type atype_string a1.aloc;
      if (isVar env a1.aname)
      then attribute_clash a1.aname a1.aloc;
      let new_env = addVar env a1.aname (fromString atype_string) in
      match a1.adefault with
        | None -> check_attributes c others new_env
        | Some e -> 
          check_expr e env;
          match e.etype with
            | None -> typing_error e.eloc
            | Some t -> 
              if not (isSubtypeOf env (fromString atype_string) t)
              then  not_subtype (stringOf t) atype_string a1.aloc;
              check_attributes c others new_env

(* Add argument list *)
let rec add_args params env =
  match params with 
    | [] -> env
    | (pname,ptype)::q -> 
      let new_env = addVar env pname (Located.elem_of ptype) in
              add_args q new_env

(* Step 2: Check method redefinition and body *)
let rec check_methods c methods env =
  match methods with 
    | [] -> ()
    | m1::others -> 
      (* Check redifinition *)
      let cparent = stringOf (getParent (findClass env c)) in
      if ( isMethod_rec env cparent m1.mname )
      then begin
        let fparent = findMethod_rec env cparent m1.mname in
        let fchild = findMethod env c m1.mname in
        if not (compare_args fchild.fargs fparent.fargs)
        then method_clash m1.mname m1.mloc
      end;
      (* Check return type *)
      let env_with_args = add_args m1.margstype env in
      let new_env = addVar env_with_args "this" (fromString c) in
      check_expr m1.mbody new_env;
      match m1.mbody.etype with
        | None -> typing_error m1.mloc
        | Some t -> 
            if not (isSubtypeOf env (Located.elem_of m1.mreturntype) t)
            then
              not_subtype (stringOf t) (Type.stringOf (Located.elem_of m1.mreturntype)) m1.mloc;
            check_methods c others env
      
(* Step 2: Check class body *)
let rec check_class t_ast env =
  match t_ast with
    | [] -> env
    | c::others -> 
      let attr_env = 
        check_attributes c.cname c.cattributes env in
        check_methods c.cname c.cmethods attr_env;
        check_class others env;
        env

(* Typing step 1 + step 2 *)
let check_classes t_ast env = 
  let env_with_type = find_types t_ast env in
    let env_with_type_itf = analyse_types t_ast env_with_type in
      check_class t_ast env_with_type_itf


(* Check and generate class environments *)
let typing (classes, exprs) =
  let env = check_classes classes (Type.init() ) in
    (* Then check top level expressions *)
    match exprs with
      | Some expr -> check_expr expr env;
      | None -> ()
