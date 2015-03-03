open AST
open TypeError
open Type

(* Return true if one class is the subtype of another class *)
let rec isSubtypeOf env class_parent class_child =
  if (class_parent = class_child)
    then true
  else 
    begin
      if (class_child = (fromString "Object"))
      then false
      else
        let class_child_env = (findClass env (stringOf class_child)) in
          isSubtypeOf env class_parent (getSuper class_child_env)
    end

(* Verifie la correspondance de typage entre une liste d'expressions et une liste d'arguments *)
let rec match_exprlist_args loc lex args env =
  match (lex, args) with
    | ([], []) -> ()
    | ([], l) -> not_enough_args loc
    | (l, []) -> too_much_args loc
    | (e::l1, (atype,_)::l2) -> 
      check_expr e env;
      match e.etype with
    | Some te ->
      if ( isSubtypeOf env atype te )
      then match_exprlist_args loc l1 l2 env
      else not_subtype (stringOf te) (stringOf atype) loc
    | None -> typing_error loc

(* Typage d'une expression 
Pour chaque expression, on commence par typer les sous-expressions pour typer l'expression résultante *)
and check_expr e env = match e.edesc with 
    | New s -> 
      if (not (isClass env (Type.stringOf (Located.elem_of s)))) 
        then unknown_type (Type.stringOf (Located.elem_of s)) e.eloc;
      e.etype <- Some (fromString (Type.stringOf (Located.elem_of s)))

    | Seq (e1,e2) -> 
      check_expr e1 env;
      check_expr e2 env;
      e.etype <- e2.etype

    | Call (e0,fname,args) -> 
      check_expr e0 env;
      begin match e0.etype with 
        | Some t -> 
          begin 
            try
              let f = (findFun_rec env (stringOf t) fname) in
              let frt_n = (stringOf f.freturn) in
              if not (isClass env frt_n) then unknown_type frt_n e.eloc;
              match_exprlist_args e.eloc args f.fargs env;
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
          | Some t -> if ( (stringOf t) <> "Boolean") then incorrect_type "Boolean" (stringOf t) e.eloc;
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
        try let var_type = (findVar env s) in
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
      if (not(isClass env var_type_string)) then unknown_type var_type_string e.eloc;
      check_expr e0 env;
      begin match e0.etype with
        | Some expr_type ->
          if (not (isSubtypeOf env (fromString var_type_string) expr_type)) 
          then not_subtype (stringOf expr_type) var_type_string e.eloc
        | None -> typing_error e.eloc
      end;
      (* New variable with the same name hide the old ones *)
      let new_env = addVar env var_name (fromString (Type.stringOf (Located.elem_of var_type))) in
      check_expr e1 new_env;
      e.etype <- e1.etype

    | Cast (t,e0) -> 
      let t_string = Type.stringOf (Located.elem_of t) in
      if (not(isClass env t_string)) then unknown_type t_string e.eloc;
      let new_t = (fromString t_string) in
      check_expr e0 env;
      begin match e0.etype with
        | Some expr_type -> 
          if ( ( isSubtypeOf env new_t expr_type) || ( isSubtypeOf env expr_type new_t ) )
          then e.etype <- Some new_t
          else not_castable (stringOf expr_type) (stringOf new_t) e.eloc
        | None -> typing_error e.eloc
      end

    | Instanceof (e0,t) -> 
      let t_string = Type.stringOf (Located.elem_of t) in
      if (not(isClass env t_string)) then unknown_type t_string e.eloc;
      check_expr e0 env;
      e.etype <- Some (fromString "Boolean")

(* Trouve tous les types définis dans le fichiers *)
let rec find_types type_asts env = 
  match type_asts with
    | [] -> env
    | c1::others -> try find_types others (addClass env c1.cname)
      with ClassAlreadyPresent _ -> type_clash c1.cname c1.cloc

(* Vérifie que le parent d'une classe est correct *)
let check_super c env =
  let cparent_string = Type.stringOf (Located.elem_of c.cparent) in
  let c_supertype = (fromString cparent_string) in
  let c_type = (fromString c.cname) in
  if not (isClass env cparent_string)
  then unknown_type cparent_string c.cloc;
  if (isSubtypeOf env c_type c_supertype)
  then inheritance_cycle (c.cname) cparent_string c.cloc;
  setSuper env c.cname cparent_string

(* Verifie les arguments d'une fonction *)
let rec check_args loc f args env =
  match args with
    | [] -> f
    | ( aname, atype )::l -> 
      if not (isClass env atype)
      then unknown_type atype loc;
      let this_arg = ((fromString atype), aname) in
      let new_args = this_arg::f.fargs in
      let new_f = { fargs = new_args ; freturn = f.freturn} in
      check_args loc new_f l env

(* Compare 2 liste d'argument *)
let rec compare_args args1 args2 =
  match (args1, args2) with
    | ([],[]) -> true
    | (_::q1, []) -> false
    | ([], _::q2) -> false
    | ((t1,_)::q1, (t2,_)::q2) -> (t1 = t2) && (compare_args q1 q2)

(* Verifie la declaration des fonctions d'une classe *)
let rec check_funs c funs env =
  match funs with 
    | [] -> env
    | f1::others -> 
      let mreturntype_string = Type.stringOf (Located.elem_of f1.mreturntype) in
      if not (isClass env mreturntype_string)
      then unknown_type mreturntype_string f1.mloc;
      let f = {fargs=[]; freturn = (fromString mreturntype_string)} in
      let f_wargs = check_args f1.mloc f f1.margstype env in
      try let new_env = addFun env c f1.mname f_wargs in
          (* On ne peut pas verifier ici si c'est une redefinition 
             car on n'a pas forcement déjà analysé les fonctions
             de la classe parent. *)
    check_funs c others new_env
      with MethodAlreadyPresent(s) -> method_clash f1.mname f1.mloc
  
(* Verifie l'interface des classes *)
let rec analyse_types type_asts env =
  match type_asts with
    | [] -> env
    | c1::others -> let super_env = check_super c1 env in
        let meth_env = check_funs c1.cname c1.cmethods super_env in
        analyse_types others meth_env

(* Vérifie la déclaration des attributs d'une classe *)
let rec check_attributes c attrs env =
  match attrs with
    | [] -> env
    | a1::others -> 
      if not (isClass env a1.atype)
      then unknown_type a1.atype a1.aloc;
      if (isVar env a1.aname)
      then attribute_clash a1.aname a1.aloc;
      let new_env = addVar env a1.aname (fromString a1.atype) in
      match a1.adefault with
        | None -> check_attributes c others new_env
        | Some e -> 
          check_expr e env;
          match e.etype with
            | None -> typing_error e.eloc
            | Some t -> 
              if not (isSubtypeOf env (fromString a1.atype) t)
              then  not_subtype (stringOf t) a1.atype a1.aloc;
              check_attributes c others new_env

(* Ajoute une liste de parametres à l'environnement *)
let rec add_params params env =
  match params with 
    | [] -> env
    | (pname,ptype)::q -> let new_env = addVar env pname (fromString ptype) in
                          add_params q new_env

(* On verifie la definition des fonctions *)
let rec check_funs_def c funs env =
  match funs with 
    | [] -> ()
    | f1::others -> 
      (* Si c'est une redefinition, on verifie les arguments *)
      let cparent = stringOf (getSuper (findClass env c)) in
      if ( isFun_rec env cparent f1.mname )
      then begin
        let fparent = findFun_rec env cparent f1.mname in
        let fchild = findFun env c f1.mname in
        if not (compare_args fchild.fargs fparent.fargs)
        then method_clash f1.mname f1.mloc
      end;
      (* On type le corps de la methode, et on verifie coherence *)
      let env_with_args = add_params f1.margstype env in
      let new_env = addVar env_with_args "this" (fromString c) in
      check_expr f1.mbody new_env;
      match f1.mbody.etype with
        | None -> typing_error f1.mloc
        | Some t -> 
          if not (isSubtypeOf env (fromString f1.mreturntype) t)
          then  not_subtype (stringOf t) f1.mreturntype f1.mloc;
          check_funs_def c others env
      
(* Recursively check the classes *)
let rec check_class t_ast env =
  match t_ast with
    | [] -> env
    | c::others -> 
      let attr_env = check_attributes c.cname c.cattributes env in
        check_methods c.cname c.cmethods attr_env;
        check_class others env;
        env

(* Check the classes *)
let check_classes t_ast env = 
  let env_with_type = find_types t_ast env in
    let env_with_type_itf = analyse_types t_ast env_with_type in
      check_class t_ast env_with_type_itf


(* Check and generate class environments *)
let typing (classes, exprs) =
  let env = check_classes classes (Env.init()) in
    (* Then check top level expressions *)
    match exprs with
      | Some expr -> check_expr expr env;
      | None -> ()
