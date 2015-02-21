
exception ClassAlreadyExist of string
exception MethodAlreadyExist of string

type t = string

let stringOf t = t

let fromString t = t

let DEFAULT_NUM = 10

(* Environnement type *)
type tEnv = {
	env_v: envVariables;
	env_c: envClasses 
}
(* Environnement of variables *)
and	envVariables = (string, t)  Hashtbl.t
(* Environnement of classes *)
and envClasses = (string, tClass) Hashtbl.t
(* Class type *)
and tClasse = {
	cparent : t;
	cmethods : methods
}
(* Methods *)
and methods = (string, tMethod) Hashtbl.t
(* Method type *)
and tMethod = {
	fargs : tArg list;
	freturn : t
}
(* Method argument type*)
and tArg = t * string



(****** Initialize the environement and add the default classes ******)

(* Definition of class Object *)
let makeClassObject () =
	let c = makeClass in
	let t_obj = (fromString "Object") in
	let t_bool = (fromString "Boolean") in
	Hashtbl.add c.methods "eq" { fargs = [(t_obj, "o")]; freturn = t_bool };
	Hashtbl.add c.methods "neq" { fargs = [(t_obj, "o")]; freturn = t_bool };
	c

(* Definition of class Int *)
let makeClassInt () =
	let c = makeClass in
	let t_int = (fromString "Int") in
	let t_bool = (fromString "Boolean") in
	Hashtbl.add c.methods "add" { fargs = [(t_int, "n")]; freturn = t_int };
	Hashtbl.add c.methods "sub" { fargs = [(t_int, "n")]; freturn = t_int };
	Hashtbl.add c.methods "mul" { fargs = [(t_int, "n")]; freturn = t_int };
	Hashtbl.add c.methods "div" { fargs = [(t_int, "n")]; freturn = t_int };
	Hashtbl.add c.methods "mod" { fargs = [(t_int, "n")]; freturn = t_int };
	Hashtbl.add c.methods "neg" { fargs = []; freturn = t_int };
	Hashtbl.add c.methods "gt" { fargs = [(t_int, "n")]; freturn = t_bool };
	Hashtbl.add c.methods "ge" { fargs = [(t_int, "n")]; freturn = t_bool };
	Hashtbl.add c.methods "lt" { fargs = [(t_int, "n")]; freturn = t_bool };
	Hashtbl.add c.methods "le" { fargs = [(t_int, "n")]; freturn = t_bool };
	c

(* Definition of class Bool *)
let makeClassBool () =
	let c = makeClass in
	let t_bool = (fromString "Boolean") in
	Hashtbl.add c.methods "and" { fargs = [(t_bool, "b")]; freturn = t_bool };
	Hashtbl.add c.methods "or" { fargs = [(t_bool, "b")]; freturn = t_bool };
	Hashtbl.add c.methods "not" { fargs = []; freturn = t_bool };
	c

(* Initialize environment *)
let initialEnv () =
	let classlist = makeEnv (Hashtbl.create DEFAULT_NUM : envVariables) (Hashtbl.create DEFAULT_NUM : envClasses) in
	Hashtbl.add classlist.env_c "Object" makeClassObject();
	Hashtbl.add classlist.env_c "Int" makeClassInt();
	Hashtbl.add classlist.env_c "String" makeClass;
	Hashtbl.add classlist.env_c "Boolean" makeClassBool();
	classlist


(****** Definition of functions ******)

(* Create and initialize a new environment *)
let makeEnv v c = {
	env_v = v ;
	env_c = c
}

(* Create and initialize a new class *)
let makeClass = {
	cparent = (fromString "Object");
	methods = (Hashtbl.create DEFAULT_NUM : methods)
}

(* Get parent *)
let getParent c = c.cparent

(* Set parent *)
let setSuper env classname parentname =
	let new_env_c = Hashtbl.copy env.env_c in
	let c = findClass env classname in
	Hashtbl.add new_env_c classname {cparent = (fromString parentname); methods = c.methods} ;

(* Find variable in a specific environment *)
let findVar env v = Hashtbl.find (env.env_v)

(* Check a variable in a specific environment *)
let isVar env v =
	try findVar env v; true
	with Not_found -> false

(* Find class in a specific environment *)
let findClass env = Hashtbl.find (env.env_c)

(* Check a class in a specific environment *)
let isClass env c =
	try findClass env c; true
	with Not_found -> false

(* Find method in a specific environment *)
let findMethod env cname = 
	let c = findClass env cname in
	Hashtbl.find (c.methods)

(* Recursively find a method in class and its parent *)
let rec findMethod_rec env cname m =
	try
		Hashtbl.find (Hashtbl.find(env.env_c) cname).methods m
	with Not_found ->
	if (cname = "Object")
		then raise Not_found
	else
		begin
			let c = findClass env cname in
			findMethod_rec env (stringOf c.cparent) m
		end

(* Check a class in a specific environment *)
let isFun env c m =
	try findMethod env c m; true
	with Not_found -> false

(* Recursively check a class in a specific environment *)
let isFun_rec env c m =
	try findMethod_rec env c m; true
	with Not_found -> false

(* Add variable in a specific environment*)
let addVar env n t =
	let new_v = Hashtbl.copy env.env_v in
	Hashtbl.add new_v n t;
	makeEnv new_v env.env_c

(* Add class in a specific environment*)
let addClass env c =
	if (isClass env c) then raise (ClassAlreadyExist(c));
	let new_c = Hashtbl.copy env.env_c in
	Hashtbl.add new_c c makeClass ;
	makeEnv env.env_v new_c

(* Add method in a class*)
let addFun env cname mname m =
	if (isFun env cname mname) then raise (MethodAlreadyExist(mname));
	let new_env_c = Hashtbl.copy env.env_c in
	let old_c = findClass env cname in
	let new_methods = Hashtbl.copy old_c.methods in
	Hashtbl.add new_methods mname m;
	Hashtbl.add new_env_c cname {cparent = old_c.cparent; methods = new_methods};
	makeEnv env.env_v new_env_c