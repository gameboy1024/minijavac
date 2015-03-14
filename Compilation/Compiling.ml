open AST
open Location
open Located
open Error
open Typer

(* Structure introduced to compile the class tree in the right order *)

(* 
type 'a tree = Node of (AST.astclass * 'a tree list)



let rec find_class_by_parent cl parent = match cl with
	|[] 														-> []
	|(t::q) when Type.stringOf(elem_of t.cparent)=parent.cname	-> t::(find_class_by_parent q parent)
	|(t::q)														-> find_class_by_parent q parent

let add_sons_to_node node cl = 
	let Node(p,l)=node in let sons=find_class_by_parent cl p in
	let rec add_class_l node class_l = match class_l with
		|[] 		-> node
		|(t::q)		-> (match node with
						| Node(s,l) -> let new_node=Node(t,[]) in add_class_l (Node(s,l@[new_node])) q)
	in add_class_l node sons 

let add_sons_to_leaves tree classtable count = 
	let rec browse_tree tree count = match tree with
		|Node(s,[]) as node -> add_sons_to_node node classtable count
		|Node(s,l)			-> (match l with
									|(t::q) -> let (n,new_count)=browse_tree t count in let (lnode,last_count)=browse_tree_l q new_count in (Node(s,n::lnode),last_count)
								)
	and browse_tree_l lnode count = match lnode with
		|[] 		-> ([],count)
		|(t::q)		-> let (n,new_count)=browse_tree t count in let (lnode,last_count)=browse_tree_l q new_count in (n::lnode,last_count)
	in browse_tree tree count *)



(* Environment definition *)
type comp_env =
	{
		env_c : class_env;
		env_m : method_env;
		env_o : object_env;
		env_v : var_env;
	}
and class_env  = (string, desc_class) Hashtbl.t
and method_env = (string, desc_method) Hashtbl.t
and object_env = (int, desc_object) Hashtbl.t
and var_env    = (string, desc_var) Hashtbl.t
and desc_class = 
	{
		d_pname : string;
		d_cname : string;
		d_cattributes : (string, expression option) Hashtbl.t;
		d_cmethods: method_env;
	}
and desc_method = 
	{
		d_mname : string;
		d_margs : (string * Type.t Located.t) list;
		d_mbody : expression;
	}	
and desc_object = 
	{
		d_oclass : string;
		d_oatttr : (string, AST.value) Hashtbl.t;
	}
and desc_var = AST.value



let print_desc_class dclass = 
	print_endline ("#### Classe "^dclass.d_cname^" ###");
	print_endline ("ATTRIBUTES");
	Hashtbl.iter (fun x y -> print_endline (x^" "^(AST.string_of_expression_option y))) dclass.d_cattributes;
	print_endline ("METHODS");
	Hashtbl.iter (fun x y -> print_endline x) dclass.d_cmethods;
	print_endline "##########################"


let print_class_env cenv = 
	Hashtbl.iter (fun x y -> print_desc_class y) cenv

let print_method_env menv = 
	Hashtbl.iter (fun x y -> print_endline x) menv
(* 
let eval_env env f
	Hashtbl.iter f env *)

let make_env c m o v =
	{
		env_c = c;
		env_m = m;
		env_o = o;
		env_v = v;
	}

let make_dclass p s attr m= 
	{
		d_pname       = p;
		d_cname       = s;
		d_cattributes = attr;
		d_cmethods    = m;
	}

let make_dmethod s a b = 
	{
		d_mname = s;
		d_margs = a;
		d_mbody = b;
	}

let make_dobject c attr =
	{
		d_oclass = c;
		d_oatttr = attr;
	}

let add_var vname vval env =
	let cp_env = Hashtbl.copy env.env_v in
	Hashtbl.add cp_env vname vval;
	make_env env.env_c env.env_m env.env_o cp_env

let make_fun_env i objatt argl vall env = 
	let cp_env = Hashtbl.copy env.env_v in
	
	(* Adding method arguments to the local variable environment *)
	let rec add_arg_l argl vall v_env= match (argl,vall) with
		|([],[]) -> ()
		|(t::r,h::q) -> Hashtbl.add v_env (fst(t)) h; add_arg_l r q v_env
	in add_arg_l argl vall cp_env;
	
	(*  Adding object attributes to the local variable environment *)
	Hashtbl.iter (fun x y -> Hashtbl.add cp_env x y) objatt;

	(* Adding a pointer to the object *)
	Hashtbl.add cp_env "this" (AST.Object i);

	make_env env.env_c env.env_m env.env_o cp_env

	
let get_func s cname env = 
	try
		let dmethod = Hashtbl.find env.env_m (cname^"."^s) in (dmethod.d_mbody,dmethod.d_margs)
	with
	| Not_found -> compiler_default_error ""

let get_obj_desc env = Hashtbl.find env.env_o

let get_class_desc env = Hashtbl.find env.env_c

let get_var s env = 
	try
		Hashtbl.find env.env_v s
	with
	| Not_found -> compiler_default_error ""


let rec arglist_to_stringlist argl = match argl with
	|[] -> []
	|(t::q) -> let (s,typ)=t in s::(arglist_to_stringlist q)

let rec add_method meth m_env= 
	let dmethod = make_dmethod (meth.mname) meth.margstype (meth.mbody) in
		Hashtbl.add m_env meth.mname dmethod
		(* Hashtbl.add (env.env_m) (cname^"."^meth.mname) dmethod *)

and add_method_l meth_l m_env= match meth_l with
	|[] 		-> m_env
	|(t::q) 	-> add_method t m_env; add_method_l q m_env


let rec add_attr_l att_l  att_env = match att_l with
	| [] 		-> att_env
	| (t::q) 	-> add_attr t att_env ; add_attr_l q att_env

and add_attr attr att_env = match attr.adefault with
	| Some e 	-> Hashtbl.add att_env attr.aname (Some e)
	| None 		-> Hashtbl.add att_env attr.aname None
	


let rec retrieve_herited_att c att_env env = 
	let add_attenv=c.d_cattributes in 
		Hashtbl.iter (fun x y -> Hashtbl.add att_env x y) add_attenv;
		try
			let p= get_class_desc env c.d_pname in
			retrieve_herited_att p att_env env		
		with
		| Not_found -> att_env



let rec retrieve_herited_meth c meth_env env = 
	let add_methenv = c.d_cmethods in 
	Hashtbl.iter (fun x y -> Hashtbl.add meth_env x y) add_methenv;
		try
			let p= get_class_desc env c.d_pname in
			retrieve_herited_meth p meth_env env		
		with
		| Not_found -> meth_env


let rec add_class_l class_l env = match class_l with
	|[] -> ()
	|(t::q) -> add_class t env; add_class_l q env

and add_class cl env = 
	(* Class environment *)
	(* Retrieving all attributes *)
	try
		let cl_desc= get_class_desc env (Type.stringOf(elem_of cl.cparent)) in
		(* Retrieving herited attributes *)
		let desc_att = retrieve_herited_att cl_desc (Hashtbl.create 17) env in
		
		(* Adding current class attributes *)
		let all_desc_att = add_attr_l cl.cattributes desc_att in


		(* Retrieving herited method *)
		let desc_method = retrieve_herited_meth cl_desc (Hashtbl.create 17) env in

		(* Adding current class methods *)
		let all_desc_meth = add_method_l (cl.cmethods) desc_method in

		(* Class descriptor *)
		let desc_class = make_dclass (Type.stringOf(elem_of cl.cparent)) cl.cname all_desc_att all_desc_meth in
		Hashtbl.add env.env_c cl.cname desc_class;

		(* Method environment *)
		Hashtbl.iter (fun x y -> Hashtbl.add env.env_m (cl.cname^"."^x) y) all_desc_meth
	with
		| Not_found -> 	begin
							(* Adding current class attributes *)
							let all_desc_att = add_attr_l cl.cattributes (Hashtbl.create 17) in
							let all_desc_meth = add_method_l (cl.cmethods) (Hashtbl.create 17) in

							(* Class descriptor *)
							let desc_class = make_dclass (Type.stringOf(elem_of cl.cparent)) cl.cname all_desc_att all_desc_meth in
							Hashtbl.add env.env_c cl.cname desc_class;

							(* Method environment *)
							Hashtbl.iter (fun x y -> Hashtbl.add env.env_m (cl.cname^"."^x) y) all_desc_meth;

						end
	

	



let set_var varname varval env = 
	Hashtbl.replace env.env_v varname varval;
	
	(* Is it an object attribute ? *)
	try
		(* Getting the object id *)
		let (Object i)=Hashtbl.find env.env_v "this" in 
		(* Getting the object descriptor *)
		let desc_obj = get_obj_desc env i in
		(* Trying to replace the object attribute with the new val *)
		Hashtbl.replace desc_obj.d_oatttr varname varval

	with
	| Not_found -> () (* Not an object attribute *)


let rec findclass_l s cl = match cl with
	|[] 					-> class_not_found s
	|(t::q) when t.cname=s 	-> t
	|(t::q)					-> findclass_l s q



let compile cl classtree= 
	let init_env = make_env (Hashtbl.create 17) (Hashtbl.create 17) (Hashtbl.create 17) (Hashtbl.create 17) in 
	
	(* add_class_l cl init_env;  *)
	let rec compile_tree ct= match ct with
		|Node("Object",l) 	-> compile_tree_l l
		|Node("String",l) 	-> compile_tree_l l
		|Node("Int",l) 		-> compile_tree_l l
		|Node("Null",l) 	-> compile_tree_l l
		|Node("Bool",l) 	-> compile_tree_l l
		|Node(s,l)  		-> let c=findclass_l s cl in add_class c init_env; compile_tree_l l
	and compile_tree_l ctl = match ctl with
		|[] 	-> ()
		|(t::q) -> compile_tree t; compile_tree_l q
	in compile_tree classtree;


	(* print_class_env init_env.env_c; *)
	(* print_method_env init_env.env_m; *)
	init_env
	