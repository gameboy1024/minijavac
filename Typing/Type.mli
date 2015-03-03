type t

(* conversions between types and string *)
val stringOf : t -> string
val fromString : string -> t

exception ClassAlreadyPresent of string
exception MethodAlreadyPresent of string
type envVariables
type envClasses
type tEnv
type tClasse
type tArg = t * string
type tFun = { fargs : tArg list ; freturn : t }
val getSuper : tClasse -> t
val makeEnv : envVariables -> envClasses -> tEnv
val initialEnv : unit -> tEnv

val isVar : tEnv -> string -> bool
val findVar : tEnv -> string -> t
val addVar : tEnv -> string -> t -> tEnv

val isClass : tEnv -> string -> bool
val findClass : tEnv -> string -> tClasse
val addClass : tEnv -> string -> tEnv

val isFun : tEnv -> string -> string -> bool
val isFun_rec : tEnv -> string -> string -> bool
val findFun : tEnv -> string -> string -> tFun
val findFun_rec : tEnv -> string -> string -> tFun
val addFun : tEnv -> string -> string -> tFun -> tEnv

val setSuper : tEnv -> string -> string -> tEnv