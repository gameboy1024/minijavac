type t

(* conversions between types and string *)
val stringOf : t -> string
val fromString : string -> t

exception ClassAlreadyExists of string
exception MethodAlreadyExists of string
type envVariables
type envClasses
type tEnv
type tClass
type tArg = t * string
type tMethod = { fargs : tArg list ; freturn : t ; fstatic : bool }
val makeEnv : envVariables -> envClasses -> tEnv
val init : unit -> tEnv

val isVar : tEnv -> string -> bool
val findVar : tEnv -> string -> t
val addVar : tEnv -> string -> t -> tEnv

val isClass : tEnv -> string -> bool
val findClass : tEnv -> string -> tClass
val addClass : tEnv -> string -> tEnv

val isMethod : tEnv -> string -> string -> bool
val isMethod_rec : tEnv -> string -> string -> bool
val findMethod : tEnv -> string -> string -> tMethod
val findMethod_rec : tEnv -> string -> string -> tMethod
val addMethod : tEnv -> string -> string -> tMethod -> tEnv

val setParent : tEnv -> string -> string -> tEnv
val getParent : tClass -> t
