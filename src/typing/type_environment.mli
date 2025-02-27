open Core
open Compiler_types.Language_types
open Compiler_types.Ast_types

type row = (exception_type * security_level_type) list

type type_binding = identifier * type_expr

type type_environment = type_binding list

(* Type_Expr can be None or Some of type_expr so it has option type *)
val lookup_var_type : type_environment -> identifier -> type_expr option

val get_function_types :
  (string * type_expr) list -> string -> (type_expr, Core.Error.t) Result.t
