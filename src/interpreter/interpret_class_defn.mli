open Core
open Typing.Typed_ast
open Compiler_types.Ast_types
open Compiler_types.Language_types

type class_info =
  { fields: identifier list
  ; constructor: identifier list * expr
  ; methods:
      ( security_level_type
      * identifier (* function_name *)
      * identifier list (* arguments *)
      * expr (* body *) )
      list }

type class_environment = (identifier * class_info) list

val interpret_class_defns : class_defn list -> class_environment Or_error.t

val get_class_info : identifier -> class_environment -> class_info option

val get_method_info :
     identifier
  -> (security_level_type * identifier * identifier list * expr) list
  -> (security_level_type * identifier list * expr) Or_error.t
