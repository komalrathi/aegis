open Compiler_types.Ast_types
open Compiler_types.Language_types
open Typed_ast

type class_info =
  { fields: (identifier * type_expr) list
  ; constructor: (identifier * type_expr) list * expr
  ; methods:
      ( security_level_type
      * identifier (* function_name *)
      * (identifier * type_expr) list (* arguments *)
      * expr (* body *) )
      list }

type class_environment = (identifier * class_info) list

val get_class_info : identifier -> class_environment -> class_info option

val get_method_info :
     identifier
  -> (security_level_type * identifier * (identifier * type_expr) list * expr)
     list
  -> ( security_level_type * (identifier * type_expr) list * Typed_ast.expr
     , Core.Error.t )
     Result.t
