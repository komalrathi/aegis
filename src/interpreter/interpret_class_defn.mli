open Core
open Typing.Typed_ast
open Compiler_types.Ast_types
open Compiler_types.Language_types

type function_environment = (identifier * (identifier list * expr)) list

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
