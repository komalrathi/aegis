open Compiler_types.Ast_types
open Compiler_types.Language_types

type expr =
  | Integer of loc * int * security_level_type
  | BinOp of loc * bin_op * expr * expr
  | CompOp of loc * comp_op * expr * expr
  | Boolean of loc * bool * security_level_type
  | BoolOp of loc * bool_op * expr * expr
  | UnaryOp of loc * unary_op * expr
  | Identifier of loc * identifier
  | Assign of loc * identifier * expr
  | Let of loc * identifier * type_expr * expr * expr
  | If of loc * expr * expr * expr
  | Classify of loc * expr
  | Declassify of loc * expr
  | FunctionApp of loc * identifier * expr list
  | While of loc * expr * expr
  | Seq of loc * expr * expr
  | Print of loc * expr list
  | SecurePrint of loc * expr list
  | Skip of loc
  | Object of loc * security_level_type * identifier * expr list
  | MethodCall of loc * identifier * identifier * expr list
  | Raise of loc * exception_type * identifier
  | TryCatchFinally of loc * expr * exception_type * identifier * expr * expr

type function_defn =
  | FunctionDefn of identifier * argument list * type_expr * expr

type field_defn = FieldDefn of identifier * type_expr

type constructor = Constructor of argument list * expr

type method_defn = MethodDefn of security_level_type * function_defn

type class_defn =
  | ClassDefn of
      identifier * field_defn list * constructor * method_defn list

type program = Prog of class_defn list * function_defn list * expr
