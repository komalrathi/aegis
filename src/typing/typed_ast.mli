open Compiler_types.Ast_types
open Compiler_types.Language_types

type expr =
  | Integer of loc * int * security_level_type
  | BinOp of loc * type_expr * bin_op * expr * expr
  | CompOp of loc * type_expr * comp_op * expr * expr
  | Boolean of loc * bool * security_level_type
  | BoolOp of loc * type_expr * bool_op * expr * expr
  | UnaryOp of loc * type_expr * unary_op * expr
  | Identifier of loc * type_expr * identifier
  | Assign of loc * type_expr * identifier * expr
  | Let of loc * identifier * type_expr * expr * expr * type_expr
  | If of loc * expr * expr * expr * type_expr
  | Classify of loc * expr * type_expr
  | Declassify of loc * expr * type_expr
  | FunctionApp of loc * type_expr * identifier * expr list
  | While of loc * expr * expr * type_expr
  | Seq of loc * expr * expr * type_expr
  (* do not need type_expr for print as it is always of type unit *)
  | Print of loc * expr list
  | SecurePrint of loc * expr list
  | Skip of loc
  | Object of loc * security_level_type * identifier * expr list * type_expr
  | MethodCall of loc * type_expr * identifier * identifier * expr list
  | Raise of loc * exception_type * identifier * type_expr
  | TryCatchFinally of
      loc * expr * exception_type * identifier * expr * expr * type_expr

type function_defn =
  | FunctionDefn of identifier * argument list * type_expr * expr

type field_defn = FieldDefn of identifier * type_expr

type constructor = Constructor of argument list * expr

type class_defn =
  | ClassDefn of
      identifier * field_defn list * constructor * function_defn list

type program = Prog of class_defn list * function_defn list * expr
