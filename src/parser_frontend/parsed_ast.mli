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

(* A function definition is a function name, a list of arguments, the return
   type of the function, and the expression that is the body of the
   function. *)
type function_defn =
  | FunctionDefn of identifier * argument list * type_expr * expr

(* A field definition is a field name and a type *)
type field_defn = FieldDefn of identifier * type_expr

(* A constructor definition is a list of arguments and an expression that is
   the body of the constructor. *)
type constructor = Constructor of argument list * expr

(* A method definition is a security level and a function definition. *)
type method_defn = MethodDefn of security_level_type * function_defn

(* A class definition is a class name, a list of field definitions, a
   constructor definition, and a list of method definitions. *)
type class_defn =
  | ClassDefn of
      identifier * field_defn list * constructor * method_defn list

(* A program now defines all the classes, functions, and the main
   expression. *)
type program = Prog of class_defn list * function_defn list * expr
