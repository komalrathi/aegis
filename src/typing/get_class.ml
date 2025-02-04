open Parser_frontend

let get_class class_name class_defns =
  Core.List.find
    ~f:(fun (Parsed_ast.ClassDefn (c_name, _, _, _)) ->
      String.equal c_name class_name )
    class_defns

let get_method method_name method_defns =
  Core.List.find
    ~f:(fun
        (Parsed_ast.MethodDefn (_, Parsed_ast.FunctionDefn (fn_name, _, _, _))
          )
      -> String.equal fn_name method_name )
    method_defns
