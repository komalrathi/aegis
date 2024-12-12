type core_type = TEInt | TEBool | TFunction of core_type list * core_type

type security_level_type = TSLow | TSHigh

type type_expr = core_type * security_level_type

type interpreter_val = VInt of int | VBool of bool
