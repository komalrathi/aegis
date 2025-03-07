%{
    open Compiler_types.Ast_types 
    open Parsed_ast
    open Compiler_types.Language_types
%}

%token <int> INT
%token <string> IDENTIFIER

%token PLUS
%token MINUS
%token MULTIPLY
%token DIVIDE
%token EXPONETIATION
%token MODULUS

%token CLASSIFY
%token DECLASSIFY

%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACE
%token RIGHT_BRACE
%token COMMA

%token FN

%token LT
%token GT
%token LTE
%token GTE
%token EQUALITY

%token TRUE
%token FALSE
%token AND
%token OR
%token NOT

%token IF
%token THEN
%token ELSE
%token ASSIGN
%token LET
%token IN
%token COLON
%token SEMICOLON

%token FOR
%token WHILE

%token TYPE_INT
%token TYPE_BOOL

%token HIGH_SEC_LEVEL
%token LOW_SEC_LEVEL

%token EQUAL
%token PRINT
%token SECUREPRINT

%token CLASS
%token CONSTRUCTOR
%token NEW
%token DOT

%token DIVISION_BY_ZERO
%token INTEGER_OVERFLOW

%token RAISE
%token RESUMABLE_RAISE
%token TRY
%token CATCH
%token FINALLY

%token EOF


// Need to specify the associativity and precedence of the operators
%right ASSIGN
%left OR
%left AND
%left EQUALITY LTE LT GTE GT
%left PLUS MINUS
%left MULTIPLY DIVIDE
%left EXPONETIATION MODULUS
%right NOT


%start program

%type <expr> expr
%type <Parsed_ast.program> program
%type <bin_op> bin_op
%type <comp_op> comp_op
%type <bool_op> bool_comp_op
%type <unary_op> unary_op
%type <type_expr> type_expression
%type <argument> arg
%type <function_defn> function_defn
%type <field_defn> field_defn
%type <constructor> constructor
// %type <method_defn> method_defn
%type <class_defn> class_defn
%type <expr> block_expr

%%
// Grammar Productions
%inline unary_op:
| NOT { UnaryOpNot }

%inline bin_op:
| PLUS { BinOpPlus }
| MINUS { BinOpMinus }
| MULTIPLY { BinOpMultiply }
| DIVIDE { BinOpDivide }
| EXPONETIATION {BinOpExponentiation}
| MODULUS {BinOpModulus}

%inline comp_op:
| LT { CompOpLessThan }
| GT { CompOpGreaterThan }
| LTE { CompOpLessThanEqual }
| GTE { CompOpGreaterThanEqual }
| EQUALITY { CompOpEqual }

%inline bool_comp_op:
| AND { BoolOpAnd }
| OR { BoolOpOr }

exception_type:
| DIVISION_BY_ZERO {DivisionByZero}
| INTEGER_OVERFLOW {IntegerOverflow}

core_type:
| TYPE_INT {TEInt}
| TYPE_BOOL {TEBool}
| obj=IDENTIFIER {TEObject(obj)}

sec_level:
| HIGH_SEC_LEVEL {TSHigh}
| LOW_SEC_LEVEL {TSLow}

type_expression:
| LEFT_PAREN core_type=core_type COMMA sec_level=sec_level RIGHT_PAREN {(core_type, sec_level)}

// x: int, sec_level
arg:
| var=IDENTIFIER COLON t = type_expression {TArg(var, t)}

// fn example(x: (int, sec_level), y:(int,sec_level)) : (int, sec_level) {e} ;
function_defn:
| FN f=IDENTIFIER LEFT_PAREN args=separated_list(COMMA,arg) RIGHT_PAREN COLON t=type_expression LEFT_BRACE e=block_expr RIGHT_BRACE {FunctionDefn(f, args, t, e)}

field_defn:
| var=IDENTIFIER COLON t=type_expression SEMICOLON{FieldDefn(var, t)}

// constructor example(x:(int, sec_level), y:(int,sec_level)) {e} ;
constructor:
| CONSTRUCTOR LEFT_PAREN args=separated_list(COMMA,arg) RIGHT_PAREN LEFT_BRACE e=block_expr RIGHT_BRACE {Constructor(args, e)}

// method_defn:
// | s=sec_level f=function_defn {MethodDefn(s, f)}

class_defn:
| CLASS c=IDENTIFIER LEFT_BRACE fields=list(field_defn) constructor=constructor methods=list(function_defn) RIGHT_BRACE {ClassDefn(c, fields, constructor, methods)}

block_expr:
| e1=expr SEMICOLON e2=block_expr {Seq($startpos, e1, e2)}
| e=expr {e}

expr:
| i=INT {Integer($startpos, i, TSLow)}
| TRUE {Boolean($startpos, true, TSLow)}
| FALSE {Boolean($startpos, false, TSLow)}
| id=IDENTIFIER {Identifier($startpos, id)}
| LEFT_PAREN e=expr RIGHT_PAREN {e}
(* binary ops *)
| e1=expr op=bin_op e2=expr {BinOp($startpos, op, e1, e2)}
| e1=expr op=comp_op e2=expr {CompOp($startpos, op, e1, e2)}
| e1=expr op=bool_comp_op e2=expr {BoolOp($startpos, op, e1, e2)}
(*  unary op *)
| u=unary_op e=expr {UnaryOp($startpos, u, e)}
// object creation
// new High ExampleClass(x, y)
| NEW s=sec_level c=IDENTIFIER LEFT_PAREN args=separated_list(COMMA, expr) RIGHT_PAREN {Object($startpos, s, c, args)}
| LET x=IDENTIFIER COLON t=type_expression EQUAL e1=expr IN LEFT_BRACE e2=block_expr RIGHT_BRACE {Let($startpos, x, t, e1, e2) }
| x=IDENTIFIER ASSIGN e=expr {Assign($startpos, x, e)}
// function application
| id=IDENTIFIER LEFT_PAREN args=separated_list(COMMA, expr) RIGHT_PAREN {FunctionApp($startpos, id, args)}
// exampleObject.method(x, y)
| obj=IDENTIFIER DOT method_name=IDENTIFIER LEFT_PAREN args=separated_list(COMMA, expr) RIGHT_PAREN {MethodCall($startpos, obj, method_name, args)}

| IF LEFT_PAREN e1=expr RIGHT_PAREN THEN LEFT_BRACE e2=block_expr RIGHT_BRACE ELSE LEFT_BRACE e3=block_expr RIGHT_BRACE {If($startpos, e1, e2, e3)}
| WHILE LEFT_PAREN e1=expr RIGHT_PAREN LEFT_BRACE e2=block_expr RIGHT_BRACE {While($startpos, e1, e2)}
| FOR LEFT_PAREN LET x=IDENTIFIER COLON t=type_expression EQUAL e1=expr SEMICOLON e2=expr SEMICOLON e3=expr RIGHT_PAREN LEFT_BRACE e4=block_expr RIGHT_BRACE {
    Let($startpos, x, t, e1, (While($startpos, e2, Seq($startpos,e4,e3))))
}

| CLASSIFY LEFT_PAREN e=expr RIGHT_PAREN {Classify($startpos, e)}
| DECLASSIFY LEFT_PAREN e=expr RIGHT_PAREN {Declassify($startpos, e)}
| PRINT LEFT_PAREN args=separated_list(COMMA, expr) RIGHT_PAREN {Print($startpos, args)}
| SECUREPRINT LEFT_PAREN args=separated_list(COMMA, expr) RIGHT_PAREN {SecurePrint($startpos, args)} 
// exception handling
| RAISE LEFT_PAREN exception_name=exception_type var=IDENTIFIER RIGHT_PAREN{Raise($startpos, exception_name, var)}
| RESUMABLE_RAISE LEFT_PAREN exception_name=exception_type var=IDENTIFIER RIGHT_PAREN{ResumableRaise($startpos, exception_name, var)}
| TRY LEFT_BRACE e1=block_expr RIGHT_BRACE CATCH LEFT_PAREN exception_name=exception_type var=IDENTIFIER RIGHT_PAREN LEFT_BRACE e2=block_expr RIGHT_BRACE FINALLY LEFT_BRACE e3=block_expr RIGHT_BRACE {TryCatchFinally($startpos, e1, exception_name, var, e2, e3)}


program:
c=class_defn* f=function_defn* e=block_expr; EOF {Prog(c, f, e)}
