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

%token EOF


// Need to specify the associativity and precedence of the operators
// In is used in the Let expression Let x : type_expr = e1 in e2 -> we need to evaluate e2 first before reducing e1
%nonassoc IN

// Assign x:= expr -> We want to make sure that expression is evaluated first i.e. shifted onto the stack
%right ASSIGN

// BIDMAS -> reduce Plus/Minus first and then Multiply/Divide
%left PLUS MINUS
%left MULTIPLY DIVIDE
%left LT GT LTE GTE EQUALITY
%left AND OR NOT


// Start symbol
%start program

// Types for the productions
%type <expr> expr
%type <Parsed_ast.program> program
%type <bin_op> bin_op
%type <comp_op> comp_op
%type <bool_op> bool_comp_op
%type <unary_op> unary_op
%type <type_expr> type_expression
%type <argument> arg
%type <function_defn> function_defn

%%
// Grammar Productions

%inline bin_op:
| PLUS { BinOpPlus }
| MINUS { BinOpMinus }
| MULTIPLY { BinOpMultiply }
| DIVIDE { BinOpDivide } 

%inline comp_op:
| LT { CompOpLessThan }
| GT { CompOpGreaterThan }
| LTE { CompOpLessThanEqual }
| GTE { CompOpGreaterThanEqual}
| EQUALITY { CompOpEqual }

%inline bool_comp_op:
| AND { BoolOpAnd }
| OR { BoolOpOr }

%inline unary_op:
| NOT { UnaryOpNot }

type_expression:
| LEFT_PAREN TYPE_INT COMMA HIGH_SEC_LEVEL RIGHT_PAREN {(TEInt, TSHigh)}
| LEFT_PAREN TYPE_INT COMMA LOW_SEC_LEVEL RIGHT_PAREN {(TEInt, TSLow)}
| LEFT_PAREN TYPE_BOOL COMMA HIGH_SEC_LEVEL RIGHT_PAREN {(TEBool, TSHigh)}
| LEFT_PAREN TYPE_BOOL COMMA LOW_SEC_LEVEL RIGHT_PAREN {(TEBool, TSLow)}

// x: int, sec_level
arg:
| var=IDENTIFIER COLON t = type_expression {TArg(var, t)}

// fn example(x: (int, sec_level), y:(int,sec_level)) : (int, sec_level) {e} ;
function_defn:
| FN f=IDENTIFIER LEFT_PAREN args=separated_list(COMMA,arg) RIGHT_PAREN COLON t=type_expression LEFT_BRACE e=expr RIGHT_BRACE SEMICOLON {FunctionDefn(f, args, t, e)}


expr:
| i=INT {Integer($startpos, i, TSLow)}
| TRUE {Boolean($startpos, true, TSLow)}
| FALSE {Boolean($startpos, false, TSLow)}
| id=IDENTIFIER {Identifier($startpos, id)}
(* binary ops *)
| e1=expr op=bin_op e2=expr {BinOp($startpos, op, e1, e2)}
| e1=expr op=comp_op e2=expr {CompOp($startpos, op, e1, e2)}
| e1=expr op=bool_comp_op e2=expr {BoolOp($startpos, op, e1, e2)}
(*  unary op *)
| NOT e=expr {UnaryOp($startpos, UnaryOpNot, e)}
| LET x=IDENTIFIER COLON t=type_expression EQUAL e1=expr IN e2=expr {Let($startpos, x, t, e1, e2) }
| x=IDENTIFIER ASSIGN e=expr {Assign($startpos, x, e)}
| CLASSIFY LEFT_PAREN e=expr RIGHT_PAREN {Classify($startpos, e)}
| DECLASSIFY LEFT_PAREN e=expr RIGHT_PAREN {Declassify($startpos, e)}
| LEFT_PAREN e=expr RIGHT_PAREN {e}
| IF LEFT_PAREN e1=expr RIGHT_PAREN THEN LEFT_BRACE e2=expr RIGHT_BRACE ELSE LEFT_BRACE e3=expr RIGHT_BRACE {If($startpos, e1, e2, e3)}
| WHILE LEFT_PAREN e1=expr RIGHT_PAREN LEFT_BRACE e2=expr RIGHT_BRACE {While($startpos, e1, e2)}
| FOR LEFT_PAREN LET x=IDENTIFIER COLON t=type_expression EQUAL e1=expr SEMICOLON e2=expr SEMICOLON _e3=expr RIGHT_PAREN LEFT_BRACE e4=expr RIGHT_BRACE {
    Let($startpos, x, t, e1, (While($startpos, e2, e4))) (*need to change to Seq(e3, e4) *)
}
// TODO  seq e1; e2
// function application
| id=IDENTIFIER LEFT_PAREN args=separated_list(COMMA, expr) RIGHT_PAREN {FunctionApp($startpos, id, args)}



program:
f = function_defn* e=expr; EOF {Prog(f,e)}
