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
%token ARROW

%token LT
%token GT
%token LTE
%token GTE

%token TRUE
%token FALSE
%token AND
%token OR

%token IF
%token THEN
%token ELSE
%token ASSIGN
%token LET
%token IN
%token COLON
%token SEMICOLON

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
%left LT GT LTE GTE
%left AND OR


// Start symbol
%start program

// Types for the productions
%type <expr> expr
%type <Parsed_ast.program> program
%type <bin_op> bin_op
%type <comp_op> comp_op
%type <bool_op> bool_comp_op
%type <type_expr> type_expression
%type <argument> arg
%type <argument list> args
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

%inline bool_comp_op:
| AND { BoolOpAnd }
| OR { BoolOpOr }

type_expression:
| LEFT_PAREN TYPE_INT COMMA HIGH_SEC_LEVEL RIGHT_PAREN {(TEInt, TSHigh)}
| LEFT_PAREN TYPE_INT COMMA LOW_SEC_LEVEL RIGHT_PAREN {(TEInt, TSLow)}
| LEFT_PAREN TYPE_BOOL COMMA HIGH_SEC_LEVEL RIGHT_PAREN {(TEBool, TSHigh)}
| LEFT_PAREN TYPE_BOOL COMMA LOW_SEC_LEVEL RIGHT_PAREN {(TEBool, TSLow)}

// x: int, sec_level
arg:
| var=IDENTIFIER COLON t = type_expression {TArg(var, t)}
// (x: (int, sec_level), y:(int,sec_level))
args:
| LEFT_PAREN args=separated_list(COMMA,arg) RIGHT_PAREN {args}

// fn example(x: (int, sec_level), y:(int,sec_level)) : (int, sec_level) -> e ;
function_defn:
| FN f=IDENTIFIER args=args COLON t=type_expression ARROW e=expr SEMICOLON {FunctionDefn(f, args, t, e)}

expr:
| i=INT {Integer($startpos, i, TSLow)}
| TRUE {Boolean($startpos, true, TSLow)}
| FALSE {Boolean($startpos, false, TSLow)}
| e1=expr op=bin_op e2=expr {BinOp($startpos, op, e1, e2)}
| e1=expr op=comp_op e2=expr {CompOp($startpos, op, e1, e2)}
| e1=expr op=bool_comp_op e2=expr {BoolOp($startpos, op, e1, e2)}
| id=IDENTIFIER {Identifier($startpos, id)}
| LET x=IDENTIFIER COLON t=type_expression EQUAL e1=expr IN e2=expr {Let($startpos, x, t, e1, e2) }
// should assign have type_expr instead of expr? cannot see where the expr is annotated with the security level
| x=IDENTIFIER ASSIGN e=expr {Assign($startpos, x, e)}
| IF LEFT_PAREN e1=expr RIGHT_PAREN THEN LEFT_BRACE e2=expr RIGHT_BRACE ELSE LEFT_BRACE e3=expr RIGHT_BRACE {If($startpos, e1, e2, e3)}
| CLASSIFY LEFT_PAREN e=expr RIGHT_PAREN {Classify($startpos, e)}
| DECLASSIFY LEFT_PAREN e=expr RIGHT_PAREN {Declassify($startpos, e)}
| LEFT_PAREN e=expr RIGHT_PAREN {e}

program:
f = function_defn* e=expr; EOF {Prog(f,e)}
