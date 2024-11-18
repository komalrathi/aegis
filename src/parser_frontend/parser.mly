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

%token FUNCTION
%token ARROW

%token LEFT_PAREN
%token RIGHT_PAREN
%token COMMA

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

%token TYPE_INT
%token TYPE_BOOL

%token HIGH_SEC_LEVEL
%token LOW_SEC_LEVEL

%token EQUAL

%token EOF


// Need to specify the associativity and precedence of the operators
%right EQUAL COLON ASSIGN IN

%left PLUS MINUS
%left MULTIPLY DIVIDE
%left LT GT LTE GTE
%left AND OR

// Start symbol
%start program

// Types for the productions
%type <expr> expr
%type <Parsed_ast.expr> program
%type <bin_op> bin_op
%type <comp_op> comp_op
%type <bool_comp_op> bool_comp_op
%type <type_expr> type_expression

%%
// Grammar Productions

%inline bin_op:
| PLUS { PLUS }
| MINUS { MINUS }
| MULTIPLY { MULTIPLY }
| DIVIDE { DIVIDE } 

%inline comp_op:
| LT { LT }
| GT { GT }
| LTE { LTE }
| GTE { GTE }

%inline bool_comp_op:
| AND { AND }
| OR { OR }

type_expression:
| LEFT_PAREN TYPE_INT COMMA HIGH_SEC_LEVEL RIGHT_PAREN {(TEInt, TSHigh)}
| LEFT_PAREN TYPE_INT COMMA LOW_SEC_LEVEL RIGHT_PAREN {(TEInt, TSLow)}
| LEFT_PAREN TYPE_BOOL COMMA HIGH_SEC_LEVEL RIGHT_PAREN {(TEBool, TSHigh)}
| LEFT_PAREN TYPE_BOOL COMMA LOW_SEC_LEVEL RIGHT_PAREN {(TEBool, TSLow)}


program:
| e=expr; EOF {e}

expr:
| i=INT {Integer($startpos, i, TSLow)}
| e1=expr op=bin_op e2=expr {BinOp($startpos, op, e1, e2)}
| e1=expr op=comp_op e2=expr {CompOp($startpos, op, e1, e2)}
| TRUE {Boolean($startpos, true, TSLow)}
| FALSE {Boolean($startpos, false, TSLow)}
| e1=expr op=bool_comp_op e2=expr {BoolCompOp($startpos, op, e1, e2)}
| id=IDENTIFIER {Identifier($startpos, id)}
| LET x=IDENTIFIER COLON t=type_expression EQUAL e1=expr IN e2=expr {Let($startpos, x, t, e1, e2) }
| x=IDENTIFIER ASSIGN e=expr {Assign($startpos, x, e)}
| IF e1=expr THEN e2=expr ELSE e3=expr {If($startpos, e1, e2, e3)}
| CLASSIFY e=expr {Classify($startpos, e)}
| DECLASSIFY e=expr {Declassify($startpos, e)}
| LEFT_PAREN e=expr RIGHT_PAREN {e}