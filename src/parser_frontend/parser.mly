%{
    open Ast_types 
    open Parsed_ast
%}

%token <int> INT
%token PLUS
%token MINUS
%token MULTIPLY
%token DIVIDE
%token EOF


// Need to specify the associativity and precedence of the operators
%left PLUS MINUS
%left MULTIPLY DIVIDE

// Start symbol
%start program

// Types for the productions
%type <expr> expr
%type <Parsed_ast.expr> program
%type <bin_op> bin_op

%%
// Grammar Productions

%inline bin_op:
| PLUS { PLUS }
| MINUS { MINUS }
| MULTIPLY { MULTIPLY }
| DIVIDE { DIVIDE } 

program:
| e=expr; EOF {e}

expr:
| i=INT {Integer($startpos, i)}
| e1=expr op=bin_op e2=expr {BinOp($startpos, op, e1, e2)}

