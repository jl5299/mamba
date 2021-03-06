/* 
Hi, welcome to our parser
 */
%{ open Ast %}

%token DOT
%token IN COLON COMMA END
%token EQ NEQ LT LEQ GT GEQ AND OR
%token INT BOOL REAL CHAR STRING
%token PLUS MINUS TIMES DIVIDE EQUAL
%token IF THEN ELSE WHILE FOR
%token <int> LITERAL
%token <char> CHARLIT
%token <bool> BOOLLIT
%token <string> STRINGLIT
%token <string> ID
%token EOF

%nonassoc ELSE
%right END

%left COMMA
%right EQUAL


%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left IN
%left PLUS MINUS
%left TIMES DIVIDE
%right NEG
%nonassoc DOT

%start program
%type <Ast.program> program

%%

program:
  stmt_list EOF { List.rev $1 }

simple_typ:
  INT                 { PrimTyp(Int) }
| BOOL                { PrimTyp(Bool) }
| CHAR                { PrimTyp(Char) }
| STRING              { String }

expr:
  ID                    { Id($1) }
| LITERAL               { Lit($1) }
| BOOLLIT               { BoolLit($1) }
| CHARLIT               { CharLit($1) }
| string_lit            { $1 }
| expr PLUS   expr      { Binop($1, Add, $3) }
| expr MINUS  expr      { Binop($1, Sub, $3) }
| expr TIMES  expr      { Binop($1, Mul, $3) }
| expr DIVIDE expr      { Binop($1, Div, $3) }
| MINUS expr %prec NEG  { Uniop(Neg, $2) }
| expr EQ     expr      { Binop($1, Equal, $3) }
| expr NEQ    expr      { Binop($1, Neq,   $3) }
| expr LT     expr      { Binop($1, Less,  $3) }
| expr LEQ    expr      { Binop($1, Leq,   $3) }
| expr GT     expr      { Binop($1, Greater, $3) }
| expr GEQ    expr      { Binop($1, Geq,   $3) }
| expr AND    expr      { Binop($1, And, $3) }
| expr OR     expr      { Binop($1, Or, $3) }