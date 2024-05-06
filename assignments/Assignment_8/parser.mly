%{
    open Prolog;;
%}

%token <string>CONSTANT VARIABLE 
%token <int> NUM
%token EOF 
%token LESSTHAN GREATERTHAN  NOT_EQUAL EQUAL 
%token DIVIDE MUTLIPLY SUBTRACT ADD 
%token PIPE COND CUT ENDL COMMA RBRACKET LBRACKET RPAREN LPAREN 

%nonassoc ENDL
%left DIVIDE MUTLIPLY
%left SUBTRACT ADD 
%nonassoc GREATERTHAN LESSTHAN PIPE EQUAL
%left COMMA


%type <Prolog.goal> goal
%type <Prolog.program> program
%start program goal
%%

program:
  | list_clause EOF                     {$1}
  |  EOF                                 {[]}
;

list_clause:
  | clause list_clause                  {($1)::$2}
  |  clause                              {[$1]}
;

clause:
  | atom COND list_atom ENDL            {R(H($1), B($3))}
  |  atom ENDL                           {F(H($1))}
;

goal:
    list_atom ENDL                      {G($1)}
;

list_atom:
  | atom COMMA list_atom                {($1)::$3}
  |  atom                                {[$1]}
;

atom:
  | CUT                                 {A("_cut", [])}
  | term GREATERTHAN term                        {A(">", [$1; $3])}
  | term LESSTHAN term                        {A("<", [$1; $3])}
  | term NOT_EQUAL term                    {A("_not_eq", [$1; $3])}
  | term EQUAL term                        {A("_eq", [$1; $3])}
  | CONSTANT LPAREN list_term RPAREN                {A($1, $3)}
  | CONSTANT                                {A($1, [])}
;

list_term:
  | term COMMA list_term                {($1)::$3}
  | term                                {[$1]}
;

term:
  | list                                {$1}
  | term DIVIDE term                       {Node("/", [$1; $3])}
  | term MUTLIPLY term                      {Node("*", [$1; $3])}
  | term SUBTRACT term                     {Node("-", [$1; $3])}
  | term ADD term                      {Node("+", [$1; $3])}
  | CONSTANT LPAREN list_term RPAREN                {Node($1, $3)}
  | NUM                                 {Num($1)}
  | CONSTANT                                {Node($1, [])}
  | VARIABLE                                 {V($1)}
  |  LPAREN term RPAREN                          {$2}
;

list:
  | LBRACKET list_body RBRACKET                     {$2}
  |  LBRACKET RBRACKET                               {Node("_empty_list", [])}
;

list_body:
  | term PIPE term                       {Node("_list", [$1; $3])}
  | term COMMA list_body                 {Node("_list", [$1; $3])}
  | term                                 {Node("_list", [$1; Node("_empty_list", [])])}
;
