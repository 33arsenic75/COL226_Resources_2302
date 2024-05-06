%{
    open Ast;;
%}

%token <string> VAR CONS
%token <int> NUM
%token LP RP LB RB COMMA EQ NOT_EQ ENDL CUT COND PIPE PLUS MINUS MULT DIV GT LT EOF

%left COMMA
%nonassoc EQ PIPE LT GT
%left PLUS MINUS
%left MULT DIV
%nonassoc ENDL

%start program goal
%type <Ast.program> program
%type <Ast.goal> goal
%%

program:
    EOF                                 {[]}
  | clause_list EOF                     {$1}
;

term:
    LP term RP                          {$2}
  | term PLUS term                      {Node("+", [$1; $3])}
  | term MULT term                      {Node("*", [$1; $3])}
  | VAR                                 {V($1)}
  | CONS                                {Node($1, [])}
  | NUM                                 {Num($1)}
  | CONS LP term_list RP                {Node($1, $3)}
  | term MINUS term                     {Node("-", [$1; $3])}
  | term DIV term                       {Node("/", [$1; $3])}
  | list                                {$1}
;
clause_list:
    clause                              {[$1]}
  | clause clause_list                  {($1)::$2}
;

clause:
    atom ENDL                           {F(H($1))}
  | atom COND atom_list ENDL            {R(H($1), B($3))}
;

list:
    LB RB                               {Node("_empty_list", [])}
  | LB list_body RB                     {$2}
;

list_body:
    term                                 {Node("_list", [$1; Node("_empty_list", [])])}
  | term PIPE term                       {Node("_list", [$1; $3])}
  | term COMMA list_body                 {Node("_list", [$1; $3])}
;

goal:
    atom_list ENDL                      {G($1)}
;


term_list:
    term                                {[$1]}
  | term COMMA term_list                {($1)::$3}
;


atom_list:
    atom                                {[$1]}
  | atom COMMA atom_list                {($1)::$3}
;

atom:
  | CONS                                {A($1, [])}
  | term LT term                        {A("<", [$1; $3])}
  | term GT term                        {A(">", [$1; $3])}
  | CONS LP term_list RP                {A($1, $3)}
  | term EQ term                        {A("_eq", [$1; $3])}
  | term NOT_EQ term                    {A("_not_eq", [$1; $3])}
  | CUT                                 {A("_cut", [])}
;

