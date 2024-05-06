%{
  open Ast
  open Printf
%}

%token END COMMA EQU NOTEQ LP RP RSQ LSQ NOT OR SEP EOF UNRECOGNIZED
%token <string> V C P
%start program
%type <Ast.program> program
%type <Ast.clause> clause
%type <Ast.head> head
%type <Ast.body> body
%type <Ast.term> term

%%

program:
  | clause END   { printf "Parsed program: %s\n" (Ast.string_of_program [$1]); [$1] }
  | program clause END { let p = $2 :: $1 in printf "Parsed program: %s\n" (Ast.string_of_program p); p }

clause:
  | head              { Fact $1 }
  | head SEP body END { let r = Rule ($1, $3) in printf "Parsed clause: %s\n" (Ast.string_of_clause r); r }

body:
  | atomic_formula         { [$1] }
  | atomic_formula COMMA body { let b = $1 :: $3 in printf "Parsed body: %s\n" (Ast.string_of_body b); b }

atomic_formula:
  | P LP term_list RP      { let af = A ($1, $3) in printf "Parsed atomic formula: %s\n" (Ast.string_of_atomic_formula af); af }

term_list:
  | term                  { [$1] }
  | term COMMA term_list  { let tl = $1 :: $3 in printf "Parsed term list: %s\n" (Ast.string_of_term_list tl); tl }

term:
  | V                     { let v = V $1 in printf "Parsed variable: %s\n" (Ast.string_of_term v); v }
  | C                     { let c = C $1 in printf "Parsed constant: %s\n" (Ast.string_of_term c); c }
  | P LP term_list RP     { A ($1, $3) }

head:
  | atomic_formula         { $1 }
