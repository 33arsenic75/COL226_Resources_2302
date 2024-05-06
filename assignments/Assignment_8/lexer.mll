{
  open Parser;;
  exception InvalidToken of char ;;
}
let alphanumeric = ['A'-'Z' 'a'-'z' '0'-'9' '_']
let number = '0'|['1'-'9']['0'-'9']*
let space = [' ' '\t' '\n']+
let constant = ['a'-'z'](alphanumeric*) | ("\"" [^ '\"']+ "\"")
let variable = ['A'-'Z'](alphanumeric*)


rule read = parse
        | "/*"                  {multi_line_comment 0 lexbuf}
        | '%'                   {single_line_comment lexbuf}
        | ":-"                  {COND}
        | '.'                   {ENDL}
        | '!'                   {CUT}
        | '|'                   {PIPE}
        | "\\="                 {NOT_EQUAL}
        | '<'                   {LESSTHAN}
        | '>'                   {GREATERTHAN}
        | '/'                   {DIVIDE}
        | '*'                   {MUTLIPLY}
        | '-'                   {SUBTRACT}
        | '+'                   {ADD}
        | '='                   {EQUAL}
        | ','                   {COMMA}
        | ']'                   {RBRACKET}
        | '['                   {LBRACKET}
        | ')'                   {RPAREN}
        | '('                   {LPAREN}
        | number as n           {NUM(int_of_string n)}
        | constant as c             {CONSTANT(c)} 
        | variable as v              {VARIABLE(v)}
        | space                    {read lexbuf}
        |  eof                   {EOF}
        | _ as s                {raise (InvalidToken s)}

and single_line_comment = parse
  | '\n'                  {read lexbuf}
  |  eof                   {EOF}
  |   _                   {single_line_comment lexbuf}

and multi_line_comment depth = parse
  | "/*"                  {multi_line_comment (depth+1) lexbuf}
  | "*/"                  {if depth = 0 then read lexbuf else multi_line_comment (depth-1) lexbuf}
  |  eof                   {failwith "Syntax error: End of file in /* ... */ comment"}
  |  _                    {multi_line_comment depth lexbuf}



{
let test_case1 = "X is 3 + 4 * 2."
let test_case2 = "likes(john, pizza)."
let test_case3 = "% This is a comment."
(* let test_case4 = " This is a
                  multi-line
                  comment" *)
let test_case5 = "student('John Doe', 25)."
let test_case6 = "sum(2, 3, Result) :- Result is 2 + 3."
let test_case7 = "greater_than(5, 3) :- 5 > 3."
let test_case8 = "average(X, Y) :- (X + Y) / 2."
let test_case9 = "list_sum([1, 2, 3, 4], Result) :- sum_list([1, 2, 3, 4], Result)."
let test_case10 = "greet('Hello, world!')."
let test_case11 = "name('John O''Connor')."
let test_case12 = "X is 2 * (Y + 3)."
let test_case13 = "rect_area(Width, Height, Area) :- Area is Width * Height."
let test_case14 = "empty_list([])."
let test_case15 = "predicate_with_underscore(_, Y) :- Y > 0."
let test_case16 = "predicate with space(Argument)."
let test_case17 = "predicate_with_underscore(_) :- true."
let test_case18 = "age(25)."
let test_case19 = "nested_predicate(X, Y) :- (X + Y) / 2 > 10."
let test_case20 = "not_equal(X, Y) :- X = Y."
let test_case21 = "predicate_with_comment(X) :- X > 0. % This is a comment."
let test_case22 = "escaped_single_quote('John O\'Connor')."
let test_case23 = "single_quote('')."
let test_case24 = "_underscore(_)."
let test_case25 = "empty_parentheses()."
let test_case26 = "negative_number(-5)."
let test_case27 = "divide(X, Y, Result) :- Result is X / Y."
let test_case28 = "pi(3.14159)."
let test_case29 = "   spaces(   )."
let test_case30 = "scientific_notation(1.23e-4)."
let test_case31 = "_variable(_)."
let test_case32 = "power_of_2(X) :- X is 2^10."
let test_case33 = "modulus(X) :- X is 10 mod 3."
let test_case34 = "bitwise_and(X) :- X is 5 land 3."
let test_case35 = "bitwise_or(X) :- X is 5 lor 3."
let test_case36 = "bitwise_xor(X) :- X is 5 lxor 3."}