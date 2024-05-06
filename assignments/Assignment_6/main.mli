type typeExp = IntT | BoolT |FuncT of (typeExp * typeExp) | UnitT | TupleT of (typeExp list)

type expr = 
   Var of string
 | N of int
 | B of bool 
 | Sub of expr * expr
 | Div of expr * expr
 | GreaterT of expr * expr    
 | Paren of expr
 | Rem of expr * expr
 | Add of expr * expr
 | Mult of expr * expr
 | Not of expr
 | Absolute of expr
 | IfThenElse of expr * expr * expr 
 | Tuple of int * (expr list)
 | Negative of expr
 | And of expr * expr
 | Or of expr * expr
 | Equals of expr * expr   
 | GreaterTE of expr * expr   
 | LessTE of expr * expr
 | LessT of expr * expr          
 | Project of (int*int) * expr   (* Proj((i,n), e)  0 < i <= n *)
 | Let of defn * expr
 | FunctionAbstraction of string * expr * typeExp
 | FunctionCall of expr * expr
 and defn =
    Evaluate of string * expr * typeExp

type op = 
   VAR of string | INTOP of int | BOOLOP of bool 
 | FABSOP of string * (op list) | FCALLOP of (op list) * (op list) | APPOP | RETOP
 | PARENOP | IFTEOP of (op list) * (op list)
 | TUPLEOP of int | PROJOP of int * int
 | NEGATIVEOP | ABSOLUTEOP | PLUSOP | MINUSOP | MULTOP | DIVOP | REMOP
 | EQUALSOP | GTOP | LTOP | GEQOP | LEQOP | NOTOP | CONJOP | DISJOP

type value = NumVal of int | BoolVal of bool | FuncVal of string * (op list) | TupVal of int * (value list)


exception ValueError
exception OpError
exception UnknownError

type stack_token = VClose of value * table
and table = (string * stack_token) list

val compile: expr -> op list
val secd: (stack_token list) -> table -> (op list) -> (((stack_token list) * (table) * (op list)) list) -> value



