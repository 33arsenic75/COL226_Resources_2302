type typeExp = IntT | BoolT |FuncT of (typeExp * typeExp) | UnitT | TupleT of (typeExp list)

type expr = 
   Var of string
 | VarRec of string  
 | Tuple of int * (expr list)
 | Negative of expr
 | And of expr * expr
 | Paren of expr
 | Let of defn * expr
 | FunctionAbstraction of string * expr * typeExp
 | IfThenElse of expr * expr * expr 
 | Or of expr * expr
 | Equals of expr * expr   
 | GreaterTE of expr * expr   
 | LessTE of expr * expr
 | Rem of expr * expr
 | Add of expr * expr
 | Mult of expr * expr
 | Not of expr
 | Absolute of expr
 | LessT of expr * expr          
 | Project of (int*int) * expr   (* Proj((i,n), e)  0 < i <= n *)
 | FunctionCall of expr * expr
 | N of int
 | B of bool 
 | Sub of expr * expr
 | Div of expr * expr
 | GreaterT of expr * expr   
 and defn =
 | Sequence of (defn list)
 | Parallel of (defn list)
 |Simple of string * expr * typeExp



type value = NumVal of int | BoolVal of bool | FuncVal of string * expr | TupleVal of (int * expr list)

type closure = Clos of expr * table | VClos of value * table | DefClos of defn * table | VDefClos of table
and table = (string * closure) list

type stack_token =
  ABSTOK | NEGTOK
| IFTETOK of closure * closure
| CONJTOK of closure | DISJTOK of closure | NOTTOK
| APPTOK of closure
| DIVTOK of closure | REMTOK of closure | ADDTOK of closure | MULTTOK of closure | SUBTOK of closure
| DEFTOK of closure | SEQDEFTOK of closure | PARDEFTOK of closure
| CMPTOK | EQTOK of closure | GTTOK of closure | GEQTOK of closure | LTTOK of closure | LEQTOK of closure
| PROJTOK of int * int

exception StackError of string
exception TableError
exception TupleLengthMismatch
exception UnknownError
val krivine: closure -> stack_token list -> value
val augment: ('a * 'b) list -> 'a -> 'b -> ('a * 'b) list