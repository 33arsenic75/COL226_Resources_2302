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
 | Simple of string * expr * typeExp


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



let rec augment t s x = match t with
  [] -> [(s, x)]
| (s1, y) :: ts -> if s = s1 then (s, x) :: ts else (s1, y) :: (augment ts s x)

let rec lookupTable s t = match t with
  [] -> raise TableError
| (s1, x) :: ts -> if s = s1 then x else lookupTable s ts
let rec join t1 t2 =
  match t1 with
    [] -> t2
  | (s, x) :: t1_rem -> join t1_rem (augment t2 s x)

let rec krivine e s =
  match e with
    VClos(NumVal(x), t) -> (
      match s with
        [] -> NumVal(x)
      | ABSTOK :: s_rem -> krivine (VClos(NumVal(abs x), t)) s_rem
      | ADDTOK(VClos(NumVal(x1), t1)) :: s_rem -> krivine (VClos(NumVal(x + x1), t)) s_rem
      | ADDTOK(Clos(x1, t1)) :: s_rem -> krivine (Clos(x1, t1)) (ADDTOK(VClos(NumVal(x), t)) :: s_rem)
      | MULTTOK(VClos(NumVal(x1), t1)) :: s_rem -> krivine (VClos(NumVal(x * x1), t)) s_rem
      | NEGTOK :: s_rem -> krivine (VClos(NumVal((-x)), t)) s_rem
      | MULTTOK(Clos(x1, t1)) :: s_rem -> krivine (Clos(x1, t1)) (MULTTOK(VClos(NumVal(x), t)) :: s_rem)
      | SUBTOK(VClos(NumVal(x1), t1)) :: s_rem -> krivine (VClos(NumVal(x - x1), t)) s_rem
      | REMTOK(VClos(NumVal(x1), t1)) :: s_rem -> krivine (VClos(NumVal(x mod x1), t)) s_rem
      | REMTOK(Clos(x1, t1)) :: s_rem -> krivine (Clos(x1, t1)) (REMTOK(VClos(NumVal(x), t)) :: s_rem)
      | EQTOK(VClos(NumVal(x1), t1)) :: s_rem -> krivine (VClos(BoolVal(x = x1), t)) s_rem
      | SUBTOK(Clos(x1, t1)) :: s_rem -> krivine (Clos(x1, t1)) (SUBTOK(VClos(NumVal(x), t)) :: s_rem)
      | DIVTOK(VClos(NumVal(x1), t1)) :: s_rem -> krivine (VClos(NumVal(x / x1), t)) s_rem
      | CMPTOK :: s_rem -> krivine (VClos(BoolVal(x > 0), t)) s_rem
      | GEQTOK(VClos(NumVal(x1), t1)) :: s_rem -> krivine (VClos(BoolVal(x >= x1), t)) s_rem
      | DIVTOK(Clos(x1, t1)) :: s_rem -> krivine (Clos(x1, t1)) (DIVTOK(VClos(NumVal(x), t)) :: s_rem)
      | EQTOK(Clos(x1, t1)) :: s_rem -> krivine (Clos(x1, t1)) (EQTOK(VClos(NumVal(x), t)) :: s_rem)
      | GEQTOK(Clos(x1, t1)) :: s_rem -> krivine (Clos(x1, t1)) (GEQTOK(VClos(NumVal(x), t)) :: s_rem)
      | GTTOK(VClos(NumVal(x1), t1)) :: s_rem -> krivine (VClos(BoolVal(x > x1), t)) s_rem
      | GTTOK(Clos(x1, t1)) :: s_rem -> krivine (Clos(x1, t1)) (GTTOK(VClos(NumVal(x), t)) :: s_rem)
      | LTTOK(VClos(NumVal(x1), t1)) :: s_rem -> krivine (VClos(BoolVal(x < x1), t)) s_rem
      | LEQTOK(Clos(x1, t1)) :: s_rem -> krivine (Clos(x1, t1)) (LEQTOK(VClos(NumVal(x), t)) :: s_rem)
      | LTTOK(Clos(x1, t1)) :: s_rem -> krivine (Clos(x1, t1)) (LTTOK(VClos(NumVal(x), t)) :: s_rem)
      | LEQTOK(VClos(NumVal(x1), t1)) :: s_rem -> krivine (VClos(BoolVal(x <= x1), t)) s_rem
      | _ -> raise (StackError "Incorrect stack_token for VClos of type int")
    )
  | VClos(BoolVal(x), t) -> (
      match s with
        [] -> BoolVal(x)
      | NOTTOK :: s_rem -> krivine (VClos(BoolVal(not x), t)) s_rem
      | DISJTOK(VClos(BoolVal(x1), t1)) :: s_rem -> krivine (VClos(BoolVal(x || x1), t)) s_rem
      | DISJTOK(Clos(x1, t1)) :: s_rem -> krivine (Clos(x1, t1)) (DISJTOK(VClos(BoolVal(x), t)) :: s_rem)
      | IFTETOK(c1, c2) :: s_rem -> if x then krivine c1 s_rem else krivine c2 s_rem
      | CONJTOK(VClos(BoolVal(x1), t1)) :: s_rem -> krivine (VClos(BoolVal(x && x1), t)) s_rem
      | CONJTOK(Clos(x1, t1)) :: s_rem -> krivine (Clos(x1, t1)) (CONJTOK(VClos(BoolVal(x), t)) :: s_rem)
      | _ -> raise (StackError "Incorrect stack_token for VClos of type bool")
    )
  | Clos(VarRec(x), t) -> (
      let el = (lookupTable x t)
      in
      match el with Clos(c, t1) -> krivine (Clos(c, (augment t1 x el))) s
      | _ -> raise UnknownError
    )
  | VClos(TupleVal(n, xl), t) -> (
      match s with
        [] -> TupleVal(n, xl)
      | PROJTOK(a, b) :: s_rem -> if b = n then krivine (Clos(List.nth xl (a-1), t)) s_rem else raise TupleLengthMismatch
      | _ -> raise UnknownError
    )
  | VClos(FuncVal(x, et), t) -> (
      match s with
        [] -> FuncVal(x, et)
      | APPTOK(c) :: s_rem -> krivine (Clos(et, (augment t x c))) s_rem
      | _ -> raise (StackError "Incorrent stack_token for VClos of type func")
    )
  | Clos(Var(x), t) -> krivine (lookupTable x t) s
  | Clos(N(x), t) -> krivine (VClos(NumVal(x), t)) s
  | Clos(B(x), t) -> krivine (VClos(BoolVal(x), t)) s
  | Clos(Negative(x), t) -> krivine (Clos(x, t)) (NEGTOK :: s)
  | Clos(Add(e1, e2), t) -> krivine (Clos(e2, t)) (ADDTOK(Clos(e1, t)) :: s)
  | Clos(GreaterTE(e1, e2), t) -> krivine (Clos(e2, t)) (GEQTOK(Clos(e1, t)) :: s)
  | Clos(LessT(e1, e2), t) -> krivine (Clos(e2, t)) (LTTOK(Clos(e1, t)) :: s)
  | Clos(Div(e1, e2), t) -> krivine (Clos(e2, t)) (DIVTOK(Clos(e1, t)) :: s)
  | Clos(Rem(e1, e2), t) -> krivine (Clos(e2, t)) (REMTOK(Clos(e1, t)) :: s)
  | Clos(Not(x), t) -> krivine (Clos(x, t)) (NOTTOK :: s)
  | Clos(And(e1, e2), t) -> krivine (Clos(e2, t)) (CONJTOK(Clos(e1, t)) :: s)
  | Clos(LessTE(e1, e2), t) -> krivine (Clos(e2, t)) (LEQTOK(Clos(e1, t)) :: s)
  | Clos(Paren(e1), t) -> krivine (Clos(e1, t)) s
  | Clos(IfThenElse(e1, e2, e3), t) -> krivine (Clos(e1, t)) (IFTETOK(Clos(e2, t), Clos(e3, t)) :: s)
  | Clos(Sub(e1, e2), t) -> krivine (Clos(e2, t)) (SUBTOK(Clos(e1, t)) :: s)
  | Clos(Mult(e1, e2), t) -> krivine (Clos(e2, t)) (MULTTOK(Clos(e1, t)) :: s)
  | Clos(Or(e1, e2), t) -> krivine (Clos(e2, t)) (DISJTOK(Clos(e1, t)) :: s)
  | Clos(Equals(e1, e2), t) -> krivine (Clos(e2, t)) (EQTOK(Clos(e1, t)) :: s)
  | Clos(GreaterT(e1, e2), t) -> krivine (Clos(e2, t)) (GTTOK(Clos(e1, t)) :: s)
  | Clos(Tuple(n, el), t) -> krivine (VClos(TupleVal(n, el), t)) s
  | Clos(Project((a, b), e1), t) -> krivine (Clos(e1, t)) (PROJTOK(a, b) :: s)
  | DefClos(Sequence([d1; d2]), t) -> krivine (DefClos(d1, t)) (SEQDEFTOK(DefClos(d2, t)) :: s)
  | Clos(FunctionAbstraction(x, e1, tau), t) -> krivine (VClos(FuncVal(x, e1), t)) s
  | Clos(FunctionCall(e1, e2), t) -> krivine (Clos(e1, t)) (APPTOK(Clos(e2, t)) :: s)
  | Clos(Let(d, e1), t) -> krivine (DefClos(d, t)) (DEFTOK(Clos(e1, t)) :: s)
  | DefClos(Simple(x, e1, tau), t) -> krivine (VDefClos(augment t x (Clos(e1, t)))) s
  | DefClos(Parallel([d1; d2]), t) -> krivine (DefClos(d1, t)) (PARDEFTOK(DefClos(d2, t)) :: s)
  | VDefClos(t1) -> (
      match s with
        DEFTOK(Clos(e, t)) :: s_rem -> krivine (Clos(e, t1)) s_rem
      | PARDEFTOK(VDefClos(t)) :: s_rem -> krivine (VDefClos(join t t1)) s_rem
      | SEQDEFTOK(VDefClos(t)) :: s_rem -> krivine (VDefClos(t1)) s_rem
      | SEQDEFTOK(DefClos(d2, t)) :: s_rem -> krivine (DefClos(d2, t1)) (SEQDEFTOK(VDefClos(t1)) :: s_rem)
      | PARDEFTOK(DefClos(d2, t)) :: s_rem -> krivine (DefClos(d2, t)) (PARDEFTOK(VDefClos(t1)) :: s_rem)
      | _ -> raise (StackError "Did not find the defintion opcode after defintion")
    )
  | _ -> raise UnknownError


let t = [("X", Clos(N(1), []))] ;;

let testcode (e:closure):unit= 
  match krivine e [] with 
  NumVal(x) -> print_endline (string_of_int x)
| BoolVal(x) -> print_endline (string_of_bool x) 
| _-> raise UnknownError;;



  let empty_table = [];;
  let sample_table = [("x", Clos(N(5), [])); ("y", Clos(N(3), [])); ("z", VClos(BoolVal(true), []))];;
  let test1 = (Clos(Add(N(2), N(3)), empty_table));;
  let test2 = (Clos(Mult(N(2), N(3)), empty_table));;
  let test3 = (Clos(Var("x"), sample_table));;
  let test4 =  (Clos(Var("z"), sample_table));;
  let test5 = (Clos(FunctionCall(FunctionAbstraction("x", Add(Var("x"), N(1)), IntT), N(5)), empty_table));;
  let test6 = (Clos(IfThenElse(B(true), N(1), N(0)), empty_table));;
  let test7 = (Clos(IfThenElse(B(false), N(1), N(0)), empty_table));;
  let test8 = 
    let function_env = [("double", VClos(FuncVal("x", Mult(Var("x"), N(2))), []))] in
        (Clos(FunctionCall(Var("double"), N(5)), function_env));;
  let test9 = 
    let nested_function_env = [("outer", VClos(FuncVal("x",
                                                        Let(Simple("inner",
                                                                  FunctionAbstraction("y",
                                                                                      Add(Var("x"), Var("y")),
                                                                                      IntT),
                                                                  UnitT),
                                                            FunctionCall(Var("inner"), N(5)))),
                                              []))] in
  (Clos(FunctionCall(Var("outer"), N(3)), nested_function_env));;

(* let () = 
    testcode test1;
    testcode test2;
    testcode test3;
    testcode test4;
    testcode test5;
    testcode test6;
    testcode test7;
    testcode test8;
    testcode test9; *)

let test1 = Clos(Var"z",["z",Clos(N 3,[])]);;
let test2 = Clos(Add(Add(N 2, N 3),Add(N 2, N 3)),[]);;

let test3 = Clos(FunctionCall(FunctionAbstraction("x",Add(Var "x", N 1),IntT),N 2),[]);;

let test4 = Clos(FunctionCall(FunctionAbstraction("x",Mult(Var "x",Add(Var "x", N 1)),IntT),N 2),[]);;

let test5 = Clos(FunctionCall(FunctionAbstraction("d",Mult(Var "d", N 2),IntT),N 2),[]);;

let test6 = Clos(IfThenElse(GreaterT(N 8, N 2),
                                              FunctionCall(FunctionAbstraction("x",Div(Var "x", N 2),IntT),N 2),
                                              FunctionCall(FunctionAbstraction("x",Mult(Var "x", Add(Var "x", N 1)),IntT),N 2)),[]);;

let test7 = Clos(IfThenElse(GreaterT(N 8, N 2),
                                              Add(N 1, N 2),
                                              Div( N 9, N 0)),[]);;
