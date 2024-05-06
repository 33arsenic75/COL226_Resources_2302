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

type stack_token = VClose of value * table
and table = (string * stack_token) list

let rec compile e = match e with  
   Var(s) -> [VAR(s)]
 | N(n) -> [INTOP(n)]
 | B(b) -> [BOOLOP(b)]
 | Sub(e1,e2) -> (compile e2) @ (compile e1) @ [MINUSOP]
 | Div(e1,e2) -> (compile e2) @ (compile e1) @ [DIVOP]
 | GreaterT(e1,e2) -> (compile e2) @ (compile e1) @ [GTOP]
 | Paren(e1) ->  (compile e1) @ [PARENOP]
 | Rem(e1,e2) -> (compile e2) @ (compile e1) @ [REMOP]
 | Add(e1,e2) -> (compile e2) @ (compile e1) @ [PLUSOP]
 | Mult(e1,e2) -> (compile e2) @ (compile e1) @ [MULTOP]
 | Not(e1) -> (compile e1) @ [NOTOP]
 | Absolute(e1) -> (compile e1) @ [ABSOLUTEOP]
 | IfThenElse(e1,e2,e3) -> (compile e1) @ [IFTEOP((compile e2), (compile e3))]
 | Tuple(n, elist) -> (
  let rec aux l acc =
    match l with
      [] -> acc
    | t :: ts -> aux ts ((compile t) @ acc)
  in
    ( aux elist [] ) @ [ TUPLEOP(n) ]
)
 | Negative(e1) -> (compile e1) @ [NEGATIVEOP]
 | And(e1,e2) ->(compile e1) @ (compile e2) @ [DISJOP]
 | Or(e1,e2) ->(compile e1) @ (compile e2) @ [CONJOP]
 | Equals(e1, e2) -> (compile e1) @ (compile e2) @ [EQUALSOP]
 | GreaterTE(e1, e2) -> (compile e2) @ (compile e1) @ [GEQOP]
 | LessTE(e1, e2) -> (compile e2) @ (compile e1) @ [LEQOP]
 | LessT(e1, e2) -> (compile e2) @ (compile e1) @ [LTOP]
 | Project((a, b), t) -> ( compile t ) @ [ PROJOP(a, b) ]
 | Let(d, e) -> (
  match d with
    Evaluate(s, e1, tau) -> [FABSOP(s, (compile e) @ [RETOP])] @ (compile e1)  @ [APPOP]
)
 | FunctionAbstraction(s, es, t) -> [FABSOP(s, (compile es) @ [RETOP])]
 | FunctionCall(e1, e2) -> [FCALLOP((compile e1), (compile e2))]


exception StackError of string
exception ValueError
exception UnknownError
exception OpError
let rec lookupTable s t = match t with
  [] -> raise ValueError
| (s1, x) :: ts -> if s = s1 then x else lookupTable s ts

let rec augment t s x = match t with
  [] -> [(s, x)]
| (s1, y) :: ts -> if s = s1 then (s, x) :: ts else (s1, y) :: (augment ts s x)



let rec secd s e c d = match c with
  [] -> (
      match s with
        [VClose(x, t)] -> x
      | _ -> raise (StackError "SECD expects empty stack at the end")
    )
| INTOP(n) :: c_dash -> secd (VClose(NumVal(n), e) :: s) e c_dash d
| BOOLOP(b) :: c_dash -> secd (VClose(BoolVal(b), e) :: s) e c_dash d
| VAR(x) :: c_dash -> (
    let el = (lookupTable x e)
    in
    match el with VClose(v, t) -> secd (VClose(v, (augment t x el)) :: s) (augment e x el) c_dash d
  )
| NEGATIVEOP :: c_dash -> (
    match s with
      VClose(NumVal(n), t) :: s_dash -> secd (VClose(NumVal(-n), t) :: s) e c_dash d
    | _ -> raise (StackError "Negative does not find an integer on the stack")
  )
| ABSOLUTEOP :: c_dash -> (
    match s with
      VClose(NumVal(n), t) :: s_dash -> secd (VClose(NumVal(abs n), t) :: s) e c_dash d
    | _ -> raise (StackError "Abs does not find an integer on the stack")
  )
| PLUSOP :: c_dash -> (
    match s with
      VClose(NumVal(n1), t1) :: VClose(NumVal(n2), t2) :: s_dash -> (
          secd (VClose(NumVal(n1 + n2), e) :: s_dash) e c_dash d
        )
    | _ -> raise (StackError "Add does not find two numbers on the stack")
  )
| MINUSOP :: c_dash -> (
    match s with
      VClose(NumVal(n1), t1) :: VClose(NumVal(n2), t2) :: s_dash -> (
          secd (VClose(NumVal(n1 - n2), e) :: s_dash) e c_dash d
        )
    | _ -> raise (StackError "Subtraction does not find two numbers on the stack")
  )
| MULTOP :: c_dash -> (
    match s with
      VClose(NumVal(n1), t1) :: VClose(NumVal(n2), t2) :: s_dash -> (
          secd (VClose(NumVal(n1 * n2), e) :: s_dash) e c_dash d
        )
    | _ -> raise (StackError "Multiplication does not find two numbers on the stack")
  )
| DIVOP :: c_dash -> (
    match s with
      VClose(NumVal(n1), t1) :: VClose(NumVal(n2), t2) :: s_dash -> (
          secd (VClose(NumVal(n1 / n2), e) :: s_dash) e c_dash d
        )
    | _ -> raise (StackError "Division does not find two numbers on the stack")
  )
| REMOP :: c_dash -> (
    match s with
      VClose(NumVal(n1), t1) :: VClose(NumVal(n2), t2) :: s_dash -> (
          secd (VClose(NumVal(n1 - n2), e) :: s_dash) e c_dash d
        )
    | _ -> raise (StackError "Remainder does not find two numbers on the stack")
  )
| NOTOP :: c_dash -> (
    match s with
      VClose(BoolVal(b), t) :: s_dash -> (
        secd (VClose(BoolVal(not b), e) :: s_dash) e c_dash d
        )
    | _ -> raise (StackError "Not does not find a number on the stack")
  )
| DISJOP :: c_dash -> (
    match s with
      VClose(BoolVal(b1), t1) :: VClose(BoolVal(b2), t2) :: s_dash -> (
          secd (VClose(BoolVal(b1 || b2), e) :: s_dash) e c_dash d
        )
    | _ -> raise (StackError "And does not find two Bools on the stack")
  )
| CONJOP :: c_dash -> (
    match s with
      VClose(BoolVal(b1), t1) :: VClose(BoolVal(b2), t2) :: s_dash -> (
          secd (VClose(BoolVal(b1 && b2), e) :: s_dash) e c_dash d
        )
    | _ -> raise (StackError "Or does not find two Bools on the stack")
  )
| GTOP :: c_dash -> (
    match s with
      VClose(NumVal(n1), t1) :: VClose(NumVal(n2), t2) :: s_dash -> (
          secd (VClose(BoolVal(n1 > n2), e) :: s_dash) e c_dash d
        )
    | _ -> raise (StackError "Greater than does not find two numbers on the stack")
  )
| GEQOP :: c_dash -> (
    match s with
      VClose(NumVal(n1), t1) :: VClose(NumVal(n2), t2) :: s_dash -> (
          secd (VClose(BoolVal(n1 >= n2), e) :: s_dash) e c_dash d
        )
    | _ -> raise (StackError "Greater than or equal to does not find two numbers on the stack")
  )
| LTOP :: c_dash -> (
    match s with
      VClose(NumVal(n1), t1) :: VClose(NumVal(n2), t2) :: s_dash -> (
          secd (VClose(BoolVal(n1 < n2), e) :: s_dash) e c_dash d
        )
    | _ -> raise (StackError "Less than does not find two numbers on the stack")
  )
| LEQOP :: c_dash -> (
    match s with
      VClose(NumVal(n1), t1) :: VClose(NumVal(n2), t2) :: s_dash -> (
          secd (VClose(BoolVal(n1 <= n2), e) :: s_dash) e c_dash d
        )
    | _ -> raise (StackError "Less than or equal to does not find two numbers on the stack")
  )
| EQUALSOP :: c_dash -> (
    match s with
      VClose(NumVal(n1), t1) :: VClose(NumVal(n2), t2) :: s_dash -> (
          secd (VClose(BoolVal(n1 = n2), e) :: s_dash) e c_dash d
        )
    | _ -> raise (StackError "Equals does not find two numbers on the stack")
  )

| PARENOP :: c_dash -> secd s e c_dash d
| IFTEOP(c1, c2) :: c_dash -> (
    match s with
      VClose(BoolVal(b), t) :: s_dash -> (
          if b then secd s_dash e (c1 @ c_dash) d
          else secd s_dash e (c2 @ c_dash) d
        )
    | _ -> raise (StackError "IFTE does not find a Boolean on the stack")
  )
| FABSOP(x, c1) :: c_dash -> secd (VClose(FuncVal(x, c1), e) :: s) e c_dash d
| FCALLOP(c1, c2) :: c_dash -> secd s e (c1 @ c2 @ [APPOP] @ c_dash) d
| APPOP :: c_dash -> (
    (* Call the function *)
    match s with
      v :: VClose(FuncVal(x, c_func), e_func) :: s_dash -> (
          secd [] (augment e_func x v) c_func ((s_dash, e, c_dash) :: d)
        )
    | _ -> raise (StackError "Apply does not find a value closure and FunctionVal on the stack")
  )
| TUPLEOP(n) :: c_dash -> (
    let rec aux m stack acc =
      if m = 0 then (acc, stack)
      else
        match stack with
          VClose(s1, r) :: s_rem -> aux (m-1) s_rem (acc @ [s1])
        | _ -> raise (StackError "Tuple did not find enough numbers on the stack")
    in
    match aux n s [] with
      (t, stack_rem) -> secd (VClose(TupVal(n, t), e) :: stack_rem) e c_dash d
  )
| PROJOP(a, b) :: c_dash -> (
    match s with
      VClose(TupVal(n, t), t1) :: s_rem -> (
          if n != b then raise (StackError "Project does not match length")
          else secd (VClose((List.nth t (a-1)), t1) :: s_rem) e c_dash d
        )
      | _ -> raise (StackError "Proj did not find Tuple on the stack")
  )
| [RETOP] -> (
    (* Remove the value from the stack and dump *)
    match s with
      [VClose(v, t1)] -> (
          match d with
            (s_old, e_old, c_old) :: ds -> secd (VClose(v, e_old) :: s_old) e_old c_old ds
            | _ -> raise UnknownError
        )
    | _ -> raise (StackError "Stack on return is not of the form [RET]")
  )
  | _ -> raise OpError

  let rec string_of_opcode = function
  | VAR s -> "VAR( " ^ s ^ " ) "
  | INTOP i -> "NCONST(" ^ string_of_int i ^ ") "
  | BOOLOP b -> "BCONST(" ^ string_of_bool b ^ ") "
  | NEGATIVEOP -> "NEG "
  | ABSOLUTEOP -> "ABS "
  | PLUSOP -> "PLUS "
  | MINUSOP -> "MINUS "
  | MULTOP -> "MULT "
  | DIVOP -> "DIV "
  | REMOP -> "REM "
  | NOTOP -> "NOT "
  | CONJOP -> "CONJ "
  | DISJOP -> "DISJ "
  | EQUALSOP -> "EQUALS "
  | GTOP -> "GT "
  | LTOP -> "LT "
  | GEQOP -> "GEQ "
  | LEQOP -> "LEQ "
  | PARENOP -> "PAREN "
  | IFTEOP (c, t) ->
      "\nIFTE\n" ^ String.concat "" (List.map string_of_opcode c) ^ "\n" ^ String.concat "" (List.map string_of_opcode t) ^ "\n"
  | FABSOP (name, body) ->
      "\nFABS: " ^ name ^ "\n" ^ String.concat "" (List.map string_of_opcode body) ^ "\n"
  | FCALLOP (args, ret) ->
      "\nFCALL\n" ^ String.concat "" (List.map string_of_opcode args) ^ "\n" ^ String.concat "" (List.map string_of_opcode ret) ^ "\n"
  | APPOP -> "APP "
  | RETOP -> "RET "
  | TUPLEOP n -> "\nTUPLE(" ^ string_of_int n ^ ") "
  | PROJOP (idx, n) -> "\nPROJ(" ^ string_of_int idx ^ ", " ^ string_of_int n ^ ")\n"

let print_opcode op =
  print_endline (string_of_opcode op)

let test_code (code:expr) : unit = 
  let c = (compile code) in 
  let rec help x =
  match x with
  | NumVal i -> string_of_int i
  | BoolVal b -> string_of_bool b
  | FuncVal (name, oplist) -> "Variable: " ^ name ^ "\n" ^ ( String.concat " " (List.map string_of_opcode oplist) )
  | TupVal (_, values) -> 
      let elements_str = List.map help values |> String.concat ", " in
      "(" ^ elements_str ^ ")" in 
      print_endline (help (secd [] [] c [])) 








(* Factorial Function *)
let fact_prog = FunctionAbstraction("X", 
                          IfThenElse(Equals(Var("X"), N(0)), N(1),
                          Mult(Var("X"), FunctionCall(Var("Y"), Sub(Var("X"), N(1))))), IntT) ;;
let fact_code = Let(Evaluate("Y", fact_prog, IntT) , FunctionCall(Var("Y"), N(20))) ;;

(* Fibonacci function *)
let fib_prog = FunctionAbstraction("X",
                          IfThenElse(Equals(Var("X"), N(0)), N(0),
                          IfThenElse(Equals(Var("X"), N(1)), N(1),
                          Add(FunctionCall(Var("Y"), Sub(Var("X"), N(2))), FunctionCall(Var("Y"), Sub(Var("X"), N(1)))))), IntT) ;;

let fib_code = Let(Evaluate("Y", fib_prog, IntT) , FunctionCall(Var("Y"), N(26))) ;;


(* Nested Function *)
let inside_prog = FunctionAbstraction("Y",Mult(Var("Y"),Var("X")),IntT);;
let nested_prog = FunctionAbstraction("X",
                          Add( Var("X"),Let(Evaluate("Y", inside_prog, IntT) , FunctionCall(Var("Y"), Add(Var("X"),N(3)))) ),
                          IntT);;

let nested_code = Let(Evaluate("Y", nested_prog, IntT) , FunctionCall(Var("Y"), N(26))) ;;

(* Ifelse *)
let boolfunc = FunctionAbstraction("P",And( Var("P"), Not( Or(B(false), Not(Var("P")) ) ) ),BoolT) (* P + ~(F* ~ P) *) 
let boolexp = Let( Evaluate("X",boolfunc,BoolT),FunctionCall(Var("X"),B(true))) ;;
let ifelse_code = Add( N(18) , IfThenElse( boolexp , N(49), N(264)   ));;

(* Sum Function *)
let sum_prog = FunctionAbstraction("X",
                          IfThenElse(Equals(Var("X"), N(0)), N(0),
                          Add(Var("X"), FunctionCall(Var("Y"), Sub(Var("X"), N(1))))), IntT) ;;

let sum_code =  Let(Evaluate("Y", sum_prog, IntT) , FunctionCall(Var("Y"), N(226))) ;;

(* test_code fact_code;;
test_code fib_code;;
test_code nested_code;;
test_code ifelse_code;;
test_code sum_code;; *)

let emptytable:table = []
let tb:table = ["x",VClose(NumVal 1,emptytable)]

let myexpr:expr = Var "y";;




let help (x:value):unit = match x with 
|NumVal v -> Printf.printf "%d\n" v
|BoolVal v -> Printf.printf "%b\n" v
| FuncVal (a,b) -> Printf.printf "%s\n" a
| _ -> Printf.printf "somethingelse\n";;


(* secd [] ["x",VClose(NumVal 1,emptytable)] (compile (Var "y")) [];; *)
(* secd [] ["x",VClose(NumVal 1,emptytable)] (compile (Var "x")) []);; *)
(* let test3table:table = [
  "x",VClose(FuncVal("x",[VAR "y";RETOP]),["y",VClose(NumVal 2,[])]);"y",VClose(NumVal 1,[])
]
let test3comp = compile (FunctionCall(Var "x",Var "y")) 
compile [] test3table test3comp [];;*)

(* let test4table = ["x",VClose(NumVal 1,[])]
let test4comp = compile (FunctionCall(FunctionAbstraction("x",Var "x", IntT), Var "x")) *)

(* let test5table = ["x", VClose(NumVal 1,[]); "y", VClose(NumVal 2,[])];;
let test5comp = compile (FunctionCall(FunctionAbstraction("y",Var "y",IntT), Var "x")) *)

(* let test6table = ["x", VClose(NumVal 1,[]); "y", VClose(NumVal 2,[])];;
let test6comp = compile(FunctionCall (
                                      FunctionAbstraction("y", 
                                                          FunctionAbstraction("x", Var "y", IntT),
                                                          IntT),
                                                          Var "x"));; *)

(* let test7table = ["x", VClose(NumVal 1,[]); "y", VClose(NumVal 2,[])];;
let test7comp = compile(FunctionCall (
                                      FunctionAbstraction("x", 
                                                          FunctionCall(FunctionAbstraction("y", Var "y",IntT),Var "x"),
                                                          IntT),
                                                          Var "x"));;  *)

