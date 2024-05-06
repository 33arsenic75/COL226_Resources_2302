type program = clause list
and body = B of atom list
and head = H of atom
and clause =  R of head * body | F of head 
and atom = A of symbol * (term list)
and symbol =  string
and term = V of variable | Num of int  | Node of  symbol * (term list)
and variable = string
and signature = (symbol * int) list
type substitution = (variable * term) list
and goal = G of atom list;;

exception UnificationFailure;;
exception DoesntExist;;
exception InvalidCode;;
exception Contradiction;;

let empty_list = [];;
let rec combinelist la lb = match la,lb with
  | xa::xs , ya::ys -> ((xa,ya)::(combinelist xs ys))@[]
  | _ -> []@[];;


let rec union la lb = match la with 
  | [] -> lb@[]
  | x::xs -> if List.exists (fun y -> x = y) lb then (union xs lb)@[] else (x::(union xs lb))@[];;


let rec valid_program (prog:program) : bool = match prog with
    | F(H(a))::xs | R(H(a),_)::xs ->  (match a with 
                                        | A("<", _) -> raise (InvalidCode)
                                        | A(">", _) -> raise  (InvalidCode)
                                        | A("_cut", _) -> raise  (InvalidCode)
                                        | A("_not_eq", _) -> raise (InvalidCode)
                                        |  A("_eq", _) -> raise (InvalidCode)
                                        | _ -> valid_program xs)
    | [] -> (true && true) || (false && true);;
let rec change_term (n:int) (t:term):term = match t with 
  | V(x) -> V((string_of_int n)^x)
  | Node(s,tl) -> Node(s, List.map (change_term n) tl)
  | _ as a-> match a with 
            | _ ->t;;

let change_atom (n:int) (A(s,tl):atom):atom = A(s, List.map (change_term n) tl);;

let change_clause (n:int) (c:clause):clause = match c with
  | F(H(a)) -> F(H(change_atom n a))
  | R(H(a),B(al)) -> R(H(change_atom n a), B(List.map (change_atom n) al));;

let rec change_program_int (n:int) (p:program):program =  match p with
| hd::tl-> ((change_clause n hd)::change_program_int (n+1) tl)@[]
| [] -> []@[];;

let rec change_program_atom (p) (A(s,_)) = match p with 
  | hd::tl -> (
    match hd with 
    | F(H(A(s1,_))) -> if s = s1 then (change_clause 0 hd)::change_program_atom tl (A(s,[]))@[]
                                 else hd::change_program_atom tl (A(s,[]))@[]
    | R(H(A(s1,_)),B(al)) -> if s = s1 then (change_clause 0 hd)::change_program_atom tl (A(s,[])) @[]
                                 else hd::change_program_atom tl (A(s,[]))@[]
  )
  | [] -> []@[];;

let rec variable_term (t:term):variable list = match t with  
    | V(x) -> [x]@[]
    | Node(_,tl) ->  (List.fold_left union ([]@[]) (List.map variable_term tl))@[]
    | _ -> []@[];;

let variable_atom (A(_,tl):atom):variable list = variable_term (Node("",tl));;

let variable_goals (G(g):goal):variable list = List.fold_left union [] (List.map variable_atom g);;

let rec subst_term (s:substitution) (t:term):term = match t with 
  | Node(s1,tl) -> let (x,y) = (s1,tl) in Node(x, List.map (subst_term s) y)
  | V(x) -> (match s with 
                | s'::tl -> (match fst s' = x with 
                              | true -> snd s'
                              | false -> subst_term tl t)
                | [] -> t)       
  | _ as a -> match a with 
              | _ -> t;;

let subst_atom (s:substitution) (a:atom):atom = match a with
  | A(s1,tl) -> A(s1, []@(List.map (subst_term s) ([]@tl@[])));;


let compose_subs (s1:substitution) (s2:substitution):substitution = 
  let helper s x = (fst x, subst_term s (snd x)) in
  []@(List.map (helper s2) s1)@s2@[] ;;


let rec term_contain_vars (v:variable) (t:term): bool =
  match t with
    | Node(s, l) -> List.fold_left (||) (false && true) (List.map (term_contain_vars v) l)
    |  V(x) -> if (x = v) then true else false
    | _ -> (false || false)
;;

let rec term_mgu (t1)(t2) = 
  let helper1 (x,y) = if x = y then [] else [(x, V(y))] in 
  let helper2 (x,t)= if term_contain_vars x t then raise UnificationFailure else [(x, t)] in 
  let helper3 (n1,n2) = if n1 = n2 then [] else raise UnificationFailure in 
  let helper4 (n,x) = [(x, Num(n))] in 
  let helper5 (s1,s2,l1,l2) = if s1 <> s2 || (List.length l1 <> List.length l2) then raise UnificationFailure
                                          else
                                            let f s tt = compose_subs s (term_mgu (subst_term s (fst tt)) (subst_term s (snd tt))) in
                                            List.fold_left f [] (combinelist l1 l2) in
  match (t1,t2) with 
    |  (V(x), V(y)) -> helper1 (x,y)
    | (V(x), Node(_, _)) -> helper2 (x, t2)
    | (Node(_, _), V(y)) -> helper2 (y, t1)
    | (Num(n1), Num(n2)) -> helper3 (n1,n2)
    | (Num(n1), V(x)) -> helper4 (n1,x)
    | (V(x), Num(n2)) -> helper4 (n2,x)
    | (Node(s1, l1), Node(s2, l2)) -> helper5 (s1,s2,l1,l2)
    | _ -> raise UnificationFailure

let atom_mgu (a1:atom) (a2:atom) : substitution = match a1,a2 with
  | (A(s1, l1), A(s2, l2)) -> empty_list@(term_mgu (Node(s1, l1)) (Node(s2, l2)))@[];;

let rec term_list_print (t:term list) :unit=
  let helper (hd:term) :unit= ( match hd with 
      | V(x) -> Printf.printf "%s " x
      | Num(n) -> Printf.printf "%d " n
      | Node(s,tl) -> Printf.printf "%s(" s; term_list_print tl; Printf.printf ") ") in
      List.iter helper t

and body_list_print (t:term) :unit = match t with 
      | Node("_list", [t1; Node("_empty_list", empty_list)]) -> print_term t1
      | Node("_list", ([t1; t2])) -> (
                            print_term t1;Printf.printf ", ";body_list_print t2;
                          )
      | Node("_empty_list", empty_list) -> Printf.printf ""
      | _  -> raise Contradiction
and print_term (t:term) :unit= ( match t with 
      | Num(n) -> Printf.printf "%d " n
      | Node("_list", _) -> ( Printf.printf " ["; body_list_print t; Printf.printf "] ";)
      | Node("_empty_list", []) -> Printf.printf " [] "
      | Node(s, []) -> Printf.printf " %s " s
      | Node(s,tl) -> Printf.printf "%s(" s; term_list_print tl; Printf.printf ") "
      | V(x) -> Printf.printf "%s " x);;

let rec vars_to_subs (mgu:substitution) (v:variable list):substitution = 
  let rec helper l a = match l with 
    | [] ->raise Not_found
    | hd::tl -> if (fst hd) = a then hd else helper tl a in
  match v with
  | hd::tl -> (try (helper mgu hd)::(vars_to_subs mgu (tl@[]))
              with Not_found -> (vars_to_subs mgu tl@[]))
  | [] -> []@empty_list;;

let read_input() = 
  let terminal = Unix.tcgetattr Unix.stdin in
  let () = Unix.tcsetattr Unix.stdin Unix.TCSANOW {terminal with Unix.c_icanon = false} in
  let output = input_char stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSANOW terminal;output;;

let rec solution_print (subsitution:substitution) = match subsitution with 
  | [] -> (*Printf.printf ""*)()
  | [a] -> (match a with | (v,t) -> let(x,y) = (v,t) in (Printf.printf "%s = " x; print_term y)) 
  | (v,t)::tl -> let a = t in (Printf.printf "%s = " v; print_term a;
                 Printf.printf ", "; (solution_print (tl)));;


let atom_solver (a1:atom) (a2:atom) (mgu:substitution): substitution =
  compose_subs mgu (atom_mgu (subst_atom mgu a1) (subst_atom mgu a2))
;;

let term_solver (t1:term) (t2:term) (mgu:substitution): substitution =
  compose_subs mgu (term_mgu (subst_term mgu t1) (subst_term mgu t2))
;;

let rec term_simplification (t:term):term = match t with 
  | Node("+",[t1;t2]) ->
                          (match (term_simplification t1),(term_simplification t2) with 
                          | Num(n1), Num(n2) -> Num(n1+n2)
                          | _ -> raise UnificationFailure)
  | Node("-",[t1;t2]) -> (match (term_simplification t1),(term_simplification t2) with
                          | Num(n1), Num(n2) -> Num(n1-n2)
                          | _ -> raise UnificationFailure)
  | Node("*",[t1;t2]) -> (match (term_simplification t1),(term_simplification t2) with
                          | Num(n1), Num(n2) -> Num(n1*n2)
                          | _ -> raise UnificationFailure)
  | Node("/",[t1;t2]) -> (match (term_simplification t1),(term_simplification t2) with
                          | Num(n1), Num(n2) -> Num(n1/n2)
                          | _ -> raise UnificationFailure)
  | Num(n) -> t
  | _ -> t;;

 let evaluation (atom:atom)(mgu:substitution):substitution = 
  match atom with 
  |A(">",[t1;t2]) -> (match (term_simplification (subst_term mgu t1),(term_simplification (subst_term mgu t2))) with 
                      | Num(n1), Num(n2) -> if n1 > n2 then (empty_list@mgu@empty_list) else raise UnificationFailure
                      | _ -> raise UnificationFailure)
  |A("<",[t1;t2]) -> (match (term_simplification (subst_term mgu t1),(term_simplification (subst_term mgu t2))) with 
                      | Num(n1), Num(n2) -> if n1 < n2 then (empty_list@mgu) else raise UnificationFailure
                      | _ -> raise UnificationFailure)
  |A("_eq",[t1;t2]) -> (
                      let a = term_simplification (subst_term (empty_list@mgu) t1) in
                      let b = term_simplification (subst_term (mgu@empty_list) t2) in
                      let c = term_mgu a b in
                      compose_subs mgu c)
  |A("_not_eq",[t1;t2]) -> (
                      let a = term_simplification (subst_term (mgu@empty_list) t1) in
                      let b = term_simplification (subst_term (empty_list@mgu) t2) in
                      let c = term_mgu a b in
                      compose_subs mgu c)
  | _ as a -> match a with _ ->mgu;;


let rec goal_solver (program:program)(goal:goal) (mgu:substitution) (v:variable list):(bool*substitution) = 
  match goal with 
  | G(a::gs) -> (match a with 
                | A("_cut",_) -> let _ = goal_solver program (G(gs)) mgu v in (true,[])
                | A("_not_eq",_) -> (
                  try (false,evaluation a mgu)
                  with (UnificationFailure) -> goal_solver program (G(gs)) mgu v)
                | A(">",_) | A("<",_) | A("_eq",_)  ->(
                  try goal_solver program (G(gs)) (evaluation a mgu) v  
                  with UnificationFailure -> (false,[]))
                | _ -> 
                      (let new_program = change_program_atom program a in 
                      let rec iter prog2 = (match prog2 with 
                      | cl::ps -> (match cl with 
                                  | F(H(a')) -> (
                                    try let u = atom_solver a' a mgu in 
                                        match (goal_solver new_program (G(gs)) u v) with
                                        | (true,uu) -> (true,uu)
                                        | _ -> iter ps
                                     with UnificationFailure -> iter ps)
                                 | R(H(x),B(y)) -> (
                                    try let u = (atom_solver x a mgu) in 
                                        match (goal_solver new_program (G(y@gs)) u v) with 
                                          | (true,uuu) -> (true,uuu)
                                          | _ -> iter ps
                                    with UnificationFailure -> iter ps)
                              )
                      | [] -> (false,[]))
                                  in iter program))
  | G([]) -> (
        solution_print (vars_to_subs (mgu@[]) v);
        flush stdout;
        (true,[])) 


let prolog (program:program)(goal:goal) = goal_solver program goal [] (variable_goals goal);;

                                    
let program1 : program = [
  R(H(A("likes", [Node("john", []); Node("pizza", [])])),
    B([A("likes", [Node("john", []); Node("pizza", [])])]));
  R(H(A("likes", [Node("mary", []); Node("pizza", [])])),
    B([A("likes", [Node("mary", []); Node("pizza", [])])]));
  R(H(A("likes", [Node("jane", []); Node("sushi", [])])),
    B([A("likes", [Node("jane", []); Node("sushi", [])])]));
  R(H(A("likes", [Node("mary", []); Node("sushi", [])])),
    B([A("likes", [Node("mary", []); Node("sushi", [])])]))
];;

(* Query 1 *)
let goal1 : goal = G([A("likes", [Node("john", []); Node("pizza", [])])]);;

(* Program 2 *)
let program2 : program = [
  R(H(A("likes_eating", [V("X"); V("Y")])),
    B([A("likes", [V("X"); V("Y")])]))
];;

(* Query 2 *)
let goal2 : goal = G([A("likes_eating", [Node("john", []); Node("pizza", [])])]);;

(* Program 3 *)
let program3 : program = [
  R(H(A("likes_eating", [V("X"); V("Y")])),
    B([A("likes", [V("X"); V("Y")])]))
];;

(* Query 3 *)
let goal3 : goal = G([A("likes_eating", [Node("john", []); Node("sushi", [])])]);;

                  
  

