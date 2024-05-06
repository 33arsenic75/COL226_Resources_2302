exception NEGATIVE_ARITY
exception NOT_UNIFIABLE
type symbol = string
type signature = (symbol * int) list
type tree = V of string | C of { node: symbol; children: tree list }
let rec check_duplicates symbols = match symbols with
  | [] -> false
  | hd :: tl -> List.mem hd tl || check_duplicates tl

let rec check_non_negative_arity (signature:signature) = match signature with
  | [] -> true
  | (_, arity) :: tl ->
    if arity < 0 then
      raise NEGATIVE_ARITY
    else
      check_non_negative_arity tl

let check_sig (signature : signature) : bool =
  let symbols = List.map fst signature in
  not (check_duplicates symbols) && check_non_negative_arity signature

let rec check_wftree (signature : signature) (t : tree) : bool =
  let rec check_arity sym =
    try
      List.assoc sym signature
    with Not_found -> 0
  in
  match t with
  | V var -> true
  | C { node; children } ->
    List.length children = check_arity node &&
    List.for_all (check_wftree signature) children
  
let wftree (signature : signature) (t : tree) : bool =
  check_sig signature && check_wftree signature t
  
let rec ht (t : tree) : int =
  match t with
  | V _ -> 0
  | C { children } -> 1 + List.fold_left (fun acc child -> max acc (ht child)) 0 children
  
let rec size (t : tree) : int =
  match t with
  | V _ -> 1
  | C { children } -> 1 + List.fold_left (fun acc child -> acc + size child) 0 children
  
let rec vars (t : tree) : string list =
  match t with
  | V var -> [var]
  | C { children } -> List.flatten (List.map vars children)

let rec mirror_tree (t : tree) : tree =
  match t with
  | V var -> V var
  | C { node; children } ->
    let reversed_children = List.rev_map mirror_tree children in
    C { node; children = reversed_children }
let mirror (t : tree) : tree =
  mirror_tree t

let print_tree a =
  let rec print_t indent t =
  match t with
  | V(s) -> Printf.printf "%s%s\n" indent s
  | C({node; children}) ->
    Printf.printf "%s%s\n" indent node;
    List.iter (print_t (indent ^ "  ")) children in
    print_t "" a

let rec subst sigma t =
  let apply_subst x =
    match List.assoc_opt x sigma with
    | Some replacement -> replacement
    | None -> V x
  in
  match t with
  | V x -> apply_subst x
  | C r ->
    let children' = List.map (fun child -> subst sigma child) r.children in
    C { node = r.node; children = children' }
;;

let compose_subst sigma1 sigma2 =
  let sigma1' = List.map (fun (x, t) -> (x, subst sigma2 t)) sigma1 in
  sigma2 @ sigma1'
;;

let rec contains_var x t =
  match t with
  | V y -> x = y
  | C r -> List.exists (contains_var x) r.children
;;

let rec occurs_check x t =
  match t with
  | V y -> x = y
  | C r -> List.exists (occurs_check x) r.children

let rec mgu t u =
  let rec unify s t1 t2 =
    match (subst s t1, subst s t2) with
    | V x, V y when x = y -> s  (* Identity substitution *)
    | V x, V y -> if occurs_check x t2 then raise NOT_UNIFIABLE else compose_subst s [(x, V y)]
    | C r1, C r2 when r1.node = r2.node ->
      List.fold_left2 (unify) s r1.children r2.children
    | V x, t | t, V x ->
      if occurs_check x t then
        raise NOT_UNIFIABLE
      else
        compose_subst s [(x, t)]
    | _ -> raise NOT_UNIFIABLE
  in
  try
    unify [] t u
  with
  | Invalid_argument _ | Not_found -> raise NOT_UNIFIABLE
;;

let rec string_of_tree t =
  match t with
  | V x -> x
  | C r -> "(" ^ r.node ^ "[" ^ String.concat "; " (List.map string_of_tree r.children) ^ "])"

let string_of_sigma sigma =
  let pair_to_string (x, t) = x ^ " -> " ^ string_of_tree t in
  "[" ^ String.concat "; " (List.map pair_to_string sigma) ^ "]"

let test_mgu (n:int)t1 t2 =
  try
    let sigma = mgu t1 t2 in
    Printf.printf "Test Case: %d\nUnification successful.\nSigma:%s\n\n" (n)(string_of_sigma sigma)
  with NOT_UNIFIABLE ->
    Printf.printf "Test Case: %d\nNot unifiable.\n\n" (n);;

let same_sigma sigma1 sigma2 =
  let sort_and_equal s1 s2 =
    List.sort (fun (x1, _) (x2, _) -> String.compare x1 x2) s1 =
    List.sort (fun (x1, _) (x2, _) -> String.compare x1 x2) s2
  in
  sort_and_equal sigma1 sigma2
;;

let test_sign (n:int) sign =
  try
    let f = check_sig sign  in
    if f then 
      Printf.printf "Test Case: %d\nValid Signature\n\n" (n)
    else
       Printf.printf "Test Case: %d\nInvalid Signature\n\n" (n)
  with NEGATIVE_ARITY ->
    Printf.printf "Test Case: %d\nNegative Arity\n\n" (n);;

let test_wftree (n:int) sign t =
  try
    let f = check_wftree sign t  in
    if f then 
      Printf.printf "Test Case: %d\nValid Tree\n\n" (n)
    else
       Printf.printf "Test Case: %d\nInvalid Tree\n\n" (n)
  with NEGATIVE_ARITY ->
    Printf.printf "Test Case: %d\nInvalid Tree\n\n" (n);;

let test_ht_size_vars (n:int) t =
  let vars_str = String.concat ", " (vars t) in
  Printf.printf "TestCase %d\nHeight: %d\nSize: %d\nVars: %s\n\n" n (ht t) (size t) vars_str

let test_compo n sub1 sub2 =
  Printf.printf "Testcase %n\n %s\n\n" n (string_of_sigma (compose_subst sub1 sub2));
;;

let test_subst n sub t =
  Printf.printf "Testcase %n\n %s\n\n" n (string_of_tree (subst sub t));
;;  
let signature1:signature = [("+",2);("*",2);("1",0);("0",0);("/",0)]
let tree1: tree = (* (x+1)+0 *)
C {
  node = "+";
  children = [
    C {
      node = "+";
      children = [V "x"; C {node="1";children =[]}];
    };
    C {node="0";children =[]}
  ];
}

let signature2: signature = [("+", 2); ("*", 2); ("1", 0); ("0", 0); ("/", 0)]
let tree2: tree = (* 1 + (0*x) *)
 C {
   node = "+";
   children = [
    C { node = "1"; children = [] };  
    C { node = "*"; children = [C { node = "0"; children = [] };V "x"] }] 
}

let signature3: signature = [("-", 2); ("sin", 1); ("2", 0); ("/", 2); ("cos", 1)]
let tree3: tree =(*  (y/sin(cos(z))) - 2 *)
C { 
  node = "-"; 
  children = [C { node = "/";
                   children = [
                              V "y";  
                              C { node = "sin"; 
                                  children = [C { node = "cos"; 
                                                  children = [V "z"] }] }] };   
              C { node = "2"; children = [] }] }

;;
(* Case 1: Variables, same variable, identity substitution *)
let t1:tree = V "x"
let t2:tree = V "x"

(* Case 2: Variables, different variables, substitution {x -> y} *)
let t3:tree = V "x"
let t4:tree = V "y"

(* Case 3: Variable and non-variable, substitution {x -> C("f", [])} *)
let t5:tree = V "x"
let t6:tree = C { node = "f"; children = [] }

(* Case 4: Non-variables, same structure, identity substitution *)
let t7:tree = C { node = "g"; children = [V "a"; V "b"] }
let t8:tree = C { node = "g"; children = [V "a"; V "b"] }

(* Case 5: Non-variables, different structures, not unifiable *)
let t9:tree= C { node = "h"; children = [V "x"; V "y"] }
let t10:tree = C { node = "h"; children = [V "z"; V "w"] }

(* Case 6: Nested variables, not unifiable due to occurs-check *)
let t11:tree = C { node = "i"; children = [V "x"] }
let t12:tree = C { node = "i"; children = [C {node = "y"; children = [V "x"] }] }

(* Case 7: Nested variables,  not unifiable due to different number of children*)
let t13:tree = C { node = "i"; children = [V "x"] }
let t14:tree = C { node = "i"; children = [V "y"; V "z"] }

(* Case 8: Multiple nested variables, unifiable, substitution {x -> C("i", [V "y"]), y -> V "z"} *)
let t15:tree = C { node = "i"; children = [V "x"; V "y"] }
let t16:tree = C { node = "i"; children = [C { node = "i"; children = [V "z"] }; V "w"] }

let subst1 = [("x", V "y");("z",V "a")]
let subst2 = [("y", t7)]
let subst3 = [("y", tree1); ("c", tree2)]
let subst4 = [("z", tree3); ("x", t1); ("u", V "v")]
let subst5 = [("p", V "q"); ("r", t11); ("t", V "u")]

let testsignature ()= 
  test_sign 1 signature1;
  test_sign 2 signature2;
  test_sign 3 signature3;
;;

let testwftree ()= 
  test_wftree 1  signature1 tree1;
  test_wftree 2  signature2 tree2;
  test_wftree 3  signature3 tree3;
;;
let testHtSizeVars ()= 
  test_ht_size_vars 1 tree1;
  test_ht_size_vars 2 tree2;
  test_ht_size_vars 3 tree3;
  test_ht_size_vars 4 t1;
  test_ht_size_vars 5 t2;
  test_ht_size_vars 6 t3;
  test_ht_size_vars 7 t4;
  test_ht_size_vars 8 t5;
  test_ht_size_vars 9 t6;
  test_ht_size_vars 10 t7;
  test_ht_size_vars 11 t8;
  test_ht_size_vars 12 t9;
  test_ht_size_vars 13 t10;
  test_ht_size_vars 14 t11;
  test_ht_size_vars 15 t12;
  test_ht_size_vars 16 t13;
  test_ht_size_vars 17 t14;
  test_ht_size_vars 18 t15;
  test_ht_size_vars 19 t16;
;;

let testmgu ()= 
  test_mgu 1 t1 t2;
  test_mgu 2 t3 t4;
  test_mgu 3 t5 t6;
  test_mgu 4 t7 t8;
  test_mgu 5 t9 t10;
  test_mgu 6 t11 t12;
  test_mgu 7 t13 t14;
  test_mgu 8 t15 t16;
;;

let testCompoSubst () = 
  test_compo 1 subst1 subst2;
  test_compo 2 subst1 subst3;
  test_compo 3 subst1 subst4;
  test_compo 4 subst1 subst5;
  test_compo 5 subst2 subst3;
  test_compo 6 subst2 subst4;
  test_compo 7 subst2 subst5;
  test_compo 8 subst3 subst4;
  test_compo 9 subst3 subst5;
  test_compo 10 subst4 subst5;
  test_compo 11 subst2 subst1;
  test_compo 12 subst3 subst1;
  test_compo 13 subst4 subst1;
  test_compo 14 subst5 subst1;
  test_compo 15 subst3 subst2;
  test_compo 16 subst4 subst2;
  test_compo 17 subst5 subst2;
  test_compo 18 subst4 subst3;
  test_compo 19 subst5 subst3;
  test_compo 20 subst5 subst4;
;;

let testSubst () =
  test_subst 1 subst1 t1;
  test_subst 2 subst1 t2;
  test_subst 3 subst1 t3;
  test_subst 4 subst1 t4;
  test_subst 5 subst1 t5;
  test_subst 6 subst1 t6;
  test_subst 7 subst1 t7;
  test_subst 8 subst1 t8;
  test_subst 9 subst1 t9;
  test_subst 10 subst1 t10;
  test_subst 11 subst1 t11;
  test_subst 12 subst1 t12;
  test_subst 13 subst1 t13;
  test_subst 14 subst1 t14;
  test_subst 15 subst1 t15;
  test_subst 16 subst1 t16;

  test_subst 17 subst2 t1;
  test_subst 18 subst2 t2;
  test_subst 19 subst2 t3;
  test_subst 20 subst2 t4;
  test_subst 21 subst2 t5;
  test_subst 22 subst2 t6;
  test_subst 23 subst2 t7;
  test_subst 24 subst2 t8;
  test_subst 25 subst2 t9;
  test_subst 26 subst2 t10;
  test_subst 27 subst2 t11;
  test_subst 28 subst2 t12;
  test_subst 29 subst2 t13;
  test_subst 30 subst2 t14;
  test_subst 31 subst2 t15;
  test_subst 32 subst2 t16;

  test_subst 33 subst3 t1;
  test_subst 34 subst3 t2;
  test_subst 35 subst3 t3;
  test_subst 36 subst3 t4;
  test_subst 37 subst3 t5;
  test_subst 38 subst3 t6;
  test_subst 39 subst3 t7;
  test_subst 40 subst3 t8;
  test_subst 41 subst3 t9;
  test_subst 42 subst3 t10;
  test_subst 43 subst3 t11;
  test_subst 44 subst3 t12;
  test_subst 45 subst3 t13;
  test_subst 46 subst3 t14;
  test_subst 47 subst3 t15;
  test_subst 48 subst3 t16;

  test_subst 49 subst4 t1;
  test_subst 50 subst4 t2;
  test_subst 51 subst4 t3;
  test_subst 52 subst4 t4;
  test_subst 53 subst4 t5;
  test_subst 54 subst4 t6;
  test_subst 55 subst4 t7;
  test_subst 56 subst4 t8;
  test_subst 57 subst4 t9;
  test_subst 58 subst4 t10;
  test_subst 59 subst4 t11;
  test_subst 60 subst4 t12;
  test_subst 61 subst4 t13;
  test_subst 62 subst4 t14;
  test_subst 63 subst4 t15;
  test_subst 64 subst4 t16;

  test_subst 65 subst5 t1;
  test_subst 66 subst5 t2;
  test_subst 67 subst5 t3;
  test_subst 68 subst5 t4;
  test_subst 69 subst5 t5;
  test_subst 70 subst5 t6;
  test_subst 71 subst5 t7;
  test_subst 72 subst5 t8;
  test_subst 73 subst5 t9;
  test_subst 74 subst5 t10;
  test_subst 75 subst5 t11;
  test_subst 76 subst5 t12;
  test_subst 77 subst5 t13;
  test_subst 78 subst5 t14;
  test_subst 79 subst5 t15;
  test_subst 80 subst5 t16;
;;

let sign1:signature = [("0", 0); ("1", 0); ("0", 1)];;
let sign2:signature = [("0", 0); ("1", 0); ("+", 2)];;

let t1:tree = C {node = "+"; children = [(V "x"); (V "y"); (V "z")]} ;;

let t2:tree = C {node = "+"; children = [(V "x"); (V "y")]} ;;

let t3 = C {node = "+"; children = [(V "z"); t2]} ;;

let () = 
         (* testsignature();
         testwftree();
         testHtSizeVars();
         testmgu();
         testCompoSubst();
         testSubst(); *)
         test_sign 1 sign1;
         test_wftree 2 sign2 t1;
         test_ht_size_vars 3 t2;
         print_tree (mirror t3);
         Printf.printf"\ntestingmgu\n\n";
         testmgu();



;; 


