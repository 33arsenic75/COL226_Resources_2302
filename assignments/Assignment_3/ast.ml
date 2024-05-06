type variable = string;;
type predicate = string;;
type constant = string;;
type term =
    | V of variable
    | C of constant
    | A of predicate * (term list);; (**)

type atomic_formula = A of predicate * (term list);; (**)
type head = atomic_formula;; (**)
type body = atomic_formula list;; (**)
type clause =
    | Fact of head
    | Rule of head*body (**)
type goal = body;; (**)
type program = clause list;; (**)

(* Convert a variable to string *)
let string_of_variable v = v

(* Convert a constant to string *)
let string_of_constant c = c

(* Convert a term to string *)
let rec string_of_term = function
  | V v -> string_of_variable v
  | C c -> string_of_constant c
  | A (p, tl) -> p ^ "(" ^ String.concat ", " (List.map string_of_term tl) ^ ")"

(* Convert a list of terms to string *)
and string_of_term_list tl = String.concat ", " (List.map string_of_term tl)

(* Convert an atomic formula to string *)
let string_of_atomic_formula (A (p, tl)) = p ^ "(" ^ string_of_term_list tl ^ ")"

(* Convert a head to string *)
let string_of_head hf = string_of_atomic_formula hf

(* Convert a body to string *)
let string_of_body bd = String.concat ", " (List.map string_of_atomic_formula bd)

(* Convert a clause to string *)
let string_of_clause = function
  | Fact hf -> "Fact: " ^ string_of_head hf
  | Rule (hf, bd) -> "Rule: " ^ string_of_head hf ^ " :- " ^ string_of_body bd

(* Convert a goal to string *)
let string_of_goal gl = String.concat ", " (List.map string_of_atomic_formula gl)

(* Convert a program to string *)
let string_of_program pr = String.concat "\n" (List.map string_of_clause pr)