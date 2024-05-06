type variable = string
type symbol = string
type signature = (symbol * int) list
type term = V of variable | Num of int | Node of symbol * (term list)
type atom = A of symbol * (term list)
type head = H of atom
type body = B of atom list
type clause = F of head | R of head * body
type program = clause list
type goal = G of atom list
type substitution = (variable * term) list

let make (n:int):string = String.make(2*n) ' ';;
let string_of_variable (n:int)(v: variable):string = (make n) ^"variable: "^v;;

let string_of_int1 (n:int)(v: int):string = (make n) ^"constant: "^(string_of_int v);;
let string_of_symbol (n:int)(v: symbol):string = (make n) ^"identifier: "^v;;
let rec string_of_term (n:int)(v: term):string = match v with
  |V v -> string_of_variable n v ^ "\n"
  |Num x -> string_of_int1 n x ^ "\n"
  |Node (symbol,term_list) -> let subterms = List.map (string_of_term (n+1)) term_list in
  (string_of_symbol n symbol) ^ "\n" ^ String.concat "\n" subterms ^ "\n"
let rec string_of_atom (n:int)(a: atom): string =
  match a with
  | A (symbol, term_list) ->
    let subterms = List.map (string_of_term (n+1)) term_list in
    (string_of_symbol n symbol) ^ "\n" ^ String.concat "\n" subterms ^ "\n"
  
let string_of_head (n:int)(h:head):string = match h with
  | H a -> "head:\n"^string_of_atom (n+1) a

let string_of_body (n:int)(b: body): string = match b with
  | B(atom_list) ->
    let atom_strings = List.map (string_of_atom n)atom_list in
    "body:\n" ^ String.concat "\n" atom_strings ^ "\n"
  
let string_of_clause (n:int)(cl:clause):string = match cl with 
  | F h -> (make n) ^"Fact:\n" ^ (make (n+1)) ^ string_of_head (n+1) h
  | R (h,b) -> (make n) ^"Rule:\n" ^ (make (n+1)) ^ (string_of_head (n+1) h) ^ (make (n+1)) ^ (string_of_body (n+2) b)

let rec string_of_program (n:int)(prog: program): string =
  match prog with
  | [] -> ""
  | clause_list ->
    let clause_strings = List.map (string_of_clause n)clause_list in
    String.concat "\n" clause_strings ^ "\n"
