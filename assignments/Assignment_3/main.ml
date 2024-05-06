open Lexer
open Token
open Lexing
open Ast
open Parser
let rec string_of_token = function
  | V id -> "Variable " ^ id
  | C c -> "Constant " ^ c
  | LP -> "LP"
  | RP -> "RP"
  | COMMA -> "COMMA"
  | END -> "END"
  | UNRECOGNIZED -> "ERROR"
  | EOF -> "EOF"
  | EQU -> "EQU"
  | NOTEQ -> "NOTEQ"
  | LSQ -> "LSQ"
  | RSQ -> "RSQ"
  | NOT -> "NOT"
  | OR -> "OR"
  | SEP -> "SEP"
  | P p -> "Predicate " ^ p
let print_token token =
  print_endline (string_of_token token)
    
let print_lexbuf_info lexbuf =
  let position = lexbuf.lex_curr_p in
  Printf.printf "Current position: line %d, character %d\n"
    position.pos_lnum (position.pos_cnum - position.pos_bol + 1)


let rec tokenize_all lexbuf acc =
  match Lexer.tokenize lexbuf with
  | EOF -> List.rev acc
  | token -> tokenize_all lexbuf (token :: acc)

let tokenize_string input_string =
  let lexbuf = Lexing.from_string input_string in
  tokenize_all lexbuf []

(* let get_result str = 
  Printf.printf "\nINPUT:---> %s\n" str;
  let tok = tokenize_string str in 
  List.iter (fun token -> print_endline (string_of_token token)) (convert_tokens tok) *)
 
let convert_tokens tokens =
  let convert_token = function
    | Token.V s -> Parser.V s
    | Token.C s -> Parser.C s
    | Token.P s -> Parser.P s
    | Token.EQU -> Parser.EQU
    | Token.NOTEQ -> Parser.NOTEQ
    | Token.RSQ -> Parser.RSQ
    | Token.LSQ -> Parser.LSQ
    | Token.NOT -> Parser.NOT
    | Token.OR -> Parser.OR
    | Token.SEP -> Parser.SEP
    | Token.UNRECOGNIZED -> Parser.UNRECOGNIZED
    | Token.LP -> Parser.LP
    | Token.RP -> Parser.RP
    | Token.COMMA -> Parser.COMMA
    | Token.END -> Parser.END
    | Token.EOF -> Parser.EOF
  in
  List.map convert_token tokens

let convert_token = function
  | Token.V s -> Parser.V s
  | Token.C s -> Parser.C s
  | Token.P s -> Parser.P s
  | Token.EQU -> Parser.EQU
  | Token.NOTEQ -> Parser.NOTEQ
  | Token.RSQ -> Parser.RSQ
  | Token.LSQ -> Parser.LSQ
  | Token.NOT -> Parser.NOT
  | Token.OR -> Parser.OR
  | Token.SEP -> Parser.SEP
  | Token.UNRECOGNIZED -> Parser.UNRECOGNIZED
  | Token.LP -> Parser.LP
  | Token.RP -> Parser.RP
  | Token.COMMA -> Parser.COMMA
  | Token.END -> Parser.END
  | Token.EOF -> Parser.EOF
let get_result str = 
  let lexbuf = Lexing.from_string str in 
  let pr = Parser.program (fun x -> Printf.printf "1\n";convert_token (Lexer.tokenize x)) lexbuf in 
  Printf.printf ""

let () = 
  get_result "123";

