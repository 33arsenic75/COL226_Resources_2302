{
  open Token
  open List
}

(* let digit = []['0'-'9']+ *)
let digit = ['0'-'9']
let letterSmall = ['a'-'z']
let letterCapital = ['A'-'Z']
let V = letterCapital (letterSmall | letterCapital | digit | '_')*
let C = (letterSmall | digit) (letterSmall | letterCapital | digit | '_')*

rule tokenize = parse
  | [' ' '\t' '\n']+ { tokenize lexbuf }
  | V as id       { V (id) }
  | C as cons       { C (cons) }
  |'.' { END }
  |',' { COMMA }
  |'=' { EQU }
  |("\\=" | "\\==") {NOTEQ}
  |'(' { LP }
  |')' { RP }
  | ']' {RSQ}
  | '[' {LSQ}
  | "not" {NOT}
  | '|' {OR}
  | ":-" {SEP}
  | eof  { EOF }
  | _ { UNRECOGNIZED }
