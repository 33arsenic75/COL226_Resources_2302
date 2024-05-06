open Lexer;;
open Parser;;
open Prolog;;


if Array.length Sys.argv > 2 then (
  print_string "Too many arguments.\nExiting...\n";
  exit 0
) else if Array.length Sys.argv < 2 then (
  print_string "Input file not provided.\nExiting...\n";
  exit 0
) else ()


let initial_program = 
  let input_file = open_in Sys.argv.(1) in 
  Parser.program Lexer.read (Lexing.from_channel input_file);;

  let _ = valid_program initial_program;;
  let program:program = change_program_int 1 initial_program ;;  
let welcome =
  let hi =  "__________               _________                .__ \n"  ^
    "\\______   \\_______  ____ \\_   ___ \\_____    _____ |  |\n"  ^
    " |     ___/\\_  __ \\/  _ \\/    \\  \\/\\__  \\  /     \\|  |\n"  ^
    " |    |     |  |\\_|  <_> |     \\____/ __ \\|  Y Y  \\  |__\n" ^
    " |____|     |__|   \\____/ \\______  (____  /__|_|  /____/\n" ^
    "                                 \\/     \\/      \\/      by Abhinav Rajesh Shripad\n" in 
    print_string hi;;
    
  try
    while(true) do
      print_string ">> ";
      let line = read_line() in
      if (line = "quit" || line = "exit") then exit 0
      else try
        let g = Parser.goal Lexer.read (Lexing.from_string line) in
        match (prolog program g) with
          |  (true , _) -> print_string "true\n";
          |  (false, _) -> print_string "false\n";
      with e -> Printf.printf "%s\n" (Printexc.to_string e)
    done
  
  with _ -> print_string "\n% halt\n"
