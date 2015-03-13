open LexicalAnalyser
open SyntaxAnalyser
open Expression

let treat_file_content exp = 
  (* Print out AST *)
  print_string (string_of_class_or_expr exp)
  (* TODO: typing, evaluation *)

(* verbose is a boolean that you can use to switch to a verbose output (for example, to dump all the ast) *)
let execute lexbuf verbose = 
  print_endline("verbose=" ^ string_of_bool(verbose));
  print_endline("Lexical Analyser running...");
  try 
(*
    LexicalAnalyser.examine_all lexbuf;
    print_string "\n";
*)
    print_endline("Syntax Analyser running...");
    let exp_list = SyntaxAnalyser.file_content nexttoken lexbuf in
      print_endline("Start printing AST...");
      List.iter treat_file_content exp_list;
      print_newline()
  with
    | LexicalAnalyser.Error (kind, s, e) -> print_string("Parsing error: ");
        (* TODO: change this error handling to use Location.* *)
        report_error kind;
        print_position s e;
        print_newline()
    | Error -> 
        print_string("Syntax error: ");
        let pos = Location.curr lexbuf in 
                Location.print pos;
    | _ -> print_string("Unknown error")
