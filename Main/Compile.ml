let execute lexbuf verbose = 
  try 
    let ast = Parser.start Lexer.token lexbuf in
    print_endline "successfull parsing";
    if verbose then AST.print_program ast;
    print_endline "typing todo";
    Typing.typing ast
  with 
    | Parser.Error ->
      print_string "Syntax error: ";
      Location.print (Location.curr lexbuf)
    | Error.Error(e,l) ->
      Error.report_error e;
      Location.print l
