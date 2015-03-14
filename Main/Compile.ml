let execute lexbuf verbose = 
  try
    print_endline "Parsing started...";
    let ast = Parser.start Lexer.token lexbuf in
    print_endline "Parsing successful";
    if verbose then AST.print_program ast;
    print_endline "Typing started...";
    let t_ast = Typing.typing ast in
    print_endline "Typing successful"
  with 
    | Parser.Error ->
      print_string "Syntax error: ";
      Location.print (Location.curr lexbuf)
    | TypingError.Error(e,l) ->
      print_string "Typing error: ";
      TypingError.report_error e;
      Location.print l
    | Error.Error(e,l) ->
      Error.report_error e;
      Location.print l
