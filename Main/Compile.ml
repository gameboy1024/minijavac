open LexicalAnalyser
open SyntaxAnalyser
open Expression

let treat_expression exp = 
  print_string (string_of_expr exp);
  print_string " = ";
  try
    print_string (string_of_value (eval [] exp));
    print_newline()
  with Unbound_variable s ->
    print_endline ("Variable "^s^" inconnue!")
     | Wrong_types_bop(op, x, y) ->
    print_string ("L'operateur "^(string_of_op_b op));
    print_string (" attend deux arguments de type "^(string_type_of_op_b op));
    print_string (" et il reçoit "^(string_of_type x));
    print_endline (" et "^(string_of_type y))
     | Wrong_types_uop(op, x) ->
    print_string ("L'operateur "^(string_of_op_u op));
    print_string (" attend un argument de type "^(string_type_of_op_u op));
    print_endline (" et il reçoit "^(string_of_type x))

(* verbose is a boolean that you can use to switch to a verbose output (for example, to dump all the ast) *)
let execute lexbuf verbose = 
  print_endline "parsing todo";
  print_endline("verbose=" ^ string_of_bool(verbose));
  (*LexicalAnalyser.examine_all lexbuf*)
  
  let exp_list = expressions nexttoken lexbuf in
    List.iter treat_expression exp_list
