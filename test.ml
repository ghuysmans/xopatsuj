let () =
  let lexbuf = Lexing.from_channel stdin in
  let env = Compiler.create () in
  UVa_parser.prog UVa_lexer.top lexbuf |>
  List.iter @@ function
    | Ast.Definition d ->
      Compiler.add env d
    | Ast.Assignment (t, _v) ->
      Printf.printf "assigned %s\n" t
