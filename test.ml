let () =
  let lexbuf = Lexing.from_channel stdin in
  let env = Compiler.create () in
  UVa_parser.prog UVa_lexer.top lexbuf |>
  List.iter @@ function
    | Ast.Definition d ->
      Compiler.add env d
    | Ast.Assignment (t, v) ->
      let t = Compiler.find env t in
      ignore (Validator.unify t v)
