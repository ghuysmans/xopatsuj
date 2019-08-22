let () =
  let lexbuf = Lexing.from_channel stdin in
  let env = Compiler.create () in
  let defs, assgts = UVa_parser.prog UVa_lexer.top lexbuf in
  List.iter (Compiler.add env) defs;
  assgts |> List.iter @@ fun (_t, _v) ->
    ()
