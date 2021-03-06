let () =
  let lexbuf = Lexing.from_channel stdin in
  let env = Compiler.create () in
  let err = ref false in
  try
    while true do
      match UVa_parser.stmt UVa_lexer.top lexbuf with
      | Ast.Definition d ->
        Compiler.add env d
      | Ast.Assignment ((n, l), v) ->
        let t = Compiler.find env n in
        let open Format in
        try
          Validator.unify t v
        with
        | Validator.Too_long {actual; expected} ->
          err := true;
          eprintf "%d: Too many values (%d > %d)\n" l actual expected
        | Validator.Too_short ->
          err := true;
          eprintf "%d: Not enough data\n" l
        | Validator.Inconsistent_data (p, {actual; expected}) ->
          err := true;
          let pp ppf (`Pos p) = fprintf ppf "%d@%d" v.(p) p in
          let pp' ppf =
            let pp_sep ppf () = Format.pp_print_string ppf " -> " in
            let pp ppf (Ast.{name; _}, i) = fprintf ppf "%s.(%d)" name i in
            pp_print_list ~pp_sep pp ppf
          in
          eprintf "%d: In %a: expected %a, got %a\n" l pp' p pp expected pp actual
    done
  with
  | End_of_file ->
    Format.(pp_print_flush err_formatter ());
    if !err then exit 2
  | UVa_parser.Error ->
    Printf.eprintf "%d: syntax error\n" (lexbuf.Lexing.lex_curr_p.pos_lnum);
    exit 1
  | Failure _ -> (* FIXME? *)
    Printf.eprintf "%d: unexpected character\n" (lexbuf.Lexing.lex_curr_p.pos_lnum);
    exit 1
  | Compiler.Unbound_type n ->
    Printf.eprintf "%d: unbound type %s\n" (lexbuf.Lexing.lex_curr_p.pos_lnum) n;
    exit 3
