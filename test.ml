let () =
  let lexbuf = Lexing.from_channel stdin in
  let env = Compiler.create () in
  let err = ref false in
  try
    UVa_parser.prog UVa_lexer.top lexbuf |>
    List.iter (function
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
          eprintf "%d:%s: too many values (%d > %d)\n" l n actual expected
        | Validator.Too_short ->
          err := true;
          eprintf "%d:%s: not enough data\n" l n
        | Validator.Inconsistent_data (p, {actual; expected}) ->
          err := true;
          let pp ppf a =
            let pp_sep ppf () = Format.pp_print_string ppf "; " in
            Array.to_list a |>
            fprintf ppf "[%a]" (pp_print_list ~pp_sep pp_print_int)
          in
          let pp' ppf =
            let pp_sep ppf () = Format.pp_print_char ppf '.' in
            pp_print_list ~pp_sep pp_print_string ppf
          in
          eprintf "%d:%a: expected %a, got %a\n" l pp' p pp expected pp actual
    );
    Format.(pp_print_flush err_formatter ());
    if !err then exit 2
  with
  | UVa_parser.Error ->
    Printf.eprintf "%d: syntax error\n" (lexbuf.Lexing.lex_curr_p.pos_lnum);
    exit 1
  | Failure _ -> (* FIXME? *)
    Printf.eprintf "%d: unexpected character\n" (lexbuf.Lexing.lex_curr_p.pos_lnum);
    exit 1
  | Compiler.Unbound_type n ->
    Printf.eprintf "%d: unbound type %s\n" (lexbuf.Lexing.lex_curr_p.pos_lnum) n;
    exit 3
