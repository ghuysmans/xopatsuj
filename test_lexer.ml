type token = [%import: UVa_parser.token] [@@deriving show {with_path = false}]

let () =
  let lexbuf = Lexing.from_channel stdin in
  let rec f () =
    let token = UVa_lexer.top lexbuf in
    Format.printf "%a\n" pp_token token;
    if token != EOF then f ()
  in
  f ();
  Format.(pp_print_flush std_formatter ())
