open Ast

let () =
  let lexbuf = Lexing.from_channel stdin in
  try
    while true do
      UVa_parser.stmt UVa_lexer.top lexbuf |>
      Format.printf "%a\n" pp
    done
  with End_of_file ->
    Format.(pp_print_flush std_formatter ())
