open Ast

let () =
  let lexbuf = Lexing.from_channel stdin in
  let stmts = UVa_parser.prog UVa_lexer.top lexbuf in
  Format.printf "%a\n" pp stmts
