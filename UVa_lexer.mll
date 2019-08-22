{
  let next_line lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- {
      pos with Lexing.pos_bol = lexbuf.Lexing.lex_curr_pos;
               Lexing.pos_lnum = pos.Lexing.pos_lnum + 1
    }
}

let id = ['A'-'Z' 'a'-'z']+
let num = ['0'-'9']+
let newline = '\n'

rule top = parse
| [' ' '\t'] { top lexbuf }
| '.' newline+ { next_line lexbuf; UVa_parser.NL }
| "is" { UVa_parser.IS }
| id as id { UVa_parser.ID (id, lexbuf.lex_curr_p.pos_lnum) }
| '[' (num as num) ']' { UVa_parser.LENGTH (int_of_string num) }
| "=" { UVa_parser.EQUAL }
| num as num { UVa_parser.NUM (int_of_string num) }
| eof { UVa_parser.EOF }
