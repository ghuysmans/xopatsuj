%token <string * int> ID
%token IS
%token <int> LENGTH
%token NL
%token EQUAL
%token <int> NUM
%token EOF
%start <Ast.t> prog
%%

atom:
  | LENGTH { Ast.Empty $1 }
  | ID { Ast.Ref (fst $1) }
stmt:
  | n=ID; IS; p=list(atom); NL { Ast.Definition {name = fst n; parts = Array.of_list p} }
  | name=ID; EQUAL; value=list(NUM); NL { Ast.Assignment (name, Array.of_list value) }
prog: list(stmt); EOF { $1 }
