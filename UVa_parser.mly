%token <string> ID
%token IS
%token <int> LENGTH
%token NL
%token EQUAL
%token <int> NUM
%token EOF
%start <Ast.t> prog
%%

atom:
  | LENGTH { Ast.Length $1 }
  | ID { Ast.Ref $1 }
stmt:
  | name=ID; IS; parts=list(atom); NL { Ast.Definition {name; parts} }
  | name=ID; EQUAL; value=list(NUM); NL { Ast.Assignment (name, Array.of_list value) }
prog: list(stmt); EOF { $1 }
