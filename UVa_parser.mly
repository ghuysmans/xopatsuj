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
def: name=ID; IS; parts=list(atom); NL { Ast.{name; parts} }
assgt: name=ID; EQUAL; value=list(NUM); NL { (name, Array.of_list value) }
prog: list(def); list(assgt); EOF { $1, $2 }
