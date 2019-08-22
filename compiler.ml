open Ast

type u = U of u def
type t = (string, u) Hashtbl.t

exception Unbound_type of string

let find env name =
  try
    Hashtbl.find env name
  with Not_found ->
    raise (Unbound_type name)

let link env = function
  | Ref name -> Ref (find env name)
  | Empty _ as l -> l
  | Assigned _ as a -> a

exception Redefinition of string

let add env {name; parts} =
  if Hashtbl.mem env name then
    (* avoid shadowing *)
    raise (Redefinition name)
  else
    let parts = Array.map (link env) parts in
    Hashtbl.add env name (U {name; parts})

let create () =
  Hashtbl.create 10


let sample =
  let env = create () in
  List.iter (add env) [
    {name = "A"; parts = [|Empty 1|]};
    {name = "B"; parts = [|Empty 3|]};
    {name = "C"; parts = [|Ref "A"; Ref "B"|]};
    {name = "SEQ"; parts = [|Empty 1; Ref "A"; Empty 4; Ref "C"; Ref "B"; Ref "A"|]};
  ];
  env
