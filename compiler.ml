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
  | Length _ as l -> l

exception Redefinition of string

let add env {name; parts} =
  if Hashtbl.mem env name then
    (* avoid shadowing *)
    raise (Redefinition name)
  else
    let parts = List.map (link env) parts in
    Hashtbl.add env name (U {name; parts})

let create () =
  Hashtbl.create 10


let sample =
  let env = create () in
  List.iter (add env) [
    {name = "A"; parts = [Length 1]};
    {name = "B"; parts = [Length 3]};
    {name = "C"; parts = [Ref "A"; Ref "B"]};
    {name = "SEQ"; parts = [Length 1; Ref "A"; Length 4; Ref "C"; Ref "B"; Ref "A"]};
  ];
  env
