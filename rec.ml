open Ast

(*
I can't match the type without using recursive tuples or GADTs, but I want
to do it at runtime, so this would be utterly useless.
*)

type u = (int, u) atom list

let rec rcount l =
  List.map (function
    | Value i -> i
    | Ref x -> rcount x
  ) l |>
  List.fold_left (+) 0

let c    = [Ref [Value 1]; Ref [Ref [Value 2]]]
let t: u = [Ref [Value 1]; Ref [Ref [Value 2]]] (* lost count *)
