open Ast

type 'a m = {actual: 'a; expected: 'a}
exception Too_long of int m
exception Too_short
exception Inconsistent_data of string * int array m

let foldi_left f init a =
  Array.fold_left (fun (i, acc) x -> i + 1, f ~i acc x) (0, init) a |> snd

let unify typ value =
  let rec f start (Compiler.U {name; parts}) = (* returns an index *)
    parts |> foldi_left (fun ~i pos -> function
      | Empty len ->
        if pos < Array.length value && pos + len <= Array.length value then
          parts.(i) <- Assigned (Array.sub value pos len)
        else
          raise Too_short;
        pos + len
      | Assigned expected ->
        let len = Array.length expected in
        if pos < Array.length value && pos + len <= Array.length value then
          let actual = Array.sub value pos len in
          if expected = actual then
            pos + Array.length expected
          else
            raise (Inconsistent_data (name, {expected; actual}))
        else
          raise Too_short
      | Ref u ->
        f pos u
    ) start
  in
  let actual = f 0 typ in
  let expected = Array.length value in
  if actual <> expected then raise (Too_long {expected; actual})


(*
let () =
  List.iter (check Env.sample) [
    (*
    "A", [| 10 |];
    "B", [| 1; 2; 3 |];
    "C", [| 1; 1; 2; 3; 4 |];
    *)
    (*        1  (A) 1  1  1  1  A  (B  B  B) *)
    "SEQ", [| 1; 10; 1; 1; 1; 1; 10; 2; 3; 9; 2; 3; 9; 11 |];
    (*
    "SEQ", [| 1; 10; 1; 1; 1; 1; 2; 2; 2; 10 |];
    *)
  ]
*)
