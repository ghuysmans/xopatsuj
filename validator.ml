open Ast

type 'a m = {actual: 'a; expected: 'a}
exception Too_long of int m
exception Too_short
exception Inconsistent_data of (Compiler.u def * int) list * [`Pos of int] m

let foldi_left f init a =
  Array.fold_left (fun (i, acc) x -> i + 1, f ~i acc x) (0, init) a |> snd

let unify typ value =
  let rec f stack start (Compiler.U ({parts; _} as d)) = (* returns an index *)
    let h = Hashtbl.create (Array.length parts) in
    parts |> foldi_left (fun ~i pos -> function
      | Length len ->
        if pos >= Array.length value || pos + len > Array.length value then
          raise Too_short;
        pos + len
      | Ref u ->
        let pos' = f ((d, i) :: stack) pos u in (* TODO dedicated length? *)
        (match Hashtbl.find_opt h u with
        | None ->
          Hashtbl.replace h u pos
        | Some o ->
          let len = pos' - pos in
          for i = 0 to len - 1 do
            if value.(pos + i) <> value.(o + i) then
              let expected = `Pos (o + i) in
              let actual = `Pos (pos + i) in
              raise (Inconsistent_data (List.rev ((d, i) :: stack), {expected; actual}))
          done);
        pos'
    ) start
  in
  let expected = f [] 0 typ in
  let actual = Array.length value in
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
