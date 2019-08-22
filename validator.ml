open Ast

type 'a m = {actual: 'a; expected: 'a}
exception Mismatch of int array m
exception Too_short
exception Too_long of int m

let unify (env: Env.t) (typ, value) =
  let substs = Hashtbl.create 10 (* FIXME? *) in
  let expected = Array.length value in
  let rec f i {name; parts} = (* returns an index *)
    if Hashtbl.mem substs name then
      let offset, length = Hashtbl.find substs name in
      if i + length > expected || offset + length > expected then
        raise Too_short
      else
        let actual = Array.sub value i length in
        let expected = Array.sub value offset length in
        if actual = expected then
          i + length
        else
          raise (Mismatch {actual; expected})
    else
      let i' = List.fold_left (fun i (U x) ->
        match x with
        | Ref u -> f i u
        | Length length ->
            v := Content {offset = i; length};
            i + length
      ) i parts in
      Hashtbl.add substs def (i, i' - i)
  in
  let u =
    match find env typ with
    | Value u -> !u
    | Ref r -> r
  in
  let actual = f 0 u in
  if actual = expected then
    ()
  else
    raise (Too_long {actual; expected})


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
