type 'r atom =
  | Length of int
  | Ref of 'r
  [@@deriving show {with_path = false}]

type 'r def = {
  name: string;
  (* is *)
  parts: 'r atom list;
} [@@deriving show {with_path = false}]

type t =
  string def list * (string * int array) list
  [@@deriving show {with_path = false}]
