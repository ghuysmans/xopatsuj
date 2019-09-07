type 'r atom =
  | Length of int
  | Ref of 'r
  [@@deriving show {with_path = false}]

type 'r def = {
  loc: int;
  name: string;
  (* is *)
  parts: 'r atom array;
} [@@deriving show {with_path = false}]

type t =
  | Definition of string def
  | Assignment of (string * int) * int array
  [@@deriving show {with_path = false}]
