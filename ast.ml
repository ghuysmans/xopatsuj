type 'r atom =
  | Empty of int
  | Assigned of int array
  | Ref of 'r
  [@@deriving show {with_path = false}]

type 'r def = {
  name: string;
  (* is *)
  parts: 'r atom array;
} [@@deriving show {with_path = false}]

type stmt =
  | Definition of string def
  | Assignment of (string * int) * int array
  [@@deriving show {with_path = false}]

type t = stmt list [@@deriving show {with_path = false}]
