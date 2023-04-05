open! Base

type t =
  { col : int
  ; row : int
  }
[@@deriving compare, sexp]

let equal pos0 pos1 =
  Int.equal pos0.col pos1.col && Int.equal pos0.row pos1.row 

let to_string pos =
  "(col=" ^ (Int.to_string pos.col) ^ ",row=" ^ (Int.to_string pos.row) ^ ")"
