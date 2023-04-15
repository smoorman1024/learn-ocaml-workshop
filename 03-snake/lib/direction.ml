open! Base

type t =
  | Left
  | Up
  | Right
  | Down
[@@deriving sexp_of]

let next_position t (p : Position.t) =
  match t with
  | Left  -> { p with col = p.col - 1 }
  | Right -> { p with col = p.col + 1 }
  | Up    -> { p with row = p.row + 1 }
  | Down  -> { p with row = p.row - 1 }

let%test "Testing Left..." =
  let p0 : Position.t = {row=5; col=5} in
  let np0 : Position.t = next_position Left p0 in
  Int.equal np0.row 5 && Int.equal np0.col 4

let%test "Testing Right..." =
  let p0 : Position.t = {row=5; col=5} in
  let np0 : Position.t = next_position Right p0 in
  Int.equal np0.row 5 && Int.equal np0.col 6

let%test "Testing Up..." =
  let p0 : Position.t = {row=5; col=5} in
  let np0 : Position.t = next_position Up p0 in
  Int.equal np0.row 6 && Int.equal np0.col 5

let%test "Testing Down..." =
  let p0 : Position.t = {row=5; col=5} in
  let np0 : Position.t = next_position Down p0 in
  Int.equal np0.row 4 && Int.equal np0.col 5
