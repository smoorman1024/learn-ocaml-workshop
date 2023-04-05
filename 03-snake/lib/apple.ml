open! Base

type t = { location : Position.t } [@@deriving sexp_of]

let location t = t.location


let rec create ~height ~width ~invalid_locations =
  Random.self_init ~allow_in_tests:true ();
  let uniqCount = List.length (List.dedup_and_sort ~compare:Position.compare invalid_locations) in
  if uniqCount >= (height * width)
  then
    None
  else
    let locx = Random.int width in
    let locy = Random.int height in
    let loc : Position.t =
      { col = locx
      ; row = locy
      } in
    let pos : t =
      { location = loc
      } in
    Stdio.printf "width=%d height=%d locx=%d locy=%d\n" width height locx locy;
    if List.mem invalid_locations loc ~equal:Position.equal
    then
      create ~height:height ~width:width ~invalid_locations:invalid_locations
    else
      Some pos
;;

let%test "Testing 1 plus 1 ..." = Int.equal 2 (1 + 1)

let%test "Testing apple create..." =
  let bound = 3 in
  let spawnLoc = create ~height:bound ~width:bound ~invalid_locations:[] in
  match spawnLoc with
  | None -> false
  | Some t ->
    Stdio.printf "spawnLoc=%s\n" (Position.to_string t.location);
    t.location.col >= 0 && t.location.col < bound && t.location.row >= 0 && t.location.row < bound

let%test "Testing no valid apples..." =
  let bound = 2 in
  let pos00 : Position.t = {row=0; col=0} in
  let pos01 : Position.t = {row=0; col=1} in
  let pos10 : Position.t = {row=1; col=0} in
  let pos11 : Position.t = {row=1; col=1} in
  let invalidLocs = [ pos00; pos01; pos10; pos11 ] in
  let spawnLoc = create ~height:bound ~width:bound ~invalid_locations:invalidLocs in
  match spawnLoc with
  | None -> true
  | Some t ->
    Stdio.printf "spawnLoc=%s\n" (Position.to_string t.location);
    t.location.col >= 0 && t.location.col < bound && t.location.row >= 0 && t.location.row < bound

