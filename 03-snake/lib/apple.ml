open! Base

type t = { location : Position.t } [@@deriving sexp_of]

let location t = t.location

let rec create ~height ~width ~invalid_locations = 
  let locx = Random.int width in
  let locy = Random.int height in
  let loc : Position.t = 
    { col = locx
    ; row = locy 
    } in
  let pos : t = 
    { location = loc
    } in
  if List.mem invalid_locations loc ~equal:Position.equal
  then
    create ~height:height ~width:width ~invalid_locations:invalid_locations
  else
    Some pos

