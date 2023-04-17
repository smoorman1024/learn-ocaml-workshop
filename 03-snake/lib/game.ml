open! Base

type t =
  { mutable snake : Snake.t
  ; mutable apple : Apple.t
  ; mutable game_state : Game_state.t
  ; height : int
  ; width : int
  ; amount_to_grow : int
  }
[@@deriving sexp_of]

let in_bounds t (p : Position.t) =
  p.col >= 0 && p.col < t.width && p.row >=0 && p.row < t.height

let%test "Testing in_bounds 0..." =
  Stdio.printf "\nTesting in_bounds 0...\n";
  let h = 10 in
  let w = 10 in
  let oa = Apple.create ~height:h ~width:w ~invalid_locations:[] in
  match oa with 
  | None -> 
    Stdio.printf "Could not print apple\n";
    false
  | Some a ->
    Stdio.printf "Created apple\n";
    let g : t =
      { snake = Snake.create ~length:2
      ; apple = a
      ; height = h
      ; width = w
      ; amount_to_grow = 0
      ; game_state = In_progress
      } in
    let p : Position.t = 
      { row = 0
      ; col = 0
      } in
    in_bounds g p
;;

let%test "Testing in_bounds 1..." =
  Stdio.printf "\nTesting in_bounds 0...\n";
  let h = 10 in
  let w = 10 in
  let oa = Apple.create ~height:h ~width:w ~invalid_locations:[] in
  match oa with 
  | None -> false
  | Some a ->
    let g : t =
      { snake = Snake.create ~length:2
      ; apple = a
      ; height = h
      ; width = w
      ; amount_to_grow = 0
      ; game_state = In_progress
      } in
    let p : Position.t = 
      { row = 11
      ; col = 0
      } in
    not (in_bounds g p)
;;

(* Make sure that the game returned by [create] is in a valid state. In particular, we
   should fail with the message "unable to create initial apple" if [Apple.create] is
   unsuccessful, and "unable to create initial snake" if the initial snake is invalid
   (i.e. goes off the board). *)
let create ~height ~width ~initial_snake_length ~amount_to_grow =
  let s = Snake.create ~length:initial_snake_length in
  let head = Snake.head_location s in
  let oa = Apple.create ~height:height ~width:width ~invalid_locations:[] in
  match oa with
  | None -> raise (Failure "unable to create initial apple")
  | Some a ->
    let g : t =
      { snake = s 
      ; apple = a
      ; height = height
      ; width = width
      ; amount_to_grow = amount_to_grow
      ; game_state = In_progress
      } in
    if not (in_bounds g head) then
      raise (Failure "unable to create initial snake")
    else
      g;;

let snake t = t.snake
let apple t = t.apple
let game_state t = t.game_state

let set_direction (t : t) direction =
  let s = Snake.set_direction (snake t) direction in
  let t_ref = ref t in
  t_ref := { !t_ref with snake = s };
  ()

(*
   [step] should:
   - move the snake forward one square
   - check for collisions (end the game with "Wall collision" or "Self collision")
   - if necessary:
     -- consume apple
     -- if apple cannot be regenerated, win game; otherwise, grow the snake *)
let step t =
  let ss = Snake.step (snake t) in
  match ss with
  | None -> raise (Failure "Self collision")
  | Some s -> 
    let head = Snake.head_location s in
    if not (in_bounds t head) then
      raise (Failure "Wall collision")
    else
      let t_ref = ref t in
      if Position.equal (Apple.location (apple t)) head
      then
        let s = Snake.grow_over_next_steps s t.amount_to_grow in
        let sa = Apple.create ~height:t.height ~width:t.width ~invalid_locations:(Snake.locations s) in
        match sa with
        | Some a -> 
          t_ref := 
            { !t_ref with 
              snake = s;
              apple = a; 
            };
          ()
        | None ->
          t_ref :=
            { !t_ref with
              snake = s;
              game_state = Win;
            };
          ()
      else
        t_ref :=
          { !t_ref with
            snake = s;
          };
        ()






module For_testing = struct
  let create_apple_force_location_exn ~height ~width ~location =
    let invalid_locations =
      List.init height ~f:(fun row ->
          List.init width ~f:(fun col -> { Position.row; col }))
      |> List.concat
      |> List.filter ~f:(fun pos -> not ([%compare.equal: Position.t] location pos))
    in
    match Apple.create ~height ~width ~invalid_locations with
    | None -> failwith "[Apple.create] returned [None] when [Some _] was expected!"
    | Some apple -> apple
  ;;

  let create_apple_and_update_game_exn t ~apple_location =
    let apple =
      create_apple_force_location_exn
        ~height:t.height
        ~width:t.width
        ~location:apple_location
    in
    t.apple <- apple
  ;;

  let create_game_with_apple_exn
      ~height
      ~width
      ~initial_snake_length
      ~amount_to_grow
      ~apple_location
    =
    let t = create ~height ~width ~initial_snake_length ~amount_to_grow in
    create_apple_and_update_game_exn t ~apple_location;
    t
  ;;
end
