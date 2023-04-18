open! Base

type t =
  { (* [direction] represents the orientation of the snake's head. *)
    direction : Direction.t
  ; (* [extensions_remaining] represents how many more times we should extend the
       snake. *)
    extensions_remaining : int
  ; (* [locations] represents the current set of squares that the snake
       occupies. The first element of the list is the head of the snake. We hold
       as an invariant that [locations] is always non-empty. *)
    locations : Position.t list
  }
[@@deriving sexp_of]

(* Note that at the beginning of the game, the snake will not need to grow at all, so
   [extensions_remaining] should be initialized to 0. *)
let create ~length =
  let initFunc i =
    let p : Position.t =
    { col = length-i-1
    ; row = 0
    } in
    p in
  let poss = List.init length ~f:initFunc in
  let s : t =
    { direction = Right
    ; extensions_remaining = 0
    ; locations = poss
    } in
  s

let to_string snake =
  "Snake: direction=" ^ (Direction.to_string snake.direction)
  ^ " extensions_remaining=" ^ (Int.to_string snake.extensions_remaining)
  ^ " locations=" ^ (String.concat ~sep:"," (List.map ~f:Position.to_string snake.locations))

let%test "Testing create..." =
  Stdio.printf "\nTesting create...\n";
  let p0 : Position.t =
    { col = 1
    ; row = 0
    } in
  let p1 : Position.t =
    { col = 0
    ; row = 0
    } in
  let snake = (create ~length:2) in
  Stdio.printf "p0 %s\n" (Position.to_string p0);
  Stdio.printf "snake %s\n" (to_string snake);
  [%compare.equal: Position.t list] snake.locations [ p0; p1 ] && Direction.equal snake.direction Right

(*
   Notice that this function should not actually grow the snake, but only record that we
   should grow the snake one block for the next [by_how_much] squares. *)
let grow_over_next_steps t by_how_much =
  { t with extensions_remaining = t.extensions_remaining + by_how_much }

let%test "Testing grow_over_next_steps..." =
  Stdio.printf "\nTesting grow_over_next_steps...\n";
  let snake0 = (create ~length:2) in
  let snake1 = grow_over_next_steps snake0 2 in
  let snake2 = grow_over_next_steps snake1 3 in
  Stdio.printf "snake0: %s snake1: %s snake2: %s\n" (to_string snake0) (to_string snake1) (to_string snake2);
  Int.equal snake1.extensions_remaining 2 && Int.equal snake2.extensions_remaining 5

let locations t =
  t.locations

let head_location t =
  let defaultPos : Position.t =
    { col = 1
    ; row = 0
    } in
  let hl = List.hd t.locations in
  match hl with
  | Some loc -> loc
  | _ -> defaultPos

let set_direction t direction =
  match t.direction, direction with
  | Direction.Right, Direction.Left -> t
  | Direction.Left, Direction.Right -> t
  | Direction.Up, Direction.Down -> t
  | Direction.Down, Direction.Up -> t
  | _, _ -> { t with direction = direction }

let%test "Testing set_direction..." =
  Stdio.printf "\nTesting set_direction...\n";
  let s0 = create ~length:2 in
  let s1 = set_direction s0 Down in
  Stdio.printf "s0: %s s1 %s" (to_string s0) (to_string s1);
  Direction.equal s1.direction Down


(*
   [step] should:
   - move the snake forward one block, growing it and updating [t.locations] if necessary
   - check for self collisions *)
let step t =
  let head = head_location t in
  let nextHead = Direction.next_position t.direction head in
  let snakelen = List.length (locations t) in
  let ffunc i pos = i < (snakelen-1+t.extensions_remaining) in
  let remaining = List.filteri ~f:ffunc (locations t) in
  let collision = List.exists ~f:(fun p->Position.equal p nextHead) remaining in
  if collision
  then
    None
  else
    let nextLocations = List.append [ nextHead ] remaining in
    let nextSnake : t =
    { direction = t.direction
    ; extensions_remaining = (Int.max 0 (t.extensions_remaining - 1))
    ; locations = nextLocations
    } in Some nextSnake

let%test "Testing step 0..." =
  Stdio.printf "\nTesting step...\n";
  let l1 : Position.t list = [
      { col = 2
      ; row = 0
      };
      { col = 1
      ; row = 0
      };
    ] in
  let s0 = create ~length:2 in
  let ss1 = step s0 in
  match ss1 with
  | None -> false
  | Some s1 -> Stdio.printf "s0: %s s1:%s" (to_string s0) (to_string s1);
  [%compare.equal: Position.t list] s1.locations l1
  
let%test "Testing step 1..." =
  Stdio.printf "\nTesting step...\n";
  let l1 : Position.t list = [
      { col = 3
      ; row = 0
      };
      { col = 2
      ; row = 0
      };
      { col = 1
      ; row = 0
      };
    ] in
  let s0 = create ~length:3 in
  let ss1 = step s0 in
  match ss1 with
  | None -> false
  | Some s1 -> Stdio.printf "s0: %s s1:%s" (to_string s0) (to_string s1);
  [%compare.equal: Position.t list] s1.locations l1

let%test "Testing step 2..." =
  Stdio.printf "\nTesting step...\n";
  let l1 : Position.t list = [
      { col = 2
      ; row = 0
      };
      { col = 1
      ; row = 0
      };
      { col = 0
      ; row = 0
      };
    ] in
  let s0 = create ~length:2 in
  let s0 = grow_over_next_steps s0 1 in
  let ss1 = step s0 in
  match ss1 with
  | None -> false
  | Some s1 -> Stdio.printf "s0: %s s1:%s" (to_string s0) (to_string s1);
  [%compare.equal: Position.t list] s1.locations l1 && Int.equal s1.extensions_remaining 0

let%test "Testing step 3..." =
  Stdio.printf "\nTesting step...\n";
  let l1 : Position.t list = [
      { col = 1
      ; row = 1
      };
      { col = 1
      ; row = 0
      };
      { col = 0
      ; row = 0
      };
    ] in
  let s0 = create ~length:2 in
  let s0 = grow_over_next_steps s0 1 in
  let s0 = set_direction s0 Up in
  let ss1 = step s0 in
  match ss1 with
  | None -> false
  | Some s1 -> Stdio.printf "s0: %s s1:%s" (to_string s0) (to_string s1);
  [%compare.equal: Position.t list] s1.locations l1 && Int.equal s1.extensions_remaining 0

