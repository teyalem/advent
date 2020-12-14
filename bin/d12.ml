(* wrapping modulo operation, negetive value is turned to value + 360. *)
let wrap_mod angle =
  let m = angle mod 360 in
  if m < 0
  then m + 360
  else m

let manhattan_distance (x1, y1) (x2, y2) =
  (abs (x2 - x1)) + (abs (y2 -y1))

(* codes *)
module Code = struct
  type action = North | South | East | West | Left | Right | Forward
  type t = action * int

  exception Invalid_action of char

  let to_action = function
    | 'N' -> North
    | 'S' -> South
    | 'E' -> East
    | 'W' -> West
    | 'L' -> Left
    | 'R' -> Right
    | 'F' -> Forward
    | c -> raise (Invalid_action c)

  let to_code a n = (to_action a), n

  let parse str = Scanf.sscanf str "%c%d" (fun a n -> to_code a n)
end

(* ship model of part 1 *)
module Ship = struct
  open Code
  type code = Code.t

  (* the ship model *)
  type t = {
    mutable x: int;
    mutable y: int;
    mutable angle: int; (* degree, 0 is North and clockwise *)
  }

  let make angle = {
    x = 0;
    y = 0;
    angle = angle;
  }

  let rec do_action s = function
    | North, n -> s.y <- s.y + n
    | South, n -> s.y <- s.y - n
    | East, n -> s.x <- s.x + n
    | West, n -> s.x <- s.x - n
    | Left, n -> s.angle <- wrap_mod (s.angle - n)
    | Right, n -> s.angle <- wrap_mod (s.angle + n)
    | Forward, n -> begin
        match s.angle with (* this recursion helps keeping meaning *)
        | 0 -> do_action s (North, n)
        | 90 -> do_action s (East, n)
        | 180 -> do_action s (South, n)
        | 270 -> do_action s (West, n)
        | n -> prerr_int n; assert false
      end

end

(* ship model of part 2 *)
module Ship2 = struct
  open Code
  type code = Code.t

  type waypoint = {
    mutable x: int;
    mutable y: int;
  }

  let waypoint x y = { (* relative to ship *)
    x = x;
    y = y;
  }

  (* the ship model *)
  type t = {
    mutable x: int;
    mutable y: int;
    mutable wp : waypoint;
  }

  let make (wx, wy) = {
    x = 0;
    y = 0;
    wp = waypoint wx wy;
  }

  let print s =
    Printf.printf "x: %d y: %d wp.x: %d wp.y: %d\n" s.x s.y s.wp.x s.wp.y

  (* degree, 0 is North and clockwise *)
  let rotate_coord (wx, wy) = function (* angle *)
    | 0 -> wx, wy
    | 90 -> wy, -wx
    | 180 -> -wx, -wy
    | 270 -> -wy, wx
    | _ -> assert false

  let do_action s = function
    | North, n -> s.wp.y <- s.wp.y + n
    | South, n -> s.wp.y <- s.wp.y - n
    | East, n -> s.wp.x <- s.wp.x + n
    | West, n -> s.wp.x <- s.wp.x - n
    | (Left as d), n
    | (Right as d), n ->
      let angle = if d = Left then 360 - n else n in
      let wx, wy = rotate_coord (s.wp.x, s.wp.y) angle in
      s.wp <- waypoint wx wy
    | Forward, n ->
      s.x <- s.x + n*s.wp.x;
      s.y <- s.y + n*s.wp.y

end

let main path =
  (* data *)
  let data = open_in path |> Util.read_lines |> List.map Code.parse in
  begin
    (* PART 1 *)
    let ship = Ship.make 90 (* east facing *) in
    List.iter (Ship.do_action ship) data;
    manhattan_distance (0, 0) (ship.x, ship.y)
    |> print_int;

    print_newline ();

    (* PART 2 *)
    let ship = Ship2.make (10, 1) in
    List.iter (fun c -> Ship2.do_action ship c) data;
    manhattan_distance (0, 0) (ship.x, ship.y)
    |> print_int
  end

let _ = Arg.parse [] main ""
