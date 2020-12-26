open Advent

module Coord = struct
  type t = int * int

  let make x y = x, y
  let x (x, _) = x
  let y (_, y) = y

  let manhattan_distance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)
  let add (x1, y1) (x2, y2) = x1 + x2, y1 + y2
  let sub (x1, y1) (x2, y2) = x1 - x2, y1 - y2
end

module Direction = struct
  type t = Left | Right | Up | Down

  let of_char c =
    match c with
    | 'L' -> Left
    | 'R' -> Right
    | 'U' -> Up
    | 'D' -> Down
    | _ -> raise (Invalid_argument "of_char")
end

module Move = struct
  type t = Direction.t * int

  let dir (dir, _) = dir
  let dis (_, dis) = dis

  let parse str =
    Scanf.sscanf str "%c%d" (fun c d -> Direction.of_char c, d)
end

module Grid = struct
  module C = struct
    type color = Red | Blue | Empty
    type t = color * int

    let default = Empty, 0

    let of_char _ = assert false
    let to_char = function
      | Red, _ -> '#'
      | Blue, _ -> '%'
      | Empty, _ -> '.'
  end

  include SparseBlock.Make(C)

  let red = C.Red
  let blue = C.Blue

  let get_pos b (x, y) = get b x y
  let set_pos b (x, y) c = set b x y c

  let draw_wire grid wire color =
    let pos = ref (0, 0)
    and count = ref 0 
    and crosses = ref [] in

    wire |> List.iter
      (fun move ->
         let d =
           let open Direction in
           match Move.dir move with
           | Left -> -1, 0
           | Right -> 1, 0
           | Up -> 0, -1
           | Down -> 0, 1
         and n = Move.dis move in

         for _ = 1 to n do
           pos := Coord.add !pos d;
           incr count;
           let c, i = get_pos grid !pos in
           if c = Empty
           then set_pos grid !pos (color, !count) (* paint *)
           else if c <> color
           then crosses := (!pos, i + !count) :: !crosses
           else ()
         done);
    !crosses

end

let parse_wire wire =
  Delim.split "," wire
  |> List.map Move.parse

let parse_wires l =
  match l with
  | [ one; two ] ->
    parse_wire one, parse_wire two
  | _ -> assert false

let main path =
  let one, two = open_in path |> IO.read_lines |> parse_wires in
  begin
    (* PART 1 *)
    let len = 20000 in
    let grid = Grid.make len len in
    ignore @@ Grid.draw_wire grid one Grid.red;
    let crosses = Grid.draw_wire grid two Grid.blue in

    List.map (fun c -> Coord.manhattan_distance (0, 0) @@ fst c) crosses
    |> List.fold_left min max_int
    |> print_int;

    print_newline ();

    (* PART 2 *)
    List.map snd crosses
    |> List.fold_left min max_int
    |> print_int;
  end

let () = Arg.parse [] main ""
