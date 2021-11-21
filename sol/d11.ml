open Ut

module Tile = struct
  (* kinds of tile *)
  type t = Floor | EmptySeat | OccupiedSeat

  let default = Floor

  exception Invalid_tile

  (* convert a char into a tile
   * if a char is not a tile, raise Invalid_tile *)
  let of_char = function
    | '.' -> Floor
    | 'L' -> EmptySeat
    | '#' -> OccupiedSeat
    | _ -> raise Invalid_tile

  (* convert a tile into a char *)
  let to_char = function
    | Floor -> '.'
    | EmptySeat -> 'L'
    | OccupiedSeat -> '#'

end

(* tile module for part 1 *)
module Tile1 = struct
  include Tile

  (* If a seat is empty (L) and there are no occupied seats adjacent to it, the
   * seat becomes occupied.  If a seat is occupied (#) and four or more seats
   * adjacent to it are also occupied, the seat becomes empty.  Otherwise, the
   * seat's state does not change. *)

  (* rule of part1 *)
  let next neighs tile =
    let onum = List.(filter ((=) Tile.OccupiedSeat) neighs |> length) in
    match tile with
    | Floor -> Floor
    | EmptySeat -> if onum = 0 then OccupiedSeat else EmptySeat
    | OccupiedSeat -> if onum >= 4 then EmptySeat else OccupiedSeat

end

(* tile module for part 2 *)
module Tile2 = struct
  include Tile

  (* Also, people seem to be more tolerant than you expected: it now takes five
   * or more visible occupied seats for an occupied seat to become empty (rather
   * than four or more from the previous rules). The other rules still apply.  *)

  (* rule of part2 *)
  let next neighs tile =
    let onum = List.(filter ((=) Tile.OccupiedSeat) neighs |> length) in
    match tile with
    | Floor -> Floor
    | EmptySeat -> if onum = 0 then OccupiedSeat else EmptySeat
    | OccupiedSeat -> if onum >= 5 then EmptySeat else OccupiedSeat

end

(* Map of Waiting Area *)
module Map(T: BlockBoard.SignCellType) = struct

  module B = Block.Make(T)
  module Board = BlockBoard.Make(B)

  include B
  include Board
  include CellularAutomata.Make(Board)(BlockBoard.Position2D)(T)

end

(* normal cellular automata *)
module Map1 = Map(Tile1)

(* modified cellular automata *)
module Map2 = struct

  include Map(Tile2)

  (* find first seat that can people see in position x, y.
   * if the end of map is reached, return Floor.
   * DO NOT USE 0, 0 AS DX, DY BECAUSE THEY CAUSE INFINITE LOOP. *)
  let rec find_first_seat map (dx, dy) (x, y) =
    let x, y = x + dx, y + dy in
    match get_cell map (x, y) with
    | Some t when t = default -> find_first_seat map (dx, dy) (x, y)
    | Some t -> t
    | None -> default

  (* return seats people see in part 2 *)
  let see map pos =
    let f dpos = find_first_seat map dpos pos in
    List.map f (neighbor_positions (0, 0))

  (* overloaded function *)
  let next_state map =
    let updates = ref [] in (* update list *)
    let collect_updates pos elt =
        let neighs = see map pos in
        let next_elt = next neighs elt in

        (* update only if the element is changed *)
        if next_elt <> elt
        then updates := (pos, next_elt) :: !updates
        else ()
    in

    iteri_cell collect_updates map;
    List.iter (fun (pos, elt) -> set_cell map pos elt) !updates;
    map

end

let main path =
  let data = open_in path |> IO.input_lines in
  begin
    (* PART 1 *)
    let map = Map1.parse data in

    try
      while true do
        let tmp = Map1.copy map in
        ignore @@ Map1.next_state map;

        if tmp = map then raise Exit else ()
      done
    with Exit -> ();

    Map1.count_occur Tile.OccupiedSeat map |> print_int;

    print_newline ();

    (* PART 2 *)
    let module Map2 = Map(Tile2) in
    let map = Map2.parse data in

    try
      while true do
        let tmp = Map2.copy map in
        ignore @@ Map2.next_state map;

        if tmp = map then raise Exit else ()
      done
    with Exit -> ();

    Map2.count_occur Tile.OccupiedSeat map |> print_int;
  end

let _ = Arg.parse [] main ""
