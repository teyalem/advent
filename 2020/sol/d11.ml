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

(* Map of Waiting Area *)
module Map = Block.Make(Tile)

(* normal cellular automata *)
module Map1 = struct
  type elt = Map.elt
  type pos = int * int
  type t = Map.t

  (* rule of part1 *)
  let next neighs tile =
    let open Tile in
    let onum = List.(filter ((=) OccupiedSeat) neighs |> length) in
    match tile with
    | Floor -> Floor
    | EmptySeat -> if onum = 0 then OccupiedSeat else EmptySeat
    | OccupiedSeat -> if onum >= 4 then EmptySeat else OccupiedSeat

  let get b (x, y) = try Some (Map.get b x y) with _ -> None
  let set b (x, y) = Map.set b x y

  let neighbors b pos =
    List.filter_map (get b)
    @@ Neigh.neighbors Neigh.moore pos

  let iteri f = Map.iteri (fun x y e -> f (x, y) e)
end

(* modified cellular automata *)
module Map2 = struct
  include Map1

  (* find first seat that can people see in position x, y.
   * if the end of map is reached, return Floor.
   * DO NOT USE 0, 0 AS DX, DY BECAUSE THEY CAUSE INFINITE LOOP. *)
  let rec find_first_seat map (dx, dy) (x, y) =
    let x, y = x + dx, y + dy in
    match get map (x, y) with
    | Some t when t = Tile.default -> find_first_seat map (dx, dy) (x, y)
    | Some t -> t
    | None -> Tile.default

  (* rule of part2 *)
  let next neighs tile =
    let open Tile in
    let onum = List.(filter ((=) OccupiedSeat) neighs |> length) in
    match tile with
    | Floor -> Floor
    | EmptySeat -> if onum = 0 then OccupiedSeat else EmptySeat
    | OccupiedSeat -> if onum >= 5 then EmptySeat else OccupiedSeat

  let neighbors map pos =
    let f dpos = find_first_seat map dpos pos in
    List.map f Neigh.moore
end

let () =
  let map = IO.read_lines () |> Map.parse in
  begin
    (* PART 1 *)
    let next_state = CellularAutomata.make_automata (module Map1) in

    try
      while true do
        let tmp = Map.copy map in
        ignore @@ next_state map;

        if tmp = map then raise Exit else ()
      done
    with Exit -> ();

    Map.count_occur Tile.OccupiedSeat map |> Printf.printf "%d\n";

    (* PART 2 *)
    let next_state = CellularAutomata.make_automata (module Map2) in

    try
      while true do
        let tmp = Map.copy map in
        ignore @@ next_state map;

        if tmp = map then raise Exit else ()
      done
    with Exit -> ();

    Map.count_occur Tile.OccupiedSeat map |> Printf.printf "%d\n";
  end
