open Advent

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

  (* If a seat is empty (L) and there are no occupied seats adjacent to it, the
   * seat becomes occupied.  If a seat is occupied (#) and four or more seats
   * adjacent to it are also occupied, the seat becomes empty.  Otherwise, the
   * seat's state does not change. *)

  (* rule of part1 *)
  let rule_part1 onum = function
    | Floor -> Floor
    | EmptySeat -> if onum = 0 then OccupiedSeat else EmptySeat
    | OccupiedSeat -> if onum >= 4 then EmptySeat else OccupiedSeat

  (* Also, people seem to be more tolerant than you expected: it now takes five
   * or more visible occupied seats for an occupied seat to become empty (rather
   * than four or more from the previous rules). The other rules still apply.  *)

  (* rule of part2 *)
  let rule_part2 onum = function
    | Floor -> Floor
    | EmptySeat -> if onum = 0 then OccupiedSeat else EmptySeat
    | OccupiedSeat -> if onum >= 5 then EmptySeat else OccupiedSeat

end

(* Map of Waiting Area *)
module Map = struct

  include Block.Make(Tile)

  (* get adjacent tiles of position mx, my *)
  let get_adjs mx my map =
    let result = ref [] in
    for x = mx - 1 to mx + 1 do
      for y = my - 1 to my + 1 do
        let t =
          match get map x y with
          | t -> if x = mx && y = my then Tile.Floor else t
          | exception Invalid_argument _ -> Tile.Floor
        in
        result := t :: !result
      done
    done;
    !result

  (* return how many occupied seat people see in part 1 *)
  let see_part1 map x y =
    let adjs = get_adjs x y map in
    List.(filter ((=) Tile.OccupiedSeat) adjs |> length)

  (* find first seat that can people see in position x, y.
   * if the end of map is reached, return Floor.
   * DO NOT USE 0, 0 AS DX, DY BECAUSE THEY CAUSE INFINITE LOOP. *)
  let rec find_first_seat map (dx, dy) (x, y) =
    let x = x + dx
    and y = y + dy
    in
    match get map x y with
    | Tile.Floor -> find_first_seat map (dx, dy) (x, y)
    | t -> t
    | exception Invalid_argument _ -> Tile.Floor

  (* return how many occupied seat people see in part 2 *)
  let see_part2 map x y =
    let f dx dy = find_first_seat map (dx, dy) (x, y) in
    [ f ~-1 ~-1; f  0 ~-1; f  1 ~-1;
      f ~-1   0;           f  1  0;
      f ~-1   1; f  0   1; f  1  1; ]
    |> List.filter ((=) Tile.OccupiedSeat)
    |> List.length

  (* update prediction using seef as simulating people's eye and rulef as
   * simulating people's decision.
   * seef: map -> int -> int -> int
   * rulef: int -> tile -> tile *)
  let update seef rulef map = begin
    let result = make (dimx map) (dimy map) in
    map |> iteri (fun x y c ->
        let onum = seef map x y in
        set result x y (rulef onum c));
    result
  end

  (* check the prediction is stable by comparing it with previous result *)
  let is_stable ma mb = (ma = mb)

end

let main path =
  let data = open_in path |> IO.read_lines |> Map.parse in
  begin

    (* PART 1 *)
    let map = ref data
    and pmap = ref Map.(make (dimx data) (dimy data)) in

    while not (Map.is_stable !map !pmap) do
      let tmp = Map.update Map.see_part1 Tile.rule_part1 !map in
      pmap := !map;
      map := tmp;
    done;

    let num_occupied_seats = Map.count_occur Tile.OccupiedSeat !map in
    print_int num_occupied_seats;

    print_newline ();

    (* PART 2 *)
    let map = ref data
    and pmap = ref Map.(make (dimx data) (dimy data)) in

    while not (Map.is_stable !map !pmap) do
      let tmp = Map.update Map.see_part2 Tile.rule_part2 !map in
      pmap := !map;
      map := tmp;
    done;

    let num_occupied_seats = Map.count_occur Tile.OccupiedSeat !map in
    print_int num_occupied_seats

  end

let _ = Arg.parse [] main ""
