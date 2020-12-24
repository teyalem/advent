open Advent

module Direction = struct
  type t = East
         | SouthEast | SouthWest
         | West
         | NorthWest | NorthEast

  let all_directions = [ East; SouthEast; SouthWest; West; NorthWest; NorthEast ]

  let rec parse s =
    match Stream.next s with
    | 'e' -> East
    | 'w' -> West
    | 's' -> begin
        match parse s with
        | East -> SouthEast
        | West -> SouthWest
        | _ -> assert false
      end
    | 'n' -> begin
        match parse s with
        | East -> NorthEast
        | West -> NorthWest
        | _ -> assert false
      end
    | _ -> assert false

end

module Position = struct
  (* position of hexagonal grid *)
  type t = int * int (* x, y *)

  (* standard coord *)
           (* North is y+ *)
           (* East is x+ *)

  let zero = 0, 0

  let make x y = x, y

  let next_pos (x, y) dir =
    let module Dir = Direction in
    match dir with
    | Dir.East -> x + 1, y
    | Dir.West -> x - 1, y
    | Dir.NorthWest -> x, y + 1
    | Dir.NorthEast -> x + 1, y + 1
    | Dir.SouthWest -> x - 1, y - 1
    | Dir.SouthEast -> x, y - 1

  let find_pos dirlist =
    List.fold_left (fun pos dir -> next_pos pos dir) zero dirlist

end

module Tile = struct
  type t = Black | White

  let default = White

  let of_char _ = assert false

  let to_char = function
    | Black -> '#'
    | White -> '.'

  let next_tile num_black tile =
    match tile with
    | Black -> if num_black = 0 || num_black > 2 then White else Black
    | White -> if num_black = 2 then Black else White

  let flip tile =
    match tile with
    | Black -> White
    | White -> Black

end

module Floor = struct
(*  type t = (Position.t * Tile.t) list

  let empty = []

  let flip_tiles dirlists =

    let flip_one floor dirs =
      let pos = Position.find_pos dirs in

      match List.assoc_opt pos floor with
      | None -> (pos, Tile.Black) :: floor
      | Some t ->
        let floor = List.remove_assoc pos floor in
        (pos, Tile.flip t) :: floor
    in

    List.fold_left (fun floor dirs -> flip_one floor dirs) empty dirlists
    *)

  include Block.Make(Tile)

  let flip_tiles floor dirlists =

    let flip_one dirs =
      let x, y  = Position.find_pos dirs in
      let x, y = x + dimx floor / 2, y + dimy floor / 2 in (* move pos to center *)
      set floor x y (Tile.flip @@ get floor x y)
    in

    List.iter flip_one dirlists

  let count_black_neighs floor pos =
    let all_neigh_pos = List.map (Position.next_pos pos) Direction.all_directions in
    all_neigh_pos |> List.map (fun (x, y) -> match get floor x y with c -> c | exception _ -> Tile.default)
    |> List.filter ((=) Tile.Black)
    |> List.length

  let turn_day floor =
    let updates = ref [] in (* update list *)
    floor |> iteri
      (fun x y tile ->
         let b = count_black_neighs floor (x, y) in
         let next_tile = Tile.next_tile b tile in
         if next_tile <> tile
         then updates := (x, y, next_tile) :: !updates (* only update tile when it's changed *)
         else ());

    List.iter (fun (x, y, t) -> set floor x y t) !updates (* perform update *)

end

(* parse a line of directions *)
let rec parse_directions s =
  match Stream.peek s with
  | Some _ ->
    let dir = Direction.parse s in
    dir :: parse_directions s
  | None -> []

(* parse input data of AoC 2020 Day 24 *)
let parse_data str =
  let lines = Delim.split_line str |> List.map Stream.of_string in
  List.map parse_directions lines

let main path =
  let data = open_in path |> IO.read_file |> parse_data in
  let size = 150 in (* enough for my input *)
  begin
    (* PART 1 *)
    let floor = Floor.make size size in
    Floor.flip_tiles floor data;
    Floor.count_occur Tile.Black floor |> print_int;

    print_newline ();

    (* PART 2 *)
    for _ = 1 to 100 do
      Floor.turn_day floor
    done;
    Floor.count_occur Tile.Black floor |> print_int;

  end

let _ = Arg.parse [] main ""
