open Ut

(* Direction in hex grid *)
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

module HexagonalPosition = struct
  (* position of hexagonal grid *)
  type t = int * int (* x, y *)

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
    List.fold_left (fun pos dir -> next_pos pos dir) (0,0) dirlist

  let neighbor_positions (pos: t) : t list =
    List.map (next_pos pos) Direction.all_directions

end

(* Tile module for Floor *)
module Tile = struct
  type t = Black | White

  let default = White

  let of_char _ = assert false (* No parsing *)

  let to_char = function
    | Black -> '#'
    | White -> '.'

  let next neighs tile =
    let num_black = List.(filter ((=) Black) neighs |> length) in
    match tile with
    | Black -> if num_black = 0 || num_black > 2 then White else Black
    | White -> if num_black = 2 then Black else White

  let flip tile =
    match tile with
    | Black -> White
    | White -> Black

end

(* a floor block *)
module Floor = struct

  module B = Block.Make(Tile)
  module Board = BlockBoard.Make(B)

  include B
  include Board
  include CellularAutomata.Make(Board)(HexagonalPosition)(Tile)

  let flip_tiles floor dirlists =
    let flip_one dirs =
      let x, y  = HexagonalPosition.find_pos dirs in
      let x, y = x + dimx floor / 2, y + dimy floor / 2 in (* move pos to center *)
      set floor x y @@ Tile.flip @@ get floor x y
    in
    List.iter flip_one dirlists

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
  let data = open_in path |> IO.input_all |> parse_data in
  let size = 150 in (* enough for my input *)
  begin
    (* PART 1 *)
    let floor = Floor.make size size in
    Floor.flip_tiles floor data;
    Floor.count_occur Tile.Black floor |> print_int;

    print_newline ();

    (* PART 2 *)
    for _ = 1 to 100 do
      ignore @@ Floor.next_state floor
    done;
    Floor.count_occur Tile.Black floor |> print_int;

  end

let _ = Arg.parse [] main ""
