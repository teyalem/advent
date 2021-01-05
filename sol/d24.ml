open Advent

module Tile = struct
  type t = Empty | Bug | Zip

  let default = Empty

  let of_char c =
    match c with
      '.' -> Empty
    | '#' -> Bug
    | '?' -> Zip
    | _ -> assert false

  let to_char c =
    match c with
      Empty -> '.'
    | Bug -> '#'
    | Zip -> '?'

  let next neigh c =
    let b = List.filter ((=) Bug) neigh |> List.length in
    match c with
      Bug -> if b = 1 then Bug else Empty
    | Empty -> if b = 1 || b = 2 then Bug else Empty
    | Zip -> Zip

end

module Pos = struct
  type t = int * int

  let ps = [ 0, -1; 0, 1; -1, 0; 1, 0 ] (* up, down, left, right *)

  let neighbor_positions (x, y) =
    List.map (fun (dx, dy) -> x + dx, y + dy) ps

end

module Eris = struct
  module B = Block.Make(Tile)
  module Board = BlockBoard.Make(B)

  include B
  include Board
  include CellularAutomata.Make(Board)(Pos)(Tile)

  let find_dup map =
    let rec loop ms m =
      let nm = next_state @@ copy m in
      if List.mem nm ms
      then nm
      else loop (nm::ms) nm
    in
    loop [copy map] map

  let biodiversity_rating map =
    let rs = ref [] in
    iteri (fun x y c ->
        if c = Bug
        then rs := (x, y) :: !rs
        else ()) map;
    !rs
    |> List.map (fun (x, y) ->
        let p = x + y * dimx map in
        let point = int_of_float (2.0 ** float p) in
        point)
    |> sum
end

module InfEris = struct
  type t = Eris.t array
  open Eris

  let left_side map =
    to_matrix map
    |> (fun m -> m.(0)) (* x = 0 *)
    |> Array.to_list

  let right_side map =
    to_matrix map
    |> (fun m -> m.(dimx map - 1)) (* x = last *)
    |> Array.to_list

  let up_side map =
    to_matrix map
    |> Array.map (fun arr -> arr.(0)) (* y = 0 *)
    |> Array.to_list

  let down_side map =
    to_matrix map
    |> Array.map (fun arr -> arr.(dimy map - 1)) (* y = last *)
    |> Array.to_list

  let sides = [ down_side; up_side; right_side; left_side ]

  let is_inner (x, y) =
    0 <= x && x <= 4 &&
    0 <= y && y <= 4

  let is_outer p = not @@ is_inner p

  let get_neighs infmap level pos =
    let map = infmap.(level)
    and outer_map = try infmap.(level-1) with _ -> make 5 5
    and inner_map = try infmap.(level+1) with _ -> make 5 5
    in
    let mid = dimx map / 2, dimy map / 2 in
    List.map2 (fun f dp ->
        let p = Coord.add pos dp in
        if p = mid
        then f inner_map
        else if is_outer p
        then [ Option.value ~default: Tile.default @@ get_cell outer_map (Coord.add (2, 2) dp) ]
        else [ Option.value ~default: Tile.default @@ get_cell map p ])
      sides
      Pos.ps
    |> List.concat

  let init map =
    let n = 201 in
    let m = Array.init n (fun _ -> make 5 5) in
    m.(n/2) <- map;
    m

  let next_state infmap = begin
    let updates = ref [] in (* update list *)

    let collect_update i pos elt =
      let neighs = get_neighs infmap i pos in
      let next_elt = next neighs elt in

      (* update only if the element is changed *)
      if next_elt <> elt && pos <> (2, 2)
      then updates := (i, pos, next_elt) :: !updates
      else ()
    in

    for i = 0 to Array.length infmap - 1 do
      iteri_cell (collect_update i) infmap.(i)
    done;

    (* Perform Update *)
    List.iter (fun (i, pos, elt) -> set_cell infmap.(i) pos elt) !updates
  end

  let count_occur elt infmap =
    Array.map (Eris.count_occur elt) infmap
    |> Array.fold_left (+) 0

  let print infmap =
    let len = Array.length infmap in
    Array.iteri (fun i m ->
        let i = i - len / 2 in
        Printf.printf "level %d:\n" i;
        Eris.print m) infmap

end

let main path =
  let data = open_in path |> IO.read_lines |> Eris.parse in
  begin
    (* PART 1 *)
    let dup = Eris.find_dup data in
    Eris.biodiversity_rating dup |> print_int;

    print_newline ();

    (* PART 2 *)
    let infmap = InfEris.init data in

    for _ = 1 to 200 do
      InfEris.next_state infmap
    done;
    InfEris.count_occur Tile.Bug infmap |> print_int

  end

let () = Arg.parse [] main ""
