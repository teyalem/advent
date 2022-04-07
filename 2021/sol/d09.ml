open Ut

module IntTile = struct
  type t = int
  let default = 0
  let of_char c = Char.(code c - code '0')
  let to_char _ = assert false
end

module IB = Block.Make(IntTile)

let ds = [ -1, 0; 1, 0; 0, -1; 0, 1 ]

let collect_low_points b =
  let lpoints = ref [] in
  IB.iteri (fun x y e ->
      if
        List.map (fun (dx, dy) -> x+dx, y+dy) ds
        |> List.map (fun (x, y) -> try IB.get b x y with _ -> 10)
        |> List.for_all (fun l -> e < l)
      then
        lpoints := (x, y) :: !lpoints) b;
  !lpoints

let risk_level e = e + 1

let explore_basin b (x, y) =
  let dimx = IB.dimx b and dimy = IB.dimy b in
  let initl = IB.get b x y in
  let visited = Array.make_matrix dimx dimy false in
  let tiles = ref [] in
  let rec aux (x, y) =
    if not visited.(x).(y) then begin
      visited.(x).(y) <- true;
      tiles := (x, y) :: !tiles;
      List.map (fun (dx, dy) -> x+dx, y+dy) ds
      |> List.filter (fun (x, y) ->
          0 <= x && x < dimx && 0 <= y && y < dimy)
      |> List.filter (fun (x, y) ->
          let l = IB.get b x y in initl < l && l < 9)
      |> List.iter aux
    end
  in
  aux (x, y);
  !tiles

let () =
  let data = open_in Sys.argv.(1) |> IO.input_lines |> IB.parse in
  let low_points = collect_low_points data in
  begin

    (* PART 1 *)
    low_points
    |> List.map (fun (x, y) -> IB.get data x y)
    |> List.map risk_level
    |> sum
    |> Printf.printf "%d\n";

    (* PART 2 *)
    low_points
    |> List.map (explore_basin data)
    |> List.map List.length
    |> List.sort (Fun.flip Int.compare)
    |> (function a::b::c::_ -> a*b*c
               | _ -> assert false)
    |> Printf.printf "%d\n"
  end
