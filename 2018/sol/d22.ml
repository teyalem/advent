open Ut

let get m x y = m.(x).(y)
let set m x y v = m.(x).(y) <- v

let region_map depth (tx, ty) (dx, dy) =
  let mat = Array.make_matrix dx dy 0 in
  for x = 0 to dx - 1 do
    for y = 0 to dy - 1 do
      let geologic_index =
        if x = 0 && y = 0 || x = tx && y = ty then 0
        else if y = 0 then x * 16807
        else if x = 0 then y * 48271
        else get mat (x-1) y * get mat x (y-1)
      in
      let erosion_level = (geologic_index + depth) mod 20183 in
      set mat x y erosion_level
    done
  done;
  Array.(map (map (fun n -> n mod 3)) mat)

let total_risk_level mat (tx, ty) =
  let c = ref 0 in
  for x = 0 to tx do
    for y = 0 to ty do
      c := !c + get mat x y
    done
  done;
  !c

type tool = Neither | Torch | Climbing_gear

module Cave = struct
  type space = int array array * (int * int)
  type state = (int * int) * tool
  type data = unit
  type weight = int

  let data_id = ()

  let is_end (_, tpos) ((pos, tool), ()) =
    pos = tpos && tool = Torch

  let is_passable mat tool (x, y) =
    match get mat x y with
    | 0 (* rocky *) -> tool <> Neither
    | 1 (* wet *) -> tool <> Torch
    | 2 (* narrow *) -> tool <> Climbing_gear
    | _ -> assert false

  let other_tool = function
    | Neither -> [Torch; Climbing_gear]
    | Torch -> [Neither; Climbing_gear]
    | Climbing_gear -> [Neither; Torch]

  let neighbors (mat, _) ((pos, tool), ()) =
    let dimx, dimy = Array.(length mat, length mat.(0)) in
    (* move *)
    (Neigh.(neighbors von_neumann pos)
     |> List.filter (fun (x, y) ->
         0 <= x && x < dimx && 0 <= y && y < dimy)
     |> List.filter (is_passable mat tool)
     |> List.map (fun pos -> 1, (pos, tool), ()))
    @
    (* tool change *)
    (other_tool tool
     |> List.filter (fun t -> is_passable mat t pos)
     |> List.map (fun t -> 7, (pos, t), ()))
end

let rescue mat tpos =
  let module S = struct
    type t = (int * int) * tool
    let compare = Stdlib.compare
  end
  in
  Pathfind.dijkstra (module S) (module Int) (module Cave)
    (mat, tpos)
    ~start: ((0, 0), Torch)
  |> fst

let parse str =
  Scanf.sscanf str "depth: %d\ntarget: %d,%d" (fun d tx ty -> d, (tx, ty))

let () =
  let d, tpos =
    open_in Sys.argv.(1)
    |> In_channel.input_all
    |> parse
  in
  let m = region_map d tpos (4000, 4000) in
  (* PART 1 *)
  total_risk_level m tpos |> print_int;
  print_newline ();

  (* PART 2 *)
  rescue m tpos |> print_int;
