open Ut

let distance (x, y) (a, b) =
  abs (a - x) + abs (b - y)

let parse str =
  Scanf.sscanf str "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d"
    (fun sx sy bx by -> (sx, sy), (bx, by))

let nobeacon_at y ((sx, sy), d) =
  let l = d - abs (y - sy) in
  if l < 0 then None
  else Some (sx - l, sx + l)

let pair_compare (a, b) (x, y) =
  match Int.compare a x with
  | 0 -> Int.compare b y
  | v -> v

let connect_ranges =
  let rec aux buf (l, h) = function
    | [] -> List.rev ((l, h) :: buf)
    | (a, b) :: xs ->
      if a <= h+1 then aux buf (l, max h b) xs
      else aux ((l, h) :: buf) (a, b) xs
  in
  fun rs ->
    List.sort_uniq pair_compare rs
    |> List.map (fun (l, h) ->
        let l = max 0 @@ min 4_000_000 l
        and h = max 0 @@ min 4_000_000 h in
        l, h)
    |> (function
        | x::xs -> aux [] x xs
        | _ -> assert false)

let count_range rs =
  let rmin = List.fold_left (fun m (l, _) -> min m l) max_int rs
  and rmax = List.fold_left (fun m (_, h) -> max m h) min_int rs in
  rmax - rmin + 1

let tuning_frequency (x, y) =
  x * 4_000_000 + y

let () =
  let data = open_in Sys.argv.(1) |> IO.input_lines |> List.map parse in
  let sensors = List.map (fun (s, b) -> s, distance s b) data in

  let ty = 2_000_000 in
  let beacons =
    data
    |> List.filter_map (fun (_, (bx, by)) ->
        if by = ty then Some bx else None)
    |> List.sort_uniq Int.compare
  in

  let ranges = List.filter_map (nobeacon_at ty) sensors in
  print_int @@ count_range ranges - List.length beacons;
  print_newline ();

  let s = 4_000_000 in
  Seq.init (s+1) Fun.id
  |> Seq.map (fun y ->
      let nobeacons =
        sensors
        |> List.filter_map (nobeacon_at y)
        |> connect_ranges
      in
      y, nobeacons)
  |> Seq.find_map (fun (y, rs) ->
      if List.length rs > 1 then
        let x = 1 + (snd @@ List.hd rs) in
        Some (x, y)
      else None)
  |> Option.iter (fun pos -> Printf.printf "%d\n" @@ tuning_frequency pos)
