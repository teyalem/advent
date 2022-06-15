let serial_number = 9445
let len = 300

let hund n = (n / 100) mod 10

let power_level x y =
  let rack_id = x + 10 in
  hund ((rack_id * y + serial_number) * rack_id) - 5

let grid =
  Array.init len (fun x ->
      Array.init len (fun y -> power_level (x+1) (y+1)))

(* partial sum *)
let total_grid =
  Array.to_seq grid
  |> Seq.map (fun arr ->
      Array.to_seq arr
      |> Seq.scan Int.add 0
      |> Array.of_seq)
  |> Seq.scan (Array.map2 Int.add) (Array.make (len+1) 0)
  |> Array.of_seq

let sum_square n x y =
  let t = total_grid in
  t.(x + n).(y + n) - t.(x + n).(y) - t.(x).(y + n) + t.(x).(y)

let max_square n =
  let len = len - n + 1 in
  List.init len (fun x ->
      List.init len (fun y ->
          (x+1, y+1), sum_square n x y))
  |> List.concat
  |> List.fold_left (fun (_, pn as p) (_, nn as n) ->
      if nn > pn then n else p)
    ((0, 0), min_int)

let max_squares () =
  Seq.init 300 succ
  |> Seq.map (fun i ->
      let (x, y), n = max_square i in (x, y, i), n)
  |> Seq.fold_left (fun (_, pn as p) (_, nn as n) ->
      if nn > pn then n else p)
    ((0, 0, 0), min_int)
  |> fst

let () =
  (* PART 1 *)
  let x, y = max_square 3 |> fst in
  Printf.printf "%d,%d\n" x y;
  (* PART 2 *)
  let x, y, size = max_squares () in
  Printf.printf "%d,%d,%d\n" x y size
