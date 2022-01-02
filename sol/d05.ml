open Ut

let parse_seg str =
  Scanf.sscanf str "%d,%d -> %d,%d"
    (fun a b c d -> (a, b), (c, d))

let sign n =
  if n > 0 then 1
  else if n < 0 then -1
  else 0

let paint b ((x1, y1), (x2, y2)) =
  let next (x, y) =
    let dx = sign @@ x2 - x1
    and dy = sign @@ y2 - y1 in
    x + dx, y + dy
  in
  let rec aux (x, y) =
    b.(y).(x) <- b.(y).(x) + 1;
    if x <> x2 || y <> y2 then
      aux @@ next (x, y)
  in
  aux (x1, y1)

let count b =
  let cnt = ref 0 in
  Mat.iteri (fun x y n -> if n >= 2 then incr cnt) b;
  !cnt

let size = 1000

let () =
  let data = IO.read_lines () |> List.map parse_seg in
  begin
    (* PART 1 *)
    let b = Mat.make size size 0 in
    List.filter (fun ((x1, y1), (x2, y2)) -> x1 = x2 || y1 = y2) data
    |> List.iter (paint b);
    Printf.printf "%d\n" @@ count b;

    (* PART 2 *)
    let b = Mat.make size size 0 in
    List.iter (paint b) data;
    Printf.printf "%d\n" @@ count b
  end
