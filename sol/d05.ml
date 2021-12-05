open Ut

let parse_seg str =
  Scanf.sscanf str "%d,%d -> %d,%d"
    (fun a b c d -> (a, b), (c, d))

let sign n =
  if n > 0 then 1
  else if n < 0 then -1
  else 0

let paint b ((x1, y1), (x2, y2)) =
  let move (x, y) =
    let dx = sign @@ x2 - x1
    and dy = sign @@ y2 - y1 in
    x + dx, y + dy
  in
  let cov x y = b.(y).(x) <- b.(y).(x) + 1 in
  let rec aux (x, y) =
    if x == x2 && y == y2 then
      cov x y
    else begin
      cov x y;
      aux @@ move (x, y)
    end
  in
  aux (x1, y1)

let count b =
  let dimx = Array.length b.(0) and dimy = Array.length b in
  let cnt = ref 0 in
  for y = 0 to dimy-1 do
    for x = 0 to dimx-1 do
      if b.(y).(x) >= 2 then incr cnt
    done
  done;
  !cnt

let size = 1000

let () =
  let data =
    open_in Sys.argv.(1) |> IO.input_lines |> List.map parse_seg
  in
  begin
    (* PART 1 *)
    let b = Array.make_matrix size size 0 in
    List.filter (fun ((x1, y1), (x2, y2)) -> x1 = x2 || y1 = y2) data
    |> List.iter (paint b);
    Printf.printf "%d\n" @@ count b;

    (* PART 2 *)
    let b = Array.make_matrix size size 0 in
    List.iter (paint b) data;
    Printf.printf "%d\n" @@ count b
  end
