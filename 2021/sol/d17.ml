let parse str =
  Scanf.sscanf str "target area: x=%d..%d, y=%d..%d"
    (fun a b c d -> (a, b), (c, d))

let tri n =
  n * (n+1) / 2

let invtri n : float =
  (-1. +. sqrt (8.*.float n +. 1.)) /. 2.

let highest_pos (yl, _) =
  tri (-yl-1)

let sign n =
  if n > 0 then 1
  else if n < 0 then -1
  else 0

(* brute-force with heuristic *)
let all_initvel (xl, xh) (yl, yh) =
  let probe dx dy =
    let rec aux x y dx dy =
      if xl <= x && x <= xh && yl <= y && y <= yh then true
      else if x > xh || y < yl then false
      else aux (x + dx) (y + dy) (dx - sign dx) (dy - 1)
    in
    aux 0 0 dx dy
  in
  let cnt = ref 0 in
  for x = int_of_float @@ ceil @@ invtri xl to xh do
    for y = yl to -yl-1 do
      if probe x y then incr cnt
    done
  done;
  !cnt

let () =
  let (xr, yr) = read_line () |> parse in
  begin
    (* PART 1 *)
    highest_pos yr |> Printf.printf "%d\n";

    (* PART 2 *)
    all_initvel xr yr |> Printf.printf "%d\n";
  end
