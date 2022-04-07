open Ut

let calc_paper (l, w, h) =
  let a = l*w and b = w*h and c = h*l in
  let more = min a @@ min b c in
  2*(a+b+c) + more

let calc_ribbon (l, w, h) =
  let m = max l @@ max w h in
  2*(l+w+h - m) + l*w*h

let read_dim str =
  Scanf.sscanf str "%dx%dx%d" (fun l w h -> (l, w, h))

let () =
  let data = IO.read_lines () |> List.map read_dim in
  begin
    (* Part 1 *)
    List.map calc_paper data
    |> List.fold_left Int.add 0
    |> Printf.printf "%d\n";

    (* Part 2 *)
    List.map calc_ribbon data
    |> List.fold_left Int.add 0
    |> Printf.printf "%d\n";
  end
