open Ut

let parse str =
  Scanf.sscanf str "%d: %d" (fun d r -> d, r)

let scanner_pos range time =
  let rmax = range - 1 in
  rmax - abs (time - rmax) mod (2*range - 2)

let caught =
  List.filter (fun (d, r) -> scanner_pos r d = 0)

let severity =
  List.fold_left (fun acc (d, r) -> acc + d*r) 0

let delay =
  List.fold_left (fun s (d, r) ->
      Seq.filter (fun t -> scanner_pos r (t+d) <> 0) s)
    (Seq.ints 0)

let () =
  let data = IO.read_lines () |> List.map parse in
  (* PART 1 *)
  caught data
  |> severity
  |> Printf.printf "%d\n";

  (* PART 2 *)
  delay data
  |> Seq.take 1
  |> Seq.iter (Printf.printf "%d\n");
