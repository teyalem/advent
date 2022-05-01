open Ut

let dir = function (* q, r, s *)
  | "n" -> 0, -1, 1
  | "s" -> 0, 1, -1
  | "nw" -> -1, 0, 1
  | "se" -> 1, 0, -1
  | "ne" -> 1, -1, 0
  | "sw" -> -1, 1, 0
  | _ -> assert false

let add (a, b, c) (d, e, f) = a+d, b+e, c+f

let distance (q, r, s) =
  (abs q + abs r + abs s) / 2

let () =
  let data = IO.read_all () |> String.split_on_char ',' |> List.map dir in
  (* PART 1 *)
  List.fold_left add (0, 0, 0) data
  |> distance
  |> Printf.printf "%d\n";

  (* PART 2 *)
  List.to_seq data
  |> Seq.scan add (0, 0, 0)
  |> Seq.map distance
  |> Seq.fold_left max min_int
  |> Printf.printf "%d\n";
