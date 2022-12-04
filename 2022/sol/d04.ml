open Ut

let parse str =
  Scanf.sscanf str "%d-%d,%d-%d" (fun a b c d -> (a, b), (c, d))

let contains (a, b) (c, d) =
  a <= c && d <= b

let overlap (a, b) (c, d) =
  not (if a < c then b < c else d < a)

let solve pred pairs =
  pairs
  |> List.filter (fun (a, b) -> pred a b)
  |> List.length

let () =
  let data = open_in Sys.argv.(1) |> IO.input_lines |> List.map parse in
  solve (fun a b -> contains a b || contains b a) data |> print_int;
  print_newline ();
  solve overlap data |> print_int
