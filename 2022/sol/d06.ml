open Ut

let all_diff xs =
  let xs = List.of_seq xs in
  List.(length xs = length @@ sort_uniq Char.compare xs)

let find_start nchars packet =
  Seq.windows nchars packet
  |> Seq.mapi (fun i x -> i, x)
  |> Seq.find_map (fun (i, xs) -> if all_diff xs then Some i else None)
  |> (fun n -> Option.get n + nchars)

let () =
  let packet = open_in Sys.argv.(1) |> input_line |> String.to_seq in
  find_start 4 packet |> print_int;
  print_newline ();
  find_start 14 packet |> print_int
