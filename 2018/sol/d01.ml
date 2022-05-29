open Ut

let () =
  let data = IO.read_lines () |> List.map int_of_string in
  let sum = List.fold_left Int.add 0 data in
  (* PART 1 *)
  print_int sum;
  print_newline ();

  (* PART 2 *)
  let round = List.to_seq data |> Seq.scan (+) 0 |> Seq.drop 1 |> Seq.memoize in
  let dups =
    round
    |> Seq.filter_map (fun n ->
        Seq.find (fun m -> n <> m && (m - n) mod sum = 0) round
        |> Option.map (fun m -> n, m))
    |> Seq.map (fun (n, m) -> n, m, (m - n) / sum)
    |> Seq.filter (fun (_, _, c) -> c > 0)
  in
  let minc = dups |> Seq.map (fun (_, _, c) -> c) |> Seq.fold_left min max_int in
  dups
  |> Seq.filter (fun (_, _, c) -> c = minc)
  |> Seq.take 1
  |> Seq.iter (fun (_, m, _) -> print_int m)
