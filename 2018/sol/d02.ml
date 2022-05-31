module M = Map.Make(Char)

let inc m c =
  M.update c (function None -> Some 1 | Some n -> Some (n+1)) m

let count_chars =
  String.fold_left inc M.empty

let has_letter n m =
  M.exists (fun _ k -> k = n) m

let cksum ms =
  let f n = List.filter (has_letter n) ms |> List.length in
  f 2 * f 3

let count_diff a b =
  Seq.map2 (fun a b -> if a = b then 0 else 1) a b
  |> Seq.fold_left Int.add 0

let common_letters a b =
  Seq.zip a b
  |> Seq.filter_map (fun (a, b) -> if a = b then Some a else None)

let find_common_letters (boxes : string Seq.t) : string option =
  Seq.product boxes boxes
  |> Seq.filter (fun (a, b) -> a <> b)
  |> Seq.map (fun (a, b) -> String.(to_seq a, to_seq b))
  |> Seq.find (fun (a, b) -> count_diff a b = 1)
  |> Option.map (fun (a, b) -> common_letters a b)
  |> Option.map String.of_seq

let () =
  let data =
    open_in Sys.argv.(1)
    |> In_channel.input_all
    |> String.trim
    |> String.split_on_char '\n'
  in
  (* PART 1 *)
  data |> List.map count_chars |> cksum |> print_int;
  print_newline ();

  (* PART 2 *)
  data |> List.to_seq |> find_common_letters |> Option.iter print_endline;
