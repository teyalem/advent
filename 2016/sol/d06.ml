open Ut

let collect_chars =
  Seq.unfold (fun ss ->
      let l = List.filter_map Seq.uncons ss in
      if l = [] then None else Some (List.split l))

let hist f cs =
  cs
  |> List.sort Char.compare
  |> group_count
  |> List.sort f
  |> List.hd
  |> fst

let most_common_letter cs =
  hist (fun (_, a) (_, b) -> Int.compare b a) cs

let least_common_letter cs =
  hist (fun (_, a) (_, b) -> Int.compare a b) cs

let () =
  let data = IO.read_lines () |> List.map String.to_seq |> collect_chars in
  begin
    (* PART 1 *)
    data
    |> Seq.map most_common_letter
    |> String.of_seq
    |> Printf.printf "%s\n";

    (* PART 2 *)
    data
    |> Seq.map least_common_letter
    |> String.of_seq
    |> Printf.printf "%s\n";
  end
