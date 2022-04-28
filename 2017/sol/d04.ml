open Ut

let not_same eq ss =
  ss
  |> List.to_seq
  |> Seq.group eq
  |> Seq.exists (fun s -> Seq.length s > 1)
  |> not

let is_valid_part1 ss =
  ss
  |> List.sort String.compare
  |> not_same String.equal

let chars str =
  str
  |> String.to_seq
  |> List.of_seq
  |> List.sort Char.compare

let is_valid_part2 ss =
  ss
  |> List.map chars
  |> List.sort (List.compare Char.compare)
  |> not_same (List.equal Char.equal)

let () =
  let data = IO.read_lines () |> List.map (String.split_on_char ' ') in
  let aux f = List.filter f data |> List.length |> Printf.printf "%d\n" in
  (* PART 1 *)
  aux is_valid_part1;
  aux is_valid_part2
