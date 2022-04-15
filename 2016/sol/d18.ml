open Ut

let next_tile seq =
  match String.of_seq seq with
  | "^^." | ".^^" | "^.." | "..^" -> '^'
  | _ -> '.'

let next_row seq =
  let border = Seq.return '.' in
  [ border; seq; border ]
  |> List.fold_left Seq.append Seq.empty
  |> Seq.windows 3
  |> Seq.map next_tile
  |> Seq.memoize

let count_safe n first_row =
  Seq.iterate next_row first_row
  |> Seq.take n
  |> Seq.concat
  |> Seq.fold_left (fun acc c -> if c = '.' then acc + 1 else acc) 0

let () =
  let data = IO.read_all () |> String.to_seq in
  begin
    (* PART 1 *)
    count_safe 40 data |> Printf.printf "%d\n";

    (* PART 2 *)
    (* SLOW (20s) *)
    count_safe 400_000 data |> Printf.printf "%d\n";
  end
