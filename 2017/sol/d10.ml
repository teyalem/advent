open Ut
open Knot_hash

let validation_number (arr, _, _) = arr.(0) * arr.(1)

let () =
  let data = IO.read_all () |> String.trim in
  (* PART 1 *)
  let lengths =
    data
    |> String.split_on_char ','
    |> List.map String.trim
    |> List.map int_of_string
  in
  let arr = Array.init 256 Fun.id in
  List.fold_left apply (arr, 0, 0) lengths
  |> validation_number
  |> Printf.printf "%d\n";

  (* PART 2 *)
  knot_hash data |> hex |> Printf.printf "%s\n"
