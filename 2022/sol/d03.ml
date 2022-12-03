open Ut

module CS = Set.Make(Char)

let to_int c =
  if 'a' <= c && c <= 'z' then
    Char.(code c - code 'a' + 1)
  else if 'A' <= c && c <= 'Z' then
    Char.(code c - code 'A' + 27)
  else
    assert false

let split str =
  let open String in
  let l = length str / 2 in
  (sub str 0 l |> to_seq, sub str l l |> to_seq)

let same_item str =
  let a, b = split str in
  let a, b = CS.(of_seq a, of_seq b) in
  CS.(inter a b |> choose)

let solve1 ss =
  List.map same_item ss
  |> List.map to_int
  |> List.fold_left (+) 0

let rec group3 = function
  | [] -> []
  | a::b::c::xs -> [a; b; c] :: group3 xs
  | _ -> assert false

let badge (xs : string list) : char =
  xs
  |> List.map (fun s -> String.to_seq s |> CS.of_seq)
  |> (function x::xs -> List.fold_left CS.inter x xs | [] -> assert false)
  |> CS.choose

let solve2 ss =
  group3 ss
  |> List.map badge
  |> List.map to_int
  |> List.fold_left (+) 0

let () =
  let data = open_in Sys.argv.(1) |> IO.input_lines in
  solve1 data |> print_int;
  print_newline ();
  solve2 data |> print_int
