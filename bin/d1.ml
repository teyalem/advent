(* helper: read number list from in_channel *)
let read_ints_from file =
  Util.read_lines_from file
  |> List.map int_of_string

(* Part one *)

let rec find_sum_2020 = function
  | [] -> Error "No Numbers in list"
  | n::ns ->
    let other = List.fold_left
        (fun a b -> if n + b = 2020 then b else a) 0 ns
    in if other != 0 then Ok (n, other)
    else find_sum_2020 ns

let main1 path =
  let file = open_in path in
  read_ints_from file
  |> find_sum_2020
  |> Result.get_ok
  |> fun (a, b) -> print_int (a*b)

(* Part two *)

let rec find_sum total num = function
  | [] -> []
  | n::ns ->
    if num = 1 then
      if n = total then [n]
      else find_sum total num ns
    else
      match find_sum (total - n) (num - 1) ns with
      | [] -> find_sum total num ns
      | result -> n::result

let main2 path =
  let file = open_in path in
  read_ints_from file
  |> find_sum 2020 3
  |> List.fold_left Int.mul 1
  |> print_int

let _ = Arg.parse [] main2 ""
