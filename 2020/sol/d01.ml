open Ut

(* find two numbers that add up to 2020. *)
let rec find_sum_2020 = function
  | [] -> Error "No Numbers in list"
  | n::ns ->
    let other = List.fold_left
        (fun a b -> if n + b = 2020 then b else a) 0 ns
    in if other <> 0 then Ok (n, other)
    else find_sum_2020 ns

(* generalized version of above. *)
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

let main path =
  let data = open_in path |> IO.input_lines |> List.map int_of_string in
  begin
    (* PART 1 *)
    find_sum_2020 data
    |> Result.get_ok
    |> fun (a, b) -> print_int (a*b);

    print_newline ();

    (* PART 2 *)
    find_sum 2020 3 data
    |> List.fold_left Int.mul 1
    |> print_int

  end

let _ = Arg.parse [] main ""
