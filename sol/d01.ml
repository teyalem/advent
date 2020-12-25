open Advent

let needed_fuel mass = mass/3 - 2

let sum_needed_fuel mass =
  let rec loop tmp =
    if tmp < 0
    then []
    else tmp :: (loop @@ needed_fuel tmp)
  in
  sum @@ loop @@ needed_fuel mass

let main path =
  let data = open_in path |> IO.read_lines |> List.map int_of_string in
  begin
    (* PART 1 *)
    List.map needed_fuel data |> sum |> print_int;

    print_newline ();

    (* PART 2 *)
    List.map sum_needed_fuel data |> sum |> print_int;
  end

let () = Arg.parse [] main ""
